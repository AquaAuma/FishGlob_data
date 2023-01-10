#' Do taxonomic check per region
#' A species is flagged if:
#' (1) it has periods of absence or presence lasting less than 95% of the years and
#' (2) it has less than 4 periods of presence vs periods of absence
#' outputs per region: 
#' -a list of flagged species (txt file) 
#' -statistics of flagged species (csv file)
#' -a plot of flagged species vs years indicating presence / absence
#' code adapted from https://github.com/pinskylab/OceanAdapt/blob/master/R/add-spp-to-taxonomy.Rmd#L33
#' @param df fishglob dataset
#' @param region name of region as per survey column
#'
#' @return
#' @export
#'
flag_spp <- function(df, region){
  
  cat("taxonomy check for region: ", region, sep="\n")
  
  #select region (survey column)
  df %>%
    dplyr::mutate(survey = forcats::as_factor(survey)) %>%
    dplyr::filter(survey == region) -> df_region
  
  #get grouped species names per year
  df_region %>%
    dplyr::select(accepted_name, year) %>%
    dplyr::group_by(accepted_name) %>%
    dplyr::distinct() -> df
  
  #make species x year matrix
  years <- unique(df$year)
  df2 <- data.frame(matrix(ncol = length(years)+1, nrow = length(df$accepted_name)))
  x <- c('spp', unique(df$year))
  colnames(df2) <- x
  df2$spp <- df$accepted_name
  
  #fill up matrix with FALSE/TRUE depending on species presence/absence
  n <- 2
  for (y in years){
    df2[,n] <- c(apply(df, 1, function(r) any(r %in% c(y))))
    n <- n+1
  }
  
  #summarize
  df2 <- df2 %>%
    dplyr::group_by(spp) %>%
    dplyr::summarize_all(any)
  
  #create new dataframe
  df3 <- data.frame(matrix(ncol = 3, nrow = length(df2$spp)))
  colnames(df3) <- c('spp', 'rlmax', 'PresToAbs')
  
  #fill up the dataframe
  for (n in 1:length(df2$spp)){
    
    #Compute the lengths and values of runs of equal values in a vector
    r <- rle(df2[n,])
    
    sp <- c(df2[n,1])
    
    rlmax <- max(r$lengths)
    
    df3[n,1] <- sp
    df3[n,2] <- rlmax #maximum run length
    df3[n,3] <- length(r$values) #total nb of runs
    
  }
  
  #get species with maximum run length lower than 95% of the years (56)
  #and that occurred in less than 4 runs
  x <- round(0.95*length(years))
  df3 <- df3[df3$rlmax < x,]
  df3 <- df3[df3$PresToAbs < 4,]
  
  testdf <- data.frame(spp = df3$spp)
  
  if (dim(testdf)[1] > 0) {
    
    #write txt of flagged spp in region
    sink(here::here("outputs", "Flags", "taxonomic_flagging", paste0(region, "_flagspp.txt")))
    cat(testdf$spp, sep = " ; ")
    sink()
    
    #write stats of species flagged
    data <- data.frame("name" = c("Total number of species", "Percentage of species flagged"),
                       "nb" = c(length(df2$spp), round(100*(length(testdf$spp) /  length(df2$spp)), 1)))
    write.csv(data, file = here::here("outputs", "Flags", "taxonomic_flagging", paste0(region, "_stats.csv")), row.names = FALSE)
    
    #create absence dataframe for plotting
    allyears <- min(years):max(years)
    abs <- data.frame("accepted_name" = rep(df3$spp, each = length(allyears)),
                      "year" = rep(allyears, length(df3$spp)))
    
    #get presence dataframe for plotting
    pres <- subset(df_region, accepted_name %in% testdf$spp) %>%
      dplyr::select(accepted_name, year)
    
    #make summary scatterplot for region
    p <- ggplot2::ggplot() +
      #absences
      ggplot2::geom_point(data = abs, ggplot2::aes(x = year, y = accepted_name, color = "black"), alpha = 0.2) +
      #presences
      ggplot2::geom_point(data = pres, ggplot2::aes(x = year, y = accepted_name, fill = "black")) +
      ggplot2::ggtitle(paste0("Region: ", region)) +
      ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5),
                     axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust=1, size = 9),
                     legend.position = "bottom") +
      ggplot2::scale_fill_manual(values=c('black'), labels = c("presence"), name = "") +
      ggplot2::scale_colour_manual(values=c('black'), labels = c("absence"), name = "") +
      ggplot2::scale_x_continuous(breaks = allyears)
    
    ggplot2::ggsave(p, filename = here::here("outputs", "Flags", "taxonomic_flagging", paste0(region, "_taxonomic_flagging.png")))
    
  }else{
    
    print("----- No species flagged for this region")
  }
  
  
}



