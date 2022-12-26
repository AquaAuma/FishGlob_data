cleanspl <- function(name){
  temp <- paste0(toupper(substr(name, 1, 1)), tolower(substr(name, 2, nchar(name))))
  temp <- gsub("\\((.+?)\\)$", "", temp)
  temp <- gsub("\\.$", "", temp) #remove last .
  temp <- gsub(" $", "", temp) #remove empty space
  temp <- gsub("            $", "", temp) #remove empty spaces
  temp <- gsub("^ ", "", temp)
  temp <- gsub(" .$", "", temp) #remove last single character
  temp <- gsub(" .$$", "", temp) #remove last double characters
  temp <- gsub(" unid$", "", temp)
  temp <- gsub(" unident$", "", temp)
  temp <- gsub(" spp$", "", temp)
  temp <- gsub(" spp.$", "", temp)
  temp <- gsub(" sp$", "", temp)
  temp <- gsub(" sp.$", "", temp)
  temp <- gsub(" s.c$", "", temp)
  temp <- gsub(" s.p$", "", temp)
  temp <- gsub(" s.f$", "", temp)
  temp <- gsub(" s.o$", "", temp)
  temp <- gsub(" so$", "", temp)
  temp <- gsub(" yoy$", "", temp)
  temp <- gsub(" YOY$", "", temp)
  temp <- gsub(" n. gen$", "", temp)
  temp <- gsub(" aff.$", "", temp)
  temp <- gsub(" kroyer", "", temp)
}