survey <- survey %>% 
  mutate(WingSpread = replace(WingSpread, WingSpread==-9, NA),
         DoorSpread = replace(DoorSpread, DoorSpread==-9, NA),
         Speed = replace(Speed, Speed==-9, NA),
         Distance = replace(Distance, Distance==-9, NA),
         Depth = replace(Depth, Depth==-9, NA))

survey <- survey %>% 
  mutate(WingSpread = replace(WingSpread, WingSpread== 0, NA),
         DoorSpread = replace(DoorSpread, DoorSpread== 0, NA),
         Distance = replace(Distance, Distance == 0, NA))

# select only certain gears 
# 1. summary of gears per survey
gears <- data.frame(survey) %>% 
  dplyr::group_by(Survey, Gear) %>% 
  dplyr::summarise(hauls = length(unique(HaulID)), years = length(unique(Year))) %>% 
  select(Survey, Gear, hauls, years)

# 2. only select certain gears per survey (GOV and/or most dominant in cases without GOV)
survey <- survey %>% 
  filter(!(Survey=="NS-IBTS" & Gear %in% c('ABD', 'BOT', 'DHT', 'FOT', 'GRT', 'H18', 'HOB', 'HT', 'KAB', 'VIN')),
         !(Survey=="BITS" & Gear %in% c('CAM', 'CHP', 'DT', 'EGY', 'ESB', 'EXP', 'FOT', 'GRT', 'H20', 'HAK', 'LBT','SON')),
         !(Survey=="PT-IBTS" & Gear=='CAR'),
         !(Survey=="Can-Mar" & Gear=='Y36'))

#################################################################
# Re-estimate the wing/doorspread from linear model per survey
#################################################################

### EVHOE ###
evhoe <- survey %>%
  filter(Survey=='EVHOE') %>%
  select(-TotalNo, -NoMeas, -CatCatchWgt, -LngtCode, -LngtClass, -HLNoAtLngt, -AphiaID) %>%
  distinct()

# doorspread
lm0 <- lm(DoorSpread ~ Depth * SweepLngt , data=evhoe) #; summary(lm0)
pred0 <- predict.lm (object=lm0, newdata=evhoe, interval='confidence', level=0.95)
evhoe$door_fit <- pred0[,1]

# wingpread
evhoe <- evhoe %>% 
  mutate(WingSpread = replace(WingSpread, !(Year %in% c(2016:2018)), NA)) # measurements are made from 2016 and onward

lm0 <- lm(WingSpread ~ DoorSpread * SweepLngt  , data=evhoe) #; summary(lm0)
evhoe[is.na(evhoe$DoorSpread),]$DoorSpread <- evhoe[is.na(evhoe$DoorSpread),]$door_fit
pred0 <- predict.lm (object=lm0, newdata=evhoe, interval='confidence', level=0.95)
evhoe$wing_fit <- pred0[,1]
evhoe[is.na(evhoe$WingSpread),]$WingSpread <- evhoe[is.na(evhoe$WingSpread),]$wing_fit

# combine
evhoe <- evhoe %>%
  select(HaulID, DoorSpread, WingSpread) %>%
  dplyr::rename(DoorSpread2=DoorSpread, WingSpread2=WingSpread)
area2 <- evhoe


### North Sea ###
nsibts <- survey %>%
  filter(Survey=='NS-IBTS',
         !is.na(Depth)) %>%
  select(-TotalNo, -NoMeas, -CatCatchWgt, -LngtCode, -LngtClass, -HLNoAtLngt, -AphiaID) %>%
  distinct()

# doorspread
lm0 <- lm(DoorSpread ~ log(Depth) + SweepLngt + Ship  , data=nsibts) # add ships/sweeplength
lm1 <- lm(DoorSpread ~ log(Depth) + Country , data=nsibts) # use country for hauls that miss sweeplength and/or ships x doorspr. 

addship <- subset(nsibts, nsibts$Ship %in% lm0$xlevels$Ship & nsibts$SweepLngt >0) # select data with ship information + SweepLngt
noship  <- subset(nsibts, !(nsibts$HaulID %in% addship$HaulID)) # select data without ship information

pred0 <- predict(lm0, newdata=addship, interval='confidence', level=0.95) # add prediction to addship
addship$fit <-pred0[,1]
nsibts <- cbind(nsibts, addship[match(nsibts$HaulID,addship$HaulID), c("fit")])
colnames(nsibts)[ncol(nsibts)] <- "door_fit"

pred0 <- predict(lm1, newdata=noship, interval='confidence', level=0.95) # do the same for noship
noship$fit <-pred0[,1]
nsibts <- cbind(nsibts, noship[match(nsibts$HaulID,noship$HaulID), c("fit")])
colnames(nsibts)[ncol(nsibts)] <- "door_fit2"

nsibts <- nsibts %>%
  mutate(door_fit = coalesce(door_fit,door_fit2)) %>%
  select(-door_fit2)  # merge into one, remove door_fit2

# wingpread
nsibts <- nsibts %>% 
  mutate(WingSpread = replace(WingSpread, WingSpread %in% c(50), NA)) # remove one "outlier" at the high end

lm0 <- lm(WingSpread ~ log(Depth) + Country + DoorSpread + SweepLngt, data=nsibts) # add sweeplngt
lm1 <- lm(WingSpread ~ log(Depth) + Country + DoorSpread , data=nsibts) # model for hauls without sweeplngt

nsibts[is.na(nsibts$DoorSpread),]$DoorSpread <- nsibts[is.na(nsibts$DoorSpread),]$door_fit # include the DoorSpread prediction
addship <- subset(nsibts, nsibts$SweepLngt >0) # select data with SweepLngt information
noship  <- subset(nsibts, !(nsibts$HaulID %in% addship$HaulID)) # select data without SweepLngt information

pred0 <- predict(lm0, newdata=addship, interval='confidence', level=0.95) # add prediction to addship
addship$fit <-pred0[,1]
nsibts <- cbind(nsibts, addship[match(nsibts$HaulID,addship$HaulID), c("fit")])
colnames(nsibts)[ncol(nsibts)] <- "wing_fit"

pred0 <- predict(lm1, newdata=noship, interval='confidence', level=0.95) # do the same for noship
noship$fit <-pred0[,1]
nsibts <- cbind(nsibts, noship[match(nsibts$HaulID,noship$HaulID), c("fit")])
colnames(nsibts)[ncol(nsibts)] <- "wing_fit2"

nsibts <- nsibts %>%
  mutate(wing_fit = coalesce(wing_fit,wing_fit2)) %>%
  select(-wing_fit2) # merge into one, remove wing_fit2  

nsibts[is.na(nsibts$WingSpread),]$WingSpread <- nsibts[is.na(nsibts$WingSpread),]$wing_fit

# combine
nsibts <- nsibts %>%
  select(HaulID, DoorSpread, WingSpread) %>%
  dplyr::rename(DoorSpread2=DoorSpread, WingSpread2=WingSpread)
area2 <- rbind(nsibts, area2)


### SWC-IBTS ###
swc <- survey %>%
  filter(Survey=='SWC-IBTS') %>%
  select(-TotalNo, -NoMeas, -CatCatchWgt, -LngtCode, -LngtClass, -HLNoAtLngt, -AphiaID) %>%
  distinct()

# doorspread 
swc$SweepLngt <- as.numeric(swc$SweepLngt)  # two hauls NA sweeplength
swc <- swc  %>%
  mutate(SweepLngt = if_else(is.na(SweepLngt), 60, SweepLngt))#  (mean(swc$SweepLngt[swc$Ship == "749S"],na.rm = T) =60)

lm0 <- lm(DoorSpread ~ log(Depth) + SweepLngt , data=swc)
pred0 <- predict.lm (object=lm0, newdata=swc, interval='confidence', level=0.95)
swc$door_fit <- pred0[,1]

# wingpread
lm0 <- lm(WingSpread ~ log(Depth) + DoorSpread , data=swc)
swc[is.na(swc$DoorSpread),]$DoorSpread <- swc[is.na(swc$DoorSpread),]$door_fit
pred0 <- predict.lm (object=lm0, newdata=swc, interval='confidence', level=0.95)
swc$wing_fit <- pred0[,1]
swc[is.na(swc$WingSpread),]$WingSpread <- swc[is.na(swc$WingSpread),]$wing_fit

# combine
swc <- swc %>%
  select(HaulID, DoorSpread, WingSpread) %>%
  dplyr::rename(DoorSpread2=DoorSpread, WingSpread2=WingSpread)
area2 <- rbind(swc, area2)


### BITS ###
bits <- survey %>%
  filter(Survey=='BITS') %>%
  select(-TotalNo, -NoMeas, -CatCatchWgt, -LngtCode, -LngtClass, -HLNoAtLngt, -AphiaID) %>%
  distinct()

# doorspread
bits <- bits %>% 
  mutate(DoorSpread = replace(DoorSpread, DoorSpread >200, NA)) # remove two outliers

lm0 <- lm(DoorSpread ~ log(Depth) + Country + Gear, data=bits) 
lm1 <- lm(DoorSpread ~ log(Depth) + Gear , data=bits) # no country

addcountry <- subset(bits, bits$Country %in% lm0$xlevels$Country) # select data with country x doorspread information
nocountry <- subset(bits, !(bits$Country %in% lm0$xlevels$Country)) # select data without country x doorspread information

pred0 <- predict(lm0, newdata=addcountry, interval='confidence', level=0.95) # add prediction to addcountry
addcountry$fit <-pred0[,1]
bits <- cbind(bits, addcountry[match(bits$HaulID,addcountry$HaulID), c("fit")])
colnames(bits)[ncol(bits)] <- "door_fit"

pred0 <- predict(lm1, newdata=nocountry, interval='confidence', level=0.95) # do the same for nocountry
nocountry$fit <-pred0[,1]
bits <- cbind(bits, nocountry[match(bits$HaulID,nocountry$HaulID), c("fit")])
colnames(bits)[ncol(bits)] <- "door_fit2"

bits <- bits %>%
  mutate(door_fit = coalesce(door_fit,door_fit2)) %>%
  select(-door_fit2)  # merge into one, remove door_fit2  

# wingspread
lm0 <- lm(WingSpread ~ DoorSpread, data=bits) 
bits[is.na(bits$DoorSpread),]$DoorSpread <- bits[is.na(bits$DoorSpread),]$door_fit
pred0 <- predict.lm (object=lm0, newdata=bits, interval='confidence', level=0.95)
bits$wing_fit <- pred0[,1]
bits[is.na(bits$WingSpread),]$WingSpread <- bits[is.na(bits$WingSpread),]$wing_fit

# combine
bits <- bits %>%
  select(HaulID, DoorSpread, WingSpread) %>%
  dplyr::rename(DoorSpread2=DoorSpread, WingSpread2=WingSpread)
area2 <- rbind(bits, area2)


### IE-IGFS ###
ie <- survey %>%
  filter(Survey=='IE-IGFS',
         Year>1989) %>%
  select(-TotalNo, -NoMeas, -CatCatchWgt, -LngtCode, -LngtClass, -HLNoAtLngt, -AphiaID) %>%
  distinct()

# doorspread
lm0 <- lm(DoorSpread ~ log(Depth) + SweepLngt , data=ie)
pred0 <- predict.lm (object=lm0, newdata=ie, interval='confidence', level=0.95)
ie$door_fit <- pred0[,1]

# wingspread
lm0 <- lm(WingSpread ~ DoorSpread + SweepLngt, data=ie) 
ie[is.na(ie$DoorSpread),]$DoorSpread <- ie[is.na(ie$DoorSpread),]$door_fit
pred0 <- predict.lm (object=lm0, newdata=ie, interval='confidence', level=0.95)
ie$wing_fit <- pred0[,1]
ie[is.na(ie$WingSpread),]$WingSpread <- ie[is.na(ie$WingSpread),]$wing_fit

# combine
ie <- ie %>%
  select(HaulID, DoorSpread, WingSpread) %>%
  dplyr::rename(DoorSpread2=DoorSpread, WingSpread2=WingSpread)
area2 <- rbind(ie, area2)


### FR-CGFS ###
cgfs <- survey %>%
  filter(Survey=='FR-CGFS') %>%
  select(-TotalNo, -NoMeas, -CatCatchWgt, -LngtCode, -LngtClass, -HLNoAtLngt, -AphiaID) %>%
  distinct()

# doorspread
lm0 <- lm(DoorSpread ~ log(Depth) , data=cgfs)
pred0 <- predict.lm (object=lm0, newdata=cgfs, interval='confidence', level=0.95)
cgfs$door_fit <- pred0[,1]

# wingspread
cgfs <- cgfs %>% 
  mutate(WingSpread = replace(WingSpread, WingSpread %in% c(10), NA)) # remove fixed number in 1994

lm0 <- lm(WingSpread ~ DoorSpread , data=cgfs) 
cgfs[is.na(cgfs$DoorSpread),]$DoorSpread <- cgfs[is.na(cgfs$DoorSpread),]$door_fit
pred0 <- predict.lm (object=lm0, newdata=cgfs, interval='confidence', level=0.95)
cgfs$wing_fit <- pred0[,1]
cgfs[is.na(cgfs$WingSpread),]$WingSpread <- cgfs[is.na(cgfs$WingSpread),]$wing_fit

# combine
cgfs <- cgfs %>%
  select(HaulID, DoorSpread, WingSpread) %>%
  dplyr::rename(DoorSpread2=DoorSpread, WingSpread2=WingSpread)
area2 <- rbind(cgfs, area2)


### NIGFS ###
nigfs <- survey %>%
  filter(Survey=='NIGFS') %>%
  select(-TotalNo, -NoMeas, -CatCatchWgt, -LngtCode, -LngtClass, -HLNoAtLngt, -AphiaID) %>%
  distinct()

# doorspread
lm0 <- lm(DoorSpread ~ log(Depth) , data=nigfs)
pred0 <- predict.lm (object=lm0, newdata=nigfs, interval='confidence', level=0.95)
nigfs$door_fit <- pred0[,1]

# wingspread
lm0 <- lm(WingSpread ~ DoorSpread , data=nigfs) 
nigfs[is.na(nigfs$DoorSpread),]$DoorSpread <- nigfs[is.na(nigfs$DoorSpread),]$door_fit
pred0 <- predict.lm (object=lm0, newdata=nigfs, interval='confidence', level=0.95)
nigfs$wing_fit <- pred0[,1]
nigfs[is.na(nigfs$WingSpread),]$WingSpread <- nigfs[is.na(nigfs$WingSpread),]$wing_fit

# combine
nigfs <- nigfs %>%
  select(HaulID, DoorSpread, WingSpread) %>%
  dplyr::rename(DoorSpread2=DoorSpread, WingSpread2=WingSpread)
area2 <- rbind(nigfs, area2)


### ROCKALL ###
rock <- survey %>%
  filter(Survey=='ROCKALL') %>%
  select(-TotalNo, -NoMeas, -CatCatchWgt, -LngtCode, -LngtClass, -HLNoAtLngt, -AphiaID) %>%
  distinct()

# doorspread
lm0 <- lm(DoorSpread ~ log(Depth) + SweepLngt , data=rock) 
pred0 <- predict.lm (object=lm0, newdata=rock, interval='confidence', level=0.95)
rock$door_fit <- pred0[,1]

# wingspread
lm0 <- lm(WingSpread ~ DoorSpread + SweepLngt , data=rock) 
rock[is.na(rock$DoorSpread),]$DoorSpread <- rock[is.na(rock$DoorSpread),]$door_fit
pred0 <- predict.lm (object=lm0, newdata=rock, interval='confidence', level=0.95)
rock$wing_fit <- pred0[,1]
rock[is.na(rock$WingSpread),]$WingSpread <- rock[is.na(rock$WingSpread),]$wing_fit

# combine
rock <- rock %>%
  select(HaulID, DoorSpread, WingSpread) %>%
  dplyr::rename(DoorSpread2=DoorSpread, WingSpread2=WingSpread)
area2 <- rbind(rock, area2)


### PORTUGAL ###
pt <- survey %>%
  filter(Survey=='PT-IBTS') %>%
  select(-TotalNo, -NoMeas, -CatCatchWgt, -LngtCode, -LngtClass, -HLNoAtLngt, -AphiaID) %>%
  distinct()

# no doorspread information

# wingspread
pt <- pt %>% 
  mutate(WingSpread = replace(WingSpread, WingSpread>20, NA)) # remove at high end

pt$cat <- "shallow"
pt$cat[pt$Depth > 120] <- "deep" # seems to be a break-point when plotting (wingspread~depth)

lm0 <- lm(WingSpread ~ Depth* cat , data=pt) 
pred0 <- predict.lm (object=lm0, newdata=pt, interval='confidence', level=0.95)
pt$wing_fit <- pred0[,1]
pt[is.na(pt$WingSpread),]$WingSpread <- pt[is.na(pt$WingSpread),]$wing_fit

pt$DoorSpread <- pt$WingSpread / 0.3 # doorspread probably not needed, rough estimate

# combine
pt <- pt %>%
  select(HaulID, DoorSpread, WingSpread) %>%
  dplyr::rename(DoorSpread2=DoorSpread, WingSpread2=WingSpread)
area2 <- rbind(pt, area2)


### Can-mar ###
cmar <- survey %>%
  filter(Survey=='Can-Mar') %>%
  select(-TotalNo, -NoMeas, -CatCatchWgt, -LngtCode, -LngtClass, -HLNoAtLngt, -AphiaID) %>%
  distinct()

# doorspread, wingspread information fixed, no changes

# combine
cmar <- cmar %>%
  select(HaulID, DoorSpread, WingSpread) %>%
  dplyr::rename(DoorSpread2=DoorSpread, WingSpread2=WingSpread)
area2 <- rbind(cmar, area2)

rm(bits, cgfs, ie, nsibts, pt, nigfs, pred0, lm0, lm1, evhoe, swc, rock, cmar,
   addship,noship, addcountry ,nocountry)

# Paste new estimates to survey data frame
area2 <- area2 %>% distinct()
survey0 <- left_join(survey, area2, by='HaulID')
survey0 <- survey0 %>%
  mutate(DoorSpread = coalesce(DoorSpread, DoorSpread2),
         WingSpread = coalesce(WingSpread, WingSpread2)) %>%
  select(-DoorSpread2, -WingSpread2)

# now get area.swept
dist <- survey0 %>%
  select(HaulID, Survey,Year,Ship, Country,Distance,Speed,HaulDur) %>%
  distinct()

#plot(dist$Distance,dist$Speed*1.852*dist$HaulDur/60*1000)
dist$Distance[dist$Distance >11000] <- NA # remove Distances at high end (seem wrong, see plot)
survey0$Distance[survey0$Distance > 11000] <- NA # seems wrong so also in survey data
dist$Speed[dist$Speed >30] <- NA # remove strande speeds
dist$Distance2 <- dist$Speed*1.852*dist$HaulDur/60*1000 # calculate 2nd distance

dist <- dist %>%
  mutate(Distance = coalesce(Distance, Distance2))%>%
  select(-Distance2) 

# NAs remaining missing speeds  
# take average speed per survey, year, ship
avgspeed <- aggregate(dist$Speed, by=list(dist$Survey, dist$Year, dist$Ship), 
                      FUN = mean, na.rm=T) 
colnames(avgspeed) <- c("Survey", "Year", "Ship","Speed2")

dist <- left_join(dist, avgspeed, by=c("Survey", "Year", "Ship"))
dist <- dist %>%
  mutate(Speed = coalesce(Speed, Speed2))%>%
  select(-Speed2) 

# take average speed per survey, year, country
avgspeed <- aggregate(dist$Speed, by=list(dist$Survey, dist$Year, dist$Country), 
                      FUN = mean, na.rm=T) 
colnames(avgspeed) <- c("Survey", "Year", "Country","Speed2")

dist <- left_join(dist, avgspeed, by=c("Survey", "Year", "Country"))
dist <- dist %>%
  mutate(Speed = coalesce(Speed, Speed2))%>%
  select(-Speed2) 

# take average speed per survey, country
avgspeed <- aggregate(dist$Speed, by=list(dist$Survey, dist$Country), 
                      FUN = mean, na.rm=T)
colnames(avgspeed) <- c("Survey", "Country", "Speed2")

dist <- left_join(dist, avgspeed, by=c("Survey",  "Country"))
dist <- dist %>%
  mutate(Speed = coalesce(Speed, Speed2))%>%
  select(-Speed2) 

# take average speed
dist$Speed2 <- mean(dist$Speed,na.rm=T)
dist <- dist %>%
  mutate(Speed = coalesce(Speed, Speed2))%>%
  select(-Speed2) 

dist$Distance2 <- dist$Speed*1.852*dist$HaulDur/60*1000 # calculate 2nd distance

dist <- dist %>%
  mutate(Distance_pred = coalesce(Distance, Distance2))%>%
  select(-Distance2,-Distance,-Speed,-HaulDur,-Survey,-Year,-Ship, -Country) 

survey0 <- left_join(survey0, dist, by=c("HaulID"))

survey0 <- survey0 %>%
  mutate(Distance = coalesce(Distance, Distance_pred)) %>%
  select(-Distance_pred)

survey0 <- survey0 %>% 
  mutate(Area.swept = Distance*0.001*WingSpread*0.001,
         Area.doors = Distance*0.001*DoorSpread*0.001)

survey <- cbind(survey, survey0[match(survey$HaulID,survey0$HaulID), c("Area.swept","Area.doors")])


rm(dist,gears,survey0,avgspeed,area2)