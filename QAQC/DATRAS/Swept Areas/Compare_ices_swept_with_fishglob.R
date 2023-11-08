
setwd("....")
tr <- read.table("SweptAreaAssessmentOutput_20213009.csv.txt",sep=",",header=T)
fglob <- read.csv("areas_swept_datras_fishglob.csv")

# match data
tr$uni <- paste(tr$Survey, tr$Year,tr$Quarter,tr$Country,tr$Ship,tr$Gear,tr$StNo, tr$HaulNo)
tr     <- cbind(tr, fglob[match(tr$uni,fglob$HaulID), c("Area.swept")])
colnames(tr)[ncol(tr)] <- "swept_fishglob"

# remove all NA's and zero's
tr <- subset(tr , tr$swept_fishglob >0 )
tr <- subset(tr , tr$SweptAreaWSKM2 >0 )

library(ggplot2)
ggplot() + geom_point(data=tr, aes(SweptAreaWSKM2,swept_fishglob),col="lightblue")+
  facet_wrap(~ Survey,ncol=3) +
  geom_abline(intercept = 0, slope = 1) + theme_bw()

library(data.table)
dt <- data.table(tr)
dt[, .(mCor = cor(SweptAreaWSKM2,swept_fishglob)), by=Survey]

# Survey      mCor
# 1:  SP-PORC 0.9598433
# 2:  NS-IBTS 0.9438436
# 3:  PT-IBTS 0.9259785
# 4:  FR-CGFS 0.8519028
# 5:  IE-IGFS 0.9889950
# 6:    NIGFS 0.9896492
# 7:  ROCKALL 0.9984515
# 8:   SCOROC 1.0000000
# 9:  SP-ARSA 0.9920043
# 10: SP-NORTH 0.8742710
# 11: SWC-IBTS 0.9843520
# 12: SCOWCGFS 0.9998731
# 13:     BITS 0.2612501
# 14:    EVHOE 0.8991387


