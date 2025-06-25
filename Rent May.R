library(dplyr)
setwd("C:/Users/arntj/OneDrive/文件/RDir/Rent")
p <- read.table("For-R.csv", header = T, sep = ",")
dates <- p[,1]
p <- p[,2:9]
row.names(p) <- dates
ubill <- 133.86
ufirst <- which(row.names(p) == "6-Apr")
ulast <- which(row.names(p) == "5-May")
gbill <- 27.81
gfirst <- which(row.names(p) == "21-Apr")
glast <- which(row.names(p) == "18-May")
ibill <- 49.95
ifirst <- which(row.names(p) == "20-Apr")
ilast <- which(row.names(p) == "19-May")
rfirst <- which(row.names(p) == "1-May")
rlast <- which(row.names(p) == "31-May")
nadays <- data.frame(Rob = 0, Jesse = 0, Luis.1 = 31, Luis.2 = 31, Luis.3 = 31, Zach = 0, Mike = 2, Basil = 29)
rbase <- data.frame(Rob = 595, Jesse = 712.50, Luis.1 = 0, Luis.2 = 0, Luis.3 = 0, Zach = 0, Mike = 895, Basil = 750)
pprop <- p/rowSums(p,na.rm = T)
udays <- ulast-ufirst+1
uperday <- ubill/udays
uprop <- uperday*pprop[ufirst:ulast,]
utot <- colSums(uprop, na.rm = T)
gdays <- glast-gfirst+1
gperday <- gbill/gdays
gprop <- gperday*pprop[gfirst:glast,]
gtot <- colSums(gprop, na.rm = T)
idays <- ilast-ifirst+1
iperday <- ibill/idays
iprop <- iperday*pprop[ifirst:ilast,]
itot <- colSums(iprop, na.rm = T)
rdays <- rlast-rfirst+1
plastmonth <- p[rfirst:rlast,]
daysaway <- colSums(plastmonth == 0, na.rm = T)
radj <-rbase/rdays*1/2*-daysaway
rna <- colSums(is.na(plastmonth))*rbase/rdays*(-1)
tots <- rbind(utot, gtot, itot, rbase, radj, rna)
row.names(tots) <- c("util", "gas", "inet", "rent", "radj", "rna")
sumtots <- tots %>% colSums() %>% round(2)
print(sumtots)
