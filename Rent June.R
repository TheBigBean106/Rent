library(dplyr)
setwd("C:/Users/arntj/OneDrive/文件/RDir/Rent")
p <- read.table("For-R.csv", header = T, sep = ",") #present: 1, absent: 0, no longer/not yet living here: NA
dates <- p[,1] #create a vector populated with contents of the first column of p
p <- p[,2:9] #only reading column 2 to 9 to get rid of the dates  
row.names(p) <- dates #change the row name from numbers to dates
ubill <- 163.97-13.74 #changes every month
ufirst <- which(row.names(p) == "6-May") #changes every month
ulast <- which(row.names(p) == "5-Jun") #changes every month
gbill <- 21.28 #changes every month
gfirst <- which(row.names(p) == "19-May") #changes every month
glast <- which(row.names(p) == "18-Jun") #changes every month
ibill <- 49.95 #changes every month
ifirst <- which(row.names(p) == "20-May") #changes every month
ilast <- which(row.names(p) == "19-Jun") #changes every month
rfirst <- which(row.names(p) == "1-Jun") #changes every month
rlast <- which(row.names(p) == "30-Jun") #changes every month
rbase <- data.frame(Rob = 595, Jesse = 712.50, Luis.1 = 0, Luis.2 = 0, Luis.3 = 0, Zach = 0, Mike = 0, Basil = 750) #data.frame is a table
pprop <- p/rowSums(p,na.rm = T) #prop means proportion, na.rm means remove na
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
plastmonth <- p[rfirst:rlast,] #table of attendance last month
daysaway <- colSums(plastmonth == 0, na.rm = T) #calculates how many 0s there are in plastmonth
radj <-rbase/rdays*1/2*(-daysaway)
rna <- colSums(is.na(plastmonth))*rbase/rdays*(-1) #calculate discounts for new tenants and tenants moving out
tot <- rbind(utot, gtot, itot, rbase, radj, rna) #combines several vectors into a data.frame (table)
row.names(tot) <- c("util", "gas", "inet", "rent", "radj", "rna")
sumtot <- tot %>% colSums() %>% round(2) # %>% means take the preceding object and do what follows
print(sumtot)
