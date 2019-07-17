#cat r_processDeliveryData.R | R --slave
cat("Running r_processDeliveryData.R\n")
library(tidyverse)
library(data.table)

#Get all the files
dfold <- "/home/esa/production_forecasts/data/FACS/"
files <- list.files(path = dfold, pattern = "FAC0091_*")

opath <- '/home/esa/production_forecasts/results/FACS/'

s <- matrix(nrow = length(files), ncol = 2)
s[, 1] <- files
rownames(s) <- files 
for (i in files) {
#for (i in files[1]) {
#  i<- "FAC0091_201907080511122.dat"
    d <- read.delim(file = paste0(dfold, "/", i), header = FALSE, sep = ";", stringsAsFactors = FALSE, colClasses = 'character')
    s[i, 2] <- ncol(d)
    d[d$V1 == 'P', 'V4'] <- NA
    d[d$V1 == 'P', 'V5'] <- NA
    d[d$V1 == 'P', 'V6'] <- NA
    d[d$V1 == 'P', 'V7'] <- NA
    d[d$V1 == 'P', 'V8'] <- NA
    if (ncol(d) == 25) {
      d[d$V1 == 'P', 'V14'] <- NA
      d[d$V1 == 'P', 'V15'] <- NA
      
    } else if (ncol(d) == 26) {
    d[d$V1 == 'P', 'V15'] <- NA
    d[d$V1 == 'P', 'V16'] <- NA
    } else {
      stop(ncol(d))
    }
    d[d$V1 == 'R', 'V5'] <- NA
    d[d$V1 == 'R', 'V12'] <- NA
    
    
    
    ofile <- paste0(opath, "/", i)
    write.table(d, file = ofile, col.names = FALSE, row.names = FALSE, sep = ";", quote = FALSE)    
}


