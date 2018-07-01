#UP Election Data Analysis
library(readr)
library(dplyr)
constituency = read.csv("C:/Users/Admin/Desktop/Blog - ZoomInside/UP Elections/constituencyCSV.csv")
const = read.csv("constituencyCSV.csv")
str(const)
det_res <- read.csv("detailedresults.csv")
str(detail_result)

nrow(filter(const, win_party == 'BJP'))
const$win_party
