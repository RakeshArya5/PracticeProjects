
#plot function 
dr <- read.csv("C:/Users/Admin/Desktop/countries.csv")
str(dr)
#plot (dr$PClass, dr$Age)
par(col = "orange")
plot(dr$Region, dr$IncomeGroup)

install.packages("HistData")
library(HistData)
library(RColorBrewer)

data("VADeaths")
par(mfrow=c(2,3))

hist(VADeaths, breaks = 40, col = brewer.pal(4,"Set3"), main = "Set 3 Colors")
hist(VADeaths, col = brewer.pal(4,"Set3"))
VADeaths
str(VADeaths)
par(mfrow=c(2,3))
hist(VADeaths, breaks = 10, col = brewer.pal(4,"Set3"), main = "Set3 3 Colors")
hist(VADeaths, breaks = 3, col = brewer.pal(3, "Set2"), main = "Set2 3 Colors")
hist(VADeaths, breaks = 7, col = brewer.pal(3, "Set1"), main = "Set1 3 Colors")
hist(VADeaths, breaks= 2, col=brewer.pal(8,"Set3"),main="Set3 8 colors")
hist(VADeaths,col=brewer.pal(8,"Greys"),main="Greys 8 colors")
hist(VADeaths,col=brewer.pal(8,"Greens"),main="Greens 8 colors")

AirPassengers
par(mfrow=c(1,1))
plot(AirPassengers,type="o")
