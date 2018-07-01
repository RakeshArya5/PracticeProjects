#ter_ind_analysis
#libraries
library(ggplot2)
library(ggmap)
# 1.Input dataset: Input complete dataset
# set working directory
setwd("~/RCodes")
ter.data <- read.csv("ter_act_ind.csv")
str(ter.data)

#filling all blank values with NA
ter.data[ter.data==""] <- NA

# Number of attacks histogram
ggplot(ter.data, aes(x = iyear, fill = attacktype1_txt )) + geom_histogram()+
  labs(x="Year", y = "# of Attacks", fill ="Attack Type", title =  "Attacks Histogram") +
  theme(plot.title = element_text(hjust = 0.5) )

    
#qplot(iyear, data = ter.data, geom = "histogram", fill = attacktype1_txt, xlab = "Year", ylab = "#Attacks", main = " No of Attacked in Years") #okay
#qplot(attacktype1_txt, data = ter.data, geom = "density", fill = attacktype1_txt) #okay


#The imported data has months in numbers
# adding month col with month name

df <- data.frame(month = c('jan','feb','mar','apr','may','jun','jul','aug','sep','oct','nov','dec'))

for ( i in 1:nrow(ter.data)){
  ter.data$month[i] <- as.character(df$month[ter.data$imonth[i]])
}
#above code is working correctly- the dataset is populated month as text

# Frequency of attacks in months with attack types
ggplot(data = ter.data) + geom_bar(mapping = aes(x = month, fill = attacktype1_txt)) + 
  labs(x="Month", y = "# of Attacks", fill ="Attack Type", title =  "Month-Wise Attacks") +
  theme(plot.title = element_text(hjust = 0.5) )


# Frequency of attacks in months with weapon types
ggplot(data = ter.data) + geom_bar(mapping = aes(x = month, fill = weaptype1_txt)) + 
  labs(x="Month", y = "# of Attacks", fill ="Weapon Type", title =  "Month-Wise Attacks (Weapons)") +
  theme(plot.title = element_text(hjust = 0.5) )

#to check which month has maximum number of activities
ter.data.month <- aggregate(eventid~month, data = ter.data, length)
str(ter.data.month)

#create summary table klngs per year
ter.data.summ <- aggregate(nkill~iyear, data = ter.data, sum)
ggplot(ter.data.summ, aes(x = iyear, y=nkill)) + geom_smooth( se = FALSE)

#find the numbers of activites in yearwise
ter.data.summ1 <- aggregate(eventid~iyear, data = ter.data, length)

#adding count column in main data set
ter.data.summ$count <- ter.data.summ1$eventid

summary (ter.data.summ)
#Some visualization in R

#frequecy table showing the number of events
hist(ter.data.summ$count, col= "blue", main = "")
#number of kilngs
hist(ter.data.summ$nkill, col= "red", main = "")

#showing the terrorist activities and number of people killed- simple smooth curve
ggplot(ter.data.summ, aes(x = iyear, y=nkill)) + geom_smooth()

#with fill # but it not giving any result
ggplot(ter.data.summ, aes(x = iyear, y=nkill, fill = count)) + geom_smooth()

#linear growht in the terrorist activities
ggplot(ter.data.summ, aes(x = iyear, y=nkill)) + geom_smooth(method = lm)


# 2. Taking working data from 2010 to 2016
# takaing only some colums for anaysis
ter.data.latest <- ter.data[c(2:4,12:15, 30,36, 59, 83, 99)]
str(ter.data.latest)

#Selecting data only for 2010 and above
ter.data.latest.2009 <- ter.data.latest[ter.data.latest$iyear>2009,] 
str(ter.data.latest.2009)

#checking data
summary(ter.data.latest.2009$iyear) # will show data since 2009

# 3. Top prov/state with ter activities
states.summ <- aggregate(nkill~provstate, data = ter.data.latest.2009, sum)
str(states.summ)

# Take all state where no of kill > 0
states.summ <- states.summ[states.summ$nkill > 0,]

#ordering states on the basil of killings

attach(states.summ)
states.summ <- states.summ[order(-nkill),]
detach(states.summ)

# take only top 10 states 
top.ten.states <- states.summ[1:10,]
str(top.ten.states)

#converting states/provinces into character
top.ten.states$provstate <- as.character(top.ten.states$provstate)

# 4. Show top 10 states on the map
# first task is to get the geocodes on all the 10 locations
#library(ggplot2)
#install.packages("ggplot2")
#install.packages("ggmap")


# getting the longitides and lattitudes of all the locations
# in this process there will be a few states that will not get co ordinates
# you need to run this code again and again to get the minimum number for missing values
# then fill the missing coordicates using the geocode function 
for (i in 1:nrow(top.ten.states)) {
  latlon = geocode(top.ten.states$provstate[i])
  top.ten.states$lon[i] = as.numeric(latlon[1])
  top.ten.states$lat[i] = as.numeric(latlon[2])
}
# 5. Fill the top ten states in the map
top.ten.states

India_center = geocode("India")
India <-ggmap(get_map(location=India_center,zoom=4), extent="panel")
#taking the circle scale to make the scale of the circle fit in the map
#if we dont do the same the the value 700+ will cover the whole screen

circle_scale <- .03
India + geom_point(aes(x=lon, y=lat), data=top.ten.states, col="red",
                 alpha=0.4, size=top.ten.states$nkill*circle_scale)



