#Exploratory Analysis
setwd("~/RCodes")

# Data Source Name: globalterrorismdb_0617dist

# Necessary packages to load in 
library(tidyverse)
library(ggplot2) # Data visualizationc
library(dplyr)
options(dplyr.width=Inf)
library(readr) # Read_csv function
library(ggmap)

# Get all data
#library(tidyverse)
gtd = read.csv("globalterrorismdb_0617dist.csv")



#getting subset of India
india.db <- gtd %>% filter (country_txt == "India")
str(india.db)
# Data Cleaning


#filling all blank values with NA
india.db[india.db==""] <- NA

# Select columns that will use in EDA and renaming selected columns. We are selecting 27 variables
india.db = select (india.db , eventid, year = iyear, month = imonth, day=iday, country_code = country, 
                     country_name=country_txt,region_code=region, region_name=region_txt, provstate,city,
                     latitude,longitude,location,success, attacktype1=attacktype1_txt, 
                     attacktype2=attacktype2_txt,attacktype3=attacktype3_txt,targtype1=targtype1_txt,
                     targsubtype1=targsubtype1_txt,weaptype1=weaptype1_txt, weapsubtype1=weapsubtype1_txt,
                     property,propextent_txt,propvalue,gname,nkill,nwound)

glimpse(india.db)


#World Activities Vs India Activities
library(ggplot2)
library(gridExtra)
plot1<-ggplot(data=gtd) +
  geom_area(mapping=aes(x=iyear), stat="count", fill= ('darkcyan')) +
  ggtitle("Yearly Terrorist Attacks, 1970-2016", subtitle = "Source: Global Terrorism Database")

plot2<-ggplot(data=india.db) +
  geom_area(mapping=aes(x=year), stat="count", fill= ('dodgerblue3')) +
  ggtitle("Yearly Terrorist Attacks, 1970-2016", subtitle = "India")

grid.arrange(plot1, plot2, ncol=2)

#Histogram of Attacks by Year
ggplot(india.db, aes(x = year)) + geom_histogram(fill= ('dodgerblue3'), color='darkslateblue', binwidth=1)+
  labs(x="Year", y = "# of Attacks", title =  "Attacks Histogram") +
  theme(plot.title = element_text(hjust = 0.5) )

#Histogram of Attacks by Month
#To see if there is any spike in any month 

#To prepare column of in the database as text month column is not available in the main database
df <- data.frame(month = c('jan','feb','mar','apr','may','jun','jul','aug','sep','oct','nov','dec'))

for ( i in 1:nrow(india.db)){
  india.db$month_txt[i] <- as.character(df$month[india.db$month[i]])
}

# dataset- group by incidents
india.db.month <- aggregate(eventid~month_txt, data = india.db, length)

# Second way of doing the above
  #india.db.month<-india.db %>%
  #group_by(month_txt)%>%
  #summarise(numberOfEvents=n())
  
  
#Graph
ggplot(india.db.month, aes(x=month_txt, y=eventid)) + 
  geom_point(size=5,color='firebrick1') + 
  geom_segment(aes(x=month_txt, 
                   xend=month_txt, 
                   y=0, 
                   yend=eventid)) + 
  labs(x= 'Month', y= '# of Events') +
  labs(title="Attacks by Month", 
       subtitle="Source: Global Terrorism Database") + 
  theme(axis.text.x = element_text(angle=45, vjust=0.6, size=10), plot.title = element_text(hjust = 0.5), 
        panel.background = element_rect(fill = "lightblue", colour = "lightblue"))

#Number of incidents by states
# using pipe

# it is observed that State Andhra pradesh has two entries; cleaning this one
india.db$provstate[india.db$provstate=="Andhra pradesh"] <- "Andhra Pradesh"
# Cleaning Odisha to Orissa
india.db$provstate[india.db$provstate=="Odisha"] <- "Orissa"

#below code is working fine but we need arrange this in order
india.db %>%
  group_by(provstate)%>% 
  count() %>%  # count the number of times a provstate appear
  arrange(desc(n)) %>%
 # arrange(-n) %>%
 ggplot(aes(x=reorder(provstate,n), y=n))+
  geom_bar(stat = "identity",aes(fill=n>500)) +
  theme(axis.text.x = element_text(angle=90,size=8,vjust=1, hjust=0.1), plot.title = element_text(hjust = 0.5)) +
  labs(x= 'State', y= '# of Events') +
  ggtitle('Attacks by State')+
  #coord_flip()+
  scale_fill_manual(values = c('chocolate1', 'firebrick'),guide=FALSE)


# Showing top ten states on the map

top.ten.states <- india.db %>% group_by(provstate)%>% count() %>%  arrange(desc(n))
top.ten.states <- top.ten.states[1:10,]

India_center = geocode("India")
India <-ggmap(get_map(location=India_center,zoom=4), extent="panel")

#taking the circle scale to make the scale of the circle fit in the map
#if we dont do the same the the value 700+ will cover the whole screen
# getting the longitides and lattitudes of all the locations
# in this process there will be a few states that will not get co ordinates


#converting states/provinces into character
top.ten.states$provstate <- as.character(top.ten.states$provstate)

for (j in 1:nrow(top.ten.states)) {
  latlon = geocode(top.ten.states$provstate[j])
  top.ten.states$lon[j] = as.numeric(latlon[1])
  top.ten.states$lat[j] = as.numeric(latlon[2])
}

# you need to run this code again and again to get the minimum number for missing values
# then fill the missing coordicates using the geocode function 
# we can manually assign the values of lon and lat from the table below
#top.ten.states[4,3]
#provstate     n      lon      lat
#<chr> <dbl>    <dbl>    <dbl>
#  1 Jammu and Kashmir  2197 76.57617 33.77817
#2             Assam  1120 92.93757 26.20060
#3           Manipur  1011 93.90627 24.66372
#4            Punjab   939 75.34122 31.14713
#5      Chhattisgarh   835 81.86614 21.27866
#6         Jharkhand   801 85.27994 23.61018
#7             Bihar   633 85.31312 25.09607
#8            Orissa   595 85.09852 20.95167
#9       West Bengal   561 87.85498 22.98676
#10       Maharashtra   280 75.71389 19.75148

circle_scale <- .015
India + geom_point(aes(x=lon, y=lat), data=top.ten.states, col="red",
                   alpha=0.4, size=top.ten.states$n*circle_scale)
#Indian geography can be divided into four categories on the basis on attacks
#Jammu and Kashmir, Red-Corridor, North-East and Others
# introduced a column state_cat to show the categories

for( i in 1:nrow(india.db))
{
 if (india.db$provstate[i]=="Jammu and Kashmir")
 {
   india.db$state_cat[i] ="Jammu and Kashmir"
 }
  else if ( (india.db$provstate[i]=="Assam") |
          (india.db$provstate[i]=="Manipur") |
          (india.db$provstate[i]=="Mizoram") |    
          (india.db$provstate[i]=="Arunachal Pradesh") |
          (india.db$provstate[i]=="Nagaland") |
          (india.db$provstate[i]=="Tripura") |
          (india.db$provstate[i]=="Meghalaya") )
        { india.db$state_cat[i] ="North East" }
  
  else if ( (india.db$provstate[i]=="Bihar") |
            (india.db$provstate[i]=="Andhra Pradesh") |
            (india.db$provstate[i]=="Jharkhand") |
            (india.db$provstate[i]=="West Bengal") |
            (india.db$provstate[i]=="Orissa") |
            (india.db$provstate[i]=="Chhattisgarh") |
            (india.db$provstate[i]=="Telangana") |
            (india.db$provstate[i]=="Madhya Pradesh") )
        { india.db$state_cat[i] ="Red Corridor" }
  
  else   (india.db$state_cat[i] ="Others")
  
}  

# visualizing on the basis of categories

india.db %>%
  group_by(state_cat)%>% 
  count() %>%  # count the number of times a provstate appear
  arrange(desc(n)) %>%
  # arrange(-n) %>%
  ggplot(aes(x=reorder(state_cat,n), y=n))+
  geom_bar(stat = "identity", aes(fill=n>2500)) +
  theme(axis.text.x = element_text(angle=90,size=8,vjust=1, hjust=0.1), plot.title = element_text(hjust = 0.5)) +
  labs(x= 'State', y= '# of Events') +
  ggtitle('Attacks by Category')+
  coord_flip()+
  scale_fill_manual(values = c('darkorange', 'firebrick'))

# attacks on the map


# Number of attacks by attack_type histogram
#par(mfrow = c(2,2))

ggplot(india.db, aes(x = year, fill = attacktype1 )) + geom_histogram()+
  labs(x="Year", y = "# of Attacks", fill ="Attack Type", title =  "Attacks Type Histogram") +
  theme(plot.title = element_text(hjust = 0.5) )
 
 
# to show pie chart (using barchart) that people kill in categories
# creating and cleaing data for piechart

india.db.pie <- aggregate(nkill~state_cat, data = india.db, sum)
ggplot(india.db.pie, aes(x="", y=nkill, fill=state_cat))+
  geom_bar(width = 1, stat = "identity") + coord_polar("y", start=0) +
  labs(fill ="Region", title =  "Killings in Region")+
  scale_fill_manual(values=c("firebrick3", "darkorange1", "darkorchid3", "gold4"))+
  theme(plot.title = element_text(hjust = 0.5))

#library(plotrix)
#pie3D(india.db.pie$nkill, labels = india.db.pie$state_cat, main="3D Pie chart of Sales per region")

# Looking in wordclouds

if (!("wordcloud" %in% installed.packages())) {
  install.packages("wordcloud", repos = "https://cran.r-project.org")}

library(wordcloud)
par(mfrow = c(1,3))
wordcloud(india.db$targtype1,max.words = 100,random.order = FALSE, colors=brewer.pal(8, "Dark2"))
wordcloud(india.db$attacktype1,max.words = 100,random.order = FALSE, colors=brewer.pal(8, "Dark2"))
wordcloud(india.db$gname,max.words = 100,random.order = FALSE, colors=brewer.pal(8, "Dark2"))
