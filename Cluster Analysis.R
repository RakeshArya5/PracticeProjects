#k-mean Cluster Analysis
#input data
teen <- read.csv("snsdata.csv")
str(teen)
#to see NA in factor data
table(teen$gender)
table(teen$gender, useNA = "ifany")      
#to see NA's in numerical data
summary(teen$age)
#we see that there are 5k NA's in age and 2K NA's in gender; maked 7K NA's i.e 25% of data that need to taken care if
#there are a few senselees record in age eg 3 and 103 in high school
teen$age <- ifelse(teen$age >=13 & teen$age <= 20, teen$age, NA)
summary(teen$age)

#Dummy coading missing values - actually factorial values
#we are creating two more columns; Female and no_gender
teen$female <- ifelse(teen$gender == "F" & !is.na(teen$gender),1,0) #this coading Female as 1 and Male as 1
teen$no_gender <- ifelse(is.na(teen$gender),1,0)
table(teen$female, useNA = "ifany")
table(teen$gender)
table(teen$no_gender)

#data preparation - imputing missing values
# giving average age to all the missing values in each gradyear
mean(teen$age)
mean(teen$age, na.rm = TRUE)
aggregate(data = teen, age ~ gradyear, mean, na.rm = TRUE)

ave_age <- ave(teen$age, teen$gradyear, FUN = function (x) mean(x, na.rm = TRUE))
ave_age
summary(ave_age)
teen$age <- ifelse(is.na(teen$age), ave_age, teen$age)
summary(teen$age)
#training the model on the data
library(stats)
interests <- teen[5:40]
interests_z <- as.data.frame(lapply(interests, scale))
summary(interests_z)
set.seed(2345)
teen_clusters <- kmeans(interests_z, 5) #giving k value is 5 

#evaluating model performance
teen_clusters$size
teen_clusters$centers #to see cluster homogeneity

#improving Model
teen$clutter <- teen_clusters$cluster
teen[1:5, c("clutter", "gender", "age", "friends")]
#to find the average age in all the clusters
aggregate(data = teen, age ~ clutter, mean)
aggregate(data = teen, female ~ clutter, mean)
aggregate(data = teen, friends ~ clutter, mean)

