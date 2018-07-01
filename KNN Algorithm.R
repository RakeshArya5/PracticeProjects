#k-NN algorithm
# Step 1 - Data collection
#data is related to wh breast cancer - University of Winconsin
wbcd <- read.csv("C:/Users/Admin/Desktop/Machine Learning/wbcd.csv", stringsAsFactors = FALSE)
table(wbcd$diagnosis)

wbcd <- wbcd[-1]

#Labels - target feature is coaded as a factor
wbcd$diagnosis<- factor(wbcd$diagnosis, levels = c('M','B'), labels = c('Malignant','Benign'))
prop.table(table(wbcd$diagnosis))
round(prop.table(table(wbcd$diagnosis))*100, digits = 2)

#Now the other 30 variables are all continuous 
summary((wbcd[c("radius_mean","area_mean","smoothness_mean")]))
# Tranformation - Normalizing numeric data
# When we have data of different scale, we need to normalize the data

normalize <- function (x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
#Test function                          
normalize(c(1,2,3,4,5))

#lapply function takes a list and apply a specific function to each list element
#Data frame is a list of equal length vectors
normalize(c(wbcd$area_mean))
wbcd_n <- as.data.frame(lapply(wbcd[2:31], normalize))
summary(wbcd_n$area_mean)

#preparing data for modeling
wbcd_train <- wbcd_n[1:469,]
wbcd_test <- wbcd_n[470:569,]


#setting labels in training and test datasets
wbcd_train_labels<- wbcd[1:469, 1]
wbcd_test_labels<- wbcd[470:569, 1]

install.packages("class")
library(class)
wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test, cl = wbcd_train_labels, k = 21)

install.packages("gmodels")
library(gmodels)
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred, prop.chisq=FALSE)


