#market basket analysis
read.csv("groceries.csv")
install.packages("arules")
library(arules)
groceries <- read.transactions("groceries.csv", sep = ",")
summary(groceries)

groceries <- read.transactions("groceries1.csv")
summary(groceries)
#to see the first five items
inspect(groceries[1:5])

#to find the item frequency
itemFrequency(groceries[,1:3])
#visualization
itemFrequencyPlot(groceries, support = .01)
itemFrequencyPlot(groceries, topN = 20)
image(groceries[1:500])
image(sample(groceries,5000))

#training model
apriori(groceries) #this doesnot show anything as default support =.1 and confidence = .8
#changing the parameters of the the algorithms

groceriesrule <- apriori(groceries, parameter = list(support=.006, confidence = .25, minlen=2) )
summary((groceriesrule))
inspect(groceriesrule[1:3])
inspect(sort(groceriesrule, by = "lift")[1:5])


waterrules <- subset(groceriesrule, items %in% "bakery")

inspect(waterrules)
