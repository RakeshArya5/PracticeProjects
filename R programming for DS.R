  x <- rnorm(1000)
head(x)
mean(x)
sd(x)
min(x)
max(x)
tail(x)
exp(1/2)
exp(-1/2)
mean(exp(rnorm(1000)))
#quadratic equations solution
#X^2+3X+1 = 0
a<-1
b<-3
c1<-1
my.vector<-c((-b+sqrt(b^2-4*a*c1))/(2*a),(-b-sqrt(b^2-4*a*c1))/(2*a))

A<-1
B<-3
C<-1
my.vector<-c((-B+sqrt(B^2-4*A*C))/(2*A),(-B-sqrt(B^2-4*A*C))/(2*A))
my.vector
#error 
c(-0.4,-2.6)/my.vector-1
c(-0.4,-2.6)/my.vector-1

#exercise- 3
set.seed(1234)
x <- rnorm(100, mean=.5, sd=.3)
mean(x)
sd(x)
?hist
hist(x, axes = FALSE)
axis(4)  
axis(1)


set.seed(1)
x <- rnorm(100) 
head(x)

?"tail"
?tail

help(tail)

internal(tail)

x <- 5 : 6 
x

x <- 5 + 6 
y <- x + 3 
z <- y - 10 
z

#Data Structures
#generate levels

gl(2, 10, labels = c("Male", "Female)"))

?gl
gl(2, 8, labels = c("Control", "Treat"))
gl(2, 1, 20)
gl(2, 2, 20)

#factors
a<- as.factor(c(rep("male",10), rep("xx",4)))
str(a)

#martices
matrix(c(1,2,3,4,5,6)+pi, nrow=2)

matrix(c(1,2,3,4,5,6), nrow = 2)
?matrix
matrix(c(1,2,3,4,5,6)+pi, nrow=2)<6

a1 <-data.frame(treatment = c(4,5), bp = c(2,3))
a1 + 3
cbind(treatment = c("a","b"), bp = c(2,3))

class(paste("R Session", 1))
?paste

data.frame(x=1:3,y=c("A","B","C"))+2
#functions

square <- function(x) {
  
  return(x*x)
}
  
square(1:5)  
power <- function(x,y){
  
  return(x^y)
}

power(1:5,3)

x <- seq(0,6, length =100)
seq(2,4, length =10)
y <- 2*x + 3 + rnorm(100)

plot(x,y)

plot(sin,0,2*pi)
plot(sin,0,pi)

f <- function(x){3*sin(x/2)+x}
f(0)
f<-function(x){3*sin(x/2)+x}
plot (f,-7,7)

iter <- stats::rpois(1, lambda = 10)
## print an informative message
cat("iteration = ", iter <- iter + 1, "\n")
cat(paste(letters, 100* 1:26), fill = TRUE, labels = paste0("{", 1:10, "}:"))


my.display<-function(x){
  cat("summary of input: \n"
  return(summary(x))
}

require(graphics)

plot(density(c(-20, rep(0,98), 20)), xlim = c(-4, 4))

my.display <- function(x, display=FALSE,type="hist", prob)
{
  if(display=="hist") hist(x) 
  else if(display=="plot")
  plot(x)  
}

set.seed(1234)
my.data<-rnorm(200)
median(my.data<-rnorm(200))

#loops

for ( i in 1:3)
{
  cat (i, "+", i, "=", i+i, "\n")
}

#if statement

for (i in 1:10)
{
  if (i%%2==0) cat("This is even ","\n") else cat ("This is odd")
  cat(i,"\n")
 
}
#exercise 3.1

set.seed(1786)
data.exercise.3.1<-exp(matrix(rnorm(2000),nrow=100))
index1.temp <-sample(1:100,10)
index2.temp <-sample(1:20,10)
for(i in 1:10){
  data.exercise.3.1[index1.temp[i],index2.temp[i]]<--1
}

my.data <- data.exercise.3.1
}

my.data <- data.frame(data1 = rnorm(10), data2 = rnorm(10), data3 = rnorm(10))
lapply(my.data, sum)
sapply(my.data, sum)


k1<-10
k2<-100000
my.data<-as.data.frame(matrix(rnorm(k1*k2),nrow=k1))
mean1<-numeric(k2)
mean2<-numeric(k2)
for(i in 1:k2){
  mean1[i]<-mean(my.data[,i])
}

time1<-as.numeric(Sys.time())
for(i in 1:k2){
  mean1[i]<-mean(my.data[,i])
}
time2<-as.numeric(Sys.time())
time3<-as.numeric(Sys.time())
mean2<-sapply(my.data,mean)
time4<-as.numeric(Sys.time())
(time2-time1)/(time4-time3)


for(i in 0:10) { 
  if(i%%2!=0) cat(i) 
}

notfound<-TRUE 
i<-0 
while(notfound) { 
  if(i%%2!=0) { 
    cat(i) 
    notfound<-FALSE 
  } 
}

#matrices and vectors
x<- -5:5

index <- abs(x)<3
x[index]

A<- matrix(-4:5, nrow=2, ncol=5)
A[A<0]

A[A<0] <- 0
A[2,]
A[,c(2,4)]

set.seed(9852)
my.data<-list()
for(i in 1:100){
  my.data[[i]]<-matrix(rnorm(16),nrow=4)
}

my.index<-list()
for(i in 1:100){
  my.index[[i]]<-(my.data[[i]]<0)
}

my.negatives<-matrix(rep(0,16),nrow=4)
for(i in 1:100){
  my.negatives<-my.negatives+my.index[[i]]
}
my.negatives
sum(my.negatives)

my.negative.values<-numeric(0) 
for(i in 1:100){
  my.negative.values<-c(my.negative.values,my.data[[i]][my.index[[i]]])
}
summary(my.negative.values)


set.seed(9007)
my.data<-data.frame(x=rnorm(10),y=rnorm(10)+5,z=rchisq(10,1))
additional.data<-data.frame(x=rnorm(3),y=rnorm(3)+5,z=rchisq(3,1))

set.seed(45)
my.data<-data.frame(x=rnorm(10),y=rnorm(10),z=rnorm(10))

write.csv(my.data,"Exercise 6.2.csv")


set.seed(45)
my.data<-data.frame(x=rnorm(10),y=rnorm(10),z=rnorm(10))
save(my.data,file="Exercise 6.4.Rdata")

rm(my.data)
my.data
load("Exercise 6.4.Rdata")
head(my.data)
write.table(mtcars,file = "mtcars.txt")
write.table(mtcars,file = "mtcars1.txt",sep = ",")
write.table(mtcars,file = "mtcars.txt",row.names = FALSE, sep = ",")
df <- read.table("mtcars1.txt",header = TRUE,sep = ",")
write.table(mtcars,file = "mtcars2.txt",sep = ",")
write.csv(mtcars,file = "mtcars3.txt")


#lab 5

set.seed(9007)
my.data<-data.frame(x=rnorm(10),y=rnorm(10)+5,z=rchisq(10,1))

write.table(round(my.data,digits = 2),"Assignment 6a.txt",row.names=FALSE)
write.table(format(my.data,digits = 20),"Assignment 6b.txt",row.names=FALSE)
my.data2 <- my.data *10e5
write.table(my.data2, "Assignment 6c.txt",row.names=FALSE)
df <- read.table("Assignment 6c.txt",header=TRUE)
my.data3 <- df/10e5
my.data[1,1] - my.data3[1,1]

options(digits=20)
my.data[1,1] - my.data3[1,1]

install.packages("RODBC")
library(RODBC)
connStr <- paste(
  "Server=msedxeus.database.windows.net",
  "Database=DAT209x01",
  "uid=RLogin",
  "pwd=P@ssw0rd",
  "Driver={SQL Server}",
  sep=";"
)
conn <- odbcDriverConnect(connStr)

my.data.frame <- sqlQuery (conn,
                         "SELECT SUM(Revenue), SUM(Units), ProductID
                         FROM bi.salesFact
                         WHERE Date > '2013-12-31' AND Date < '2015-01-01'
                         GROUP BY ProductID"
)

write.csv(my.data.frame,"temp.csv")

names(my.data.frame)<-c("SUM(Revenue)","SUM(Units)","ProductID")



#data frames

data.frame.x<-data.frame(names=c("Gretha","Robert","John","Heather"),
                         age=c(30,18,25,70),
                         nickname=c("Quicksilver","The Man","Nifty","Starlight"))

data.frame.y<-data.frame("Person_name"=c("William","Nancy","Charlotte","Henry"),
                         age=c(15,75,32,51),
                         "pet_dog"=c("King","Whity","Captain Vom","Doggie"))

merge(data.frame.x,data.frame.y)

data.frame.z<-merge(data.frame.y,data.frame.x,
                    by.x=c("Person_name","age"),
                    by.y=c("names","age"),all=TRUE)
data.frame.z

names(iris)
levels(iris$Species)
median(iris$Sepal.Length)
setosa.data<-subset(iris, 
                    Species == "setosa" & Sepal.Length<median(Sepal.Length),
                    select = -Species) 
summary(setosa.data)


my.text<-"Over the last decade, bluetongue virus have spread northwards from the mediterranean area. 
Initially this was ascribed to climate changes, 
but it has since been realized that a major contributing factor has been new transmitting vectors, 
culicoides obsoletus and culicoides pulicaris, which have the ability to aquire and transmit the disease. 
Recently, schmallenberg virus has emerged in northern europe, transmitted by biting midges as well."

my.lowercase<-c("bluetongue","culicoides","europe","mediterranean",
                "northern","schmallenberg")

my.uppercase<-c("Bluetongue","Culicoides","Europe","Mediterranean",
                "Northern","Schmallenberg")

my.new.text<-my.text

#way1
my.new.text <- gsub("bluetongue","Bluetongue", my.new.text)

#way2

for(i in 1:length(my.lowercase)){
  my.new.text<-gsub(my.lowercase[i],my.uppercase[i],my.new.text)
}

set.seed(885)
my.posixct<-as.POSIXct(sample((60*60*24*365*50):(60*60*24*365*55),20), 
                       origin = as.Date("1960-01-01"))
my.posixct

my.posixct2 <- my.posixct + 9010
head(data.frame(my.posixct,my.posixct2))


dat1 <- data.frame(name = c("cat","vic","osc"), age = c(9,7,4))
dat2 <- data.frame(name = c("vic","jon","cat"), gender = c("male","male","female"))

merge(dat1, dat2)
merge(dat1, dat2, all = TRUE)

subset(airquality, !is.na(Solar.R))


#lab 8
set.seed(449)
your.dates<-as.Date(sample(18000:20000,20), origin = "1960-01-01")
your.days<-c(julian(your.dates))
your.days<-c(julian(your.dates,origin="1960-01-01"))
your.days<-c(julian(your.dates,origin=as.Date("1960-01-01")))

set.seed(119) 
my.days<-sample(18000:20000,20)

weekdays(my.days)

old.locale <- Sys.getlocale("LC_TIME")

Sys.setlocale("LC_TIME", "English")

Sys.setlocale("LC_TIME", old.locale)

install.packages("chron")
library(chron)
my.days.structure<-month.day.year(my.days)
my.days.structure<-month.day.year(my.days,origin=as.Date("1960-01-01"))
my.days.structure<-month.day.year(my.days,origin="1960-01-01")
my.days.structure<-month.day.year(my.days,origin=c(1,1,1960))

my.dates<-as.Date(my.days, origin = "1960-01-01") 

my.date.info<-c(Weekday=weekdays(my.dates),my.days.structure)
my.date.info<-data.frame(Weekday=weekdays(my.dates),my.days.structure)

#manipulating data
log.airquality<-log(airquality)
summary(log.airquality)

my.summary<-function(x){data.frame(Min=min(x,na.rm=TRUE),
                                    Median=median(x,na.rm=TRUE),
                                    Mean=mean(x,na.rm=TRUE),
                                    Max=max(x,na.rm=TRUE))}

lapply(airquality, my.summary)

dat <- data.frame(gender = c("Male","Male","Male","Male","Male", 
"Female","Female","Female", "Female","Female"), height = c(10,5,12,10,2,7,6,12,9,4))

table(dat$gender)
tapply(dat$height, dat$gender, mean)


aggregate(height~gender, data=dat, mean)

by(dat$height, dat$gender, mean)

dat2 <- data.frame(gender = c("Male","Male","Male","Male","Male", 
                             "Female","Female","Female", "Female","Female"), 
                  tmt = c("active","placebo","active","placebo","active","placebo","active","placebo","active","placebo"),
                  height = c(10,5,12,10,2,7,6,12,9,4))

tapply(dat2$height, list(dat2$gender,dat2$tmt), mean) #this will make cross tab 
aggregate(height~gender+tmt, data=dat2, mean)

aggregate(height~gender+tmt, data=dat2)

by(dat2$height, dat2$gender, mean)

class(presidents)
cycle(presidents)

#apply cycle fuction to group the data
#President is ts data type ie time series

tapply(presidents, cycle(presidents), mean, na.rm=T)
tapply(presidents, cycle(presidents), sum, na.rm=T)

#way1
tapply(dat2$height, list(dat2$gender,dat2$tmt), mean)

#way2: aggregate function is the groupby function
aggregate(height~gender+tmt, data=dat2, mean)

#way3
by(dat2$height, list(dat2$gender, dat2$tmt), mean)


x<-rnorm(1000000)
y<-sample(1:1000,1000000,replace=T)
groups<-paste("group",1:1000)


my.data<-data.frame(data=rnorm(1000000),group=sample(groups,1000000,replace=T))

# r takes different times in the calculation of tapply and aggregate function
# tapply():

time0<-Sys.time()
tapply(my.data$data,my.data$group,mean)
time1<-as.numeric(Sys.time()-time0)

# aggregate():
time0<-Sys.time()
aggregate(data~group,mean,data=my.data)
time2<-as.numeric(Sys.time()-time0)

# by():
time0<-Sys.time()
by(my.data$data,my.data$group,mean)
time3<-as.numeric(Sys.time()-time0)

my.runtime<-data.frame(tapply=c(time1,time1/time1),
                     aggregate=c(time2,time2/time1),
                      by=c(time3,time3/time1))
rownames(my.runtime)<-c("Time elapsed:","Relative to tapply():")
my.runtime

only.time <- data.frame(tapply= time1, aggregate= time2, by= time3)
only.time

tapply(dat$height, dat$gender, mean)
attach(dat)
tapply(height, gender, mean)

detach(dat)


x1<-1:3
my.data<-data.frame(x1=4:6,x2=7:9)
attach(my.data)
my.data

cbind(x1,x2)

my.data2<-data.frame(x1=10:12,x2=13:15)
attach(my.data2)
cbind(x1,x2)
searchpaths() [1:3]

detach(my.data, my.data2)


x1<-1:3
my.data<-data.frame(x1=4:6,x2=7:9)
my.data2<-data.frame(x1=10:12,x2=13:15)
attach(my.data)
attach(my.data2)
sum.and.diff <- with(my.data, cbind(x1+x2, x1-x2))
sum.and.diff
cbind(x1+x2, x1-x2)
detach(my.data, my.data2)
 
#cut() function
summary(airquality$Wind)
#The cut() function supplies the intervals to which an observation belongs
my.cut <- cut(airquality$Wind, breaks = 2*(1:11)-1)

my.cut<-cut(airquality$Wind,breaks=2*(1:11)-1)

table(my.cut)

tapply(airquality$Solar.R, my.cut, mean, na.rm=T)
tapply(airquality$Solar.R, my.cut, sum, na.rm=T)

Z <- stats::rnorm(10000)
table(cut(Z, breaks = -6:6))
sum(table(cut(Z, breaks = -6:6, labels = FALSE)))
sum(graphics::hist(Z, breaks = -6:6, plot = FALSE)$counts)

#cheching the air quality of ozone in no of days in a  month where quality is > 80

my.table <- with(airquality, table(ozhi = Ozone >80, Month ))
my.table                  

my.table.2<-addmargins(my.table,1:2)                  
my.table.2               
my.table.3<-prop.table(my.table,2)                  
my.table.3<-addmargins(my.table.3,1)                 
my.table.3     
round(100*my.table.3)

DF <- as.data.frame(UCBAdmissions)
head(DF)
#Organizing data as a contingency table with xtabs:

mytable_11 <- xtabs(Freq ~ Gender + Admit + Dept, data=DF)

ftable(mytable_11)
margin.table(mytable,1:2)
prop.table(margin.table(mytable,1:2),1)
prop.table(margin.table(mytable,2:3),1)

#We tabulate gender vs. department:
prop.table(margin.table(mytable,c(1,3)),1)

#Data for department A can be extracted as
DepA<-mytable_11[,,2]
ftable(DepA)
prop.table(DepA,1)


summary(swiss)
my.cut2<-cut(swiss$Agriculture,breaks=10*(0:10))
my.cut3<-cut(swiss$Catholic,breaks=10*(0:10))

tapply(swiss$Fertility,list(my.cut2,my.cut3),mean)

#ex9

colMeans(airquality)
tapply(airquality, mean)
lapply(airquality, mean)
sapply(airquality, mean)

tapply(chickwts$weight, chickwts$feed, mean)
aggregate(weight~feed, data=chickwts, mean)
by(chickwts$weight, chickwts$feed, mean)


with(airquality, table(Month, Temp < 65))
tapply(airquality$Month, airquality$Temp < 65)
table(LowTemp = airquality$Temp < 65, airquality$Month)
sapply(airquality, airquality$Temp < 65)


prop.table(with(airquality, table(Month, Temp > 80)),1)
prop.table(with(airquality, table(Month, Temp > 80)),2)
prop.table(with(airquality, table(Temp > 80, Month)),1)
prop.table(with(airquality, table(Temp > 80, Month)),2)


lapply(airquality, mean)
sapply(airquality, mean)
as.list(sapply(airquality, mean))

#Lab 9


my.data<-data.frame(Treatment=c(rep("A",4),rep("B",4)),
                    Stone=rep(rep(c("Small","Large"),c(2,2)),2),
                    Success=rep(c(1,0),4),
                    Count=c(81,6,192,71,234,36,55,25))

my.table<-xtabs(Count~Treatment+Success+Stone,data=my.data)
ftable(my.table)
prop.table(my.table[1,,],1)
prop.table(my.table[1,,],2)

prop.table(my.table[1,,],2)
prop.table(my.table[2,,],2)

margin.table(prop.table(my.table, 1:2),1)
margin.table(prop.table(my.table, 1:2),2)
prop.table(margin.table(my.table, 1:2),1)
prop.table(margin.table(my.table, 1:2),2) 



prop.table(my.table[,,1],1)
prop.table(my.table[,,1],2)
prop.table(my.table[,,2],1)

ftable(my.table)


prop.table(my.table[,,1],1)

prop.table(my.table[1,,],2)
prop.table(my.table[1,,],1)


prop.table(margin.table(my.table, 1:3),1)
prop.table(margin.table(my.table, c(3,1)),1)
prop.table(margin.table(my.table, 3:1),2)
prop.table(margin.table(my.table, c(1,3)),2)


#graphics
library(ggplot2)
head(diamonds)
qplot(carat, price, data=diamonds)

qplot(carat,
       price,
       data=diamonds,
       color = cut,
       log="xy",
       facets=~clarity,
       main="Diamonds")

p <- ggplot(data = diamonds)
p <- p + aes(x = carat, y = price)
p <- p + geom_point()
p


p<- ggplot(data = diamonds)
p <- p + aes(x = carat, y = depth)
p <- p + geom_point()
p <- p + geom_density2d()
p

summary(diamonds)
depth.groups<-cut(diamonds$depth,breaks=40+(0:5)*8)
ggplot(diamonds) +
  aes(price, fill=depth.groups) +
  geom_density(alpha=.3)

library(ggplot2)
install.packages("ggmap")
library(ggmap)

head(state.x77)
popdata<-data.frame(state=row.names(state.x77),murder=state.x77[,5])
popdata$state<-as.character(popdata$state)

for (i in 1:nrow(popdata)) {
  latlon = geocode(popdata$state[i])
  popdata$lon[i] = as.numeric(latlon[1])
  popdata$lat[i] = as.numeric(latlon[2])
}

usa_center = geocode("United States")
USA <-ggmap(get_map(location=usa_center,zoom=4), extent="panel")
USA + geom_point(aes(x=lon, y=lat), data=popdata, col="black",
                 alpha=0.4, size=popdata$murder)

packageurl <- "https://mran.revolutionanalytics.com/snapshot/2015-11-30/bin/windows/contrib/3.2/ggplot2_1.0.1.zip"
install.packages(packageurl, repos=NULL, type="source")
qplot(hp, qsec, data=mtcars, geom=c("point","smooth"), method="lm")
qplot(hp, qsec, data=mtcars, geom=c("point"), method="lm")
qplot(hp, qsec, data=mtcars, geom=c("point","smooth"))
qplot(hp, qsec, data=mtcars)
mtcars

p<- ggplot(data=mtcars)

p<- p + aes(x=qsec, y= hp)

p <- p+ geom_point() + geom_smooth(method = lm)

p

hist(Temp, data=airquality, breaks=10)
hist(airquality$Temp, breaks=10)
qplot(Temp, data=airquality, binwidth=5)
qplot(airquality$Temp, breaks=5)


x<-rnorm(1000, mean=-5) 


plot(density(x))
ggplot(data=x) + geom_density()
ggplot() + aes(x = x) + geom_density()
qplot(x)
qplot(x, geom = "density")



my.data<-data.frame(federal.states=c("Baden-Württemberg","Bayern","Berlin",
                                     "Brandenburg","Bremen","Hamburg","Hessen",
                                     "Mecklenburg-Vorpommern","Niedersachsen",
                                     "Nordrhein-Westfalen","Rheinland-Pfalz",
                                     "Saarland","Sachsen","Sachsen-Anhalt",
                                     "Schleswig-Holstein","Thüringen"), 
                    Population=c(10716644,12691568,3469849,2457872,661888,1762791,
                                 6093888,1599138,7826739,17638098,4011582,989035,4055274,
                                 2235548,2830864,2156759))

str(my.data)
my.data$federal.states<-as.character(my.data$federal.states)
latlon <- geocode(my.data$federal.states)
my.data$federal.states[1]<-"Baden-Wurttemberg"
my.data$federal.states[16]<-"Thuringen Germany"
my.data <- cbind(my.data,latlon)
my.data$lon <- latlon$lon; my.data$lat <- latlon$lat

Germany <- ggmap(get_map(location="Germany",zoom=6), extent="panel")

circle_scale <- 0.000002
Germany + geom_point(aes(x=lon, y= lat), data= my.data, col = "red", alpha = 0.4, size = my.data$Population*circle_scale)
