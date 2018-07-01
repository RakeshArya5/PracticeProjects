#using qplot
set.seed(1410)
dsmall <- diamonds[sample(nrow(diamonds), 100), ]
qplot(carat, price, data = diamonds)
qplot(carat, price, data = dsmall, geom = c("point", "smooth"))
qplot(carat, price, data = diamonds, geom = c("point", "smooth"), se = FALSE)
qplot(color, price / carat, data = diamonds, geom = "jitter", alpha = I(1 / 5))
qplot(carat, data = diamonds, geom = "histogram")
qplot(carat, data = diamonds, geom = "density")

#Getting data from PDf in R
install.packages("tabulizer")
library(pdftools)
#setwd("C:/Users/Admin/Desktop/Blog - ZoomInside/UP Elections")

info <- pdf_info("08.Constituency Data Summery.pdf")
fonts <- pdf_fonts("08.Constituency Data Summery.pdf")
files <- pdf_attachments("08.Constituency Data Summery.pdf")
const_data <- pdf_text("08.Constituency Data Summery.pdf")

const_data.df <- as.data.frame(const_data)

head(const_data.df)
class(const_data)

library(tabulizer)
library(dplyr)
