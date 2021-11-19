
rm(list=ls())
shell("cls")
graphics.off

#rm(list=ls())

install.packages("tidyverse") # data manipulation, makes codes more readable
install.packages("learnr") # makes it easy to turn any R Markdown document into an interactive tutorial
install.packages("readxl") # supports importing data in R (excel files)
install.packages("readr") # support data exporting functions
install.packages("dplyr") # supports in data manipulation and operations


library(tidyverse)
library(learnr) 
library(dplyr)
library(readxl)
library(readr)


getwd()
## setting directory
#setwd()

setwd("C:/Users/INSPIRON SERIES 7548/Documents/GitHub/R Data Analytics")
#-----------------------------------------------------------------------------------------------


## dataframe and importing data

# read.csv() command

read.csv("C:/Users/INSPIRON SERIES 7548/Documents/GitHub/R Data Analytics")


diamond<- read.csv("C:/Users/INSPIRON SERIES 7548/Documents/GitHub/R Data Analytics") # diamond is an object I have created

diamond # view the dataset

head(diamond) # gives the first 6 rows

str(diamond) # describes the structure of data, numeric (has dec points), integer (no decimal points), character
# get to know the scale of measurement for each. whether categorized or not

head(diamond, 3) # display the first 3 rows of the data.frame

class(diamond) # nature of dataset

summary(diamond) # gives the general summary of the dataset


nrow(diamond) # displays the number of rows of the data.frame (no. of observations)

ncol(diamond) # no.of variables/no.of columns

dim(diamond) # displays the number of rows and columns in a vector of length 2 (#rows, #cols)

colnames(diamond) # display the column names (if any)

rownames(diamond) # display the row names (if any)




#----------------------------------------------------------------------------------------------
# OPTION 1: Save the excel sheet as a  .csv file and use read_csv() # we have covered this


#Option 2: use the add on package readxl to read in the excel file using read_excel()

### library(readxl)

?read_excel # see the help file for read_excel()

read_excel("C:/Users/INSPIRON SERIES 7548/Documents/GitHub/R Data Analytics/lab2_data.xlsx") # gives the first 10 observations
#Alternatively store in an object

diamond2<- read_excel("C:/Users/INSPIRON SERIES 7548/Documents/GitHub/R Data Analytics/lab2_data.xlsx") # 

#QN: Find out when to use read_xls() and read_xlsx()

#-------------------------------------------------------------------------------------------------------

## exporting data

# the data exporting functions include readr(), write_delim() etc

# delim is the character that seperates columns e.g 
#-""=csv(this results in the write_csv function)
# "\t"= tabl delimited

#### library(readr)


# make sure library(readr) is loaded

#Option 1

# Write data to txt file: tab separated values
# sep = "\t"
write.table(diamond, file = "diamond.txt", sep = "\t",
            row.names = TRUE, col.names = NA)   
# check for the diamond.txt in your working folder

#Note: You can also read .txt files to R

#Option 2

# Write data to csv files:  
# decimal point = "." and value separators = comma (",")
write.csv(diamond, file = "diamond.csv") # check new diamond.csv
#written in the working folder

#--------------------------------------------------------------------------------
#Data manipulation 

#DM1: creating new variables
str(diamond)

class(diamond$length)
class(diamond$width)
diamond$sum <- diamond$length + diamond$width

diamond$mean <- (diamond$length + diamond$width)/2

#DM1: Recording a variable
summary(diamond$price)
# using mean as the threshold

diamond$pricecat <- ifelse(diamond$price > 4534, c("low"), c("high")) 


#diamond$pricecat3<-ifeslse(diamond$price<= 383, "low",
#                           ifelse(diamond$price >383 & diamond$price <=6224, "middle", "high" ))


############################################################################################
#DM2: RENAMING 

# we can change column names by using the rename() function from the R package dplyr
# we could rename the column "weight" to WEIGHT in the dataset

library(dplyr)

diamond=rename(diamond, WEIGHT = weight) # renaming weight to WEIGHT

colnames(diamond) # see the changed name


#-------------------------------------------------------------------
#DM3: Subsetting data

diamond[3, 2] # in  diamond row 3 and column 2


str(diamond)

#Considering only fair quality diamond
diamondfair<-subset(diamond,quality=="Fair")

#subsetting rows by vector of indices
diamond[c(1,3),] # give me rows 1 and 3 plus there entire columns

#subset a column
diamond$weight


#DM4: REMOVE A COLUMN

#same data set

diamond <- subset(diamond, select = -c(weight, quality))

# or create a new data set

diamond3<-subset(diamond, select = -c(weight, quality))

#-------------------------------------------------------------------------------
#DM6: Level of measurements

#Let R know that quality, color and clarity are categorical
diamond$quality<-as.factor(diamond$quality)
diamond$color<-as.factor(diamond$color)


class(diamond$quality)
class(diamond$color)

table(diamond$quality)
table(diamond$color)


#--------------------------------------------------------------------------------
# DM6: dESCRIPTIVE STATISTICS age > 45 & age <= 75

# central tendency: mean, media, mode
#variability: range, interquartile range, variance, standard deviation
#Frequency tables

diamond<- read.csv("C:/Users/INSPIRON SERIES 7548/Documents/GitHub/R Data Analytics/lab2_data.csv")

str(diamond)


summary(diamond)

#measure of central tendency and dispersion

#weight is a continuous variable/ scale so we run a summary statistics

min(diamond$weight); max(diamond$weight); mean(diamond$weight); sd(diamond$weight)

range(diamond$weight); var(diamond$weight); median(diamond$weight)

# quality is a categorical variable so we run a frequency table
table(diamond$quality) # categories variable


# color is a categorical variable so we run a frequency table

table(diamond$color)

colorper <-table(diamond$color)

prop.table(colorper)*100

round(prop.table(colorper)*100)
#percentages


#-------------------------------------------------------------------------------------------------------------------------

# Data visualization in R

library(ggplot2)
diamond<- read.csv("C:/Users/INSPIRON SERIES 7548/Documents/GitHub/R Data Analytics/lab2_data.csv")
#Univariate analysis
# Scale : Histogram
str(diamond)

hist(diamond$weight)
hist(diamond$price)
hist(diamond$price, breaks=12, col="red")

#categorical, piechart, bar chart
#bar chart
counts <- table(diamond$quality) 		# Simple Bar chart
barplot(counts, main="Quality of Diamond",   xlab="Quality type") 

library("epiDisplay") # showing data in ascending order
tab1(diamond$quality,sort.group = "decreasing")
# or
tab1(diamond$quality,sort.group = "decreasing", horiz=TRUE)

#pie-chart (not part of the df)
#quality
slices<-c(39, 35, 13, 8, 5) # forms a vector
slices
lbls <- c("Ideal", "Premium", "Very Good","Good", "Fair") # label the slices directly
pie(slices, labels = lbls, main="Quality of Gold")

#Pie Chart with Annotated Percentages
slices<-c(39, 35, 13, 8, 5) # from frequency table
lbls <- c("Ideal", "Premium", "Very Good","Good", "Fair")
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct)		# add percents to labels 
lbls <- paste(lbls,"%",sep="")  # ad % to labels 
pie(slices,labels = lbls, col=rainbow(length(lbls)), main="Quality of Gold")


#3D pie chart
install.packages("plotrix") # to plot a 3D pie chart
library(plotrix)
slices<-c(39, 35, 13, 8, 5) # from frequency table
lbls <- c("Ideal", "Premium", "Very Good","Good", "Fair")
pie3D(slices, labels = lbls, explode=0.1, main="Quality of Gold")

#Alternatively
pie( table(diamond$quality),
     col = c("white", "gray90", "gray60","blue", "black"))

#-------------------------------------------------------------------------------
#for two variables

#boxplot
boxplot(price~quality,data=diamond,main="Ass btn quality and price", xlab="quality", ylab="price")


------------
#Building a plot with ggplot2() command

ggplot(diamond) + geom_point(aes(x = weight, y = price))

ggplot(diamond, aes(x = weight, y = price)) + geom_point()



#library(tidyverse)
dev.off() # clears all the graphic in the plots box




#-----------------------------------------------------------------------------------------

## cleaning the environment space
#console

ls() # reads the name of the dataframes imported

#rm will remove all of the objects that are stored in your global environment

#(which may be what you want) but will not unload any of the packages that you have loaded.

# clear environment just type

rm(list=ls())

#-----------------------------------------------------------------
# to clear console type
Ctrl+L  





#alternatively
dev.off()
#or
graphics.off()


#Exercise



