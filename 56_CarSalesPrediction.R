setwd("C:/Users/priya/Desktop/Priyanshi/SEM8/R Project") # This command is used to set the working directory to dir

getwd() # To check your current working directory, you can run this command

data <- read.csv("56_CarSalesPrediction.csv") #By default the read.csv() function gives the output as a data frame

summary(data)

carna=data #copying the data into a new variable called "carna"

summary(carna)

#DATA CLEANING
#This statement replaces the NA values in Engine.HP column with the median of other values in the Engine.HP Column and lets it be the same if it is not NA 
carna$Engine.HP=ifelse(is.na(carna$Engine.HP),median(carna$Engine.HP,na.rm=TRUE),carna$Engine.HP)
summary(carna) #Hence the NA values are removed from the column as seen in the summary

carna$Engine.Cylinders=ifelse(is.na(carna$Engine.Cylinders),median(carna$Engine.Cylinders,na.rm=TRUE),carna$Engine.Cylinders)
summary(carna)

carna$Number.of.Doors=ifelse(is.na(carna$Number.of.Doors),median(carna$Number.of.Doors,na.rm=TRUE),carna$Number.of.Doors)
summary(carna)

carna$Market.Category=ifelse(is.na(carna$Market.Category),median(carna$Market.Category,na.rm=TRUE),carna$Market.Category)
summary(carna)

#normalization
normalize<- function(x){
  return((x-min(x))/(max(x)-min(x)))
} #From the mathematical point of view when we refer to "normalization" it means transforming your values to the range between 0 and 1.

carna$Year<-normalize(carna$Year) #normalizing all the values of the column "year"
summary(carna) #Pre-processing done!

carna$Engine.HP<-normalize(carna$Engine.HP) #normalizing all the values of the column "engine.HP"
summary(carna) #Pre-processing done!

carna$Engine.Cylinders<-normalize(carna$Engine.Cylinders) #normalizing all the values of the column "engine.cylinder"
summary(carna) #Pre-processing done!

carna$Number.of.Doors<-normalize(carna$Number.of.Doors) #normalizing all the values of the column "number-of-doors"
summary(carna) #Pre-processing done!

carna$Market.Category<-normalize(carna$Market.Category) #normalizing all the values of the column "market.category"
summary(carna) #Pre-processing done!

carna$highway.MPG<-normalize(carna$highway.MPG) #normalizing all the values of the column "highway.MPG"
summary(carna) #Pre-processing done!

carna$city.mpg<-normalize(carna$city.mpg) #normalizing all the values of the column "city.mpg"
summary(carna) #Pre-processing done!

carna$Popularity<-normalize(carna$Popularity) #normalizing all the values of the column "popularity"
summary(carna) #Pre-processing done!

carna$MSRP<-normalize(carna$MSRP) #normalizing all the values of the column "MSRP"
summary(carna) #Pre-processing done!

Y<- carna[,"MSRP"]
X<- carna[,"Year"]
model1<-lm(Y ~ X)
model1

plot(Y ~ X) #It will hence produce a dot plot after which I will produce a regression line
abline(model1, col="Blue", lwd=4) #Hence regression line is produced(Positively Related)

Y<- carna[,"MSRP"]
X<- carna[,"Engine.HP"]
model2<-lm(Y ~ X)
model2

plot(Y ~ X) #It will hence produce a dot plot after which I will produce a regression line
abline(model2, col="Blue", lwd=4) #Hence regression line is produced(Positively Related)

Y<- carna[,"MSRP"]
X<- carna[,"Engine.Cylinders"]
model3<-lm(Y ~ X)
model3

plot(Y ~ X) #It will hence produce a dot plot after which I will produce a regression line
abline(model3, col="Blue", lwd=4) #Hence regression line is produced(Positively Related)

Y<- carna[,"MSRP"]
X<- carna[,"Number.of.Doors"]
model4<-lm(Y ~ X)
model4

plot(Y ~ X) #It will hence produce a dot plot after which I will produce a regression line
abline(model4, col="Blue", lwd=4) #Hence regression line is produced(Negatively Related)

Y<- carna[,"MSRP"]
X<- carna[,"Market.Category"]
model5<-lm(Y ~ X)
model5

plot(Y ~ X) #It will hence produce a dot plot after which I will produce a regression line
abline(model5, col="Blue", lwd=4) #Hence regression line is produced(Negatively Related)

Y<- carna[,"MSRP"]
X<- carna[,"highway.MPG"]
model6<-lm(Y ~ X)
model6

plot(Y ~ X) #It will hence produce a dot plot after which I will produce a regression line
abline(model6, col="Blue", lwd=4) #Hence regression line is produced(Negatively Related)

Y<- carna[,"MSRP"]
X<- carna[,"city.mpg"]
model7<-lm(Y ~ X)
model7

plot(Y ~ X) #It will hence produce a dot plot after which I will produce a regression line
abline(model7, col="Blue", lwd=4) #Hence regression line is produced(Negatively Related)

Y<- carna[,"MSRP"]
X<- carna[,"Popularity"]
model8<-lm(Y ~ X)
model8

plot(Y ~ X) #It will hence produce a dot plot after which I will produce a regression line
abline(model8, col="Blue", lwd=4) #Hence regression line is produced(Negatively Related)

p1<- predict(model1, data.frame("X"=2011)) #Predicted value of MSRP when Year=2011
p1

p2<- predict(model2, data.frame("X"=320)) #Predicted value of MSRP when Engine.HP=320
p2

p3<- predict(model3, data.frame("X"=4)) #Predicted value of MSRP when Engine.Cylinders=4
p3

p4<- predict(model4, data.frame("X"=4)) #Predicted value of MSRP when Number.of.Doors=4
p4

p5<- predict(model5, data.frame("X"="Luxury,Performance")) #Predicted value of MSRP when Market.Category="Luxury,Performance"
p5

p6<- predict(model6, data.frame("X"=28)) #Predicted value of MSRP when highway.MPG=28
p6

p7<- predict(model7, data.frame("X"=18)) #Predicted value of MSRP when city.mpg=18
p7

p8<- predict(model8, data.frame("X"=3916)) #Predicted value of MSRP when Popularity=10
p8

summary(carna)

#EDA stands for Exploratory data analysis
#EDA: Data Validation and Data Quality
#The str() function will do a sanity check on the structure and show sample data for each variable.
str(carna)

#Summary function works same,but a more complete function is the skim() function from the "skimr" package. 
#It breaks down the variables by type with relevant summary information, PLUS a small histogram for each numeric variable.
install.packages("skimr")
library(skimr)
skim(carna)

#EDA: Plot graphs using DataExplorer

#The plot_histogram() function will return a separate bar chart for each of our numeric variables. 
#It shows the frequency (number of occurrences) for each value in the variable.
install.packages("DataExplorer")
library(DataExplorer)
plot_histogram(carna)

#The boxplot (box and whisker diagram) displays the distribution of data for a variable. 
#The box shows us a "five number summary" - minimum, first quartile, median, third quartile, and maximum.
plot_boxplot(carna, by="MSRP")
plot_correlation(carna, type='continuous') #multivariyate correlation analysis
#here we learn that as engine HP incrases, the MSRP of the car also increases.


#Using ggplot2
install.packages("ggplot2")
head(carna)
library('ggplot2')
p<- ggplot(data=carna, aes(x=MSRP, y=Year) )
p

#Adding geometries to the ggplot i.e. p
p + geom_point() #point graph
p + geom_boxplot() #boxplot graph not apt though!
p + geom_abline() + geom_jitter() #line graph with jitter
p<- ggplot(data=carna, aes(x=MSRP, y=Engine.HP) )
p

#Adding geometries to the ggplot i.e. p
p + geom_point() #point graph
p + geom_boxplot() #boxplot graph not apt though!
p + geom_abline() + geom_jitter(aes(size=MSRP, colour= Year)) #line graph with jitter
p + geom_abline()

