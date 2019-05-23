#Project Goal, can we determine the chemical components that leads to good (or bad) tasting wine?
#Then, using this knowledge could be predict future wines that we think will taste better or worse than others?


#Install Packages to Perform Data Exploration and visualization
install.packages("dplyr")
install.packages("tidyr")
install.packages("ggplot2")
install.packages("GGally")

library(ggplot2)
library(GGally)
library(dplyr)
library(tidyr)

#First things first, we need to bring in our data
#the Wine Predictor Data set is in the Wine and Dine Repository and can be read into R with the following
#2 lines of code
Wine_Data_Set<-'https://raw.github.com/eridsarge/Operation-Wine-and-Dine/master/Wine Predictor.csv'
Wine_Predictor<-read.csv(Wine_Data_Set)


#Explore Data Set to Get understanding of Data Elements
head(Wine_Predictor)
View(Wine_Predictor)


#It looks like we have 11 variables that could potentially have an impact on predicting wine quality
#Lets first check the data to assess missing variables
Wine_Predictor%>%is.na%>%which()
#We are lucky, there are no missing values in this data set
#if a few points from a variable were missing I probably would have considered just dropping those rows 
#or inserting the mean value (using mean may be favorable since we do not have a ton of observations in this set)


#Start to understand our variables by performing analyzing basic summary statistics
summary(Wine_Predictor)


#A picture says 1000 words
#so im going to plot the distribution for each variable to better understand what is going on
#lets start with a trusty historgram and look at each variable
#used a looping function to view all variables at once

par(mfrow=c(4, 3))
colnames <- dimnames(Wine_Predictor)[[2]]
for (i in 1:12) {
  hist(Wine_Predictor[,i], main=colnames[i], probability=TRUE, col="gray", border="white")
}

#lets get some general observations before we start attempting to construct our model
#Many Red Wines appear to have Low Citric Acid (Potentially good predictor of good wines?)
#Residual Sugar and Chlorides dont seem to have alot of variation, could this be helpful?
#density and PH stand out due to what appears to be a normal distribution (potentially good predictors)


#Additional visualizations to look at variable correlation
ggpairs(Wine_Predictor)
ggcorr(Wine_Predictor, hjust=.9,size=3,palette = "RdBu", label = TRUE,layout.exp = 1)
#looks like tasters like wine with higher Alcohol Content?
#it also looks like the following variables could be correlated
#total sulfur and free sulfur (which makese sense?)
#fixed acidity and citric acidity (more of one acid means more of another?)
#density and fixed acidity (does acid increase density in liquids?)


#the Most important observation is around our response variable, quality
#quality is a categorical variable that does not appear to have a normal distribution
#my new thought is rather than predicting the individual score, it may make more sense
#to attempt to predict if a wine is simply "good" or "bad"


#how do we determine our cut off point though for good vs bad wines?
#lets look at the quartiles to see how our quality value is split
quantile(Wine_Predictor$quality)


#I also want to look at how our quality variable is bucketed if we split it by the 
#50% quartile to make sure we do not have too many good or too many bad wines
Quality_Count<-mutate(Wine_Predictor,Wine_Quality=ifelse(quality<6,"Bad","Good"))%>%
  count(Wine_Quality)%>%group_by(Wine_Quality)%>%rename(Wine_Count=n)

ggplot(data=Quality_Count,aes(x=Wine_Quality,y=Wine_Count,fill=Wine_Quality))+
  geom_bar(stat="identity")


#now that we have decided our cutoff, lets edit our data so that it falls in line with our new approach
Wine_Predictor_Final<-mutate(Wine_Predictor,quality=ifelse(quality<6,0,1),id=row_number())

View(Wine_Predictor_Final)


#Now that we have the data set we want to work with, split out data into training and validation
train<- Wine_Predictor_Final%>% sample_frac(.70)
test<- anti_join(Wine_Predictor_Final, train, by = 'id')


#a quick plot to see the distribution of quality in training and test
par(mfrow=c(1, 2))
barplot(table(train$quality))
barplot(table(test$quality))
#it looks like are training and test sets have a similar number of good and bad wines
#so we should be able to use this for buidling our model


#moving on to the fun part, creating our logistic regression model
#im going to start by using all our variables to see how our results look on the training data
wineglm<-glm(quality~fixed.acidity+volatile.acidity+citric.acid+residual.sugar+chlorides+free.sulfur.dioxide+
            total.sulfur.dioxide+density+pH+sulphates+alcohol,data=train,family=binomial)

summary(wineglm)
#our summary results reveal some interesting results
#the following values appear to be strong predictors of quality
#volatile.acidity, chlorides, free sulfur dioxide, total sulfur dioxide, sulphates, alcohol
#looking at the estimate values, is it safe to reason people like drier, less acidic red wines?


#several of our variables are not coming back as significant though
#lets try running our model and see if it improves our current AIC score after removing these variables
wineglm2<-glm(quality~volatile.acidity+citric.acid+chlorides+free.sulfur.dioxide+
               total.sulfur.dioxide+sulphates+alcohol,data=train,family=binomial)

summary(wineglm2)
#citric acid is no longer significant
#from our previous correlation matrix, we know that there is an observed relationship between 
#both the sulfurs and both the acids in wine


#so lets run our model one more time leaving in only 1 sulfur and 1 acids based on Pvalue and Estimates
wineglm3<-glm(quality~volatile.acidity+chlorides+
                total.sulfur.dioxide+sulphates+alcohol,data=train,family=binomial)

summary(wineglm3)
#Oh no, our 3rd model now has an even higher AIC than our first model! Is this a problem?
#Personally, I feel best about our final, paired down model though
#it only has variables with significant p-values and removes values that are highly correlated
#it would feel as though we would be overfitting the model if these variables were left in
#*****future iteration might be to perform PCA on these correlated variables?


#Now lets see how the model performs on our test data to determine if we made a good choice with selecting option 3
Wine_Confidence<-predict.glm(wineglm3, newdata = test, type = "response")

#The Logistic Regression outputs a Confidence for each of our Values
#to start, I am going to set the cut off at .5
#meaning that if the model is greater than 50% confident a wine will be good then classify it as good
#otherwise classify it as a bad wine
Wine_Prediction<-mutate(test,Wine_Prediction=ifelse(Wine_Confidence>0.5,1,0))

#now lets look at the percentage of time we predicted good or bad
1-mean(Wine_Prediction$Wine_Prediction!=Wine_Prediction$quality)
#Over 70% accuracy! Way better than picking based on the picture on the label! 


#Another consideration is to see how often we got false positives and false negatives
table(Wine_Prediction$Wine_Prediction,Wine_Prediction$quality)
#right now there is a pretty even split between false positives and false negatives


#if we were nervous about trying a "bad" wine we could go back and adjust our Wine Confidence to be higher than .5 
#for me though I feel willing to roll the dice since this isnt a life or death situation
#(although a more serious experiement may warrent a higher confidence, say if we were tasting a delicious
#medium rare chicken or puffer fish sushi)


#So what is our Conclusion?
#Looking back at our variable estimates, it seems that people like the following:
#less acidic wine
#wines with fewer chlorides (research tells me that high chlorides can cause a salty taste)
#lower sulfur dioxide (some people can be allergic, including my aunt)
#Sulphates do on the other hand seem to lead to better tasting wine
#Finally, who doesnt like a nice, dry glass of red? More booze means better quality, so cheers!


