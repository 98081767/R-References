#---------------------------------------------
# LAB 4.6 ISLR - Classification regression using Logistic regression, LDA, QD and KNN
#---------------------------------------------
# involves examining Stock market (Smraket) data
# consisting of percentage returns for the S&P 500 stock index over 1250 days from 2001 to 2005.
# A data frame with 1250 observations on the following 9 variables.
# for each date we have recorded: 
# - the percentage returns for each of the five previous trading days, Lag1 to Lag5. 
# - Volume (number of shared traded on the previous day in billions)
# - Today (the percentage return on the date in question)
# - Direction (whether the market was up or down this date)


rm(list=ls())

install.packages("ISLR")
library(ISLR)

names(Smarket)
fix(Smarket)
dim(Smarket)

summary(Smarket)
#Year           Lag1                Lag2                Lag3                Lag4                Lag5              Volume           Today          
#Min.   :2001   Min.   :-4.922000   Min.   :-4.922000   Min.   :-4.922000   Min.   :-4.922000   Min.   :-4.92200   Min.   :0.3561   Min.   :-4.922000  
#1st Qu.:2002   1st Qu.:-0.639500   1st Qu.:-0.639500   1st Qu.:-0.640000   1st Qu.:-0.640000   1st Qu.:-0.64000   1st Qu.:1.2574   1st Qu.:-0.639500  
#Median :2003   Median : 0.039000   Median : 0.039000   Median : 0.038500   Median : 0.038500   Median : 0.03850   Median :1.4229   Median : 0.038500  
#Mean   :2003   Mean   : 0.003834   Mean   : 0.003919   Mean   : 0.001716   Mean   : 0.001636   Mean   : 0.00561   Mean   :1.4783   Mean   : 0.003138  
#3rd Qu.:2004   3rd Qu.: 0.596750   3rd Qu.: 0.596750   3rd Qu.: 0.596750   3rd Qu.: 0.596750   3rd Qu.: 0.59700   3rd Qu.:1.6417   3rd Qu.: 0.596750  
#Max.   :2005   Max.   : 5.733000   Max.   : 5.733000   Max.   : 5.733000   Max.   : 5.733000   Max.   : 5.73300   Max.   :3.1525   Max.   : 5.733000  
#Direction 
#Down:602  
#Up  :648 
stock = Smarket

#show top 100 rows
stock[1:100,]

pairs(stock)
#check correlation between pairs (remove direction column)
cor(stock[,-9])
#            Year         Lag1         Lag2         Lag3         Lag4         Lag5      Volume        Today
#Year   1.00000000  0.029699649  0.030596422  0.033194581  0.035688718  0.029787995  0.53900647  0.030095229
#Lag1   0.02969965  1.000000000 -0.026294328 -0.010803402 -0.002985911 -0.005674606  0.04090991 -0.026155045
#Lag2   0.03059642 -0.026294328  1.000000000 -0.025896670 -0.010853533 -0.003557949 -0.04338321 -0.010250033
#Lag3   0.03319458 -0.010803402 -0.025896670  1.000000000 -0.024051036 -0.018808338 -0.04182369 -0.002447647
#Lag4   0.03568872 -0.002985911 -0.010853533 -0.024051036  1.000000000 -0.027083641 -0.04841425 -0.006899527
#Lag5   0.02978799 -0.005674606 -0.003557949 -0.018808338 -0.027083641  1.000000000 -0.02200231 -0.034860083
#Volume 0.53900647  0.040909908 -0.043383215 -0.041823686 -0.048414246 -0.022002315  1.00000000  0.014591823
#Today  0.03009523 -0.026155045 -0.010250033 -0.002447647 -0.006899527 -0.034860083  0.01459182  1.000000000

#as you can see there is little correlation between today's return and lag variables.
# the only correlation is with Year and volume

#attach stock so you can just name variables
attach(stock)
#view volume increase over the years
plot(Volume)
plot(Volume~Year)

boxplot(Volume~Year)

#----------------------------------------------
# LOGISTIC REGRESSION
# we want to predict Direction using Lag1-5 and Volume
# the glm() function fits generalised linear models, a class models that includes logistic regression. It's similar to lm()
# we need to pass the argument family=binonmial to tell R to run a logistic regression
#----------------------------------------------
#this returns the coding that R uses for the dummy variables
contrasts(Direction)
#     Up
#Down  0
#Up    1

glm.fit = glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume, data=stock, family=binomial)
summary(glm.fit)

#Deviance Residuals: 
#  Min      1Q  Median      3Q     Max  
#-1.446  -1.203   1.065   1.145   1.326  

#Coefficients:
#             Estimate  Std. Error z value Pr(>|z|)
#(Intercept) -0.126000   0.240736  -0.523    0.601
#Lag1        -0.073074   0.050167  -1.457    0.145 <-- smallest p-value. The negative slope indicates if the market had a positive return yesterday, then it is less likely to go up today. But the p-value is still large
#Lag2        -0.042301   0.050086  -0.845    0.398
#Lag3         0.011085   0.049939   0.222    0.824
#Lag4         0.009359   0.049974   0.187    0.851
#Lag5         0.010313   0.049511   0.208    0.835
#Volume       0.135441   0.158360   0.855    0.392

#to get the co-efficients
coef(glm.fit)
summary(glm.fit)$coef

#The predict() function can be used to predict the probability that the market will go up, given values of the predictors. 
#The type=response option tells R to output the probabilities in the form P(Y=1|X)
#If no dataset is supplied to the predict function, then the probabilities are computed for the training data that was used to fit he regression model
#- Next shows the first 10 probabilities
# we know that these values correspond to the probabiliy of the market going up because the contrasts() function has created a dummy variable of 1 for Up.
glm.probs=predict(glm.fit, type="response")
glm.probs[1:10]
# 1         2         3         4         5         6         7         8         9        10 
#0.5070841 0.4814679 0.4811388 0.5152224 0.5107812 0.5069565 0.4926509 0.5092292 0.5176135 0.4888378 
#
# This is the probability for each row whether the market will go Up. 



# In order to make a prediction as to whether the market will go up or down on a particular day, 
# we must convert the predicted probabilities into class labels - Up or Down. 
# The following two commands create a vector of class predictions based on whether the predicted probability of a market increase is 
# creater or less than 0.5

#create a vector of 1250 "Down" elements
glm.pred=rep("Down", 1250)
#update the Vector where the probability of each row is greater than 50%
glm.pred[glm.probs > .5]="Up"

#with the predictions, the table() function can be used to produce a confusion matrix in order to determine how many
#observations were correctlly or incorrectly classified.
table(glm.pred, Direction)
#             Direction
#glm.pred     Down      Up            TOTAL
#Down         145       141(21%)      286
#Up           457(75%)  507           964
#TOTALS       602       648           1250

(507+145) / 1250
#0.52 (52% success rate)
mean(glm.pred==Direction)
#0.52 (52% success rate) - pretty poor success rate. To reject the Null hypothesis (p=0.5) it needs to be over 0.6 at least
#we also trained the model on the same set of 1250 observations. ie. 100-52.2 = 47.8% 'training' error rate.

#to better assess the accuracy of the logistic regression model we can fit the model using part of the data and see how it predicts with the held out data. 
# this will yield a more realistic error rate, 
# First we create a vector corresponding to observations from 2001 to 2004. We will then use this data to predict 2005 data. 

#train is a boolean vector - true= <2005, false = 2005
train = (Year<2005)
stock.2005=stock[!train,]
dim(stock.2005)
#[1] 252   9

Direction.2005=Direction[!train]

#add "subset = train" to train the regression on values < 2005
glm.fit = glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume, data=stock, family=binomial, subset=train)
summary(glm.fit)

#Deviance Residuals: 
#  Min      1Q  Median      3Q     Max  
#-1.302  -1.190   1.079   1.160   1.350  

#Coefficients:
#             Estimate Std. Error z value Pr(>|z|)
#(Intercept)  0.191213   0.333690   0.573    0.567
#Lag1        -0.054178   0.051785  -1.046    0.295
#Lag2        -0.045805   0.051797  -0.884    0.377
#Lag3         0.007200   0.051644   0.139    0.889
#Lag4         0.006441   0.051706   0.125    0.901
#Lag5        -0.004223   0.051138  -0.083    0.934
#Volume      -0.116257   0.239618  -0.485    0.628

#now that glm.fit has been trained on <2005 data, predict on 2005 using stock.2005
glm.probs=predict(glm.fit, stock.2005, type="response")
glm.probs[1:10]

#create a vector of 252 "Down" elements
glm.pred=rep("Down", 252)
#update the Vector where the probability of each row is greater than 50%
glm.pred[glm.probs > .5]="Up"

table(glm.pred, Direction.2005)
#                 Direction.2005
#       glm.pred Down   Up
#Down             77    97
#Up               34    44
#TOTAL            111   141   252

#error rate 1- ((77+44)/252) = 52% - worse than random guessing
#[1] 0.5198413
mean(glm.pred==Direction.2005)
#[1] 0.4801587
mean(glm.pred!= Direction.2005)
#[1] 0.5198413

#recall that the logistic regression model had underwhelming p-values will all of it's predictors. 
#perhaps we should remove the predictors that are ineffective. (After all, using predictors that have no relationships causes a deteriation in the error rate)
# - since such predictors cause an increase in variance without any corresponding decrease in bias. 


glm.fit = glm(Direction~Lag1+Lag2, data=stock, family=binomial, subset=train)
summary(glm.fit)

#Coefficients:
#             Estimate Std. Error z value Pr(>|z|)
#(Intercept)  0.03222    0.06338   0.508    0.611
#Lag1        -0.05562    0.05171  -1.076    0.282
#Lag2        -0.04449    0.05166  -0.861    0.389

glm.probs=predict(glm.fit, stock.2005, type="response")
glm.probs[1:10]

#create a vector of 252 "Down" elements
glm.pred=rep("Down", 252)
#update the Vector where the probability of each row is greater than 50%
glm.pred[glm.probs > .5]="Up"

table(glm.pred, Direction.2005)
#           Direction.2005
#glm.pred     Down  Up
#Down           35  35
#Up             76 106
mean(glm.pred==Direction.2005)
#[1] 0.5595238 <-- success rate

#however - on days when the market was up the model was correct 58% of the time
#this suggests a buying strategy of buying on days when the model predicts an increasing market. 
106/(106+76)
#[1] 0.5824176

#suppose we want to predict the returns associated with particular values of Lag1 and Lag2
#e.g we want to predict Direction on a day when Lag1 and Lag2 equal 1.2 and 1.1, and on a day when they equal 1.5 and -0.8.

predict(glm.fit, newdata=data.frame(Lag1=c(1.2, 1.5), Lag2=c(1.1,-0.8)), type="response")
# 1         2 
# 0.4791462 0.4960939 


#----------------------------------------------
# 4.6.3 LINEAR DISCRIMINANT ANALYSIS
#
#----------------------------------------------


