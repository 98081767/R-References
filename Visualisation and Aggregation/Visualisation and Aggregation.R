#------------------------------------------------------------
# TOPICS:
#   - AGGREGATION
#   - VISUALISATIONS USING QPLOT, GGPLOT and SHINY
#------------------------------------------------------------

#create a function
xsquared = function(val) {
  val2 = val^2
  return(val2)
}

numbers = 1:10
xsquared(numbers)


moments = function(x) {
  # the mean is the first moment of x
  mean = mean(x)
  # standard deviation is the second moment of x
  sd = sd(x)
  # number of rows is given by the length of the vector
  rows = length(x)
  # combine the three vectors into a data frame
  moments = data.frame(mean, sd, rows)
  return(moments)
}
#apply our function to some numbers
moments(numbers)

#aggregation
setwd("C:/Personal/UTS/36106 Data Algorithms and Meaning/Week 4")
data = read.csv("diamonds.csv", header=TRUE)

str(data)
fix(data)

#the aggregate function will calculate the average carat for each cut
# the item you want to aggregate is on the left of the ~ and grouping factors on the right
# how to use formulas: http://www.dummies.com/programming/r/how-to-use-the-formula-interface-in-r/
aggregate(formula = carat ~ cut, data = data, FUN = mean)
#cut     carat
#1      Fair 1.0461366
#2      Good 0.8491847
#3     Ideal 0.7028370
#4   Premium 0.8919549
#5 Very Good 0.8063814


#We might wish to calculate the mean of several numeric variables. 
#To do this, we need to use the cbind() function to collect our numeric variables together, then pass this to the formula on the left of the tilda:
#this is to average out all the variables
aggregate(formula = cbind(carat, depth, price, x, y, z) ~ cut, data = data, FUN = mean)

#We might then wish to add another grouping variable, to see how the means vary over combinations of two
#variables. In the formula, we simply use + to add additional grouping variables. Let's try it with color and
#clarity. We will also store the result in a data frame, aggr, and output the top 10 rows:
aggr = aggregate(formula = cbind(carat, depth, price, x, y, z) ~ cut + color + clarity, data = data, FUN = mean)
head(aggr, 10)

#cut color clarity     carat    depth    price        x        y        z
#1       Fair     D      I1 1.8775000 65.60000 7383.000 7.517500 7.422500 4.905000
#2       Good     D      I1 1.0400000 61.35000 3490.750 6.305000 6.293750 3.841250
#3      Ideal     D      I1 0.9600000 61.45385 3526.923 6.067692 6.043077 3.716923
#4    Premium     D      I1 1.1550000 61.90000 3818.750 6.703333 6.675000 4.140833
#5  Very Good     D      I1 0.9500000 62.20000 2622.800 6.244000 6.242000 3.886000
#6       Fair     E      I1 0.9688889 65.64444 2095.222 6.170000 6.061111 4.008889
#7       Good     E      I1 1.3308696 61.66087 4398.130 6.926522 6.898696 4.263913
#8      Ideal     E      I1 1.0377778 61.85000 3559.389 6.346111 6.327222 3.918889
#9    Premium     E      I1 1.0430000 60.80667 3199.267 6.393333 6.344333 3.876000
#10 Very Good     E      I1 1.0695455 61.48182 3443.545 6.425000 6.436818 3.955000

#If we want to order our data frame by a variable, we can use the order() function applied to the rows
#index in the data frame. The first argument is the variable name which we want to sort by, and the second
#argument is whether we want to sort by decreasing or not. This is handy to remember
head(aggr[order(aggr$carat, decreasing = T), ])

#Lastly, we can use any function we like for the aggregate numeric variable calculation, such as a function
#similar to the moments function we defined earlier. We have to modify the syntax slightly to comply with the
#FUN argument in aggregate.
aggregate(formula = carat ~ cut, data = data, FUN = function(x) { c(mean = mean(x), sd = sd(x), rows = length(x)) })
#        cut   carat.mean     carat.sd   carat.rows
#1      Fair 1.046137e+00 5.164043e-01 1.610000e+03
#2      Good 8.491847e-01 4.540544e-01 4.906000e+03
#3     Ideal 7.028370e-01 4.328763e-01 2.155100e+04
#4   Premium 8.919549e-01 5.152616e-01 1.379100e+04
#5 Very Good 8.063814e-01 4.594354e-01 1.208200e+04

#----------------------------------
# ADVANCED DATA VISUALISATION
#----------------------------------
# qplot - http://docs.ggplot2.org/current/
#
install.packages("ggplot2")
library(ggplot2)

head(data)

#qplot() requires 3 arguments: x (variable), data, geom (visualisation)
# the auto geom means that it reads it as a numeric varaible and tries to plot the histogram
qplot(x=carat, data=data, geom="auto")

#add fill property to create a stacked histogram
qplot(x=carat, data=data, geom="auto", fill=clarity)

#now create a density distribution (i.e histogram that has a smooth curve)
qplot(x=carat, data=data, geom="density", fill=clarity, alpha=I(0.25))

#now to show scatter plot between carat and price
qplot(x = carat, y = price, data = data, geom = "point", color = clarity,
      main = "Plot of Price against Carat by Color")

#-------------------------------------
# ggplot 
#. http://docs.ggplot2.org/current/
#. http://www.statmethods.net/advgraphs/ggplot2.html
#. http://tutorials.iq.harvard.edu/R/Rgraphics/Rgraphics.html

# if you want to add more customisations you need to use the core ggplot syntax
# core ggplot syntax: ggplot(data, aes()) + geom()
# elements:
# 1. data
# 2. aes (aesthetics)
# 3. geom (geometry)
----------------------------------------------------
  
#this is the same as above but using ggplot
ggplot(data = data, aes(x = carat, y = price, color = clarity)) + geom_point()

#you will notice in the image that there is a relationship between price and carat for different clarity. 
# we can add trend lines and let R choose the line method.
ggplot(data = data, aes(x = carat, y = price, color = clarity)) + geom_point() + geom_smooth(method = "auto")


# there is a very distinct line for clarity I1. Judging fromt he plot, it might be worthwhile to separate out the plots
# this is known as faceting the plot. By adding facet_wrap(~clarity)
# while we are here we can also colour code by cut to see if there is any relationship.
ggplot(data = data, aes(x = carat, y = price, color = cut)) + geom_point() + facet_wrap( ~ clarity) + geom_smooth(method = "auto")


#-------------------------------------
# shiny 
# https://shiny.rstudio.com/
# https://shiny.rstudio.com/tutorial/
# allows interactive data analysis
#-------------------------------------

install.packages("shiny")

library(shiny)
library(ggplot2)

# Structure:
ui = shinyUI(bootstrapPage(
  #selectInput is a function for creating a simple selection. Display a trend line?
  selectInput(inputId = "variable",
              label = "Select a variable to overlay",
              choices = c("clarity","cut","color"),
              selected = "clarity"),
  #choose the geometry to use
  selectInput(inputId = "geometry",
              label = "Select a geometry",
              choices = c("density", "histogram")),
  #display the plot which we will define in the server
  plotOutput(outputId = "main_plot")
))

# server function - This tells R that this is our server
# input from the UI, output back to the UI
server = shinyServer(function(input, output) {
  # function to create plot and pass to UI
  output$main_plot = renderPlot({
    # filter out data frame
    data = data[, c("price", input$variable)]
    # define base plot
    plot = qplot(x = price, data = data, geom = input$geometry, fill = data[, 2], alpha = I(0.25))
    # output the plot
    plot
  })
})

#run the shiny app
shinyApp(ui, server)




