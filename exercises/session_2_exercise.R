#Exercise 2

#making a change for the testBranch

library(ggplot2) # only load (run) once

#-----------------------------------------------------Exercise 2.2.1-----------------------------------------------------------

set.seed(1) #to obatain same values
x <- runif(100, 1, 100) # it creats 100 uniform distributed numbers from 1 to 100 into a vector
mean_x <- round(mean(x), 2)  #it calculates the mean of the 100 numbers and round it into 2 decimal float
mean_x
#?runif


#-----------------------------------------------------Exercise 2.2.2-----------------------------------------------------------


x <- seq(1,1000,1)
x <- c(1:1000)
x

y <- 1000-x
y

total_money<- sum(x)+sum(y)

#-----------------------------------------------------Exercise 2.2.3-----------------------------------------------------------


#The following code draws 10,000 random values from a beta distribution with the shape parameters a = 4 and b = 4 
#and multiplies them by a scale parameter scale = 15,000.

n <- 1e4 #number of observations
scale <- 25e4 #stands for upper bound on income that the beta distribution is capable of generating
income <- round( rbeta(n=n, shape1=2, shape2=10) * scale, 2)

#Let’s assume these simulated values represent the gross income in US$ of 10,000 people. The following lines of code plot a histogram, 
#which displays the counts of people that obtain an income in the respective range.



# Plot the resulting curve
ggplot(data.frame(x = income), aes(x=x)) +
  geom_histogram(color = "#0065BD", fill = "#0065BD", alpha=0.5, bins = 100) +
  scale_x_continuous(breaks = seq(0, scale, 2.5e4)) + 
  labs(x = "Gross income", 
       y = "Counts") + 
  theme_minimal()


# compute the share of each person

share <- income/sum(income)

#-----------------------------------------------Exercise 2.2.4 Investigating the indivisual share value--------------------------------------

income_s <- sort(income) #sort income ascendingly
group <- c("Lower 1%", "Lower 50%", "Top 10%", "Top 1%") #a new vector, giving headers 
p <- c(.1, .5, .9, .99) #define percentage

boundary <- round(income_s[p*n], 0) # finding the boundary salary of each group 

low10_m <- mean( income_s[c(1:(.1*n))] ) #find the mean of [the first one to the first 10% of n into the lowest 10]
low50_m <- mean( income_s[c(1:(.5*n))] )
top10_m <- mean( income_s[c((.9*n):n)] )
top1_m <- mean( income_s[c((.99*n):n)] )

means <-  round( c(low10_m, low50_m, top10_m, top1_m) , 0)  # round all means to whole number

income_summary <- data.frame(group, boundary, means) #build a data frame with group as first column, boundary, means as second and third
income_summary

##       group boundary means
## 1  Lower 1%      618   398
## 2 Lower 50%     1865  1073
## 3   Top 10%     4014  4979
## 4    Top 1%     6125  6737


#-----------------------------------------------------Exercise 2.3-----------------------------------------------------------

?data.frame
#1.createa a data t
df <- data.frame(var1 = c(1,2,3,4,5),
                 var2 = c(6,7,8,9,10),
                 var3 = c(11,12,13,14,15),
                 var4 = c(16,17,18,19,20),
                 var5 = c(21,22,23,24,25))
df
means <- colMeans(df)
means
#means <- apply(df, 2, mean)
?apply

#--------------Diamond-------------
library(tidyverse)
names(diamonds) 
View(diamonds)

#which
df_diamonds <-diamonds


rowsUsingWhich <- df_diamonds[which(df_diamonds$cut=="Ideal"),]
rowsUsingWhich


#filter
filter(df_diamonds,cut=="Ideal")

#select
?select
df_diamonds %>% select('cut')

