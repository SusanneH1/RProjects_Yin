#----------------------------------------------------------------Assignment 1--------------------------------------------------------------
library(tidyverse)
#-------------------------------------------------------------- Question ------ 2 Dice -------------------------------------------------------------

#-------------------- 1. create 2 vectors:-------------------

dice1 <- c(1:6)
dice2 <- c(1:6)

#-------------------- 2. using expand_grid() -----------------

?expand.grid


# create the data frame with all 36 possibilities
# while naming the first column as d2, second column as d1
# so that after switching, it looks exactly same as the example in pdf

outcome <- expand.grid(d2=dice1,d1=dice2)

# names(outcome) <- c("d1","d2") #alternative for naming the variables
# switching the columns so that the first column stays with 1 till all possibilities come out

outcome <- outcome[,c("d1","d2")]

#-------------------- 3. probability column -----------------

#outcome$prob <- c(rep(0.36,36))
outcome$prob <- 1/36

#-------------------- 4. sum column --------------------------

outcome$sum <- outcome$d1+outcome$d2


#-------------------- 5. sum >= 7 & dice1 == 3 ---------------

#sum of probability of the subset of data from "outcome" where the sum >=7 and the first dice is 3
#sum(subset(outcome,sum>=7 & d1 == 3)[,c("prob")])

sum(subset(outcome,sum>=7 & d1 == 3)$prob)

#-------------------- 6. sum >= 4 & <= 9 ---------------------

sum(subset(outcome,sum<=9&sum>=4)$prob)

#-------------------- 7. most probable sum -------------------

max(by(outcome$prob,outcome$sum,sum)) #choose the max from outcome table summed the probablity of outcome by the variable"sum" 


#-------------------------------------------------------------Question ------ Probability of Delay -------------------------------------------------------------


#-------------------- 8. possible numbers of delays ----------

?dbinom
#dbinom(0:10,10,0)
#dbinom(0:10,10,.1)

temp_function <- function(p) {
  dbinom(0:10,10,p)
} #define a function to run the dbinom from all possible p automatically

p_prob <-seq(0,1,0.1) #define the possible probabilities that need to be ran

distri <- sapply(p_prob, temp_function) #run the dbinom on all possible probabilities

distr_table <- as.data.frame(distri) # change it to data table

names(distr_table) <- c(0:10) #change the column names to possible numbers of delay
#names(distr_table)
row.names(distr_table) <- seq(0,1,0.1) # change the row names to possible probabilities of delay
#distr_table

#-------------------- 9. train ride data  ----------------------

sim_rides <- function(N, p){
  sample(c("L", "O"), size=N, replace=TRUE, prob=c(p, 1-p))
}
set.seed(1237)
obs <- sim_rides(10, .3)
obs

cal_function <- function(p){
  dbinom(sum(obs=="L"),length(obs),p)
} #create a function to calculate the possibility that this event happen

event_likelihood <- sapply(p_prob, cal_function)

#-------------------- 10. posterior probability  --------------

prior <- c(0.000, 0.004, 0.041, 0.123, 0.209, 0.246, 0.209, 0.123, 0.041, 0.004, 0.000)

posterior <- event_likelihood*prior
posterior

