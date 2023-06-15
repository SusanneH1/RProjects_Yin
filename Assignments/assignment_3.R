#Assignment 3
# load the necessary libraries
library(rethinking)
library(tidyverse)
library(readr) #to read csv file
library(ggplot2)


#Pre-Processing
#-------------------------------1. Load the data set Aging.csv and store it in an object------------------------------------------------


data <- read_csv("Assignments/Aging.csv")
#View(data)

#-------------------------------2. Delete all observations (rows) containing missing values (NA)------------------------------------------------

clean_data <- na.omit(data)
View(clean_data)


#-------------------------------3. Groups people into 2 age groups------------------------------------------------

str(clean_data) #check the class
avg_age<- round(mean(clean_data$Age),0)
clean_data$AG <- 0
clean_data$AG[clean_data$Age>avg_age] <-2 #old
clean_data$AG[clean_data$Age<=avg_age] <-1

#clean_data$AG <- ifelse(clean_data$Age>avg_age,2,1) 
#Alternatively doing it like this single line 


#Gaussian Models of Decision Quality and Risk Seeking

#-------------------------------4. Gaussian model for the variables DecisionQuality and RiskSeeking (separately) using the quap() function------------------------------------------------

#-------------------------------4.1 DecisionQuality-------


library(rethinking)

# For DecisionQuality
model_DQ <- quap( 
  alist( 
    DecisionQuality ~ dnorm( mu, sigma ), 
    mu ~ dnorm(0.55,0.15), 
    sigma ~ dunif(0,0.15) 
 ), 
  data = clean_data )
# DecisionQuality ranges from 0 to 1 but people dont always choose the one with higher expectation 

# Estimated parameter values
estimated_params_DQ <- precis(model_DQ)
samples_DQ <- extract.samples(model_DQ,1000)
HPDI(samples_DQ$mu,prob=0.95)
HPDI(samples_DQ$mu)

#-------------------------------4.2 RiskSeeking--------

model_RS <- quap( 
  alist( 
    RiskSeeking ~ dnorm( mu, sigma ), 
    mu ~ dnorm(0.5,15), 
    sigma ~ dunif(0,0.1) 
 ), 
  data = clean_data )

estimated_params_RS <- precis(model_RS)
samples_RS <- extract.samples(model_RS,1000) 
HPDI(samples_RS$mu,prob=0.95)

#Interpretation

#For Decision Quality:
#  The 95% HPDI for the mu is [0.6340597, 0.6664789 ]. It means that with 95% confidence, the mean value of Decision Quality falls within this range. 
#  Which further confirms that people don't always choose the options with higher expectations

#For Risk Seeking:
#  The 95% HPDI for the mu is [0.4530877, 0.4851757 ]. It means that with 95% confidence, the mean value of Risk Seeking falls within this range. 
#  It tells us that there is a slight tendency of choosing the low-risk option


#-------------------------------5. Show that young and old people, on average, do not differ in decision quality------------------------------------------------

# model for old people
model_old <- quap( 
  alist( 
    DecisionQuality ~ dnorm( mu, sigma ), 
    mu ~ dnorm(0.55,0.15), 
    sigma ~ dunif(0,0.15) 
  ), 
  data = clean_data[clean_data$AG==2,] )

# model for young people
model_young <- quap( 
  alist( 
    DecisionQuality ~ dnorm( mu, sigma ), 
    mu ~ dnorm(0.55,0.15), 
    sigma ~ dunif(0,0.15) 
  ), 
  data = clean_data[clean_data$AG==1,] )

# Estimate models for each group

# Sample from posterior distributions
samples_young <- extract.samples(model_young, n=10000)
samples_old <- extract.samples(model_old, n=10000)

# Calculate difference
difference <- samples_young - samples_old
difference_mu <- samples_young$mu - samples_old$mu


# Visualize the distribution of difference.

ggplot(data.frame(difference_mu), aes(x = difference_mu)) + 
  geom_density(color = "grey", linewidth = 2) + 
  labs(x = expression(mu), 
       y = "Density") + 
  ggtitle("Distribution of Difference between Young and Old") +
  theme_minimal()


ggplot(data.frame(difference_mu), aes(x = difference_mu)) +
  geom_histogram(fill = 'grey', color = "black", bins = 30) +
  labs(x = expression(Difference ~ mu), y = "Frequency") +
  ggtitle("Distribution of Difference between Young and Old") +
  theme_minimal()

# Linear Prediction

#-------------------------------6. Standardize all variables except age group------------------------------------------------


data_standardized <- clean_data
data_standardized[1:6]<-scale(data_standardized[1:6]) #except age group
view(data_standardized) 


#-------------------------------7. Standardize all variables except age group------------------------------------------------
# https://www.r-bloggers.com/2021/07/how-to-do-a-simple-linear-regression-in-r/

# First Option
model_DQ_Numeracy <- lm(DecisionQuality ~ Numeracy, data = data_standardized)
summary(model_DQ_Numeracy)
model_DQ_Speed <- lm(DecisionQuality ~ Speed, data = data_standardized)
model_DQ_NegAffect <- lm(DecisionQuality ~ NegAffect, data = data_standardized)

model_RS_Numeracy <- lm(RiskSeeking ~ Numeracy, data = data_standardized)
model_RS_Speed <- lm(RiskSeeking ~ Speed, data = data_standardized)
model_RS_NegAffect <- lm(RiskSeeking ~ NegAffect, data = data_standardized)

# Second Option as in course


# for DecisionQuality
model_DQ_Numeracy  <- quap(
  alist(
    DecisionQuality ~ dnorm(mu, sigma),
    mu <- a + b * Numeracy,
    a ~ dnorm(0, 1),
    b ~ dnorm(-1, 1),
    sigma ~ dunif(0, 1)
  ),
  data = data_standardized
)
#b=0.35

precis(model_DQ_Numeracy)

model_DQ_Speed <- quap(
  alist(
    DecisionQuality ~ dnorm(mu, sigma),
    mu <- a + b * Speed,
    a ~ dnorm(0, 3),
    b ~ dnorm(0.5, 0.5),
    sigma ~ dunif(0, 1)
  ),
  data = data_standardized
)
precis(model_DQ_Speed)
#b=0.23
model_DQ_NegAffect  <- quap(
  alist(
    DecisionQuality ~ dnorm(mu, sigma),
    mu <- a + b * NegAffect,
    a ~ dnorm(0, 3),
    b ~ dnorm(1, 0.5),
    sigma ~ dunif(0, 1)
  ),
  data = data_standardized
)
precis(model_DQ_NegAffect)
#b=-0.11

# for RiskSeeking
model_RS_Numeracy <- quap(
  alist(
    RiskSeeking ~ dnorm(mu, sigma),
    mu <- a + b * Numeracy,
    a ~ dnorm(0, 3),
    b ~ dnorm(0.5, 0.2),
    sigma ~ dunif(0, 1)
  ),
  data = data_standardized
)

precis(model_RS_Numeracy)
#b=-0.02

model_RS_Speed <- quap(
  alist(
    RiskSeeking ~ dnorm(mu, sigma),
    mu <- a + b * Speed,
    a ~ dnorm(0, 3),
    b ~ dnorm(0.5, 0.2),
    sigma ~ dunif(0, 1)
  ),
  data = data_standardized
)
precis(model_RS_Speed)
#b=-0.1

model_RS_NegAffect <- quap(
  alist(
    RiskSeeking ~ dnorm(mu, sigma),
    mu <- a + b * NegAffect,
    a ~ dnorm(0, 3),
    b ~ dnorm(1, 0.5),
    sigma ~ dunif(0, 1)
  ),
  data = data_standardized
)
precis(model_RS_NegAffect)
#b=-0.24

# Adavantages
# 1. Standardizing variables allows for easier comparison of their effects as we put variables on the same scale, we can easily compare the magnitudes of their coefficients and determine which predictors have stronger impacts on the outcome.
# 2. Easier to interpret. The standardized coefficients represent the change in the outcome variable associated with a one standard deviation change in the predictor variable. This makes it easier to understand the impact of predictors


# Numeracy has most effect on DeciaionQuality
# NegAffect has most effect on RiskSeeking


#-------------------------------8. Build a linear prediction model to estimate the associations among the variables Speed,Numeracy and DecisionQuality------------------------------------------------


model_DQ_Speed_Numeracy <- quap(
  alist(
    DecisionQuality ~ dnorm(mu, sigma),
    mu <- a + b1 * Speed + b2 * Numeracy,
    a ~ dnorm(0, 3),
    b1 ~ dnorm(0.5, 0.5),
    b2 ~ dnorm(-1 , 1),
    sigma ~ dunif(0, 1)
  ),
  data = data_standardized
)
precis(model_DQ_Speed_Numeracy)


# Assumptions:
# 1. the higher the numeracy -> the higher the speed
# 2. the higher the speed -> the lower the decision quality
# 3. the higher the numeracy -> the higher the decision quality
# -> Confounding effects


# Result Checking
# 1. b1 = 0.13 > 0 -> speaks against my second assumption 
# 2. b2 = 0.3 > 0 -> speaks for my third assumption 
# 3. If Numeracy mediates the relationship between Speed and DecisionQuality, we would expect the coefficient for Speed to decrease significantly when Numeracy is included in the model. In my case, the coefficient for Speed decreases from 0.23 (when it's the only predictor) to 0.13 (when both predictors are included). This suggests that there might be some mediation
# 4. However, If Speed is a confounder for the relationship between Numeracy and DecisionQuality, we would expect the coefficient for Numeracy to change significantly when Speed is included in the model. In your case, the coefficient for Numeracy decreases from 0.35 (when it's the only predictor) to 0.30 (when both predictors are included). This suggests that there might be some confounding effect