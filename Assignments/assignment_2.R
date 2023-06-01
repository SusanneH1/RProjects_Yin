#-----------------------------------------------Assignment 2-------------------------------------------------------
pacman::p_load(tidyverse)

#Bayesian Updating
#-----------------------1-------------------------
#data <- c(rep("P",6),rep("N",4))


likelihood <- dbinom(6, 10, c(.7,.5,.8))

# you think it is twice as likely that the reviews belong to Company B compared to Company A and also compared to Company C
# -> meaning P(B)=2(PA)=2P(C) 
prior <- c(.25,.5,.25)

posterior <- likelihood*prior / (sum(prior * likelihood))
posterior
posterior_A <-posterior[1]
posterior_A

#-----------------------2-------------------------
#Since the question specifically mentions not to include the first 10 reviews again, it implies that we should use the old posterior as the new prior and update it based on the likelihood of the 10 most recent reviews

likelihood_new <- dbinom(9,10,c(.7,.5,.8))
prior_new <- posterior
posterior_new <-likelihood_new*prior_new/sum(prior_new*likelihood_new)
posterior_new[3]-posterior[3]

#-----------------------3-------------------------
# the probability that the next shipment will contain defective products -> P(Next_Defective | Defective).
D_A <- 0.1 # Defect rate of 10% from A
D_B <- 0.2 # Defect rate of 20% from B

P_A <- 0.5 # as company C receives equally many shipments from both factories-> Probability of the current shipment being from Factory A
P_B <- 0.5 # Probability of the current shipment being from Factory B
P_D_given_A <- D_A # (P(D|A))Probability of a defective shipment given it is from Factory A (0.1)
P_D_given_B <- D_B 



# P(A|D): Probability that the current shipment is from Factory A given it is defective = (P(D|A) * P(A)) / (P(D|A) * P(A) + P(D|B) * P(B)) 

P_D <- P_D_given_A * P_A + P_D_given_B * P_B #Probability of defect
P_A_given_D <- (P_D_given_A * P_A) / P_D
P_B_given_D <- (P_D_given_B * P_B) / P_D

# Probability of the next shipment being defective given the current shipment is defective
P_Next_Defective_given_Defective <- P_D_given_A * P_A_given_D + P_D_given_B * P_B_given_D

P_Next_Defective_given_Defective


#No Question 4 found in the slide

#-----------------------5-------------------------

# First Section: First ignore the information on defected products and compute the posterior probability that the shipment is from Factory A given the output of the algorithm.
# Prior probabilities ignorning the defect information 
prior_A <- 0.5
prior_B <- 0.5

# Probability of a positive algorithm output given the shipment is from Factory A
prob_A_positive <- 0.8
prob_B_positive <- 0.65

# Total probability of a positive algorithm output
prob_positive <- (prob_A_positive * prior_A) + ((1-prob_B_positive) * prior_B)

# Posterior probability that the shipment is from Factory A given a positive algorithm output
posterior_A <- (prior_A * prob_A_positive) / prob_positive
posterior_A



#Section 2:Redo your calculation, using both information, the fact that there are defected products and the fact that the algorithm is positive for Factory A.


prior_factories <- c(0.5, 0.5)  # Prior probabilities for Factory A and Factory B
prior_defect <- c(0.1, 0.2)  # Defect probabilities for Factory A and Factory B
prob_positive_given_factories <- c(0.8, 0.35)  # Probability of positive test given Factory A and Factory B

# Calculate the posterior probabilities
posterior_factories <- (prior_defect * prob_positive_given_factories * prior_factories) / sum(prior_defect * prob_positive_given_factories * prior_factories)

# Probability of next shipment containing defective products given positive test for Factory A
prob_defect_given_positive <- posterior_factories[1]

prob_defect_given_positive








#Bayesian Workflow

#-----------------------6-------------------------
library(ggplot2)

# Parameters for the beta distribution as I know that land takes about one-third of the surface area of the Earth
alpha <- 2
beta <- 4

# Generate a sequence of proportions, exclude 0 and 1 as the proportion of land on Earth cannot be 0 or 1.
proportions <- seq(0.01, 0.98, by = 0.01)

# Calculate the prior probabilities using the beta distribution
prior <- dbeta(proportions, alpha, beta)

# Create a data frame for plotting
df <- data.frame(proportions, prior)

# Plot the prior distribution
ggplot(df, aes(x = proportions, y = prior)) +
  geom_line() +
  labs(x = "Proportion of Land", y = "Prior Probability") +
  ggtitle("Prior Distribution of Land Proportion")

#-----------------------7-------------------------
number <- 10000
set.seed(1000)
sample <- c(rbeta(number,2,4))
avg <- sum(sample)/number

#-----------------------8-------------------------
prop <- seq(0, 1, length.out = 12)
priors <- vector("numeric", length(prop))
for (i in seq_along(prop)){
  priors[i] <- round( sum(sample >= prop[i] & sample < prop[i+1]) / 1e4 , 2)
}
poss <- tibble(prop_L = seq(0, 1, .1),
               prior = priors[1:11])

N <- 100

#compute_post <- function(obs, poss){ 
#  likelihood <- dbinom(obs, N, prob = poss$prop_L)
#  posterior <- likelihood*poss$prior
#  posterior_norm <- posterior/sum(posterior)
#  tibble(poss,lh=round(likelihood, 3), post=round(posterior_norm,3))
#}
#post <- compute_post(26,poss)
#post


likelihood <- dbinom(26, N, prob = poss$prop_L)
poss$likelihood <-likelihood
posterior <- likelihood * poss$prior
posterior_norm <- posterior / sum(posterior)
posterior_norm
poss$post <-posterior_norm
poss



#-----------------------9-------------------------

samples <- sample(poss$prop_L, size = 1000, replace = TRUE, prob = poss$post)

#-----------------------10-------------------------

tosses <- 100

# Generate posterior predictive distribution
predicted_lands <- rbinom(n = 1000, size = tosses, prob = samples)
predicted_lands

predicted_data <- data.frame(Lands = predicted_lands)
predicted_data

# Plot histogram of predicted_lands
ggplot(predicted_data, aes(x = Lands)) +
  geom_histogram(bins = 20, fill = "lightgrey", color = "black") +
  labs(x = "Number of Lands", y = "Simulated Frequency") +
  ggtitle("Posterior Predictive Distribution of Lands")

