#-----------------------------------------------Assignment 2-------------------------------------------------------
pacman::p_load(tidyverse)

#Bayesian Updating
#-----------------------1-------------------------
data <- c(rep("P",6),rep("N",4))

# what we are looking for: P(A | 6 positive, 4 negative)
# which means we need to calculate: (P(6 positive, 4 negative | A) * P(A)) / P(6 positive, 4 negative)
# P(6 positive, 4 negative | A) = P(6 positive | A) * P(4 negative | A)

def()

likelihood_A <- dbinom(6, 10, 0.7) * dbinom(4, 10, 0.3)
likelihood_A
likelihood_B <- dbinom(6, 10, 0.5) * dbinom(4, 10, 0.5)
likelihood_C <- dbinom(6, 10, 0.8) * dbinom(4, 10, 0.2)
likelihood_C

# you think it is twice as likely that the reviews belong to Company B compared to Company A and also compared to Company C
# -> meaning P(B)=2(PA)=2P(C) -> P(A)=1/1+2+2=0.2
prior_A <- 0.2
prior_B <- 0.4
prior_C <- 0.4


posterior_A <- (likelihood_A * prior_A) / (likelihood_A * prior_A + likelihood_B * prior_B + likelihood_C * prior_C)
posterior_A

#-----------------------2-------------------------
posterior_C_old <- (likelihood_C * prior_C) / (likelihood_A * prior_A + likelihood_B * prior_B + likelihood_C * prior_C)
posterior_C_old



#Creating two methods for the calculation

compute_likelihood <- function(company,P,T){ #company index, number of positive reviews, total number of revies
  poss <- NULL
  if (company == 'A') {
    poss <- 0.7
    #prior <- prior_A
  } else if (company == 'B') {
    poss <- 0.5
    #prior <- prior_B
  } else if (company == 'C') {
    poss <- 0.8
    #prior <- prior_C
  } else {
    print("No such company exists!")
  }
  
  likelihood <- dbinom(P, T, prob = poss) * dbinom(T - P, T, prob = 1 - poss)
  
  return(likelihood)
}



compute_post <- function(company,P,T){
  
  prior <- NULL
  if (company == 'A') {
    #poss <- 0.7
    prior <- prior_A
  } else if (company == 'B') {
    #poss <- 0.5
    prior <- prior_B
  } else if (company == 'C') {
    #poss <- 0.8
    prior <- prior_C
  } else {
    print("No such company exists!")
  }
  sum <- compute_likelihood('A',P,T)*prior_A+compute_likelihood('B',P,T)*prior_B+compute_likelihood('C',P,T)*prior_C
  post <- prior*compute_likelihood(company,P,T)
  post_norm <- post/sum
  #print(post)
  #print(sum)
  #print(post_norm)
  return(post_norm)
}



c_old<-compute_post('C',6,10)
c_old





posterior_C_old # 0.1110857
c_new <- compute_post('C',9,10)
c_new #  0.7962477
c_all <- compute_post('C',15,20)
c_all # 0.4307816
c_all-posterior_C_old # 0.3196959

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


#-----------------------5-------------------------No Question 4 found in the slide

# Probability of defective products from Factory A and Factory B
prob_defective_A <- 0.1
prob_defective_B <- 0.2

# Probability of correct identification
prob_correct_A <- 0.8
prob_correct_B <- 0.65

# Prior probabilities of the shipments being from Factory A and Factory B
prior_A <- 0.5
prior_B <- 0.5

# Likelihood of positive test given Factory A
likelihood_positive_A <- prob_correct_A

# Likelihood of positive test of A given Factory P(Positive | B) = 1 - P(Correct_B)
likelihood_positive_B <- 1 - prob_correct_B

# Calculate the Probability of having identifying A as positive: P(Positive) = P(Positive | A) * P(A) + P(Positive | B) * P(B)
ident_positive <- likelihood_positive_A * prior_A + likelihood_positive_B * prior_B

# Calculate the posterior probability P(A | Positive) = (P(Positive | A) * P(A)) / P(Positive)
posterior_A <- likelihood_positive_A * prior_A / ident_positive

# Calculate the posterior probability P(B | Positive)
posterior_B <- likelihood_positive_B * prior_B / ident_positive

# Likelihood of defective products given Factory A: P(Defective | A)
likelihood_defective_A <- prob_defective_A

# Likelihood of defective products given Factory B: P(Defective | B)
likelihood_defective_B <- prob_defective_B

# Calculate the probability of the next shipment containing defective products given Positive & Defective_Now: 
#P(Next_Defective | Positive & Defective_Now) = (P(Defective_A) * P(Defective_A | Positive)) + (P(Defective_B) * P(Defective_B | Positive)) / (P(Defective_A | Positive) + P(Defective_B | Positive))
prob_next_defective <- (likelihood_defective_A * posterior_A + likelihood_defective_B * posterior_B) / (posterior_A + posterior_B)

prob_next_defective


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
posterior <- likelihood * poss$prior
posterior_norm <- posterior / sum(posterior)

post <- tibble(poss, likelihood = round(likelihood, 3), posterior = round(posterior_norm, 3))
post


#-----------------------9-------------------------

samples <- sample(poss$prop_L, size = 1000, replace = TRUE, prob = post$posterior)

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

