# Install Package Trying
install.packages(c("coda","mvtnorm","devtools","loo"))
library(devtools)
devtools::install_github("rmcelreath/rethinking")


head(dat,5)
install.packages("cmdstanr")

# we recommend running this is a fresh R session or restarting your current session
install.packages("cmdstanr", repos = c("https://mc-stan.org/r-packages/", getOption("repos")))
# install.packages("remotes")
remotes::install_github("stan-dev/cmdstanr")
cmdstanr::install_cmdstan()
install.packages(c("coda","mvtnorm","devtools","loo","dagitty","shape"))
devtools::install_github("rmcelreath/rethinking")



pacman::p_load(tidyverse, 
               rstan, 
               rethinking)
# Coding Starting

library(rethinking)
data("Howell1")
dat<-Howell1
View(dat)

model <- lm(height ~ weight + age, data = dat)
head(model)




#from teacher


age_bar = round(mean(dat$age), 2)
m3 <- quap(
  alist(weight~dnorm(mu,sd),
        mu <- a+b*(age-age_bar),
        a~dnorm(40,7),
        b~dnorm(0,3),
        sd~dunif(0,20)),
  data=dat
)
precis(m3)
dat_a<-dat
dat_a <- dat %>% filter(age>=18)
plot(dat_a$weight,dat_a$height)
plot(dat_a$age,dat_a$height)
