pacman::p_load(tidyverse, rethinking)


  
?hist
?rbern

hist(replicate(1000,sum(runif(16,-1,1))))
hist(replicate(1000,sum(rbinom(1000,.5))))
hist(replicate(1000,sum(rnorm(1000,0,1))))
hist(replicate(1000,sum(rbeta(100,2,2))))
hist(rbinom(1000,1000,.5))

  
 
