#### [ Simulated Wearable Monitor Data ] ####
library(tidyverse)
library(MCMCpack)
library(MASS)
set.seed(123)

nobs_state <- 1
nhidden_state <- 3

# #### Simulate Transition Probabilities ####
# trans_prob <- rdirichlet(nhidden_state, c(1, 1, 1))

#### Or Specify Transition Probabilities ####
trans_prob <- rbind(c(.8, .1, .1),
                     c(.1, .8, .1),
                     c(.1, .1, .8))


# #### Simulate Emission Distributions (Normal) ####
# mean_min <- -100
# mean_max <- 100
# first_mean <- runif(nobs_state, min = mean_min, max = mean_max)
# emission_means <- matrix(0, nrow = nhidden_state, ncol = length(first_mean))
# emission_means[1,] <- first_mean
# for (i in 2:nhidden_state) {
#   emission_means[i,] <- first_mean + (i-1)*(mean_max - mean_min)/nhidden_state
# }
# 
# ## All SDs are the same
# separability <- 4 # higher is more separable, must be positive
# emission_sds <- list()
# for (i in 1:nhidden_state) {
#   emission_sds[[i]] <- (mean_max - mean_min)/(nhidden_state*separability)
# }


#### Or Specify Emission Distributions (Normal) ####
first_mean <- rep(5, nobs_state)
emission_means <- matrix(0, nrow = nhidden_state, ncol = length(first_mean))
emission_means[1,] <- first_mean
for (i in 2:nhidden_state) {
  emission_means[i,] <- first_mean + (i-1)*50
}

## All SDs are the same
separability <- .25 # higher is more separable, must be positive
emission_sds <- list()
for (i in 1:nhidden_state) {
  emission_sds[[i]] <- 5/(nhidden_state*separability)  
}



#### Simulate Sequence of Hidden States ####
nobs <- 1000
first_state <- sample(1:nhidden_state, size = 1)
hidden_data <- numeric(nobs)
hidden_data[1] <- first_state

for (i in 2:nobs) {
  hidden_data[i] <- sample(1:nhidden_state, prob = trans_prob[hidden_data[i-1],], size = 1)
}

#### Simulate Sequence of Observed Values ####
observed_data <- matrix(0, nrow = nobs, ncol = nobs_state) # each row is an observation
for (i in 1:nobs) {
  observed_data[i,] <- mvrnorm(1, mu = emission_means[hidden_data[i],], Sigma = emission_sds[[hidden_data[i]]])
}






write.csv(observed_data, "./Practice/Data/FakeMonitorData1.csv", row.names = F)
write.csv(hidden_data, "./Practice/Data/FakeMonitorHiddenStates1.csv", row.names = F)
write.csv(trans_prob, "./Practice/Data/FakeMonitorTransProbs1.csv", row.names = F)
for (i in 1:nhidden_state) {
  write.csv(emission_sds[[1]], paste0("./Practice/Data/FakeMonitorEmissionSDs1_", i, ".csv"), row.names = F)
}
write.csv(emission_means, "./Practice/Data/FakeMonitorEmissionMeans1.csv", row.names = F)
