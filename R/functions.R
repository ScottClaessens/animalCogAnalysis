# custom functions
simData <- function(seed = 2113, species_p = 0.6, species_sd = 0.5, N_ind = 10,
                    N_trials = 20, phi = 0.05, lambda = 0.05, payoff = 100) {
  
  # PARAMETERS
  
  # seed       - random seed for simulation
  # species_p  - true species-level probability of success
  # species_sd - standard deviation for individual animals of that species, on the logit scale
  # N_ind      - number of individuals
  # N_trials   - number of trials per individual
  # phi        - reinforcement learning parameter - updating (0 = no learning)
  # lambda     - reinforcement learning parameter - choice-sensitivity
  # payoff     - reinforcement learning parameter - payoff from correct response
  
  # SIMULATION
  
  # set initial seed
  set.seed(seed)
  # create empty data frame
  d <- data.frame()
  # loop over individual animals
  for (n in 1:N_ind) {
    # draw probability for individual animal from distribution on logit scale
    prob <- c(inv_logit_scaled(rnorm(1, logit_scaled(species_p), species_sd)))
    # prior weights (for reinforcement learning)
    w0 <- c(0)
    w1 <- c(log(prob[1] / (1 - prob[1])) / lambda)
    # first trial
    success <- c(rbinom(1, 1, prob))
    # subsequent trials w/ learning
    for (trial in 2:N_trials) {
      # update weights and probability
      if (success[trial-1] == 0) {
        new_w0 <- (1 - phi)*w0[trial-1]
        new_w1 <- w1[trial-1]
      } else {
        new_w0 <- w0[trial-1]
        new_w1 <- (1 - phi)*w1[trial-1] + phi*payoff
      }
      w0 <- c(w0, new_w0)
      w1 <- c(w1, new_w1)
      new_prob <- exp(lambda*w1[trial]) / (exp(lambda*w0[trial]) + exp(lambda*w1[trial]))
      prob <- c(prob, new_prob)
      # new data
      success <- c(success, rbinom(1, 1, prob[trial]))
    }
    # bind individual animal data to d
    d <- rbind(d, data.frame(
      ID = n,
      trial = 1:N_trials,
      # w1 = w1,
      # prob = prob,
      success = success
    ))
  }
  # d is our simulated data frame
  d$ID <- as.factor(d$ID)
  return(d)
}

fitBinomialTests <- function(d) {
  # how many individuals?
  N <- length(unique(d$ID))
  # create empty list
  out <- list()
  # loop over individuals
  for (n in 1:N) {
    # get successes for individual
    successes <- d$success[d$ID == n]
    # fit binomial test
    out[[n]] <- binom.test(sum(successes), length(successes))
  }
  return(out)
}

fitBrms <- function(d) {
  # fit multilevel logistic regression with random intercept and random slope of trial, grouped by ID
  out <- brm(success ~ 1 + trial + (1 + trial | ID), data = d, family = bernoulli,
             prior = prior(normal(0, 1), class = b), control = list(adapt_delta = 0.99))
  # add leave-one-out cross-validation
  out <- add_criterion(out, "loo")
  return(out)
}