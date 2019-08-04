library(tidyverse)
library(rstan)

options(mc.cores = parallel::detectCores()) # parallelize
rstan_options(auto_write = TRUE)  # store compiled stan model


# The rats data set -------------------------------------------------------
library(RCurl)
# load data as character
f <- getURL('https://www.datascienceblog.net/data-sets/rats.txt')
# read table from text connection
df <- read.delim(textConnection(f), header=T, sep = " ")
print(head(df)) # each row corresponds to an individual rat

library(reshape2)
ddf <- cbind(melt(df), Group = rep(paste0("Rat", seq(1, nrow(df))), 5))
library(ggplot2)
ggplot(ddf, aes(x = variable, y = value, group = Group)) + geom_line() + geom_point()

# Data preparation --------------------------------------------------------
days <- as.numeric(regmatches(colnames(df), regexpr("[0-9]*$", colnames(df))))
rat.data <- list(N = nrow(df), T = ncol(df), x = days,
                 y = df, xbar = median(days)) 


# Hierarchical regression -------------------------------------------------
# Specification of the hierarchical regression model
# https://www.datascienceblog.net/post/machine-learning/probabilistic_programming/

rat.model <- stan(
  file = "model/rats.stan",
  data = rat.data)
# model contains estimates for intercepts (alpha) and slopes (beta)


# Model interpretation ----------------------------------------------------
print(rat.model) # optional parameters: pars, probs


# specify the params to plot via pars
plot(rat.model, pars = "alpha")
plot(rat.model, pars = "beta")


# MCMC diagnostics --------------------------------------------------------
# diagnostics:
traceplot(rat.model, pars = c("mu_alpha", "mu_beta"), inc_warmup = TRUE, nrow = 2)


# retrieve matrix of iterations, chains, and parameters
chain.data <- extract(rat.model, permuted = FALSE) 
print(chain.data[1,,]) # parameters of all chains for the 1st of 1000 iterations


library(shinystan)
launch_shinystan(rat.model)


# Prediction with the hiearchical regression model ------------------------
predict.rat.weight <- function(rat.model, newdays) {
  # newdays: vector of time points to consider
  rat.fit <- extract(rat.model)
  alpha <- rat.fit$alpha
  beta <- rat.fit$beta
  xbar <- 22 # hardcoded since not stored in rat.model
  y <- lapply(newdays, function(t) alpha + beta * (t - 22))
  return(y)
}
newdays <- seq(0, 100)
pred.weights <- predict.rat.weight(rat.model, newdays)
# extract means and standard deviations from posterior samples
pred.means <- lapply(pred.weights, function(x) apply(x, 2, mean))
pred.sd <- lapply(pred.weights, function(x) apply(x, 2, sd)) 
# create plotting data frame with 95% CI interval from sd
pred.df <- data.frame(Weight = unlist(pred.means), 
                      Upr_Weight = unlist(pred.means) + 1.96 * unlist(pred.sd), 
                      Lwr_Weight = unlist(pred.means) - 1.96 * unlist(pred.sd), 
                      Day = unlist(lapply(newdays, function(x) rep(x, 30))),
                      Rat = rep(seq(1,30), length(newdays)))
# predicted mean weight of all rats
ggplot(pred.df, aes(x = Day, y = Weight, group = Rat)) +
  geom_line()



# predictions for selected rats
sel.rats <- c(9, 8, 29)
ggplot(pred.df[pred.df$Rat %in% sel.rats, ], 
       aes(x = Day, y = Weight, group = Rat, 
           ymin = Lwr_Weight, ymax = Upr_Weight)) +   
  geom_line()  +
  geom_errorbar(width=0.2, size=0.5, color="blue")
