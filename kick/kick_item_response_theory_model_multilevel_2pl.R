library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

library(reshape2)  # For switching between long-form and wide-form data
library(ggplot2)  # For the posterior predictive model checking example
library(gridExtra)  # Also for the posterior predictive model checking example

# Load edstan
# devtools::install_github("danielcfurr/edstan")
library(edstan)


# Dataset Import ----------------------------------------------------------
# The data set is available from the edstan package.
preview_rows <- seq(from = 1, to = nrow(spelling), length.out = 10)
spelling[preview_rows, ]


# Visualization -----------------------------------------------------------
# Record existing plot presets and prepare to make side-by-side pots
par_bkp <- par(no.readonly = TRUE)
par(mfrow = c(1, 2))

# Left plot
person_scores <- apply(spelling[, 2:5], 1, sum)
person_counts <- table(person_scores)
barplot(person_counts, main = "Raw score distribution", xlab = "Raw score", 
        ylab = "Number of persons")

# Right plot
item_scores <- apply(spelling[, 2:5], 2, mean)
barplot(item_scores, main = "Proportion correct by item", ylab = "Proportion correct", 
        ylim = c(0, 1), xaxt = "n")
# x-axis with angled labels
text(x = 0.85 + (1:length(item_scores) - 1) * 1.2, y = -0.05, labels = names(item_scores), 
     xpd = TRUE, srt = 30, pos = 2)

# Return to previous plot presets
par(par_bkp)


# Kick Stan ---------------------------------------------------------------
# https://mc-stan.org/docs/2_18/stan-users-guide/item-response-models-section.html
# https://mc-stan.org/users/documentation/case-studies/tutorial_twopl.html#spellingdata

# Set up data list from wide-form data using irt_data()
X <- spelling[, 2:5]  # Response matrix
W <- cbind(1, spelling[, 1])  # Person covariate matrix
spelling_list <- irt_data(response_matrix = X)


# Convert the data from wide to long-form
wide <- as.data.frame(X)
wide$id <- 1:nrow(wide)  # Attach a person ID number to each row.
long <- melt(wide, id.vars = "id", variable.name = "item", value.name = "response")
head(long)

key <- 1:length(unique(long$item))
names(key) <- unique(long$item)
long$item.id <- key[long$item]
head(long)

K <- max(long$item.id)
J <- max(long$id)

stan_data <- list(K = K,
                  J = J,
                  N = nrow(long),
                  kk = long$item.id, 
                  jj = long$id,
                  y = long$response)

fit <- stan(file = "model/item_response_theory_model_multilevel_2pl.stan",
            data = stan_data,
            iter = 1000,
            chains = 4,
            seed = 1234)

# Diagnose ----------------------------------------------------------------

summary(fit)

traceplot(fit)

launch_shinystan(fit)


# Visualization -----------------------------------------------------------
# http://norimune.net/2949
alpha <- rstan::extract(fit)$alpha %>% apply(2,mean)
beta <- rstan::extract(fit)$beta %>% apply(2,mean)

#dataframe for ggplot2
theta <- seq(-5,5,length.out=201)
ggdf <- data.frame(matrix(ncol= K,nrow=201))

for(i in 1:K){
  ggdf[,i] <- psych::logistic(-alpha[i]*(theta-beta[i]))
}
ggdf$theta <- theta

#ggplot2
ggdf <- ggdf %>% 
  tidyr::gather(key=var,value,-theta,factor_key=TRUE) %>%
  ggplot(aes(x=theta))

ggdf + geom_line(aes(y=value,color=var))

