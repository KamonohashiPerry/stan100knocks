library(bayesm)
library(rstan)
library(shinystan)
library(bayesplot)
library(tidyverse)
library(gridExtra)

# Data Import -------------------------------------------------------------
data("customerSat")
dataset <- customerSat

# Kick Stan model ---------------------------------------------------------
ni <- nrow(dataset) # 分析対象者の数
nj <- ncol(dataset) # 項目数
nc <- length(table(as.factor(dataset$q1))) # 回答の種類
D <- 1 # 定数項

stan_data <- list(y = dataset,
                  nj = nj,
                  ni = ni,
                  nc= nc,
                  D = D)

par <- c("theta","ba","a","b")
war <- 2500
ite <- 5000
see <- 1234
dig <- 2
cha <- 4

fit <- stan(file = "model/graded_response_model.stan",
                data = stan_data,
                pars = par,
                verbose = F,
                seed = see,
                chains = cha,
                warmup = war,
                iter = ite)

# save(fit, file = "graded_response_model.RData")
load(file = "graded_response_model.RData")

# Diagnose ----------------------------------------------------------------

traceplot(fit)

print(fit, pars = par, digits_summary = dig)

summary_table <- data.frame(summary(fit)$summary)
ggplot(data = data.frame(Rhat = summary_table$Rhat), aes(Rhat)) + geom_histogram()


# Visualization -----------------------------------------------------------
alpha <- rstan::extract(fit)$a %>% apply(2,mean)

for (i in 1:nj){
  eval(parse(text=paste0("beta_",i," <- rstan::extract(fit)$b[1:10000,",i,",1:10] %>% apply(2,mean)")))
}

# histogram of theta
theta <- rstan::extract(fit)$theta %>% apply(2,mean)
ggplot(data = data.frame(theta_mean = theta), aes(theta_mean)) + geom_histogram()


for (i in 1:nj){
  eval(parse(text=paste0("ggdf_",i," <- data.frame(matrix(ncol = nc,nrow = length(theta))) ; colnames(ggdf_",i,") <- 1:10")))
}

## probability
for(i in 1:nj){
  for (j in 1:nc){
    eval(parse(text=paste0("ggdf_",j,"[,",i,"] <- 1/(1+exp(-alpha[",i,"]*(theta-beta_",j,"[",i,"])))")))
    eval(parse(text=paste0("ggdf_",j,"$theta <- theta")))
  }
}

## gather
for (i in 1:nj){
  eval(parse(text=paste0("ggdf_gt_",i," <- ggdf_",i," %>% tidyr::gather(key=var,value,-theta,factor_key=TRUE)")))
}

## ggplot
for (i in 1:nj){
  eval(parse(text=paste0("p",i," <- ggplot(data = ggdf_gt_",i,", aes(x = theta, y = value, colour = var)) + geom_line() + ggtitle(\"Q",i,"\")")))
}

# 2×3でグラフを描画
grid.arrange(p1, p2, p3, p4, p5, nrow = 3)
grid.arrange(p6, p7, p8, p9, p10,  nrow = 3)