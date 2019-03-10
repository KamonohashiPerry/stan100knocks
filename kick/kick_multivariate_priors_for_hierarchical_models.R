library(bayesm)
library(tidyverse)
library(rstan)
library(shinystan)

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

data("margarine")

#1,2,3,4,5,7の商品に関してデータを抽出し、家計IDごとにカウントし、5件以上買ったかどうかのフラグを作成する。
hhid_selected <- margarine$choicePrice %>% 
                        filter(choice %in% c(1,2,3,4,5,7)) %>% 
                        group_by(hhid) %>% 
                        summarise(purc_cnt = n()) %>% 
                        mutate(outcomes=if_else(purc_cnt>=5, 1, 0))

# Kick Stan ---------------------------------------------------------------
# https://mc-stan.org/docs/2_18/stan-users-guide/multivariate-hierarchical-priors-section.html
# https://oku.edu.mie-u.ac.jp/~okumura/rstan/sec74.html

#家計ごとに関する属性データの抽出
demos.selected <- margarine$demos %>% filter(hhid %in% hhid_selected$hhid)
demos.selected <- demos.selected %>% mutate(family_group = if_else(Fam_Size < 2, 1,
                                                                   if_else(Fam_Size < 4, 2,#,3)))
                                                                           if_else(Fam_Size < 5,3,4))))

hhid_selected <- hhid_selected %>% left_join(demos.selected %>% select(hhid, family_group), by = "hhid")

group_size <- length(table(demos.selected$family_group))
group_summary_tbl <- demos.selected %>% group_by(family_group) %>% summarize_all(funs(mean, .args=list()))
group_summary_tbl <- group_summary_tbl %>% select(family_group,Income, Fs3_4,Fs5.,college,whtcollar,retired)

X <- as.matrix(demos.selected %>% select(Income,Fs3_4,Fs5.,college,whtcollar,retired))
U <- as.matrix(group_summary_tbl %>% select(-family_group))


stan_data <- list(N = nrow(hhid_selected), # // num individuals
                  K = ncol(X), # // num ind predictors
                  J = group_size, # // num groups
                  L = ncol(U), # // num group predictors
                  jj = hhid_selected$family_group, # // group for individual
                  x = X, # // individual predictors
                  u = U, # // group predictors
                  y = hhid_selected$outcomes # // outcomes
                  )

fit <- stan(file = "model/multivariate_priors_for_hierarchical_models.stan",
            data = stan_data,
            iter = 1000,
            chains = 4,
            seed = 1234,
            control = list(max_treedepth = 15,adapt_delta=0.99))


# Diagnose ----------------------------------------------------------------

summary(fit)

traceplot(fit)

launch_shinystan(fit)


# Visualization -----------------------------------------------------------
# グループごとの係数の推定値を可視化する（作成中）
source('kick/common.R')

ms <- rstan::extract(fit)
N_mcmc <- length(ms$lp__)


param_names <- c('mcmc', paste0('b', 1:6))
d_est <- data.frame(1:N_mcmc, ms$beta)
d_est <- d_est[,1:7]
colnames(d_est) <- param_names
d_qua <- data.frame.quantile.mcmc(x=param_names[-1], y_mcmc=d_est[,-1])
d_melt <- reshape2::melt(d_est, id=c('mcmc'), variable.name='X')
d_melt$X <- factor(d_melt$X, levels=rev(levels(d_melt$X)))

p <- ggplot()
p <- p + theme_bw(base_size=18)
p <- p + coord_flip()
p <- p + geom_violin(data=d_melt, aes(x=X, y=value), fill='white', color='grey80', size=2, alpha=0.3, scale='width')
p <- p + geom_pointrange(data=d_qua, aes(x=X, y=p50, ymin=p2.5, ymax=p97.5), size=1)
p <- p + labs(x='parameter', y='value')
p <- p + scale_y_continuous(breaks=seq(from=-2, to=6, by=2))
p

