library(rstan)
library(tidyverse)
library(lubridate)
library(gtrendsR)

d <- gtrends(keyword = "Apache Arrow", geo = "", time = "today+5-y",
        gprop = c("web", "news", "images", "froogle", "youtube"),
        category = 0, hl = "en-US", low_search_volume = FALSE,
        cookie_url = "http://trends.google.com/Cookies/NID")$interest_over_time

d <- d %>% select(date,hits)
colnames(d) <- c("month", "Y")

T <- nrow(d)
T_next <- 25
data <- list(T=T, T_next=T_next, Y=d$Y)

fit <- stan (file = 'model/state_space_model_2trend_2seasonality.stan', 
             data = data, 
             pars = c('mu_all','s_all','y_next','s_mu','s_s','s_r'),
             iter=10200,
             warmup=200,
             thin=10,
             chains=3,
             seed = 123,
             control = list(adapt_delta=0.99)
)

traceplot(fit)
summary(fit)

launch_shinystan(fit)


makeDataFrameQuantile <- function(x, y_smp){
  qua <- apply(y_smp, 2, quantile, prob=c(0.1, 0.25, 0.5, 0.75, 0.9))
  d_est <- data.frame(X=x, t(qua))
  colnames(d_est) <- c('X', 'p10', 'p25', 'p50', 'p75', 'p90')
  return(d_est)
}


result <- rstan::extract(fit)
d_est <- makeDataFrameQuantile(x=(T+1):(T+T_next), y_smp=result$y_next)
d_est <- rbind(data.frame(X=T, p10=d$Y[T], p25=d$Y[T], p50=d$Y[T], p75=d$Y[T], p90=d$Y[T]), d_est)

d_pred <- d$Y
d_pred <- append(d_pred, d_est$p50[2:length(d_est$p50)])

startDate <- ymd(d$month[1])
myDates <- startDate %m+% months(c(0:(length(d_pred)-1)))

d_pred <- data.frame(d_pred) %>% mutate(month = myDates)
d_pred <- d_pred %>% mutate(estimate_value = apply(as.matrix(result$mu_all), 2, mean))

g <- ggplot(d_pred,aes (x = month,y = d_pred))
g <- g +  geom_point(shape = 20,
                     size = 0.8,
                     na.rm = TRUE)

g <- g + geom_smooth(method = "auto") + xlab("month") + ylab("prediction")
g <- g + ggtitle("Google Trend")

plot(g)

