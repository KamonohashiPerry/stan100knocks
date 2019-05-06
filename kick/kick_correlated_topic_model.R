library(tidyverse)
library(tidytext)
library(rstan)

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

# Loading data ------------------------------------------------------------
# https://www.rondhuit.com/download.html#ldcc

corpus = read_csv("dataset/livedoor_corpus.txt", col_names = "words")
corpus = corpus %>% sample_n(1000)
corpus = corpus %>% mutate(doc = seq(1:nrow(corpus)))
corpus.tidy = corpus %>% unnest_tokens(word, words)
corpus.tidy = corpus.tidy %>% mutate(word = as.integer(word))

stop_words = corpus.tidy %>% count(word, sort=T) %>% filter(n > 1000 | n < 15)
corpus.tidy = corpus.tidy %>% anti_join(stop_words)

appeared_word = unique(corpus.tidy$word)

vocab = read.table("dataset/livedoordic.txt",sep = "\t")
vocab = vocab %>% mutate(index = seq(nrow(vocab)))
vocab.new = vocab[appeared_word,]

vocab.new = vocab.new %>% mutate(new_ind = seq(nrow(vocab.new)))

corpus.lda = corpus.tidy %>% left_join(vocab.new, by = c("word" = "index"))

M = nrow(corpus)
N = nrow(corpus.lda)
V = nrow(vocab.new)
K = 20

offset = matrix(nrow = M, ncol = 2)

last_ind = 0
for (m in 1:M) {
  n_words = as.integer(corpus.lda %>% filter(doc == m) %>% count())
  offset[m,1] = last_ind + 1
  offset[m,2] = offset[m,1] + (n_words - 1)
  last_ind = offset[m,2]
}


# Kick Stan model ---------------------------------------------------------
# https://qiita.com/Hiroyuki1993/items/8a9aebb63f10bae743d2
# https://mc-stan.org/docs/2_18/stan-users-guide/latent-dirichlet-allocation.html
# http://statmodeling.hatenablog.com/entry/topic-model-5

stan_data <- list(
  K = K,
  M = M,
  V = V,
  N = N,
  W = corpus.lda$new_ind,
  Offset = offset,
  mu = rep(0.5, K),
  Sigma = diag(K) # 単位行列
)


sm <- stan_model(file = "model/correlated_topic_model.stan")

fit.vb <- vb(
  sm,
  data = stan_data,
  output_samples = 2000,
  adapt_engaged = FALSE,
  eta = .1)


ms <- rstan::extract(fit.vb)

probs <- c(0.1, 0.25, 0.5, 0.75, 0.9)
idx <- expand.grid(1:K, 1:V)

d_qua <- t(apply(idx, 1, function(x) quantile(ms$phi[,x[1],x[2]], probs=probs)))
d_qua <- data.frame(idx, d_qua)
colnames(d_qua) <- c('tag', 'item', paste0('p', probs*100))

# window関数を用いてトピックごとの上位10キーワードを取得
d_qua_window <- d_qua %>% dplyr::group_by(tag) %>%  dplyr::filter(min_rank(-p50) <= 10)
d_qua_window <- d_qua_window %>% left_join(vocab.new %>% select(V2,new_ind),by = c("item" = "new_ind"))

