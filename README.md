# Stan100knocks

・Stanの公式マニュアル  
・様々な文献/ブログの改良  
などを扱いながら100本近いstanのコードをここに蓄積していきます。

kickにはstanを実行するためのRファイルを、datasetには実行するためのデータセットを、modelにはstanファイルを置いています。

## 必須ドキュメント
+ Stan Functions Reference  
分布に応じた関数は何か探す際はこちら（検索も可能）  
https://mc-stan.org/docs/2_19/functions-reference/index.html

## 参考情報
https://mc-stan.org/docs/2_19/reference-manual/index.html  
https://mc-stan.org/docs/2_19/stan-users-guide/index.html  
https://jrnold.github.io/ssmodels-in-stan/  
https://github.com/MatsuuraKentaro/RStanBook  
https://github.com/stan-dev/example-models/tree/master/BPA  
https://github.com/sinhrks/stan-statespace  
http://statmodeling.hatenablog.com/entry/state-space-model-many-terms  
https://logics-of-blue.com/multivariate-time-series-with-stan/  
https://jrnold.github.io/bugs-examples-in-stan/index.html  
https://mc-stan.org/users/documentation/case-studies.html  
https://mc-stan.org/users/documentation/tutorials.html  


### 価格弾力性
https://github.com/stan-dev/stancon_talks/blob/master/2018-helsinki/Contributed-Talks/braylan/Self-tuning_Holidays-presented.ipynb  
### 収益最適化
https://www.smartly.io/blog/tutorial-how-we-productized-bayesian-revenue-estimation-with-stan

### ABテストについて
https://jrnold.github.io/presentation-2018-11-05/abtest.html
### 隠れマルコフについて
https://zenodo.org/record/1284341#.XLGq-uv7RTY

### 生存時間分析について
https://rstudio-pubs-static.s3.amazonaws.com/435225_07b4ab5afa824342a4680c9fb2de6098.html  
https://qiita.com/fred55/items/52a3ce36906805645e05  
http://ajhjhaf.hatenablog.com/entry/2017/08/05/194939  
http://statmodeling.hatenablog.com/entry/survival-analysis-hazard-car-model  
https://www.bananarian.net/entry/2019/02/17/090000

### WAICについて
https://www.slideshare.net/simizu706/waic


### 分布について
+ ベルヌーイ分布（y ~ bernoulli(theta)）
+ 正規分布（y ~ normal(mu, sigma)）
+ 歪正規分布（y ~ skew_normal(xi, omega, alpha)）
+ t分布（y ~ student_t(nu, mu, sigma)）
+ コーシー分布（y ~ cauchy(mu, sigma)）
+ ラプラス分布/二重指数分布（y ~ double_exponential(mu, sigma)）
+ ロジスティック分布（y ~ logistic(mu, sigma)）
+ ガンベル分布（y ~ gumbel(mu, beta)）
+ 対数正規分布（y ~ lognormal(mu, sigma)）
+ カイ二乗分布（y ~ chi_square(nu)）
+ 逆カイ二乗分布（y ~ inv_chi_square(nu)）
+ スケール化逆カイ二乗分布（y ~ scaled_inv_chi_square(nu, sigma)）
+ 指数分布（y ~ exponential(beta)）
+ ガンマ分布（y ~ gamma(alpha, beta)）
+ 逆ガンマ分布（y ~ inv_gamma(alpha, beta)）
+ ワイブル分布（y ~ weibull(alpha, sigma)）
+ フレシェ分布（y ~ frechet(alpha, sigma)）
+ レイリー分布（y ~ rayleigh(sigma)）
+ ウィーナー初到達時間分布（y ~ wiener(alpha, tau, beta, delta)）
+ パレート分布（y ~ pareto(y_min, alpha)）
+ パレート2型分布（y ~ pareto_type_2(mu, lambda, alpha)）
+ ベータ分布（theta ~ beta(alpha, beta)）
+ ベータ比例分布（theta ~ beta_proportion(mu, kappa)）
+ フォン・ミーゼス分布（y ~ von_mises(mu, kappa)）
+ 一様分布（y ~ uniform(alpha, beta)）
+ 多変量正規分布（y ~ multi_normal(mu, Sigma)）
+ 多変量t分布（y ~ multi_student_t(nu, mu, Sigma)）
+ ディリクレ分布（theta ~ dirichlet(alpha)）
+ LKJ相関分布（y ~ lkj_corr(eta)）
+ ウィシャート分布（W ~ wishart(nu, Sigma)）
+ 逆ウィシャート（W ~ inv_wishart(nu, Sigma)）
