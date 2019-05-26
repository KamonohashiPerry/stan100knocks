## 収束させるためのメモ書き

+ 初期化が失敗する場合、関数をbernoulli(inv_logit(...))からbernoulli_logit(...)に変更することでうまく動いたことがあった。  
https://discourse.mc-stan.org/t/initialization-between-2-2-failed-after-100-attempts/3941

## モデリングの注意点
+ 安易にパラメータの取りうる範囲に関して制約を掛けるのは避けたほうが良い。
+ 最初にモデルを作り、登場するデータやパラメータを作っていく。
+ 事前分布  
無情報一様分布：何も指定しない  
正規分布：平均値・回帰係数  
半コーシー分布：標準偏差・分散  
ベータ分布/一様分布：確率や比率  

## 収束判断
+ Rhatが全て1.05以下
+ チェインごとのカーネル密度がきれいに重なっているか（重なっていなかったらダメ）  
stan_dens(fit1,pars="p",separate_chains = TRUE)
+ サンプリングの自己相関が残っていないか（残っていたらダメ）  
stan_ac(fit1,pars="p",separate_chains = TRUE)

## 要素の取り出し
+ extract関数  
p <- rstan::extract(fit1)$p  
hist(p)  
mean(p)

## 情報量基準の計算
+ WAICの計算  
対数尤度の計算をgenerated quantitiesブロックで行う。

## コーディングの注意点
+ 文末にセミコロンを「;」書く。
+ コメントアウトは「//」
+ RStudioを使えば、保存と同時に文法チェックもしてくれる。
+ 最後の行は何も書かずに改行をしておく。
+ stan_model()でモデルを先に作り、sampling()で作ったモデルを実行する方が融通が効く。
+ 宣言したが使っていないパラメータは無駄なのでなくす。
+ 関数は引数の型が決まっている。
+ パラメータはreal型、vector型、matrix型で宣言。データは分布の性質に合わせる。
+ for文は処理が1行ならば{}が不要。

## 用語
+ target += f  
対数確率を加算する
+ lp__
log posteriorの略。Stanでは推定したいパラメータの偏微分に従ってサンプリングするため、定数項は考慮されていない。
+ ~(チルダ)  
その分布に従うという意味。
+ modelブロック  
モデルを記述する。様々な確率分布を用いることができる。サンプルされないローカル変数を宣言してもよいが、制約をかけることはできない。
+ dataブロック  
Rから受け取る定数の宣言を行う。
+ parametersブロック  
サンプリングされる変数の宣言を行う。変数の型や上限下限を指定することができる。
+ transformed parametersブロック  
変数の宣言と代入を行う。
+ transformed dataブロック  
定数の宣言と代入を行う。決め打ちのハイパーパラメータなど、決定論的な変換ができる。
+ generated quantitiesブロック  
サンプリング後の値を使う。normal_rng()などによる乱数生成が許される。
+ functionsブロック  
関数を定義することができる。

## 参考情報
+ Stan — 高速MCMCでパラメータ推定( https://heavywatal.github.io/rstats/stan.html )
+ Stan超初心者入門( https://www.slideshare.net/simizu706/stan-62042940 )
