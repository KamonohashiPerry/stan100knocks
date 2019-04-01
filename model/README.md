## 収束させるためのメモ書き

+ 初期化が失敗する場合、関数をbernoulli(inv_logit(...))からbernoulli_logit(...)に変更することでうまく動いたことがあった。  
https://discourse.mc-stan.org/t/initialization-between-2-2-failed-after-100-attempts/3941

## モデリングの注意点

+ 安易にパラメータの取りうる範囲に関して制約を掛けるのは避けたほうが良い。
