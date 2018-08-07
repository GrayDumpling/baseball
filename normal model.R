library(rstan)

stancode <- 'data {int N; real y[N];} parameters {real mu;} model {y ~ normal(mu,1); mu ~ normal(0,1);}'
mod <- stan_model(model_code = stancode)
N = 100
mu = rnorm(1)



fit2 <- stan(model_code = stancode, data = list(N = N, y=rnorm(N, mu)))

mle = 