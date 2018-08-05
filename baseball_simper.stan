data {

  int<lower=1> M;               // num primary positions
  int<lower=1> N;               // num players
  int<lower=1> AB[N];           // num At Bats
  int<lower=0> H[N];            // num Hits
  int<lower=1> pp[N];           // primary positions

  //hyperparameters in prior
  real<lower=0> A;              // first shape parameter for Beta distribution of mu         
  real<lower=0> B;              // second shape parameter for Beta distribution of mu
  real<lower=0> S;              // shape parameter for gamma distribution of kappa
  real<lower=0> R;              // rate parameter for gamma distribution of kappa

}

parameters {
  //simplex[J] Q[N];
  //simplex[3] P[J, M];   

  real<lower=0, upper=1> theta[N];
  real<lower=0, upper=1> mu[M];
  real<lower=0> kappa[M];

  real<lower=0, upper=1> mu_mu;
  real<lower=0> kappa_mu;
  real<lower=0> s_kappa;  //shape
  real<lower=0> r_kappa;  //rate
}

transformed parameters{
  real<lower=0> alpha[M];
  real<lower=0> beta[M];

  
  for (m in 1:M) {
    alpha[m] = mu[m] * kappa[m];
  }
  

  for (m in 1:M) {
    beta[m] = (1-mu[m]) * kappa[m];
  }

}


model {

  for (n in 1:N) {
    H[n] ~ binomial(AB[n], theta[n]);
  }

  for (n in 1:N) {
    theta[n] ~ beta(alpha[pp[n]], beta[pp[n]]);
  }


  for (m in 1:M) {
    mu[m] ~ beta(A, B);
  }

  for (m in 1:M) {
    kappa[m] ~ gamma(S, R);
  }
}
