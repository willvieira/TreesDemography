data {
  int<lower=0> N;
  vector[N] obs_size;
  vector[N] time;
  vector[N] start_size;
}
transformed data {
  // to add minimum range to Lmax parameter
  real<lower=0> maxSize = max(obs_size);
}
parameters {
  real<lower=0> r;
  real<lower=0> sigma_obs;
  real<lower=maxSize> Lmax;
}
model {
  // priors
  sigma_obs ~ normal(0, 1.5);
  r ~ lognormal(-2.5, 2);
  Lmax ~ normal(1000, 80);

  // mean
  vector[N] mu_obs = start_size .*
    exp(-r * time) +
    Lmax * (1 - exp(-r * time));

  obs_size ~ normal(mu_obs, sigma_obs);

}
