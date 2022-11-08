data {
  int<lower=0> N;
  vector[N] obs_size;
  vector[N] time;
  int<lower=1> Nt; // number of unique Tree_id
  vector[Nt] start_size;
  array[N] int<lower=0> tree_id;
}
transformed data {
  // to add minimum range to Lmax parameter
  real<lower=0> maxSize = max(obs_size);
}
parameters {
  real<lower=0> r;
  real<lower=0> sigma_obs;
  real<lower=maxSize> Lmax;
  vector[Nt] Lo;
}
model {
  // priors
  sigma_obs ~ normal(0, 1.5);
  r ~ lognormal(-2.5, 2);
  Lmax ~ normal(1000, 80);
  Lo ~ normal(150, 50);

  // mean
  vector[N] mu_obs = Lo[tree_id] .*
    exp(-r * time) +
    Lmax * (1 - exp(-r * time));

  start_size ~ normal(Lo, sigma_obs);
  obs_size ~ normal(mu_obs, sigma_obs);

}
