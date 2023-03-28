data {
  int<lower=0> N;
  real Lo;
  array[N] real<lower=Lo> size_ingrowth;
  vector[N] delta_time;
}
transformed data {
   vector<lower=0>[N] size_ingrowth_lo;
   size_ingrowth_lo = to_vector(size_ingrowth) - Lo;
}
parameters {
  real size_int;
  real<lower=0> phi_time;
  real<lower=0> sigma_size;
}
transformed parameters {
   real size_int_lo = size_int - Lo;
}
model {
  size_int ~ normal(Lo, 20);
  phi_time ~ normal(5, 2.5);
  sigma_size ~ normal(50, 20);

  vector[N] mu = size_int_lo + phi_time * delta_time;

  // for some reason, truncated distribution does not accept vectorized version
  for(n in 1:N)
    size_ingrowth_lo[n] ~ lognormal(mu[n], sigma_size);
}
generated quantities {
  vector[N] log_lik;

  for (n in 1:N)
    log_lik[n] = lognormal_lpdf(size_ingrowth_lo[n] | size_int_lo + phi_time * delta_time[n], sigma_size);
}
