data {
  int<lower=0> N;
  real Lo;
  array[N] real<lower=Lo> size_ingrowth;
  vector[N] delta_time;
}
parameters {
  real<lower=0> size_int;
  real<lower=0> phi_time;
  real<lower=0> sigma_size;
}
model {
  size_int ~ normal(Lo, 50);
  phi_time ~ normal(0, 1);
  sigma_size ~ normal(20, 5);

  vector[N] mu = size_int + phi_time * delta_time;

  // for some reason, truncated distribution does not accept vectorized version
  for(n in 1:N)
    size_ingrowth[n] ~ normal(mu[n], sigma_size) T[Lo, ];
}
