data {
  int<lower=0> N;
  real Lo;
  array[N] real<lower=Lo> size_ingrowth;
  vector[N] delta_time;
}
parameters {
  real size_int;
  real<lower=0> phi_time;
  real<lower=0> sigma_size;
}
model {
  size_int ~ normal(Lo, 20);
  phi_time ~ normal(5, 2.5);
  sigma_size ~ normal(50, 20);

  vector[N] mu = size_int + phi_time * delta_time;

  size_ingrowth ~ normal(mu, sigma_size);
}
generated quantities {
  vector[N] log_lik;
  vector[N] y_rep;

  for (n in 1:N) {
    log_lik[n] = normal_lpdf(size_ingrowth[n] | size_int + phi_time * delta_time[n], sigma_size);
    y_rep[n] = normal_rng(size_int + phi_time * delta_time[n], sigma_size);
  }
}
