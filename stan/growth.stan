data {
  int<lower=0> N;
  vector[N] obs_size;
  vector[N] time;
}
transformed data {
  // to add minimum range to Lmax parameter
  real<lower=0> maxSize = max(obs_size);
}
parameters {
  real<lower=0> r;
  real<lower=0> sigma_obs;
  real<lower=0> sigma_Lo;
  real<lower=maxSize> Lmax;
  real<lower=0> mu_Lo;
}
model {
  // priors
  sigma_obs ~ normal(0, 1.5);
  sigma_Lo ~ normal(25, 10);
  r ~ lognormal(-2.5, 2);
  Lmax ~ normal(1000, 80);
  mu_Lo ~ normal(200, 100);

  // temp variables
  vector[N] mu_obs;
  vector[N] var_growth;

  // mean and deviance
  mu_obs = mu_Lo *
    exp(-r * time) +
    Lmax * (1 - exp(-r * time));

  var_growth = exp(-2 * r * time) * sigma_Lo^2;

  obs_size ~ normal(mu_obs, sqrt(var_growth + sigma_obs^2));

}
// generated quantities {
//   vector[N] mu_obs;
//   vector[N] var_growth;
//   vector[N] log_lik;

//   for(i in 1:N) {
//     mu_obs[i] = mu_Lo *
//       exp(-r * time[i]) +
//       Lmax * (1 - exp(-r * time[i]));
    
//     var_growth[i] = exp(-2 * r * time[i]) * sigma_Lo^2;

//     log_lik[i] = normal_lpdf(obs_size[i] | mu_obs[i], sqrt(var_growth[i] + sigma_obs^2));
//   }
  
// }
