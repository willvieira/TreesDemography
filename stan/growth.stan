data {
  int<lower=0> N;
  vector[N] obs_size_t1;
  vector[N] time;
  vector[N] obs_size_t0;
  int<lower=1> Np; // number of unique plot_id
  array[N] int<lower=1,upper=Np> plot_id;
  int<lower=1> Nt; // number of unique plot_id
  array[N] int<lower=1,upper=Nt> tree_id;
}
transformed data {
  // to add minimum range to Lmax parameter
  real<lower=0> maxSize = max(obs_size_t1);
}
parameters {
  real r;
  vector[Np] rPlot_log;
  real<lower=0> sigma_plot;
  vector[Nt] rTree_log;
  real<lower=0> sigma_tree;
  real<lower=0> sigma_obs;
  real<lower=maxSize> Lmax;
}
model {
  // priors
  r ~ normal(-3.5, 1);
  rPlot_log ~ normal(0, sigma_plot);
  sigma_plot ~ exponential(3);
  rTree_log ~ normal(0, sigma_tree);
  sigma_tree ~ exponential(3);
  sigma_obs ~ normal(0, 1.5);
  Lmax ~ normal(1000, 80);

  // add plot random effect and BA effect
  vector[N] rPlot = exp(
    r + rPlot_log[plot_id] + rTree_log[tree_id]
  );

  // pre calculate component of the model mean
  vector[N] rPlotTime = exp(-rPlot .* time);

  // mean
  vector[N] mu_obs = obs_size_t0 .*
    rPlotTime +
    Lmax * (1 - rPlotTime);

  // likelihood
  obs_size_t1 ~ normal(mu_obs, sigma_obs);
}
