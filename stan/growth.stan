data {
  int<lower=0> N;
  vector[N] obs_size;
  vector[N] time;
  vector[N] start_size;
  int<lower=1> Np; // number of unique plot_id
  array[N] int<lower=0> plot_id;
}
transformed data {
  // to add minimum range to Lmax parameter
  real<lower=0> maxSize = max(obs_size);
}
parameters {
  real r;
  vector[Np] rPlot_log;
  real<lower=0> sigma_plot;
  real<lower=0> sigma_obs;
  real<lower=maxSize> Lmax;
}
model {
  // priors
  r ~ normal(-3.5, 1);
  rPlot_log ~ normal(0, sigma_plot);
  sigma_plot ~ exponential(3);
  sigma_obs ~ normal(0, 1.5);
  Lmax ~ normal(1000, 80);

  // add plot random effect
  vector[N] rPlot = exp(r + rPlot_log[plot_id]);

  // pre calculate component of the model mean
  vector[N] rPlotTime = exp(-rPlot .* time);

  // mean
  vector[N] mu_obs = start_size .*
    rPlotTime +
    Lmax * (1 - rPlotTime);

  // likelihood
  obs_size ~ normal(mu_obs, sigma_obs);
}
