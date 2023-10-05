data {
  int<lower=0> N;
  vector[N] obs_size_t1;
  vector[N] time;
  vector[N] obs_size_t0;
  int<lower=1> Np; // number of unique plot_id
  array[N] int<lower=1,upper=Np> plot_id;
}
transformed data {
  // to add minimum range to Lmax parameter
  real<lower=0> maxSize = max(obs_size_t1) * 1.2;
}
parameters {
  real r;
  real<lower=maxSize> Lmax;
  real<lower=0> sigma_obs;
  vector[Np] rPlot_log;
  real<lower=0> sigma_plot;
}
model {
  // priors
  r ~ normal(-3.5, 1);
  sigma_obs ~ normal(0, 1.5);
  Lmax ~ normal(1000, 80);
  rPlot_log ~ normal(0, sigma_plot);
  sigma_plot ~ exponential(2);
  sigma_obs ~ normal(0, 1.5);

  // What matters here:
  vector[N] rPlot = exp( // growth parameter
    r + // intercept
    rPlot_log[plot_id]  // plot random effect
  );

  // pre calculate component of the model
  vector[N] rPlotTime = exp(-rPlot .* time);

  // mean
  vector[N] mu_obs = obs_size_t0 .*
    rPlotTime +
    Lmax * (1 - rPlotTime);

  // likelihood
  obs_size_t1 ~ normal(mu_obs, sigma_obs);
}
