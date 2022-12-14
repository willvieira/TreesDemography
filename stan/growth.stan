data {
  int<lower=0> N;
  vector[N] obs_size_t1;
  vector[N] time;
  vector[N] obs_size_t0;
  int<lower=1> Np; // number of unique plot_id
  array[N] int<lower=1,upper=Np> plot_id;
  vector[N] BA_comp;
  vector[N] bio_01_mean;
  // range limit to optimal_temp parameter is species specific
  // And must be within the observed distribution of the species
  real maxTemp;
  real minTemp;
}
transformed data {
  // to add minimum range to Lmax parameter
  real<lower=0> maxSize = max(obs_size_t1) * 1.2;
}
parameters {
  real r;
  vector[Np] rPlot_log;
  real<lower=0> sigma_plot;
  real<lower=0> sigma_obs;
  real<lower=maxSize> Lmax;
  real Beta;
  real<lower=minTemp,upper=maxTemp> optimal_temp;
  real<lower=1,upper=100> sigma_temp;
}
model {
  // priors
  r ~ normal(-3.5, 1);
  rPlot_log ~ normal(0, sigma_plot);
  sigma_plot ~ exponential(3);
  sigma_obs ~ normal(0, 1.5);
  Lmax ~ normal(1000, 80);
  Beta ~ normal(-1, 1);
  optimal_temp ~ normal(5, 10);
  sigma_temp ~ normal(8, 8);

  // What matters here:
  vector[N] rPlot = exp( // growth parameter
    r + // intercept
    rPlot_log[plot_id] + // plot random effect
    BA_comp * Beta + // BA of larger individuals effect
    (-1/pow(sigma_temp, 2)) .* pow(bio_01_mean - optimal_temp, 2) //temp effect
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
