data {
  int<lower=0> N;
  vector[N] obs_size_t1;
  vector[N] time;
  vector[N] obs_size_t0;
  int<lower=1> Np; // number of unique plot_id
  array[N] int<lower=1,upper=Np> plot_id;
  vector[N] BA_comp_sp;
  vector[N] BA_comp_inter;
  vector[N] bio_01_mean;
  vector[N] bio_12_mean;
  // range limit to optimal_temp and prec parameters area species specific
  // And must be within the observed distribution of the species
  real maxTemp;
  real minTemp;
  real maxPrec;
  real minPrec;
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
  real Beta;
  real<lower=0> theta;
  real<lower=minTemp,upper=maxTemp> optimal_temp;
  real<lower=0> tau_temp;
  real<lower=minPrec,upper=maxPrec> optimal_prec;
  real<lower=0> tau_prec;
}
model {
  // priors
  r ~ normal(-3.5, 1);
  sigma_obs ~ normal(0, 1.5);
  Lmax ~ normal(1000, 80);
  rPlot_log ~ normal(0, sigma_plot);
  sigma_plot ~ exponential(2);
  sigma_obs ~ normal(0, 1.5);
  Beta ~ normal(-1, 1);
  theta ~ lognormal(1, 3);
  optimal_temp ~ beta(2, 2);
  tau_temp ~ normal(0, 1);
  optimal_prec ~ beta(2, 2);
  tau_prec ~ normal(0, 1);

  // What matters here:
  vector[N] rPlot = exp( // growth parameter
    r + // intercept
    rPlot_log[plot_id] + // plot random effect
    Beta * (BA_comp_sp + theta * BA_comp_inter) + // Intra and inter competition
    -tau_temp .* square(bio_01_mean - optimal_temp) +//temp effect
    -tau_prec .* square(bio_12_mean - optimal_prec) //prec effect
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
