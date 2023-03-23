data {
  int<lower=0> N;
  vector[N] obs_size_t1;
  vector[N] time;
  vector[N] obs_size_t0;
  int<lower=1> Np; // number of unique plot_id
  array[N] int<lower=1,upper=Np> plot_id;
  int<lower=1> Nt; // number of unique plot_id
  array[N] int<lower=1,upper=Nt> tree_id;
  vector[N] BA_comp_sp;
  vector[N] BA_comp_intra;
  vector[N] bio_01_mean;
  vector[N] bio_12_mean;
}
transformed data {
  // to add minimum range to Lmax parameter
  real<lower=0> maxSize = max(obs_size_t1) * 1.2;
}
parameters {
  real r;
  vector[Np] rPlot_log;
  vector[Nt] rTree_log;
  real<lower=0> sigma_PlotTree;
  real<lower=0,upper=1> p_plotTree;
  real<lower=0> sigma_obs;
  real<lower=maxSize> Lmax;
  real Beta;
  real<lower=0> theta;
  real<lower=0,upper=1> optimal_temp;
  real<lower=0> tau_temp;
  real<lower=0,upper=1> optimal_prec;
  real<lower=0> tau_prec;
}
model {
  // priors
  r ~ normal(-3.5, 1);
  rPlot_log ~ normal(0, sigma_PlotTree * p_plotTree);
  rTree_log ~ normal(0, sigma_PlotTree * (1 - p_plotTree));
  sigma_PlotTree ~ exponential(2);
  p_plotTree ~ beta(2, 2);
  sigma_obs ~ normal(0, 1.5);
  Lmax ~ normal(1000, 80);
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
    rTree_log[tree_id] + // tree random effect
    Beta * (BA_comp_sp + theta * BA_comp_intra) + // Intra and inter competition
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
