data {
  int<lower=0> N;
  vector[N] obs_size_t1;
  vector[N] time;
  vector[N] obs_size_t0;
  int<lower=1> Np; // number of unique plot_id
  array[N] int<lower=1,upper=Np> plot_id;
	int<lower=1> Ny; // number of unique years
	int<lower=1> Ni; // number of unique time intervals
	array[Ni] int<lower=1,upper=Ny> year0_seq; // year_id of time 0
	array[Ni] int<lower=1,upper=Ny> year1_seq; // year_id of time 1
	array[N] int<lower=1,upper=Ni> year_int; // year interval ID
  vector[N] BA_comp_sp;
  vector[N] BA_comp_intra;
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
  vector[Np] rPlot;
  vector[Ni] rYear;
  real<lower=0> sigma_plot;
  real<lower=0> sigma_year;
  real<lower=0> sigma_obs;
  real<lower=maxSize> Lmax;
  real Beta;
  real<lower=0> theta;
  real<lower=minTemp,upper=maxTemp> optimal_temp;
  real<lower=0> tau_temp;
  real<lower=minPrec,upper=maxPrec> optimal_prec;
  real<lower=0> tau_prec;
}
transformed parameters {
	// average year random effect across all years within time interval t0 and t1
	vector[Ni] rYear_interval;
	for(i in 1:Ni)
		rYear_interval[i] = mean(rYear[year0_seq[i]:year1_seq[i]]);
}
model {
  // priors
  r ~ normal(-3.5, 1);
  rPlot ~ normal(0, sigma_plot);
  sigma_plot ~ exponential(2);
  rYear ~ normal(0, sigma_year);
  sigma_year ~ exponential(2);
  sigma_obs ~ normal(0, 1.5);
  Lmax ~ normal(1000, 80);
  Beta ~ normal(-1, 1);
  theta ~ lognormal(1, 3);
  optimal_temp ~ beta(2, 2);
  tau_temp ~ normal(0, 1);
  optimal_prec ~ beta(2, 2);
  tau_prec ~ normal(0, 1);

  // What matters here:
  vector[N] r_fixedpRandom = exp( // growth parameter
    r + // intercept
    rPlot[plot_id] + // plot random effect
    rYear_interval[year_int] + // year random effect
    Beta * (BA_comp_sp + theta * BA_comp_intra) + // Intra and inter competition
    -tau_temp .* square(bio_01_mean - optimal_temp) +//temp effect
    -tau_prec .* square(bio_12_mean - optimal_prec) //prec effect
  );

  // pre calculate component of the model
  vector[N] rPlotTime = exp(-r_fixedpRandom .* time);

  // mean
  vector[N] mu_obs = obs_size_t0 .*
    rPlotTime +
    Lmax * (1 - rPlotTime);

  // likelihood
  obs_size_t1 ~ normal(mu_obs, sigma_obs);
}
