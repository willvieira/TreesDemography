data {
  int<lower=0> N;
  array[N] int nbRecruit;
  vector[N] plot_size;
  vector[N] deltaTime;
  vector[N] BA_adult_sp;
  vector[N] BA_adult;
  vector[N] bio_01_mean; // mean total annual temperature (scaled)
  vector[N] bio_12_mean; // mean total annual precipitation (scaled)
  int<lower=0> Np;
  array[N] int<lower=0> plot_id;
  // range limit to optimal_temp and prec parameters area species specific
  // And must be within the observed distribution of the species
  real maxTemp;
  real minTemp;
  real maxPrec;
  real minPrec;
}
parameters {
  real mPop_log;
  real p_log;
  vector[Np] mPlot_log;
  real<lower=0> sigma_plot;
  real<lower=0> beta_p;
  real<lower=5,upper=150> optimal_BA;
  real<lower=0> sigma_BA;
  real<lower=minTemp,upper=maxTemp> optimal_temp; // optimal temperature
  real<lower=0> tau_temp; // inverse of temperature breadth
  real<lower=minPrec,upper=maxPrec> optimal_prec; // optimal precipitation
  real<lower=0> tau_prec; // inverse of precipitation breadth
}
model {
  vector[N] lambda;
  vector[N] m;
  vector[N] p;

  mPop_log ~ normal(-5, 1.5);
  mPlot_log ~ normal(0, sigma_plot);
  sigma_plot ~ exponential(6);
  p_log ~ normal(-3, 1.5);
  beta_p ~ normal(0, .6);
  optimal_BA ~ normal(20, 10);
  sigma_BA ~ normal(15, 10);
  optimal_temp ~ beta(2, 2);
  tau_temp ~ normal(0, 2);
  optimal_prec ~ beta(2, 2);
  tau_prec ~ normal(0, 2);

  // Species basal area effect with plot random effects
  m = exp(
    mPop_log + // intercept
    mPlot_log[plot_id] + // plot random effects
    (-1/square(sigma_BA)) .* square(BA_adult_sp - optimal_BA) + // competition
    -tau_temp .* square(bio_01_mean - optimal_temp) + // temperature
    -tau_prec .* square(bio_12_mean - optimal_prec) // precipitation
  );

  // Total basal area effect on p
  p = exp(
    -exp(
      p_log
    ) +
    BA_adult * -beta_p // competition
  );

  lambda = m .*
          plot_size .*
          (1 - p^deltaTime)./(1 - p);

  nbRecruit ~ poisson(lambda);
}
