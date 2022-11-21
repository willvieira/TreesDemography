data {
  int<lower=0> N;
  array[N] int nbRecruit;
  vector[N] plot_size;
  vector[N] deltaTime;
  vector[N] BA_adult_sp;
  vector[N] BA_adult;
  int<lower=0> Np;
  array[N] int<lower=0> plot_id;
}
parameters {
  real mPop_log;
  real p_log;
  vector[Np] mPlot_log;
  real<lower=0> sigma_plot;
  real<lower=0> beta_m;
  real<lower=0> beta_p;
}
model {
  vector[N] lambda;
  vector[N] m;
  vector[N] p;

  mPop_log ~ normal(-5, 1.5);
  mPlot_log ~ normal(0, sigma_plot);
  sigma_plot ~ exponential(6);
  p_log ~ normal(-3, 1.5);
  beta_m ~ normal(0, .6);
  beta_p ~ normal(0, .6);

  // Species basal area effect with plot random effects
  m = exp(
    mPop_log + mPlot_log[plot_id] +
    BA_adult_sp * beta_m
  );

  // Total basal area effect on p
  p = exp(
    -exp(
      p_log
    ) +
    square(BA_adult) * 1/2 * -square(beta_p)
  );

  lambda = m .*
          plot_size .*
          (1 - p^deltaTime)./(1 - p);

  nbRecruit ~ poisson(lambda);
}
