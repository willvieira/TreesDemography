data {
  int<lower=0> N;
  array[N] int nbRecruit;
  vector[N] plot_size;
  vector[N] deltaTime;
  int<lower=0> Np;
  array[N] int<lower=0> plot_id;
}
parameters {
  real mPop_log;
  real<lower=0,upper=1> p;
  vector[Np] mPlot_log;
  real<lower=0> sigma_plot;
}
model {
  vector[N] lambda;
  vector[N] m;

  mPop_log ~ normal(-5, 1.5);
  mPlot_log ~ normal(0, sigma_plot);
  sigma_plot ~ exponential(6);
  p ~ beta(2, 2);

  m = exp(mPop_log + mPlot_log[plot_id]);

  lambda = m .*
          plot_size .*
          (1 - p^deltaTime)/(1 - p);

  nbRecruit ~ poisson(lambda);
}
