data {
  int<lower=0> N;
  array[N] int nbRecruit;
  vector[N] plot_size;
  vector[N] deltaTime;
  vector[N] BA_adult;
  vector[N] relativeBA_adult_sp;
  int<lower=0> Np;
  array[N] int<lower=0> plot_id;
}
parameters {
  real mPop_log;
  real<lower=0,upper=1> p;
  vector[Np] mPlot_log;
  real<lower=0> sigma_plot;
  real<lower=0> beta;
  real<lower=0> m_imm;
}
model {
  vector[N] lambda;
  vector[N] m;

  mPop_log ~ normal(-5, 1.5);
  mPlot_log ~ normal(0, sigma_plot);
  sigma_plot ~ exponential(6);
  p ~ beta(2, 2);
  beta ~ normal(0, 1);
  m_imm ~ normal(0, 1);

  // Basal area effect with plot random effects
  m = exp(
    mPop_log + mPlot_log[plot_id] +
    BA_adult * -beta
  ) .* relativeBA_adult_sp + m_imm;

  lambda = m .*
          plot_size .*
          (1 - p^deltaTime)/(1 - p);

  nbRecruit ~ poisson(lambda);
}
