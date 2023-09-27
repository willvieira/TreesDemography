data {
  int<lower=0> N;
  array[N] int nbRecruit;
  vector[N] plot_size;
  vector[N] deltaTime;
  int<lower=0> Np;
  array[N] int<lower=0> plot_id;
}
parameters {
  real m_log;
  real p_log;
  vector[Np] mPlot_log;
  real<lower=0> sigma_plot;
}
model {
  vector[N] lambda;
  vector[N] m;
  real p;

  m_log ~ normal(-5, 1.5);
  p_log ~ normal(-3, 1.5);
  mPlot_log ~ normal(0, sigma_plot);
  sigma_plot ~ exponential(6);

  // Species basal area effect with plot random effects
  m = exp(
    m_log + // intercept
    mPlot_log[plot_id] // plot random effects
  );

  // Total basal area effect on p
  p = exp(
    -exp(
      p_log
    )
  );

  lambda = m .*
          plot_size .*
          (1 - p^deltaTime)./(1 - p);

  nbRecruit ~ poisson(lambda);
}
