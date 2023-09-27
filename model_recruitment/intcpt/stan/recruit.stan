data {
  int<lower=0> N;
  array[N] int nbRecruit;
  vector[N] plot_size;
  vector[N] deltaTime;
}
parameters {
  real m_log;
  real p_log;
}
model {
  vector[N] lambda;
  real m;
  real p;

  m_log ~ normal(-5, 1.5);
  p_log ~ normal(-3, 1.5);

  // Species basal area effect with plot random effects
  m = exp(
    m_log // intercept
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
