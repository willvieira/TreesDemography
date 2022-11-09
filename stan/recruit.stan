data {
  int<lower=0> N;
  array[N] int nbRecruit;
  vector[N] plot_size;
  vector[N] deltaTime;
}
parameters {
  real<lower=0> m;
  real<lower=0,upper=1> p;
}
model {
  vector[N] lambda;

  m ~ lognormal(1.6, .8);
  p ~ beta(2, 2);

  lambda = m *
          plot_size .*
          (1 - p^deltaTime)/(1 - p);

  nbRecruit ~ poisson(lambda);
}
