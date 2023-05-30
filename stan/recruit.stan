data {
  int<lower=0> N;
  array[N] int nbRecruit;
  vector[N] plot_size;
  vector[N] deltaTime;
  vector[N] BA_adult_sp;
  vector[N] BA_adult;
  int<lower=0> Np;
  array[N] int<lower=0> plot_id;
  int<lower=1> Ny; // number of unique years
  int<lower=1> Ni; // number of unique time intervals
  array[Ni] int<lower=1,upper=Ny> year0_seq; // year_id of time 0
  array[Ni] int<lower=1,upper=Ny> year1_seq; // year_id of time 1
  array[N] int<lower=1,upper=Ni> year_int; // year interval ID
}
parameters {
  real mPop_log;
  real p_log;
  vector[Np] mPlot_log;
  real<lower=0> sigma_plot;
  vector[Ny] mYear_log;
  real<lower=0> sigma_year;
  real<lower=0> beta_p;
  real<lower=5,upper=150> optimal_BA;
  real<lower=0> sigma_BA;
}
transformed parameters {
  // average year random effect across all years within time interval t0 and t1
  vector[Ni] mYear_interval;
  for(i in 1:Ni)
    mYear_interval[i] = mean(mYear_log[year0_seq[i]:year1_seq[i]]);
}
model {
  vector[N] lambda;
  vector[N] m;
  vector[N] p;

  mPop_log ~ normal(-5, 1.5);
  mPlot_log ~ normal(0, sigma_plot);
  sigma_plot ~ exponential(6);
  mYear_log ~ normal(0, sigma_year);
  sigma_year ~ exponential(2);
  p_log ~ normal(-3, 1.5);
  beta_p ~ normal(0, .6);
  optimal_BA ~ normal(20, 10);
  sigma_BA ~ normal(15, 10);

  // Species basal area effect with plot random effects
  m = exp(
    mPop_log + // intercept
    mPlot_log[plot_id] + // plot random effects
    mYear_interval[year_int] + // year random effect
    (-1/square(sigma_BA)) .* square(BA_adult_sp - optimal_BA) // BA effect
  );

  // Total basal area effect on p
  p = exp(
    -exp(
      p_log
    ) +
    BA_adult * -beta_p
  );

  lambda = m .*
          plot_size .*
          (1 - p^deltaTime)./(1 - p);

  nbRecruit ~ poisson(lambda);
}
