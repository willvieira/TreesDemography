functions {
  int num_zeros(int[] y) {
    int sum = 0;
    for (n in 1:size(y))
      sum += (y[n] == 0);
    return sum;
  }
}
data {
  int<lower=0> N;
  array[N] int nbRecruit;
  vector[N] plot_size;
  vector[N] deltaTime;
  vector[N] BA_adult;
  int<lower=0> Np;
  array[N] int<lower=0> plot_id;
}
transformed data {
  int<lower = 0> N_zero = num_zeros(nbRecruit);
  int<lower = 1> nbRecruit_nonzero[N - N_zero];

  int nzero = 0;
  int N_nonzero = 0;
  int<lower=1> zero_pos[N_zero];
  int<lower=1> nonzero_pos[N - N_zero];

  for (n in 1:N) {
    if (nbRecruit[n] == 0) {
      nzero += 1;
      zero_pos[nzero] = n;
    }else {
      N_nonzero += 1;
      nbRecruit_nonzero[N_nonzero] = nbRecruit[n];
      nonzero_pos[N_nonzero] = n;
    }
  }
}
parameters {
  real mPop_log;
  real<lower=0,upper=1> p;
  vector[Np] mPlot_log;
  real<lower=0> sigma_plot;
  real beta;
  real<lower=0, upper=1> theta;
}
model {
  vector[N] lambda;
  vector[N] m;

  mPop_log ~ normal(-5, 1.5);
  mPlot_log ~ normal(0, sigma_plot);
  sigma_plot ~ exponential(6);
  p ~ beta(2, 2);
  beta ~ normal(0, 1);
  theta ~ beta(1, 1);

  // Basal area effect with plot random effects
  m = exp(
    mPop_log + mPlot_log[plot_id] +
    BA_adult * beta
  );

  lambda = m .*
          plot_size .*
          (1 - p^deltaTime)/(1 - p);

  target += N_zero *
            log_sum_exp(
              bernoulli_lpmf(1 | theta),
              bernoulli_lpmf(0 | theta) +
              poisson_lpmf(0 | lambda[zero_pos])
            );

  target += N_nonzero * bernoulli_lpmf(0 | theta);

  target += poisson_lpmf(nbRecruit_nonzero | lambda[nonzero_pos]);
}
