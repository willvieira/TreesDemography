functions {
  int num_zeros(array[] int y) {
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
  vector[N] BA_adult_sp;
  int<lower=0> Np;
  array[N] int<lower=0> plot_id;
}
transformed data {
  int<lower = 0> N_zero = num_zeros(nbRecruit);
  array[N - N_zero] int<lower=1> nbRecruit_nonzero;

  int nzero = 0;
  int N_nonzero = 0;
  array[N_zero] int<lower=1> zero_pos;
  array[N - N_zero] int<lower=1> nonzero_pos;

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
  real m_int;
  real<lower=0, upper=1> p;
  real beta;
  real thetaPop;
  vector[Np] thetaPlot;
  real<lower=0> sigma_plot;
}
model {
  vector[N] lambda;
  vector[N] m;
  vector[N] theta;

  m_int ~ normal(-5, 1.5);
  p ~ beta(3, 1.5);
  beta ~ normal(0, 1);
  thetaPop ~ normal(0, 1.5);
  thetaPlot ~ normal(0, sigma_plot);
  sigma_plot ~ exponential(3);

  // Plot basal area of adults on theta
  theta = inv_logit(thetaPop + thetaPlot[plot_id]);

  // Conspecific basal area effect on m
  m = exp(m_int + BA_adult * beta);

  lambda = m .*
          plot_size .*
          (1 - p^deltaTime)/(1 - p);

  target += N_zero .*
            log_sum_exp(
              bernoulli_lpmf(1 | theta[zero_pos]),
              bernoulli_lpmf(0 | theta[zero_pos]) +
              poisson_lpmf(0 | lambda[zero_pos])
            );

  target += N_nonzero .* bernoulli_lpmf(0 | theta[zero_pos]);

  target += poisson_lpmf(nbRecruit_nonzero | lambda[nonzero_pos]);
}
