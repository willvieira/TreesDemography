data {
  int<lower=0> N;
  vector[N] obs_size_t1;
  vector[N] time;
  vector[N] obs_size_t0;
  int<lower=1> Np; // number of unique plot_id
  array[N] int<lower=1,upper=Np> plot_id;
  int<lower=1> Nt; // number of unique plot_id
  array[N] int<lower=1,upper=Nt> tree_id;
  vector[N] BA_comp;
  vector[N] bio_01_mean;
  vector[N] bio_12_mean;
  // range limit to optimal_temp parameter is species specific
  // And must be within the observed distribution of the species
  real maxTemp;
  real minTemp;
  real maxPrec;
  real minPrec;
  vector[Np] latitude;
  int<lower=1> N_seq;
  vector[N_seq] latitude_seq;
}
transformed data {
  // to add minimum range to Lmax parameter
  real<lower=0> maxSize = max(obs_size_t1) * 1.2;
  // Normalize latitude
  real lat_mean = mean(latitude);
  real lat_sd = sd(latitude);
  array[Np] real lat_norm = to_array_1d((latitude - lat_mean)/lat_sd);
  real sigma_intercept = 0.1;
  // transform latitude
  int<lower=1> N_total = Np + N_seq;
  array[N_total] real total_lat;
  for(n1 in 1:Np) {
    total_lat[n1] = lat_norm[n1];
  }
  for(n2 in 1:N_seq) {
    total_lat[Np + n2] = latitude_seq[n2];
  }
  vector[N_total] zeros = rep_vector(0, N_total);
}
parameters {
  real r;
  vector[N_total] rPlot_log;
  vector[Nt] rTree_log;
  real<lower=0> sigma_tree;
  real<lower=0> sigma_obs;
  real<lower=maxSize> Lmax;
  // real Beta;
  // real<lower=minTemp,upper=maxTemp> optimal_temp;
  // real<lower=0> tau_temp;
  // real<lower=minPrec,upper=maxPrec> optimal_prec;
  // real<lower=0> tau_prec;
  real<lower=0> lengthscale_f; // lengthscale of f
  real<lower=0> sigma_f;       // scale of f
  real<lower=0> sigman;        // noise sigma
}
model {
  // covariances and Cholesky decompositions
  matrix[N_total, N_total] K_f = gp_exp_quad_cov(total_lat, sigma_f, lengthscale_f) + sigma_intercept;
  matrix[N_total, N_total] L_f = cholesky_decompose(add_diag(K_f, sigman));

  rPlot_log ~ multi_normal_cholesky(zeros, L_f);

  // priors
  r ~ normal(-3.5, 1);
  rTree_log ~ normal(0, sigma_tree);
  sigma_tree ~ exponential(2);
  sigma_obs ~ normal(0, 1.5);
  Lmax ~ normal(1000, 80);
  // Beta ~ normal(-1, 1);
  // optimal_temp ~ normal(5, 10);
  // tau_temp ~ normal(0, 1);
  // optimal_prec ~ normal(1700, 800);
  // tau_prec ~ normal(0, 1);
  lengthscale_f ~ normal(0, 1);
  sigma_f ~ normal(0, 1);
  sigman ~ normal(0, 1);

  // What matters here:
  vector[N] rPlot = exp( // growth parameter
    r + // intercept
    rPlot_log[1:Np][plot_id] + // plot random effect
    rTree_log[tree_id] //+ // tree random effect
    // BA_comp * Beta + // BA of larger individuals effect
    // -tau_temp .* square(bio_01_mean - optimal_temp) +//temp effect
    // -tau_prec .* square(bio_12_mean - optimal_prec) //prec effect
  );

  // pre calculate component of the model
  vector[N] rPlotTime = exp(-rPlot .* time);

  // mean
  vector[N] mu_obs = obs_size_t0 .*
    rPlotTime +
    Lmax * (1 - rPlotTime);

  // likelihood
  obs_size_t1 ~ normal(mu_obs, sigma_obs);
}
