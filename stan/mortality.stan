data {
	int<lower=1> N;
	int<lower=1> Np; // number of unique plot_id
	array[N] int<lower=1,upper=Np> plot_id; // sequencial plot_ids
	array[N] int<lower=0,upper=1> state_t1; // alive [0] or dead [1]
	vector[N] delta_time; // time interval between t0 and t1
  vector[N] bio_12_mean; // mean total annual precipitation
  // range limit to optimal_temp parameter is species specific
}
parameters {
	real<lower=-2,upper=8> psi; // baseline longevity
	vector[Np] psiPlot; // plot random effect
	real<lower=0> sigma_plot;
	real<lower=0,upper=1> optimal_prec;
  real<lower=0> tau_prec;
}
model {
	// Priors
	psi ~ normal(4, 1);
	psiPlot ~ normal(0, sigma_plot);
	sigma_plot ~ exponential(2);
  optimal_prec ~ beta(2, 2);
  tau_prec ~ normal(0, 2);

	// mortality rate
	vector[N] longev_log = 
		psi + // intercept
		psiPlot[plot_id] + // plot random effect
		-tau_prec .* square(bio_12_mean - optimal_prec); //temp effect

	// accounting for the time interval between sensus
	vector[N] mortality_time = 1 - exp(-delta_time .* log1p_exp(-longev_log));

	state_t1 ~ bernoulli(mortality_time);
}
