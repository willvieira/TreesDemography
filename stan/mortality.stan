data {
	int<lower=1> N;
	int<lower=1> Np; // number of unique plot_id
	array[N] int<lower=1,upper=Np> plot_id; // sequencial plot_ids
	array[N] int<lower=0,upper=1> state_t1; // alive [0] or dead [1]
	vector[N] delta_time; // time interval between t0 and t1
  vector[N] bio_01_mean;
  // range limit to optimal_temp parameter is species specific
  // And must be within the observed distribution of the species
  real maxTemp;
  real minTemp;
}
parameters {
	real<lower=-2,upper=8> psi; // baseline longevity
	vector[Np] psiPlot; // plot random effect
	real<lower=0> sigma_plot;
	real<lower=0,upper=1> optimal_temp;
  real<lower=0> tau_temp;
}
model {
	// Priors
	psi ~ normal(2, 1);
	psiPlot ~ normal(0, sigma_plot);
	sigma_plot ~ exponential(2);
  optimal_temp ~ beta(2, 2);
  tau_temp ~ normal(0, 2);

	// mortality rate
	vector[N] longev_log = 
		psi + // intercept
		psiPlot[plot_id] + // plot random effect
		-tau_temp .* square(bio_01_mean - optimal_temp); //temp effect

	// accounting for the time interval between sensus
	vector[N] mortality_time = 1 - exp(-delta_time .* log1p_exp(-longev_log));

	state_t1 ~ bernoulli(mortality_time);
}
