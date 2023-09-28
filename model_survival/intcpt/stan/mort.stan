data {
	int<lower=1> N;
  array[N] int<lower=0,upper=1> state_t1; // Indv state if alive [0] or dead [1]
  vector[N] delta_time; // time interval between t0 and t1
}
parameters {
	real<lower=-2,upper=10> psi; // baseline longevity
}
model {
	// Priors
	psi ~ normal(3, 1);

	// mortality rate
	real longev_log = inv_logit(
		psi // intercept
	);

	// account for the time interval between sensus
	vector[N] mortality_time = 1 - pow(longev_log, delta_time);
	
	state_t1 ~ bernoulli(mortality_time);
}
