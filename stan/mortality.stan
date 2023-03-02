data {
	int<lower=1> N;
	int<lower=1> Np; // number of unique plot_id
  	array[N] int<lower=1,upper=Np> plot_id; // sequencial plot_ids
	int<lower=1> Ny; // number of unique year_id
  	array[N] int<lower=1,upper=Ny> year_id; // sequencial year_ids
  	array[N] int<lower=0,upper=1> state_t1; // Indv state if alive [0] or dead [1]
  	vector[N] delta_time; // time interval between t0 and t1
}
parameters {
	real<lower=-2,upper=8> psi; // baseline longevity
	vector[Np] psiPlot; // plot random effect
  	vector[Ny] psiYear; // year random effect
	real<lower=0> sigma_plotYear; // total variance from plot and year
  	real<lower=0,upper=1> p_plotYear; // split variance between plot and year
}
model {
	// Priors
	psi ~ normal(5, 1);
	psiPlot ~ normal(0, sigma_plotYear * p_plotYear);
  	psiYear ~ normal(0, sigma_plotYear * (1 - p_plotYear));
	sigma_plotYear ~ lognormal(2, 1);
  	p_plotYear ~ beta(2, 2);

	// mortality rate
	vector[N] longev_log = inv_logit(
		psi + // intercept
		psiPlot[plot_id] + // plot random effect
		psiYear[year_id]   // year random effect
	);

	// account for the time interval between sensus
	vector[N] mortality_time = 1 - pow(longev_log, delta_time);
	
	state_t1 ~ bernoulli(mortality_time);
}
