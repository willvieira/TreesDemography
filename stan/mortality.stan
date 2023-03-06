data {
	int<lower=1> N;
	int<lower=1> Np; // number of unique plot_id
  	array[N] int<lower=1,upper=Np> plot_id; // sequencial plot_ids
  	array[N] int<lower=0,upper=1> state_t1; // alive [0] or dead [1]
  	vector[N] delta_time; // time interval between t0 and t1
		vector[N] BA_plot; // Plot total basal area
}
parameters {
	real<lower=-2,upper=8> psi; // baseline longevity
	vector[Np] psiPlot; // plot random effect
	real<lower=0> sigma_plot;
	real Beta; // slope of plot effect
}
model {
	// Priors
	psi ~ normal(5, 1);
	psiPlot ~ normal(0, sigma_plot);
	sigma_plot ~ lognormal(2, 1);
	Beta ~ normal(0, 2);

	// mortality rate
	vector[N] longev_log = inv_logit(
		psi + // intercept
		psiPlot[plot_id] + // plot random effect
		BA_plot * Beta // competition
	);

	// account for the time interval between sensus
	vector[N] mortality_time = 1 - pow(longev_log, delta_time);
	
	state_t1 ~ bernoulli(mortality_time);
}
