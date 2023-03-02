data {
	int<lower=1> N;
	int<lower=1> Np; // number of unique plot_id
	array[N] int<lower=1,upper=Np> plot_id; // sequencial plot_ids
	int<lower=1> Ny; // number of unique years
	int<lower=1> Ni; // number of unique time intervals
	array[Ni] int<lower=1,upper=Ny> year0_seq; // year_id of time 0
	array[Ni] int<lower=1,upper=Ny> year1_seq; // year_id of time 1
	array[N] int<lower=1,upper=Ni> year_int; // year interval ID
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
transformed parameters {
	// average year random effect across all years within time interval t0 and t1
	vector[Ni] psiYear_interval;
	for(i in 1:Ni)
		psiYear_interval[i] = mean(psiYear[year0_seq[i]:year1_seq[i]]);
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
		psiYear_interval[year_int] // year random effect
	);

	// account for the time interval between sensus
	vector[N] mortality_time = 1 - pow(longev_log, delta_time);
	
	state_t1 ~ bernoulli(mortality_time);
}
