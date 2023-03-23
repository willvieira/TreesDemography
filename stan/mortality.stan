data {
	int<lower=1> N;
	int<lower=1> Np; // number of unique plot_id
	array[N] int<lower=1,upper=Np> plot_id; // sequencial plot_ids
	int<lower=1> Ny; // number of unique years
	int<lower=1> Ni; // number of unique time intervals
	array[Ni] int<lower=1,upper=Ny> year0_seq; // year_id of time 0
	array[Ni] int<lower=1,upper=Ny> year1_seq; // year_id of time 1
	array[N] int<lower=1,upper=Ni> year_int; // year interval ID
	array[N] int<lower=0,upper=1> state_t1; // alive [0] or dead [1]
	vector[N] delta_time; // time interval between t0 and t1
	vector[N] size_t0; // DBH at time 0
	vector[N] BA_sp; // plot basal area of conspecific species
	vector[N] BA_inter; // plot basal area of heterospecific species
	vector[N] bio_01_mean; // mean total annual temperature (scaled)
	vector[N] bio_12_mean; // mean total annual precipitation (scaled)
}
parameters {
	real<lower=-2,upper=8> psi; // baseline longevity
	vector[Np] psiPlot; // plot random effect
	vector[Ny] psiYear; // year random effect
	real<lower=0> sigma_plot;
	real<lower=0> sigma_year;
	real<lower=90,upper=600> size_opt; // optimal size for survival
	real<lower=0,upper=10> size_var;
	real Beta; // BA_comp effect
	real<lower=0,upper=2> theta; // partition the effect between BAsp and BAinter
	real<lower=0,upper=1> optimal_temp; // optimal temperature
	real<lower=0> tau_temp; // inverse of temperature breadth
	real<lower=0,upper=1> optimal_prec; // optimal precipitation
	real<lower=0> tau_prec; // inverse of precipitation breadth
}
transformed parameters {
	// average year random effect across all years within time interval t0 and t1
	vector[Ni] psiYear_interval;
	for(i in 1:Ni)
		psiYear_interval[i] = mean(psiYear[year0_seq[i]:year1_seq[i]]);
}
model {
	// Priors
	psi ~ normal(3, 1);
	psiPlot ~ normal(0, sigma_plot);
	sigma_plot ~ exponential(2);
	psiYear ~ normal(0, sigma_year);
	sigma_year ~ exponential(2);
	size_opt ~ normal(250, 100);
	size_var ~ lognormal(1, 1);
	Beta ~ normal(0, .1);
	theta ~ exponential(2.5);
	optimal_temp ~ beta(2, 2);
	tau_temp ~ normal(0, 2);
	optimal_prec ~ beta(2, 2);
	tau_prec ~ normal(0, 2);

	// mortality rate
	vector[N] longev_log = inv_logit(
		psi + // intercept
		psiPlot[plot_id] + // plot random effect
		psiYear_interval[year_int] + // year random effect
		-square(log(size_t0/size_opt)/size_var) +// size effect
		Beta * (BA_sp + theta * BA_inter) +// Competition effect
		-tau_temp .* square(bio_01_mean - optimal_temp) + //temp effect
		-tau_prec .* square(bio_12_mean - optimal_prec) //temp effect
	);

	// account for the time interval between sensus
	vector[N] mortality_time = 1 - pow(longev_log, delta_time);
	
	state_t1 ~ bernoulli(mortality_time);
}
