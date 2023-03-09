data {
	int<lower=1> N;
	int<lower=1> Np; // number of unique plot_id
	array[N] int<lower=1,upper=Np> plot_id; // sequencial plot_ids
	array[N] int<lower=0,upper=1> state_t1; // alive [0] or dead [1]
	vector[N] delta_time; // time interval between t0 and t1
	vector[N] size_t0; // DBH at time 0
	vector[N] BA_comp_sp; // BA of larger ind from same species
	vector[N] BA_comp_inter; // BA of larger ind from other species
}
parameters {
	real<lower=-2,upper=8> psi; // baseline longevity
	vector[Np] psiPlot; // plot random effect
	real<lower=0> sigma_plot;
	real<lower=90,upper=600> size_opt; // optimal size for survival
	real<lower=0,upper=10> size_var;
	real Beta; // BA_comp effect
  real<lower=0,upper=2> theta; // partition of the importance between BA_sp and BA_inter
}
model {
	// Priors
	psi ~ normal(3, 1);
	psiPlot ~ normal(0, sigma_plot);
	sigma_plot ~ exponential(2);
	size_opt ~ normal(250, 100);
	size_var ~ lognormal(1, 1);
	Beta ~ normal(0, .1);
	theta ~ exponential(2.5);

	// mortality rate
	vector[N] longev_log = inv_logit(
		psi + // intercept
		psiPlot[plot_id] + // plot random effect
		-square(log(size_t0/size_opt)/size_var) +// size effect
		Beta * (BA_comp_sp + theta * BA_comp_inter) // Competition effect
	);

	// account for the time interval between sensus
	vector[N] mortality_time = 1 - pow(longev_log, delta_time);
	
	state_t1 ~ bernoulli(mortality_time);
}
