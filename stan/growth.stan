data {
  int<lower=0> N;
  vector[N] obs_growth;
  vector[N] time;
  vector[N] obs_size_t0;
  int<lower=1> Np; // number of unique plot_id
  array[N] int<lower=1,upper=Np> plot_id;
  vector[N] BA_comp;
  vector[N] bio_01_mean;
  vector[N] bio_12_mean;
  // range limit to optimal_temp parameter is species specific
  // And must be within the observed distribution of the species
  real maxTemp;
  real minTemp;
  real maxPrec;
  real minPrec;
}
parameters {
  	real<lower = -10, upper = 10> pdg; // Population potential Diameter Growth
	vector<lower = -2, upper = 10>[Np] pdg_pmean; // mean of random effect from plot_id
	real<lower = 0, upper = 40> pdg_psd; // sd of random effect from plot_id

	real<lower = minTemp, upper = maxTemp> T_opt; // Optimum temperature of each species
	real<lower = 0.5, upper = 100> sigmaT_opt; // Variance among individuals of optimal T within a species

	real<lower = minPrec, upper = maxPrec > P_opt; // Optimum precipitation of each species
	real<lower = 80, upper = 10000> sigmaP_opt; // Variance among individuals of optimal P within a species

	real<lower = 0, upper = 1> sigma_C; // competition effect: Mid of Generalised logistic function

	real<lower = 0, upper = 850> Phi_opt;
	real<lower = 0, upper = 100> sigmaPhi_opt;

	real<lower = 0, upper = 30> sigma_base;
}
model {
	// prios
	pdg ~ normal(1, .8);
	pdg_pmean ~ normal(0, pdg_psd);
	pdg_psd ~ exponential(3);

	T_opt ~ normal(10, 5);
	sigmaT_opt ~ normal(5, 3);
	P_opt ~ normal(1500, 350);
	sigmaP_opt ~ normal(500, 300);

	sigma_C ~ uniform(0, 1);

	Phi_opt ~ normal(300, 200);
	sigmaPhi_opt ~ normal(5, 4);

	sigma_base ~ exponential(2);

	// Likelihood
	vector[N] mu_d =
		exp(pdg + pdg_pmean[plot_id])
		.*
		(exp(-pow(BA_comp, 2)/2 * pow(sigma_C, 2)))
		.*
		(0.0001 + exp(-0.5 * pow(bio_01_mean - T_opt, 2)/sigmaT_opt^2)
		.*
		exp(-0.5 * pow(bio_12_mean - P_opt, 2)/sigmaP_opt^2))
		.*
		exp(-pow(log(obs_size_t0/Phi_opt), 2)/sigmaPhi_opt^2);

	// Growth model
	obs_growth ~ normal(mu_d, sigma_base);
}
