data
{
	// Size integer
	int<lower = 1> N; // size of response var

	// Vector data
	vector[N] T_data; // temperature, E data
	vector<lower = 0>[N] P_data; // Precipitation, E data
	vector<lower = -40, upper = 50>[N] C_data; // canopy, E data
	vector<lower = 0>[N] D_data; // diameter, I data
	vector<lower = 0>[N] Y; // response var, not 'logarithmised'
}

parameters // IMPORTANT: it worth adding constraints, at least to respect the priors, otherwise, a lot of divergence!
{
	real<lower = 0> pdg; // Potential Diameter Growth

	real<lower = -10, upper = 25> T_opt; // Optimum temperature of each species
	real<lower = 0> sigmaT_opt; // Variance among individuals of optimal T within a species

	real<lower = 0, upper = 1500 > P_opt; // Optimum precipitation of each species
	real<lower = 0> sigmaP_opt; // Variance among individuals of optimal P within a species

	real<lower = -30, upper = 40> Mid; // competition effect: Mid of Generalised logistic function
	real<lower = 0, upper = 1> Lo;

	real<lower = 0, upper = 850> Phi_opt;
	real<lower = 0> sigmaPhi_opt;

	// real<lower = 0> sigma; // Variance of individuals around there species specific mean
	real<lower = 0> sigma_base;
}

transformed parameters
{
	vector[N] mu_d =
		pdg
		*
		(Lo + ((1 - Lo) ./ (1 + exp(-0.5 * (C_data - Mid)))))
		.*
		(0.0001 + exp(-0.5 * (T_data - T_opt) .* (T_data - T_opt)/sigmaT_opt^2)
		.*
		exp(-0.5 * (P_data - P_opt) .* (P_data - P_opt)/sigmaP_opt^2))
		.*
		exp(-log(D_data/Phi_opt) .* log(D_data/Phi_opt)/sigmaPhi_opt^2);
}

model
{
	pdg ~ gamma(7^2/100.0, 7/100.0);

	T_opt ~ normal(15, 10);
	sigmaT_opt ~ pareto_type_2(0.001, 10.0, 3.0);
	P_opt ~ normal(550, 300);
	sigmaP_opt ~ gamma(200^2/20000.0, 200/20000.0);

	Mid ~ normal(0, 20);
	Lo ~ uniform(0, 1);
	
	Phi_opt ~ gamma(200^2/10000.0, 200/10000.0);
	sigmaPhi_opt ~ gamma(300^2/100000.0, 300/100000.0);

	sigma_base ~ pareto_type_2(0.01, 10, 2.4); // growth_dt[, var(growth)] = 2.6

	// Growth model
	Y ~ gamma(mu_d .* mu_d ./ sigma_base, mu_d ./ sigma_base);

}

