data
{
	// Size integer
	int<lower = 1> N; // size of response var

	// Vector data
	vector[N] T_data; // temperature, E data
	vector<lower = 0>[N] P_data; // Precipitation, E data
	vector<lower = 0, upper = 1>[N] C_data; // canopy, E data
	vector<lower = 0>[N] D_data; // diameter, I data
	vector<lower = 0>[N] Y; // response var, not 'logarithmised'
}

parameters // IMPORTANT: it worth adding constraints, at least to respect the priors, otherwise, a lot of divergence!
{
	real<lower = 0> pdg; // Potential Diameter Growth

	real<lower = 0> T_opt; // Optimum temperature of each species
	real<lower = 0> sigmaT_opt; // Variance among individuals of optimal T within a species

	real<lower = 0> P_opt; // Optimum precipitation of each species
	real<lower = 0> sigmaP_opt; // Variance among individuals of optimal P within a species

	real<lower = 0, upper = 1> beta;

	real<lower = 0> Phi_opt;
	real<lower = 0> sigmaPhi_opt;

	// real<lower = 0> sigma; // Variance of individuals around there species specific mean
	real<lower = 0> sigma_base;
}

transformed parameters
{
	vector[N] mu_d =
		pdg
		*
		(C_data + (1 - C_data) * beta)
		.*
		exp(-(T_data - T_opt) .* (T_data - T_opt)/sigmaT_opt^2)
		.*
		exp(-(P_data - P_opt) .* (P_data - P_opt)/sigmaP_opt^2)
		.*
		exp(-log(D_data/Phi_opt) .* log(D_data/Phi_opt)/sigmaPhi_opt^2);
}

model
{
	// Priors, hp1 contains their means, hp2 contains their variance
	pdg ~ gamma(5^2/100.0, 5/100.0);

	T_opt ~ normal(0, 20);
	sigmaT_opt ~ pareto_type_2(0.001, 10.0, 3.0);
	P_opt ~ normal(1500, 700);
	sigmaP_opt ~ normal(1500, 1000);

	beta ~ uniform(0, 1);

	Phi_opt ~ gamma(200^2/10000.0, 200/10000.0);
	sigmaPhi_opt ~ gamma(300^2/100000.0, 300/100000.0);

	sigma_base ~ pareto_type_2(0.01, 10, 2.4); // growth_dt[, var(growth)] = 2.6

	// Growth model
	Y ~ gamma(mu_d .* mu_d ./ sigma_base, mu_d ./ sigma_base);
}
