data
{
	// Size integer
	int<lower = 1> N; // size of response var

	// Vector data
	vector[N] T_data; // temperature, E data
	vector<lower = 0>[N] P_data; // Precipitation, E data
	vector<lower = 0, upper = 100>[N] BA_data; // Basal area
	vector<lower = -40, upper = 50>[N] CD_data; // canopyDistance
	vector<lower = 0>[N] D_data; // diameter, I data
	vector<lower = 0>[N] Y; // response var, not 'logarithmised'
}

parameters // IMPORTANT: it worth adding constraints, at least to respect the priors, otherwise, a lot of divergence!
{
	real<lower = 0, upper = 100> pdg; // Potential Diameter Growth

	real<lower = -8, upper = 25> T_opt; // Optimum temperature of each species
	real<lower = 0.2, upper = 20> sigmaT_opt; // Variance among individuals of optimal T within a species

	real<lower = 500, upper = 1700 > P_opt; // Optimum precipitation of each species
	real<lower = 80, upper = 1000> sigmaP_opt; // Variance among individuals of optimal P within a species

	real<lower = 0, upper = 1> sigma_C;
	real<lower = -8, upper = 15> Mid;

	real<lower = 0, upper = 850> Phi_opt;
	real<lower = 0, upper = 15> sigmaPhi_opt;

	// real<lower = 0> sigma; // Variance of individuals around there species specific mean
	real<lower = 0, upper = 12> sigma_base;
}

transformed parameters
{
	// CanopyDistance effect to decide whether BA matters or not
	vector[N] canopyDistEffect = (0 + ((1 - 0) ./ (1 + exp(-0.2 * (CD_data - Mid)))));

	vector[N] mu_d =
		pdg
		*
		(canopyDistEffect + ((1 - canopyDistEffect) .* exp(-(BA_data .* BA_data)/2 * (sigma_C * sigma_C))))
		.*
		(0.0001 + exp(-0.5 * (T_data - T_opt) .* (T_data - T_opt)/sigmaT_opt^2)
		.*
		exp(-0.5 * (P_data - P_opt) .* (P_data - P_opt)/sigmaP_opt^2))
		.*
		exp(-log(D_data/Phi_opt) .* log(D_data/Phi_opt)/sigmaPhi_opt^2);
}

model
{
	pdg ~ gamma(2^2/10.0, 2/10.0);

	T_opt ~ normal(6, 8);
	sigmaT_opt ~ pareto_type_2(0.001, 10.0, 3.0);
	P_opt ~ normal(1000, 350);
	sigmaP_opt ~ gamma(250^2/20000.0, 250/20000.0);

	sigma_C ~ uniform(0, 1);
	Mid ~ normal(2, 8);
	
	Phi_opt ~ gamma(200^2/10000.0, 200/10000.0);
	sigmaPhi_opt ~ gamma(4^2/15.0, 4/15.0);

	sigma_base ~ gamma(2.6^2/5, 2.6/5); // growth_dt[, var(growth)] = 2.6

	// Growth model
	Y ~ gamma(mu_d .* mu_d ./ sigma_base, mu_d ./ sigma_base);

}
