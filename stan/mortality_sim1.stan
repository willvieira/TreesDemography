
functions
{
	vector mortalityPerYear(int N, vector mu_d, vector time_interv)
	{
		vector[N] mu_d2;
		vector[N] mort;
		vector[N] mu_d1 = 1 - mu_d;
		for (i in 1:N)
			mu_d2[i] = mu_d1[i]^time_interv[i];
		mort = 1 - mu_d2;

		return mort;
	}
}

data
{
	// Size integer
	int<lower = 1> N; // size of response var

	// Vector data
	vector<lower = -35, upper = 10>[N] T_data; // temperature, E data
	vector<lower = 60, upper = 1700>[N] P_data; // Precipitation, E data
	vector<lower = -40, upper = 50>[N] C_data; // canopy, E data
	vector<lower = 0, upper = 1200>[N] D_data; // diameter, I data
	vector<lower = 0>[N] time_interv; // time between inventories
	int Y[N];
}

parameters // IMPORTANT: it worth adding constraints, at least to respect the priors, otherwise, a lot of divergence!
{
	real<lower = 0, upper = 800> psi; // baseline longevity

	real<lower = -40, upper = 40> T_opt; // Optimum temperature of each species
	real<lower = 0, upper = 100> sigmaT_opt; // Variance among individuals of optimal T within a species

	real<lower = 0, upper = 3000> P_opt; // Optimum precipitation of each species
	real<lower = 0, upper = 1000> sigmaP_opt; // Variance among individuals of optimal P within a species

	real<lower = -30, upper = 40> Mid; // competition effect: Mid of Generalised logistic function

	real<lower = 0, upper = 800> DBHopt;
	real<lower = 0, upper = 10> DBHvar;
}

transformed parameters
{

	// define the variables
	vector[N] M_d;
	vector[N] mortL;
	vector[N] lgSq;

	// part of the size effect function
	for (i in 1:N)
		lgSq[i] = pow(log(D_data[i]/DBHopt)/DBHvar, 2);


	// complete model
	M_d =
	1 ./ (1 + (
	psi
	*
	(1 ./ (1 + exp(-0.5 * (C_data - Mid))))
	.*
	exp(-0.5*(T_data - T_opt).*(T_data - T_opt)/sigmaT_opt^2)
	.*
	exp(-0.5*(P_data - P_opt).*(P_data - P_opt)/sigmaP_opt^2)
	.*
	exp(-lgSq)));

	// mortality dependent on time interval
	mortL = mortalityPerYear(N, M_d, time_interv);
}

model
{
	// Priors
	psi ~ normal(350, 150);

	T_opt ~ normal(0, 20);
	sigmaT_opt ~ pareto_type_2(0.001, 10.0, 3.0);
	P_opt ~ normal(1500, 700);
	sigmaP_opt ~ normal(1500, 1000);

	Mid ~ normal(0, 20);

	DBHopt ~ normal(200, 200);
	DBHvar ~ normal(3, 6);

	// mortality model
	Y ~ bernoulli(mortL);
}
