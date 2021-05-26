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
	int<lower = 1> N; // size of response var
	int<lower = 1> Ny; // size of unique year0

	// Vector data
	int year_sq[N]; // transformed year_sq [1:length(unique(year_sq))]
	vector<lower = -10, upper = 20>[N] T_data; // temperature, E data
	vector<lower = 60, upper = 1700>[N] P_data; // Precipitation, E data
	vector<lower = 0, upper = 100>[N] C_data; // Basal area, E data
	vector<lower = 0, upper = 1200>[N] D_data; // diameter, I data
	vector<lower = 0>[N] time_interv; // time between inventories
	int Y[N];
}

parameters
{
	real<lower = 70, upper = 400> psi; // baseline longevity
	vector<lower = -40, upper = 100>[Ny] psi_pmean; // mean of random effect from year0
	real<lower = 1, upper = 15> psi_psd; // sd of random effect from year0
	
	real<lower = -10, upper = 28> T_opt; // Optimum temperature
	real<lower = 0.2, upper = 20> sigmaT_opt; // Variance among individuals

	real<lower = 150, upper = 1900> P_opt; // Optimum precipitation
	real<lower = 80, upper = 1000> sigmaP_opt; // Variance among individuals

	real<lower = 13, upper = 40> Mid; // competition effect: Mid of Generalised logistic function
	real<lower = 0, upper = 1> Lo;

	real<lower = 0, upper = 850> DBHopt;
	real<lower = 0, upper = 12> DBHvar;
}

model
{
	// define the variables
	vector[Ny] psi_year;
	vector[N] M_d;
	vector[N] mortL;

	// Priors
	psi ~ normal(250, 70);
	psi_pmean ~ normal(0, psi_psd);
	psi_psd ~ cauchy(0, 3);

	T_opt ~ normal(6, 7);
	sigmaT_opt ~ pareto_type_2(0.001, 10.0, 3.0);
	P_opt ~ normal(1000, 350);
	sigmaP_opt ~ gamma(250^2/20000.0, 250/20000.0);

	Mid ~ normal(10, 20);
	Lo ~ uniform(0, 1);

	DBHopt ~ gamma(200^2/10000.0, 200/10000.0);
	DBHvar ~gamma(4^2/15.0, 4/15.0);

	// Likelihood
	for(j in 1:Ny)
        psi_year[j] = psi + psi_pmean[j];

	for(i in 1:N) {
		M_d[i] =
		1 ./ (1 + (
		psi_year[year_sq[i]]
		*
		(Lo + ((1 - Lo) ./ (1 + exp(0.25 * (C_data[i] - Mid)))))
		.*
		(0.0001 + exp(-0.5*(T_data[i] - T_opt).*(T_data[i] - T_opt)/sigmaT_opt^2)
		.*
		exp(-0.5*(P_data[i] - P_opt).*(P_data[i] - P_opt)/sigmaP_opt^2))
		.*
		exp(-pow(log(D_data[i]/DBHopt)/DBHvar, 2))));
	}
	
	// mortality dependent on time interval
	mortL = mortalityPerYear(N, M_d, time_interv);

	// mortality model
	Y ~ bernoulli(mortL);
}
