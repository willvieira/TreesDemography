data
{
	// Size integer
	int<lower = 1> N; // size of response var
	int<lower = 1> Np; // size of unique plot_id

	// Vector data
	int plot_id[N]; // transformed plot_id [1:length(unique(plot_id))]
	vector[N] T_data; // temperature, E data
	vector<lower = 0>[N] P_data; // Precipitation, E data
	vector<lower = -40, upper = 50>[N] C_data; // canopyDistance
	vector<lower = 0>[N] D_data; // diameter, I data
	vector<lower = 0>[N] Y; // response var, not 'logarithmised'
}

parameters
{
	real<lower = 2, upper = 100> pdg; // Population potential Diameter Growth
	vector<lower = -2, upper = 10>[Np] pdg_pmean; // mean of random effect from plot_id
	real<lower = 0, upper = 10> pdg_psd; // sd of random effect from plot_id

	real<lower = -8, upper = 25> T_opt; // Optimum temperature of each species
	real<lower = 0.2, upper = 20> sigmaT_opt; // Variance among individuals of optimal T within a species

	real<lower = 500, upper = 1700 > P_opt; // Optimum precipitation of each species
	real<lower = 80, upper = 2000> sigmaP_opt; // Variance among individuals of optimal P within a species

	real<lower = 0, upper = 1> Lo;
	real<lower = -11, upper = 21> mid;

	real<lower = 0, upper = 850> Phi_opt;
	real<lower = 0, upper = 15> sigmaPhi_opt;

	// real<lower = 0> sigma; // Variance of individuals around there species specific mean
	real<lower = 0, upper = 12> sigma_base;
}

model
{
	vector[Np] pdg_plot;
    vector[N] mu_d;

	// prios
	pdg ~ gamma(15^2/100.0, 15/100.0);
	pdg_pmean ~ normal(0, pdg_psd);
	pdg_psd ~ cauchy(0, 2);

	T_opt ~ normal(6, 8);
	sigmaT_opt ~ pareto_type_2(0.001, 10.0, 3.0);
	P_opt ~ normal(1000, 350);
	sigmaP_opt ~ normal(500, 300);

	Lo ~ uniform(0, 1);
	mid ~ uniform(-10, 20);
	
	Phi_opt ~ gamma(200^2/10000.0, 200/10000.0);
	sigmaPhi_opt ~ gamma(4^2/15.0, 4/15.0);

	sigma_base ~ gamma(5^2/20.0, 5/20.0);

	// Likelihood
    for(j in 1:Np) {
        pdg_plot[j] = pdg + pdg_pmean[j];
    }

	for(i in 1:N) {
		mu_d[i] =
			pdg_plot[plot_id[i]]
			*
			(Lo + ((1 - Lo) ./ (1 + exp(-0.45 * (C_data[i] - mid)))))
			.*
			(0.0001 + exp(-0.5 * (T_data[i] - T_opt) .* (T_data[i] - T_opt)/sigmaT_opt^2)
			.*
			exp(-0.5 * (P_data[i] - P_opt) .* (P_data[i] - P_opt)/sigmaP_opt^2))
			.*
			exp(-log(D_data[i]/Phi_opt) .* log(D_data[i]/Phi_opt)/sigmaPhi_opt^2);
	}

	// Growth model
	Y ~ gamma(mu_d .* mu_d ./ sigma_base, mu_d ./ sigma_base);

}
