data {
  int<lower=1> L;	 // # regions
  int<lower=1> M;	 // # states
  int<lower=1> P;        // # cities
  int<lower=1> T;        // # time periods
  int<lower=1> N;	 // # obs
  int<lower=1> K_zone;   // # unique HOLC zones
  vector[N] y;
  int region[N];         // vars
  int state[N];
  int city[N];
  int period[N];
  int zone[N];	   
}
transformed data {
  vector[N] y_adj;
  y_adj = (y * (N - 1) + .5) / N;
}
parameters {
  // intercepts: overall, region, state, city
  real alpha;
  real<lower=0> sigma_alpha_region;
  vector<multiplier=sigma_alpha_region>[L] alpha_region;
  real<lower=0> sigma_alpha_state;
  vector<multiplier=sigma_alpha_state>[M] alpha_state;
  real<lower=0> sigma_alpha_city;
  vector<multiplier=sigma_alpha_city>[P] alpha_city;
  real<lower=0> sigma_alpha_period;
  vector<multiplier=sigma_alpha_period>[T] alpha_period;
  // zone
  real<lower=0> sigma_beta_zone;
  vector<multiplier=sigma_beta_zone>[K_zone] beta_zone;
  // dispersion parameter: phi
  real<lower=0> phi;   
}
model {
  vector[N] mu;
  mu = inv_logit(alpha
		 + beta_zone[zone]
		 + alpha_period[period]
		 + alpha_city[city]
		 + alpha_state[state]
		 + alpha_region[region]);
  // likelihood
  y_adj ~ beta(mu * phi, (1.0 - mu) * phi);
  // priors: coefficients
  alpha ~ normal(0,2);
  beta_zone ~ normal(0,sigma_beta_zone);
  alpha_period ~ normal(0,sigma_alpha_period);
  alpha_region ~ normal(0,sigma_alpha_region);
  alpha_state ~ normal(0,sigma_alpha_state);
  alpha_city ~ normal(0,sigma_alpha_city);
  // priors: standard deviations
  sigma_alpha_period ~ normal(0,1);
  sigma_alpha_region ~ normal(0,1);
  sigma_alpha_state ~ normal(0,1);
  sigma_alpha_city ~ normal(0,1);
  sigma_beta_zone ~ normal(0,1);
}
