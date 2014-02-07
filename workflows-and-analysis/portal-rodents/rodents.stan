data{
  int<lower=0> N;
  real<lower=0> t[N];
  int<lower=0> count[N];
  int<lower=1>N_periods;
  int<lower=1>N_plots;
  
  //fixed effects
  int<lower = 0, upper = 1>total_removal[N_plots];
  
  // random effects:
  int<lower=1> period[N];
  int<lower=1> plot[N];
}

parameters {
  real Intercept;  
  real beta_t;
  real beta_total_removal;
  
  real<lower=0>beta_decadal_period; //flipping the sign == offsetting by pi radians
  real<lower=0>beta_annual_period;  //flipping the sign == offsetting by pi radians
  real<lower=-5, upper=5>annual_offset;  // offset loops around +/- pi
  real<lower=-25, upper=25>decadal_offset; // offset loops around +/- pi
  real<lower=5>decadal_period;           // "Decadal" defined as 5+ years

  // random effects
  real alpha_plot[N_plots];
  real alpha_period[N_periods];  
  real<lower=0>sigma_plot;
  real<lower=0>sigma_period;
}

model {
  real p[N];
  
  // priors
  beta_t ~ normal(0, 1);
  beta_decadal_period ~ exponential(1);
  //decadal_period ~ normal(10, 10);

  //Annual period is defined at exactly 1
  beta_annual_period ~ exponential(1);

  // random effects
  alpha_plot ~ normal(0, sigma_plot);
  alpha_period ~ normal(0, sigma_period);
  
  for(i in 1:N){
    p[i] <- inv_logit(
      Intercept + 
      beta_t * t[i] +
      beta_total_removal * total_removal[plot[i]] + 
      beta_annual_period * sin(2/1 * pi() * t[i] + annual_offset) +
      beta_decadal_period * sin(2/decadal_period * pi() * t[i] + decadal_offset) +
      alpha_plot[plot[i]] + 
      alpha_period[period[i]]
    );
  }
  count ~ binomial(45, p);
}
