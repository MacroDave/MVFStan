data {
  int T; // number of obs
  int P; //number of variables
  matrix[T,P] Y; //dataset of generated series
  int<lower=0> growth_ss; //steady state assumption on potential growth
  int<lower=0> unr_ss; //steady state assumption on NAIRU 
}

parameters {
  
  #Coefficients
  vector[1] beta; //coefficient on output gap in phillips curve
  vector[1] lambda; //coefficient on lagged inflation in phillips curve
  vector[1] phi; //coefficient on lagged output gap in output gap equation
  vector[1] theta; //coefficient in stochastic process for growth rate of potential GDP
  vector[1] tau1; //coefficeint on output gap in okun's law equation
  vector[1] tau2; //coefficeint on lagged unr gap in unr gap equation
  vector[1] tau3; //coefficient in stochastic process for change in NAIRU
  vector[1] tau4; //coefficeint on lagged eqm UNR in NAIRU equation
  
  #Assumptions (steady state)
#  vector<lower= 0>[1] growth_ss;
#  vector<lower= 0>[1] unr_ss;
  
  #State Variables (unobserved economic variables)
  vector[T] YGAP; // output gap
  vector[T] LGDP_BAR; // potential output
  vector[T] G; // growth of potential output
  vector[T] UNR_BAR; // eqm unemployment rate  
  vector[T] UNR_GAP; // gap between unemployment rate and eqm
  vector[T] G_UNR_BAR; //change in unemployment rate gap
  
  #Innovations
  real<lower = 0> sigma_LGDP_BAR; // The scale of innovations to potential output
  real<lower = 0> sigma_G; // The scale of innovations to growth in potential output
  real<lower = 0> sigma_YGAP; // The scale of innovations to output gap
  real<lower = 0> sigma_UNR_GAP; // The scale of innovations to okun's law 
  real<lower = 0> sigma_UNR_BAR; // The scale of innovations to NAIRU
  real<lower = 0> sigma_G_UNR_BAR; // The scale of innovations to change in NAIRU
  real<lower = 0> sigma_PIE; // The scale of innovations to phillips curve
}

#transformed parameters {
#  for(t in 1:T) {
#    Y[t,1] = LGDP_BAR[t] + YGAP[t];
#    Y[t,4] = UNR_BAR[t] - UNR_GAP[t];
#  }
#}

model {
  // priors
  
  //Innovations
  sigma_LGDP_BAR ~ cauchy(0.2,3);
  sigma_G  ~ cauchy(0.3,3); 
  sigma_YGAP ~ cauchy(0.9,5); 
  sigma_UNR_GAP ~ cauchy(0.4,4);
  sigma_UNR_BAR ~ cauchy(0.2,2);
  sigma_G_UNR_BAR ~ cauchy(0.1,1.5);
  sigma_PIE ~ cauchy(2,5);
  
  //coefficients
  beta ~ normal(0,1); 
  lambda ~ normal(.5,.5);
  phi ~ normal(0,1); 
  theta ~ normal(.5,.5);
  tau1 ~ normal(0,1);  
  tau2 ~ normal(0,1); 
  tau3 ~ normal(.5,.5); 
  tau4 ~ normal(.5,.5); 

  //Steady State Assumptions
#  growth_ss ~ normal(0.8,0.1); //want to center around SS assumption of 0.8% per quarter
#  unr_ss ~ normal(5.25,0.1);  //want to center around SS assumption of 5.25%
   
  //Initialize State Equations
  LGDP_BAR[1] ~ normal(0,1);
  G[1] ~ normal(0,1);
  YGAP[1] ~ normal(0,1);
  UNR_GAP[1] ~ normal(0,1);
  UNR_BAR[1] ~ normal(0,1);
  G_UNR_BAR[1] ~ normal(0,1);

  // State Equations
  for(t in 2:T) {
    LGDP_BAR[t] ~ normal(LGDP_BAR[t-1] + G[t], sigma_LGDP_BAR);
	  G[t] ~ normal( (1-theta)*G[t-1] + theta*growth_ss, sigma_G);
	  YGAP[t] ~ normal( phi*YGAP[t-1], sigma_YGAP);
	  UNR_GAP[t] ~ normal( tau2*UNR_GAP[t-1] + tau1*YGAP[t], sigma_UNR_GAP);
	  UNR_BAR[t] ~ normal( (1-tau4)*UNR_BAR[t-1] + G_UNR_BAR[t] + tau4*unr_ss, sigma_UNR_BAR);
	  G_UNR_BAR[t] ~ normal ( (1-tau3)*G_UNR_BAR[t-1], sigma_G_UNR_BAR);
  }

  // Measurement Equations
  for(t in 1:T) {
    LGDP[t] = LGDP_BAR[t] + YGAP[t];
	  PIE[t] ~ normal( lambda*PIE[t-1] + beta*YGAP[t],sigma_PIE);
	  UNR[t] = UNR_BAR[t] - UNR_GAP[t];	  
  }
}