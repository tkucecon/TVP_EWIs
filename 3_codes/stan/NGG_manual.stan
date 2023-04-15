
// -----------------------------------------------------------------------------
// About this code
// Estimate a logit model with time-varying coefficients
// assumption of normal-gamma-gamma prior on beta_0 and theta
// -----------------------------------------------------------------------------

// input data
data {
  int                      I;      // sample size
  int                      p;      // number of covariates
  int                      Tmax;   // number of time variables
  matrix[I, p]             X;      // explanatory variable matrix
  int<lower=0, upper=1>    Y[I];   // crisis dummy
  int<lower=1, upper=Tmax> Tid[I]; // time index of each sample
  // hyperparameters for NGG: theta
  real<lower=0> a_xi;
  real<lower=0> c_xi;
  real<lower=0> kappa_b;
  // hyperparameters for NGG: the initial value of beta
  real<lower=0> a_tau;
  real<lower=0> c_tau;
  real<lower=0> lambda_b;
}

// parameters accepted by the model
parameters {
  // main
  matrix[p, Tmax] beta;     // matrix of coefficients including intercept
  vector<lower=0>[p] theta; // vector of sd of coefficients (controls smoothness)
  // latent parameters for NGG prior: theta
  vector<lower=0>[p] xi;
  vector<lower=0>[p] kappa;
  // latent parameters for NGG prior: beta
  vector<lower=0>[p] tau;
  vector<lower=0>[p] lambda;
}

// transformed parameters
transformed parameters {
  vector[I] q;    // main equation of logistic regression
  for (i in 1:I) {
    q[i] = inv_logit(X[i] * beta[, Tid[i]]);
  }
}

// model part
model {
  // priors for theta: NGG prior
  theta        ~ normal(0, xi);
  xi           ~ gamma(a_xi, (a_xi * kappa / 2));
  kappa        ~ gamma(c_xi, (c_xi / kappa_b));
  // priors for the initial value of beta: NGG prior
  beta[, 1]    ~ normal(0, tau);
  tau          ~ gamma(a_tau, (a_tau * lambda / 2));
  lambda       ~ gamma(c_tau, (c_tau / lambda_b));
  // state equation: let coefficients evolve over time
  for (t in 2:Tmax) {
    beta[, t] ~ normal(beta[, t-1], theta);
  }
  // logistic regression likelihood
  Y ~ bernoulli(q); // logistic regression
}
