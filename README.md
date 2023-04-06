# Stable and Time-varying Early Warning Indicators: A Sparse Bayesian Logistic Regression Approach

GitHub page for the paper: XXXX (name of the paper to be updated)

## 1_createDB.r
This code downloads the two data set
- JST data: main data set  
 from Macrohistory database (http://data.macrohistory.net/JST/JSTdatasetR5.dta) 
 - Baron et al.(2021)[^1]data: bank equity return  
 from the author's website (http://wxiong.mycpanel.princeton.edu/)

[^1]: Baron, Matthew, Emil Verner, and Wei Xiong. "Banking crises without panics." The Quarterly Journal of Economics 136.1 (2021): 51-113.

 and creates the master data under 4_data folder. The resulting data sets are
- df.JST: contains raw values
- df.JST.normalized: contains normalized explanatory variables (mean=0, sd=1)

See the data section of the paper for the definition of each variable

## 2_fitstan.r
This is the main code to estimate a sparse dynamic logistic regression model and for some robustness checks. The baseline logistic model assumes that 
$$q_{i,t} = X_{i,t}\beta_t + \varepsilon_{i,t}$$
where $q_{i,t}$ is the logit transformed probability of financial crisis ($Pr[crisis_{i,t}]$). 

Linear models for the robustness check assumes that 
$$y_{i,t} = X_{i,t}\beta_t + \varepsilon_{i,t}$$
where $y_{i,t}$ is 99%-winsorized annual bank equity returns 1 or 2 years ahead. 

The structure of state equation differs according to the stan file (gaussian random-walk or possible structural changes). See each stan files for details.

## stan/gaussian.stan
Stan code to run MCMC estimation for the baseline model. Here I assume that the coefficients are identical across countries contemporally, but evolves over time following a random walk with Gaussian errors (state equation), namely
$$\beta_t \sim N(\beta_{t-1}, \sigma)$$
This state equation implicitly assumes that the coefficients smoothly and slowly evolves over time. To avoid the problem of over-fitting and allow for sparsity, Horseshoe priors are assumed for both $\beta_0$ and $\sigma$. For the estimation of TVP models under the assumption of sparsity, see Bitto and Fruhwirth-Schnatter (2019)[^2].

[^2]: Bitto, Angela, and Sylvia Frühwirth-Schnatter. "Achieving shrinkage in a time-varying parameter model framework." Journal of Econometrics 210.1 (2019): 75-97.

## util/plot_heat.r
A function to plot a heat-map of the time-varying coefficients. 

## util/plot_dynamic.r
A function to draw line plots of significant coefficients with 90% posterior credible region.
