# Data-and-Empirical-Result-for-Paper

This  repository include data and empirical result for my paper.

I try to divide them into three parts.

The first part is DATA CLEANING. I used PYTHON to clean the data and made it suitable for further steps.

The second part is MARGINAL DISTRIBUTION. In this part, I used R to fit the GARCH MODEL and make a data description.

After the PIT process, I got the standard residual data, which fall in 0 and 1 interval. Then we can try to fit the Copula Model.

The third part of this big model is COPULA MODEL. I used the Copula and VineCopula package in R to finish the estimation process of Copula model.

With the parameters of Copula, I calculated the VaR, CoVaR, ΔCoVaR and %CoVaR in fourth part in R.

Finally, Model Backtesting was compilited in PYTHON.
