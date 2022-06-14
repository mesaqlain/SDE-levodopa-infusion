# SDE-levodopa-infusion
R Codes to accompany the paper "Investigating Stochastic Differential Equations Modelling for Levodopa Infusion in Patients with Parkinson’s Disease" by Saqlain et al. (2020)

This repository contains the code used to perform analysis of article: Investigating Stochastic Differential Equations Modelling for Levodopa Infusion in Patients with Parkinson’s Disease (https://link.springer.com/article/10.1007/s13318-019-00580-w) by Saqlain et al. (2020)

## Abstract: 
**Background and Objectives**
- Levodopa concentration in patients with Parkinson’s disease is frequently modelled with ordinary differential equations (ODEs). Here, we investigate a pharmacokinetic model of plasma levodopa concentration in patients with Parkinson’s disease by introducing stochasticity to separate the intra-individual variability into measurement and system noise, and to account for auto-correlated errors. We also investigate whether the induced stochasticity provides a better fit than the ODE approach.

**Methods**
- In this study, a system noise variable is added to the pharmacokinetic model for duodenal levodopa/carbidopa gel (LCIG) infusion described by three ODEs through a standard Wiener process, leading to a stochastic differential equations (SDE) model. The R package population stochastic modelling (PSM) was used for model fitting with data from previous studies for modelling plasma levodopa concentration and parameter estimation. First, the diffusion scale parameter (σw), measurement noise variance, and bioavailability are estimated with the SDE model. Second, σw is fixed to certain values from 0 to 1 and bioavailability is estimated. Cross-validation was performed to compare the average root mean square errors (RMSE) of predicted plasma levodopa concentration.

**Results**
- Both the ODE and the SDE models estimated bioavailability to be approximately 75%. The SDE model converged at different values of σw that were significantly different from zero. The average RMSE for the ODE model was 0.313, and the lowest average RMSE for the SDE model was 0.297 when σw was fixed to 0.9, and these two values are significantly different.

**Conclusions**
- The SDE model provided a better fit for LCIG plasma levodopa concentration by approximately 5.5% in terms of mean percentage change of RMSE.

## Directions:

### Step 1:
Load **00Dataset.R** file to clean and prepare all the datasets. 

### Step 2:
- Load **PSM_3EQ_NoSig_1a.R** file to perform analaysis. 
- Change lines 21 and 22 according to which patient is being analysed at the moment.
- Change line 24 sigvalue to add stochasticity. 0 value means ODE model
- Lines 87-97 are fixed values  taken for Westin et al. (2011)
- Line 110 is initial value for the BIO (bioavailability) parameter
- Change directory in lines 172, 176, and 184.
- Fit results stored in NoSig_1a_0p0.txt, change name as needed
- Residuals plots stored in NoSig_1a_1_0p0.pdf
- Visual check of PSM plot vs residuals stored in NoSig_Fit_1a_0p0.pdf

### Step 3:
To compare RMSE of two different fits with different sigma values, load the file **RMSE Compare.R**

### Residuals Plot
- Load the **ResidualsPlotSDE.R** to create plots shown in the manuscript.

## References:
- Saqlain, M., Alam, M., Rönnegård, L., & Westin, J. (2020). Investigating stochastic differential equations modelling for levodopa infusion in patients with Parkinson’s disease. European journal of drug metabolism and pharmacokinetics, 45(1), 41-49.
- Westin J, Nyholm D, Pålhagen S, Willows T, Groth T, Dougherty M, Karlsson MO. A pharmacokinetic–pharmacodynamic model for duodenal levodopa infusion. Clin Neuropharmacol. 2011;34(2):61–5.

