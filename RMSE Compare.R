## Written by Murshid Saqlain (msq@du.se)
## Code to accompany the paper 
## Investigating Stochastic Differential Equations Modelling for 
## Levodopa Infusion in Patients with Parkinson’s Disease
## This script contains code to test the hypothesis that the two RMSEs
## when sigma = 0 and sigma = 0.9 are statistically different

#Create dataframe with all RMSE values from each of the sigma values.
RMSE_compare <- data.frame(cbind(RMSE0p0_VALUES_YS,RMSE0p9_VALUES_YS))
#Calculate the difference. Positive implies the RMSE went down in the SDE case.
RMSE_compare$difference <- RMSE_compare$RMSE0p0_VALUES_YS - RMSE_compare$RMSE0p9_VALUES_YS
#Calculate percentage change in RMSE for each occasion.
RMSE_compare$percentchange <- (RMSE_compare$difference / RMSE_compare$RMSE0p0_VALUES_YS)*100

#Create a column of 1s and 0s. 1 if RMSE went down (SDE is better), 0 otherwise.
for (i in 1:length(RMSE_compare$percentchange)) {
  if (RMSE_compare$difference[i] > 0 ) {
    RMSE_compare$SDE[i] <- 1 
  } else {
    RMSE_compare$SDE[i] <- 0
  }
}

## Count number of times where SDE has lower RMSE
table(RMSE_compare$SDE)
# 33 times out of 34.

## Calculate the mean percentage change.
average_rmse_change <- mean(RMSE_compare$percentchange)

## Perform a binomial test to check whether the two RMSEs are equal or not.
binom.test(33, 34, 0.5)
#Exact binomial test
#data:  33 and 34
#number of successes = 33, number of trials = 34, p-value = 4.075e-09
#alternative hypothesis: true probability of success is not equal to 0.5
#95 percent confidence interval:
#  0.8467323 0.9992556
#sample estimates:
#  probability of success 
#0.9705882 

