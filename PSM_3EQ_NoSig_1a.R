## Written by Murshid Saqlain (msq@du.se)
## Code to accompany the paper 
## Investigating Stochastic Differential Equations Modelling for 
## Levodopa Infusion in Patients with Parkinson’s Disease
## PSM Estimation Code. First 3 Equations are included. 
## All population parameters are relaxed. 
## This is the main script that performs the analysis 

## Load the PSM package.
library(PSM)
library(data.table)

## Create list to store convergence values
conv_list_0p0 <- c()

## Clear objects from previous run:
d1 <- c()

## Specify variables for computations
##  datasetX is training set for a patient, datasetT is test set for that patient
datasetX <- dataset1a  ##
datasetT <- dataset1atest
eta_number <- 1
sigvalue <- 0 ## Change this to add stochasticity, 0 = ODE model.

##Create an empty list for our model used in PSM.
Model.levodopa = list()

##Time invariant matrix is created here. 
##The individual parameters are extracted from phi.
Model.levodopa$Matrices = function(phi) {
  kai <- phi$kai;
  V1i <- phi$V1i;
  V2i <- phi$V2i; 
  CLi <- phi$CLi; 
  Qi <- phi$Qi;
  bioi <- phi$bioi;
  matA <- matrix(c(-1/kai , 0 , 0, 
                   bioi/kai , -(CLi+Qi)/V1i, Qi/V2i,
                   0, Qi/V1i, -Qi/V2i),
                 nrow=3,byrow=T)
  matC <- matrix(c(0,1/V1i,0),nrow=1)
  list(matA=matA,matC=matC)
}

##Initial Conditions. These are specified as a function with arguments Time, phi and U.
##The Time argument is the first point specified and can be useful if subjects start at
##different time points. The U can contain exogenous input to hte system which enters into
##the initial conditions.
Model.levodopa$X0 = function(Time=Na,phi,U=Na) {
  matrix(0,nrow=3)
}

##Diffusion scaling term as a function of phi.
Model.levodopa$SIG = function(phi) {
  sig1 <- phi[["sig1"]]
  
  matrix(c( 0,0,0,
            0,sig1,0,
            0,0,0), nrow=3, byrow=T)
}

##Residual variance as a function of phi.
##Specified as a matrix even though it is one-dimensional.
Model.levodopa$S = function(phi) {
  matrix(phi[["S"]])
}

##The h function translates the population parameters into individual parameters.
##Function arguments that can be used in the creation of phi are population parameters,
##random effects, and covariates.
Model.levodopa$h = function(eta,theta,covar) {
  phi <- theta
  phi$kai <- theta$ka*exp(eta[1])
  phi$V1i <- theta$V1*exp(eta[2])*covar[1]
  phi$V2i <- theta$V2*exp(eta[3])*covar[1]
  phi$CLi <- theta$CL*exp(eta[4])*((covar[1])^0.75)
  phi$bioi <- theta$bio*exp(eta[5])
  phi$Qi <- theta$Q*exp(eta[6])*((covar[1])^0.75)
  phi
}

##Function that splits the parameter vector contianing parameters to be optimized into 
##Population parameters and inter individual covariance matrix OMEGA.
Model.levodopa$ModelPar = function(THETA){
  #bio <- 0.879
  ka <- 28.5
  V1 <- 11
  V2 <- 27
  CL <- 0.52
  Q <- 0.58
  OMEGA1 <- 0.42
  OMEGA2 <- 0.1936
  OMEGA3 <- 0.0625
  OMEGA4 <- 0.0729
  OMEGA6 <- 0.2304
  OMEGA5 <- 0.000001
  S <- 1
  sig1 = sigvalue
  list(theta=list(V1 = V1, V2 = V2, ka=ka, CL = CL, Q = Q,  
                  bio=THETA['bio'],  
                  sig1 = sig1,
                  S = S), 
       OMEGA=diag(c(OMEGA1=OMEGA1,OMEGA2=OMEGA2,OMEGA3=OMEGA3,
                    OMEGA4=OMEGA4,OMEGA5=OMEGA5,OMEGA6=OMEGA6
       )))
}

##Initial values of the parameters.
levodopa.THETA <- c(bio = 0.75)

##Set upper and lower bound of the initial parameter guesses.
par <- list(LB = c(bio = 0.1), 
            Init = levodopa.THETA, 
            UB = c(bio = 1))

## Fit the model using PSM.estimate.
fit_1a_0p0 <- PSM.estimate(Model.levodopa,datasetX,par,CI=T,fast=TRUE,trace=1)

## Store whether the fit converged or not.
if (fit_1a_0p0$opt$convergence == 0) {
  conv_list_0p0 <- c(conv_list_0p0, "fit_1a_0p0", 0)
}

##Smoothed output with PSM.smooth on the trainin dataset, using the parameters from the model.
##This can be used for plots and to extract smoothed/predicted/filtered estimates of the states.
out_1a_0p0 <- PSM.smooth(Model.levodopa, datasetX, fit_1a_0p0$THETA)

##Extract the eta values for the patient that was left out.
TableEta <- vector(mode="list",length=18)
for (i in 1:18) {
  TableEta[[i]]$eta <- out_1a_0p0[[i]]$eta[]
}
TableEta <- unlist(TableEta[[eta_number]])

##Create a matrix for the extracted eta values, as required by PSM for PSM.Smooth function.
TableEta <- matrix(TableEta,nrow=6,ncol=1,byrow=F)

##Smoothed estimate using the model parameters used above on the patient(s) left out.
out_test <- PSM.smooth(Model.levodopa, datasetT, fit_1a_0p0$THETA, etaList = TableEta)

##Extract predicted values from smoothed output.
TableOut <- vector(mode="list",length=1)
for (i in 1:1) {
  TableOut[[i]]$Yp <- out_test[[i]]$Yp[1,]
  TableOut[[i]]$Time <- out_test[[i]]$Time[]
}
dataset_ex <- rbindlist(TableOut)
##Remove NA values.
dataset_ex <- dataset_ex[complete.cases(dataset_ex), ]

##Extract observed values from actual dataset.
TableOut <- vector(mode="list",length=1)
for (i in 1:1) {
  TableOut[[i]]$Y <- datasetT[[i]]$Y1[1,]
  TableOut[[i]]$Time <- datasetT[[i]]$Time1[]
}
dataset_ex2 <- rbindlist(TableOut)
##Remove NA values.
dataset_ex2 <- dataset_ex2[complete.cases(dataset_ex2), ]

##Calculate Residuals
datasetres <- c()
datasetres$Time <- as.numeric(dataset_ex$Time)
datasetres$Residuals <- as.numeric(dataset_ex2$Y-dataset_ex$Yp)
d1 <- do.call(cbind.data.frame, datasetres)

##Calculate RMSE.
RMSE_1a_0p0 <- sqrt(sum((d1$Residuals)^2)/length(d1$Residuals))

##Save Fit Results
sink("~/Reading/Paper 1/3 EQ No Sig/NoSig_1a_0p0.txt")
print(fit)
sink()

pdf('~/Reading/Paper 1/3 EQ No Sig/NoSig_1a_1_0p0.pdf')
##Plot the Residuals.
plot(d1$Time,d1$Residuals, ylab = "Residuals", xlab = "Time", xlim = c(0,1200), ylim = c(-2,2),
     main = "Patient 1011")
abline(h=0)
dev.off()

##Save Plot to visually check the residuals with PSM.plot function to compare.
pdf('~/Reading/Paper 1/3 EQ No Sig/NoSig_Fit_1a_0p0.pdf')
PSM.plot(datasetT, Smooth=out_test, type = c("Ys.Y","Xs","res"))
dev.off()