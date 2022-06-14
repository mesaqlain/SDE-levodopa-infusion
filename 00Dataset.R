## Written by Murshid Saqlain (msq@du.se)
## Code to accompany the paper 
## Investigating Stochastic Differential Equations Modelling for 
## Levodopa Infusion in Patients with Parkinson’s Disease
## This script cleans and prepares the dataset (not included in the repository)

## read the dataset
dataset <- read.table("//dustaff/home/msq/Jerker_paper_6/comb13.txt", sep="\t", header = TRUE)
dataset <- read.table("~/R/comb13.txt", sep="\t", header = TRUE)


## set NA values to 0
dataset$AMT[is.na(dataset$AMT)] <- 0
dataset$RATE[is.na(dataset$RATE)] <- 0

## subset the dataset to only flags 0 and 1.
## Flag 0 is for dose.
## Flag 1 is for concentration.
## Flag 2 is for effect, which is not used here.
dataset <- subset(dataset,
                     FLAG %in% c("0","1"))

## Ignore IGPK == 1 (going by the NONMEM code)
dataset <- dataset[-c(which(dataset$IGPK == 1)),]
                  
## organize the dataset to get the correct values at each column
for (i in 1:length(dataset$FLAG)) {
  
  if (dataset$FLAG[i] == '0'){
    dataset$dose[i] <- dataset$AMT[i]
    dataset$rate[i] <- dataset$RATE[i]
    dataset$concentration[i] <- NA
    dataset$weight[i] <- dataset$WT[i]
      
  }
      
  else {
    dataset$dose[i] <- 0
    dataset$rate[i] <- 0
    dataset$concentration[i] <- dataset$DV[i]
    dataset$weight[i] <- dataset$WT[i]
    
  }
  
}
## Mark missing value as NA, as required by PSM.

## 


## Create list as required by PSM.
subjectID<-unique(dataset$NID)
N <- length(subjectID)
dataset3<-vector(mode="list",length=N)
  for(i in 1:N){
    ## sample times for each subject, incremented by the minute from first sample time to last
    dataset3[[i]]$Time1 = dataset$TIME[dataset$NID==subjectID[i]] 
    
    ##observation matrix
    dataset3[[i]]$Y1 = matrix(dataset$concentration[dataset$NID==subjectID[i]],nrow=1,byrow=T)
    
    ## Weight
    dataset3[[i]]$weight = dataset$weight[dataset$NID==subjectID[i]]/70
    
    dose1 <- dataset$AMT[dataset$NID==subjectID[i]] ## takes doses for each subject
    rate <- dataset$RATE[dataset$NID==subjectID[i]] ## take the rate for each subject
    dv1 <- dataset$concentration[dataset$NID==subjectID[i]] ## take the concentration for each subject
    doseT <- which(dose1>0) ##no. of rows where dose1 is non-zero
    dvT <- which(dv1 > 0) ##no. of rorws where dv1 is non-zero
    TimeI <- dataset$TIME[dataset$NID==subjectID[i]] ##take the sample times for each subject
    TimeD <- TimeI[doseT] ##take the time where the dose is non-zero
    InfTime <- min(TimeI):max(TimeI) ##range of time for each subject
    Infusion <- rep(0,length(InfTime)) ##Create values of infusion, starting at 0.

    for(k in 1:length(doseT)){
      rinf <- which(InfTime==TimeD[k]) ##When dose is non-zero
      if(rate[doseT[k]]==0){ ##When rate is 0, it's bolus dose.
        Infusion[rinf] <- Infusion[rinf]+dose1[doseT[k]] } 
      else{
        lngt <- floor(dose1[doseT[k]])/rate[doseT[k]] ##Define duration of infusion for non-zero rate
        lngt <- lngt + rinf - 1 ## Define how long the infusion will keep going from start of infusion
        alng <- min(lngt,length(InfTime)) ## Find the max time infusion will go up to
        Infusion[rinf:alng] <- Infusion[rinf:alng] + rate[doseT[k]] ## Dose in incremented each min
      if(lngt<length(InfTime)) {
        Infusion[(alng+1)] <- Infusion[(alng+1)]+dose1[doseT[k]]%%rate[doseT[k]]
        }
      }  
    }
    Infgt0 <- which(Infusion>0) ## Times when infusion is non-zero
  
    ## Create list for Dose.
    dataset3[[i]]$Dose = list(Time=InfTime[Infgt0],State=rep(1,length(Infgt0)),
                             Amount=Infusion[Infgt0]) 
    
    dataset3[[i]]$Time =sort(unique(c((dataset3[[i]]$Time1),(dataset3[[i]]$Dose$Time))))
    TimeL <- length(dataset3[[i]]$Time)
    obs <- rep(NA,TimeL)
    for (z in 1:TimeL) {
      
      obs[which(dataset3[[i]]$Time1[z] == dataset3[[i]]$Time)] <- dataset3[[i]]$Y[z]
    }
    
    dataset3[[i]]$Y = matrix(obs,nrow=1,byrow=T)
    dataset3[[i]]$covar = c(dataset3[[i]]$weight[1])  
      
}


##CREATE THE DATASETS FOR TRAINING AND TEST BELOW:


##Leave no patient out, used to estimate population parameters that are fixed in the  
##training sets.
datasetall <- dataset3[36:55]
View(datasetall)

##PATIENT 101##
dataset1a <- dataset3[37:55]             ##Leave patient 1011 out.
dataset1atest <- dataset3[36]            ##Test dataset for patient 1011.

dataset1b <- dataset3[c(36,38:55)]       ##Leave patient 1012 out.
dataset1btest <- dataset3[37]            ##Test dataset for patient 1012.
###############

##PATIENT 102
dataset2a <- dataset3[c(36:37,39:55)]    ##Leave patient 1021 out.
dataset2atest <- dataset3[38]            ##Test dataset for patient 1021.

dataset2b <- dataset3[c(36:38,40:55)]    ##Leave patient 1022 out.
dataset2btest <- dataset3[39]            ##Test dataset for patient 1022.
###############

##PATIENT 201##
dataset4a <- dataset3[c(36:41,44:55)]    ##Leave patients 2012 and 2013 out.
dataset4b <- dataset3[c(36:40,42,44:55)] ##Leave patients 2011 and 2013 out.
dataset4c <- dataset3[c(36:40,43:55)]    ##Leave patients 2011 and 2012 out.

dataset4atest1 <- dataset3[41]           ##Test dataset for patient 2011.
dataset4atest2 <- dataset3[42]           ##Test dataset for patient 2012.
dataset4atest3 <- dataset3[43]           ##Test dataset for patient 2013.
###############

##PATIENT 202##
dataset5a <- dataset3[c(36:44,47:55)]    ##Leave patients 2022 and 2023 out.
dataset5b <- dataset3[c(36:43,45,47:55)] ##Leave patients 2021 and 2023 out.
dataset5c <- dataset3[c(36:43,46:55)]    ##Leave patients 2021 and 2022 out.

dataset5test1 <- dataset3[44]            ##Test dataset for patient 2021.
dataset5test2 <- dataset3[45]            ##Test dataset for patient 2022.
dataset5test3 <- dataset3[46]            ##Test dataset for patient 2023.
###############

##PATIENT 203##
dataset6a <- dataset3[c(36:47,50:55)]    ##Leave patients 2032 and 2033 out.
dataset6b <- dataset3[c(36:46,48,50:55)] ##Leave patients 2031 and 2033 out.
dataset6c <- dataset3[c(36:46,49:55)]    ##Leave patients 2031 and 2032 out.

dataset6test1 <- dataset3[47]            ##Test dataset for patient 2031.
dataset6test2 <- dataset3[48]            ##Test dataset for patient 2032.
dataset6test3 <- dataset3[49]            ##Test dataset for patient 2033.
###############

##PATIENT 204##
dataset7a <- dataset3[c(36:50,53:55)]    ##Leave patients 2042 and 2043 out.
dataset7b <- dataset3[c(36:49,51,53:55)] ##Leave patients 2041 and 2043 out.
dataset7c <- dataset3[c(36:49,52:55)]    ##Leave patients 2041 and 2042 out.

dataset7test1 <- dataset3[50]            ##Test dataset for patient 2041.
dataset7test2 <- dataset3[51]            ##Test dataset for patient 2042.
dataset7test3 <- dataset3[52]            ##Test dataset for patient 2043.
###############

##PATIENT 205##
dataset8a <- dataset3[36:53]             ##Leave patients 2052 and 2053 out.
dataset8b <- dataset3[c(36:52,54)]       ##Leave patients 2051 and 2053 out.
dataset8c <- dataset3[c(36:52,55)]       ##Leave patients 2051 and 2052 out.

dataset8test1 <- dataset3[53]            ##Test dataset for patient 2051.
dataset8test2 <- dataset3[54]            ##Test dataset for patient 2052.
dataset8test3 <- dataset3[55]            ##Test dataset for patient 2053.
###############


### FOR SIMULATED DATA ###

N = 20 #20 patients
Sim_Data <- vector(mode="list",length=N)

for (i in 1:N) {
  Sim_Data[[i]]$Dose$Time <- datasetall[[i]]$Dose$Time
  Sim_Data[[i]]$Dose$Amount <- datasetall[[i]]$Dose$Amount
  Sim_Data[[i]]$Dose$State <- datasetall[[i]]$Dose$State
  Sim_Data[[i]]$weight <- datasetall[[i]]$weight
  Sim_Data[[i]]$Time <- datasetall[[i]]$Time
  Sim_Data[[i]]$covar <- datasetall[[i]]$covar
}
View(Sim_Data)
