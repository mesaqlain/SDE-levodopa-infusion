## Written by Murshid Saqlain (msq@du.se)
## Code to accompany the paper 
## Investigating Stochastic Differential Equations Modelling for 
## Levodopa Infusion in Patients with Parkinson’s Disease
## This script that creates the plots of the residuals seen in the manuscript


library(data.table)
library(ggplot2)
library(PSM)


out_all_resid_sig <- PSM.smooth(Model.levodopa, datasetall,fit_all_resid_sig$THETA)


##Extract smoothed values from sigma estimaged case.
TableOut <- vector(mode="list",length=20)
for (i in 1:20) {
  TableOut[[i]]$Ys <- out_all_resid_sig[[i]]$Ys[1,]
  TableOut[[i]]$Time <- out_all_resid_sig[[i]]$Time[]
  TableOut[[i]]$Patient <- rep(i,length(out_all_resid_sig[[i]]$Ys[1,]))
}
dataset_ex <- rbindlist(TableOut)
##Remove NA values.
dataset_ex <- dataset_ex[complete.cases(dataset_ex), ]

##Extract observed values from actual dataset.
TableOut <- vector(mode="list",length=20)
for (i in 1:20) {
  TableOut[[i]]$Y <- datasetall[[i]]$Y1[1,]
  TableOut[[i]]$Time <- datasetall[[i]]$Time1[]
  TableOut[[i]]$Patient <- rep(i,length(datasetall[[i]]$Y1[1,]))
}
dataset_ex2 <- rbindlist(TableOut)
##Remove NA values.
dataset_ex2 <- dataset_ex2[complete.cases(dataset_ex2), ]

##Merge the datasets so only the smoothed values where there was an observation remains.
dataset_Ys_resids <-merge(x=dataset_ex2,y=dataset_ex,by.x=c("Patient","Time"),by.y=c("Patient","Time"),all.x=T,all.y=F)  

dataset_Ys_resids$Residuals <- as.numeric(dataset_Ys_resids$Y-dataset_Ys_resids$Ys)


dataset_Ys_resids <- melt(dataset_Ys_resids, id = c("Time","Patient"), measure = c("Residuals"))

patient_names <- c("1011","1012","1021","1022","1031","2011","2012","2013",
                   "2021","2022","2023","2031","2032","2033","2041","2042","2043",
                   "2051","2052","2053")
levels(dataset_Ys_resids$Patient)
class(dataset_Ys_resids$Patient)
dataset_Ys_resids$Patient=as.factor(dataset_Ys_resids$Patient)
levels(dataset_Ys_resids$Patient) <- patient_names

g1_2 <- ggplot(dataset_Ys_resids, aes(Time, value)) +
  geom_point(shape=5) + geom_smooth(method="lm") + # , formula = y ~ x ## method="loess" for smoothed regressoin
  scale_x_continuous(name="TIME (minutes after midnight)") +
  scale_y_continuous(name="Residual") +
  theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"),
        legend.title=element_blank())

#print(g1_2)

g2_2 <- g1_2 + facet_wrap( ~ Patient, ncol=5)  
print(g2_2)


## Absolute value
dataset_Ys_resids$value_abs <- abs(dataset_Ys_resids$value)

g_abs <- ggplot(dataset_Ys_resids, aes(Time, value_abs)) +
  geom_point(shape=5) + geom_smooth(method="lm") + # , formula = y ~ x ## method="loess" for smoothed regressoin
  scale_x_continuous(name="TIME (minutes after midnight)") +
  scale_y_continuous(name="Residual") +
  theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"),
        legend.title=element_blank())

g_abs2 <- g_abs + facet_wrap( ~ Patient, ncol=5)  
print(g_abs2)


## QQ Plots of ALL residuals
qqnorm(dataset_Ys_resids$value, pch = 1, frame = FALSE)
qqline(dataset_Ys_resids$value, col = "steelblue", lwd = 2)

## QQ Plots of residuals of idividuals
g_QQ <- ggplot(dataset_Ys_resids, aes(sample = value)) +
  stat_qq() + stat_qq_line() +
  scale_x_continuous(name="Theoretical Quantiles") +
  scale_y_continuous(name="Sample Quantiles") +
  theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"),
        legend.title=element_blank())

#print(g1_2)

g_QQ2 <- g_QQ + facet_wrap( ~ Patient, ncol=5)  
print(g_QQ2)
