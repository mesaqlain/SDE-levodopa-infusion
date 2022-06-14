## Written by Murshid Saqlain (msq@du.se)
## Code to accompany the paper 
## Investigating Stochastic Differential Equations Modelling for 
## Levodopa Infusion in Patients with Parkinson’s Disease
## This script creates the plots seen in the manuscript

library(data.table)
library(ggplot2)
##Extract smoothed values from sigma = 0 case.
TableOut <- vector(mode="list",length=20)
for (i in 1:20) {
  TableOut[[i]]$Ys <- out_all_0p0[[i]]$Ys[1,]
  TableOut[[i]]$Time <- out_all_0p0[[i]]$Time[]
  TableOut[[i]]$Patient <- rep(i,length(out_all_0p0[[i]]$Ys[1,]))
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

##Extract smoothed values from estimated sigma case.
TableOut <- vector(mode="list",length=20)
for (i in 1:20) {
  TableOut[[i]]$Ys_est <- out_all_est[[i]]$Ys[1,]
  TableOut[[i]]$Time <- out_all_est[[i]]$Time[]
  TableOut[[i]]$Patient <- rep(i,length(out_all_est[[i]]$Ys[1,]))
}
dataset_ex3 <- rbindlist(TableOut)
##Remove NA values.
dataset_ex3 <- dataset_ex3[complete.cases(dataset_ex3), ]


dataset_Ys_paper_2 <-merge(x=dataset_ex2,y=dataset_ex,by.x=c("Patient","Time"),by.y=c("Patient","Time"),all.x=T,all.y=F)  
dataset_Ys_paper_2 <-merge(x=dataset_Ys_paper_2,y=dataset_ex3,by.x=c("Patient","Time"),by.y=c("Patient","Time"),all.x=T,all.y=F)  

dataset_Ys_2 <- melt(dataset_Ys_paper_2, id = c("Time","Patient"), measure = c("Y", "Ys", "Ys_est"))

patient_names <- c("1011","1012","1021","1022","1031","2011","2012","2013",
                   "2021","2022","2023","2031","2032","2033","2041","2042","2043",
                   "2051","2052","2053")
levels(dataset_Ys_2$Patient)
class(dataset_Ys_2$Patient)
dataset_Ys_2$Patient=as.factor(dataset_Ys_2$Patient)
levels(dataset_Ys_2$Patient) <- patient_names

g1_2 <- ggplot(dataset_Ys_2, aes(Time, value, colour = variable, shape = variable)) +
  geom_line(aes(linetype=variable)) + geom_point() + 
  scale_size_manual(values=c(1,1,1)) +
  scale_shape_manual(values=c(5, 16, 3),labels=c("Actual", "ODE Model", "SDE Model")) +
  scale_color_manual(values=c("black","red","blue"),labels=c("Actual", "ODE Model", "SDE Model")) +
  scale_linetype_manual(values=c("blank", "solid", "dashed"),labels=c("Actual", "ODE Model", "SDE Model")) +
  scale_x_continuous(name="TIME (minutes after midnight)") +
  scale_y_continuous(name="Plasma concentration levodopa (mg/L)") +
  theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"),
        legend.title=element_blank())

#print(g1_2)

g2_2 <- g1_2 + facet_wrap( ~ Patient, ncol=5)  
print(g2_2)

