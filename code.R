setwd("/Users/angel/OneDrive - University of Edinburgh/Semestre 1/GRM/GRM_assigment1/")
blood <- read.table('datasets/Bloodpressure.txt',head=T)

par(mfrow=c(1,2))
hist(blood$recovery_time,  xlab = "recovery time", main = "Histogram of recovery time")
hist(blood$log_drug, xlab = "log_drug", main = "Histogram of log_drug")
hist(blood$blood_pressure, xlab = "blood pressure", main = "Histogram of blood pressure")

par(mfrow=c(1,2))
plot(blood$recovery_time, blood$log_drug, xlab="log_drug", ylab = "recovery time")
title("recovery time vs log drug")
plot(blood$recovery_time, blood$blood_pressure,  xlab="blood pressure", ylab = "recovery time")
title("blood pressure vs log drug")


par(mfrow=c(1,2))
plot(log(blood$recovery_time),exp(blood$log_drug),xlab="drug", ylab = "log(recovery time)")
title("log(recovery time) vs drug")
plot(log(blood$recovery_time), blood$blood_pressure,  xlab="blood pressure", ylab = "log(recovery time)")
title("log(recovery time) vs blood pressure")


#Simple linear regession model 
Model1 <- lm(formula = log(recovery_time) ~ exp(log_drug) + blood_pressure, data =blood)
summary(Model1)


par(mfrow = c(2, 2))
plot(Model1)

plot(Model1)

par(mfrow = c(1, 1))
hist(Model1$res,xlab="residuals",  main = "Histogram of the residuals", )


new.data = data.frame(log_drug=2, blood_pressure=75 )

interval <- predict(Model1, newdata = new.data, interval = "confidence", level = 0.95)
interval

exp(interval)


