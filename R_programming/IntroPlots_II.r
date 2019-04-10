library(dslabs)
data(murders)
setwd("C:/Users/fscabo/Desktop/MasterDataScience_KSchool/Ejercicios")
babies=read.delim("babies.txt",header=T,sep="\t",stringsAsFactors = F)

mean(babies$bwt)
sqrt(var(babies$bwt))

hist(murders$total)
mean(murders$total)
sqrt(var(murders$total))

median(murders$total)
IQR(murders$total)

summary(murders$total)
summary(babies$bwt)

summary(murders$total)
q1=quantile(murders$total, p=0.25)
q1
q3=quantile(murders$total, p=0.75)
q3
iqr=(q3-q1)
iqr
r <- c(q1 - 1.5*iqr, q3 + 1.5*iqr)
r

which(murders$total<=r[1])
which(murders$total>=r[2])
murders[which(murders$total>=r[2]),]

r2 <- c(q1 - 3*iqr, q3 + 3*iqr)
r2
which(murders$total>=r2[2])
murders[which(murders$total>=r2[2]),]

qqnorm(murders$total)
qqline(murders$total)

qqnorm(babies$bwt)
qqline(babies$bwt)

mad(babies$bwt)
mad(murders$total)