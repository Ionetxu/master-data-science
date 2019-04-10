#cambia al directorio en el que tengas el archivo con los datos de "babies.txt"
setwd("C:/Users/fscabo/Desktop/MasterDataScience_KSchool/Ejercicios")
babies=read.delim("babies.txt",header=T,sep="\t",stringsAsFactors = F)
plot(babies$bwt,babies$gestation)

#necesitamos poner a NA los missing (999)
babies$gestation[which(babies$gestation=="999")]=NA
plot(babies$bwt,babies$gestation)

boxplot(babies$bwt~babies$smoke)

hist(babies$bwt)

