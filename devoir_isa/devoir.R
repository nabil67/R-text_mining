# question 1  ---------------------------------------------
setwd("/home/nabil/Bureau/devoir_isabelle")
data1<-read.table("/home/nabil/Bureau/devoir_isabelle/dataJuin2015.txt", header=T)
head(data)

# question 2 --------------------------------------------------------------
data2=data[,c("sexe","alcoolWeek")]
mat=table(data1$sexe,data1$alcoolWeek)
par(mfrow = c(1,2))
pie(mat[1,])
title("graphe 1: consomation d'alcool chez les filles ", cex.main = 0.6)
pie(mat[2,])
title("graphe 1: consomation d'alcool chez les garçons ", cex.main = 0.6)
chisq.test(mat)
write.table(mat, file = "file3.csv", append = FALSE, quote = FALSE, sep = ",", na = "-", row.names = F)
# question 3 --------------------------------------------------------------

data3=data[,c("age","alcoolWeek")]
data3$age=with(data3,ifelse(data3$age > 16,2,1))
mat3=table(data3$age,data3$alcoolWeek)
par(mfrow = c(1,2))
pie(mat3[1,])
title("graphe 1: consomation d'alcool pour la classe 1 ", cex.main = 0.6)
pie(mat3[2,])
title("graphe 1: consomation d'alcool pour la classe 2 ", cex.main = 0.6)
chisq.test(mat3)

# question 4 -------------------------------------------------------------- 

dataF=data[data$sexe == "F",]
dataM=data[data$sexe == "M",]

dataM$age=with(dataM,ifelse(dataM$age > 16,2,1))
dataF$age=with(dataF,ifelse(dataF$age > 16,2,1))

mat4M=table(dataM$age,dataM$alcoolWeek)
par(mfrow = c(1,2))
pie(mat4M[1,])
title(" consomation d'alcool pour la classe 1 des garçons ", cex.main = 0.6)
pie(mat4M[2,])
title(" consomation d'alcool pour la classe 2 des garçons ", cex.main = 0.6)
chisq.test(mat4M)

# les filles
mat4F=table(dataF$age,dataF$alcoolWeek)
par(mfrow = c(1,2))
pie(mat4F[1,])
title(" consomation d'alcool pour la classe 1 des filles ", cex.main = 0.6)
pie(mat4F[2,])
title(" consomation d'alcool pour la classe 2 des filles ", cex.main = 0.6)


chisq.test(mat4F)
pie(mat[1,],clockwise=T)


# question 5 -------------------------------------------------------------- 

library(lattice)

dataF=data[data$sexe == "F",]
dataM=data[data$sexe == "M",]

dataF5=dataM[,c("alcoolWeek","noteTrim3")]
dataM5=dataF[,c("alcoolWeek","noteTrim3")]

mat5M=table(dataM5$alcoolWeek, dataM5$noteTrim3)
xyplot(dataM5$noteTrim3 ~ dataM5$alcoolWeek , auto.key = TRUE, 
title=" notes du trimestre 3 en fonction de alcolWeek chez les garçons, cex.main = 0.6")
# les filles
mat5F=table(dataF5$alcoolWeek, dataF5$noteTrim3)
library(lattice)
xyplot(dataF5$noteTrim3 ~ dataF5$alcoolWeek , auto.key = TRUE)

chisq.test(mat5F)
chisq.test(mat5M)
write.table(mat5F, file = "file5_2.csv", append = FALSE, quote = FALSE, sep = ",", na = "-", row.names = F)
 