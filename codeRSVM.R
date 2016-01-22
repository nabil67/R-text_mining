
# Installing packages -----------------------------------------------------


library(ipred)
library(SparseM)
library(RTextTools)
library(e1071)

   
# Importing data ----------------------------------------------------------

setwd("/home/bnabil/Bureau/test")

data1=read.csv("train3.csv",header=T
               ,sep=","
               ,stringsAsFactors = F
               ,encoding="UTF-8")

data2=read.csv("data_test.csv",header=T
               ,sep=","
               ,stringsAsFactors = F
               ,encoding="UTF-8")
data2$y=0
data=rbind(data1,data2)


# supervised Classsification  ---------------------------------------------
doc_matrix=create_matrix(data$desc, language="french", removeNumbers=TRUE,
                         stemWords=TRUE, removeSparseTerms=0)
container=create_container(doc_matrix, data$y, trainSize=1:11600,
                              testSize=11601:12000, virgin=FALSE)

models=train_models(container, algorithms=c("MAXENT","SVM","RF"))
results=classify_models(container, models)

# testing training  "dim(data1)=12000"

yobs=data1[11601:12000,]$y
svmpredict=cbind(as.numeric(as.character(results$SVM_LABEL)))
MAXENTpredict=cbind(as.numeric(as.character(results$MAXENTROPY_LABEL)))
RBpredict=cbind(as.numeric(as.character(results$RBTROPY_LABEL)))

test=data.frame(cbind(yobs,svmpredict,MAXENTpredict))
colnames(test)=c("obs","svm","maxent")
test$labSVM=with(test,ifelse(test$svm==test$obs,0,1))
test$labMAXENT=with(test,ifelse(test$maxent==test$obs,0,1))

# Error%

print(test)
apply(test,2,mean)[4:5]*100



data2[208,]$title






vtest=cbind(as.numeric(as.character(container@testing_codes)))
vtrain=cbind(data1$y[9001:12000])
mat=cbind(vtest,vtrain)
err=table(mat[,1]%in%mat[,2])


