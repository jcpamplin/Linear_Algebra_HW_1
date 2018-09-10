library(caret)
# COMPUTE THE PCA
# by default covariance version. Scale=T will give the correlation matrix
sapply(leuk, class)!="integer"


write.csv(leuk, file="C:\\Users\\Debosmita\\Documents\\LA-HW\\temp.csv")

temp = leuk

foo <- function(leuk) {
  out <- lapply(leuk, function(x) length(unique(x)))
  want <- which(!out > 1)
  unlist(want)
}

temp = leuk[,-foo(leuk)]

pca = prcomp(temp[,1:4812],scale=T)
biplot(pca)

temp_allb = temp[which(temp$V5001 == 'ALL-B'),]
pca_allb = prcomp(temp_allb[,1:4812], 3,scale = F,rank. = 3)
biplot(pca_allb, main = "ALL-B")

temp_aml = temp[which(temp$V5001 == 'AML'),]
pca_aml = prcomp(temp_aml[,1:4812], 3,scale = F,rank. = 3)
biplot(pca_aml, main="AML")

temp_allt = temp[which(temp$V5001 == 'ALL-T'),]
pca_allt = prcomp(temp_allt[,1:4812], 3,scale = F,rank. = 3)
biplot(pca_allt, main="ALL-T")

pca = prcomp(temp[,1:4812], 3,scale = F,rank. = 3)

pca$sdev[1]^2/(sum(pca$sdev^2))