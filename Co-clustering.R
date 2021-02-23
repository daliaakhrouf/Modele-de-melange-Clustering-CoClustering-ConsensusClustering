library(R.matlab)
library(mclust)
library(Rmixmod)
library(Rcpp)
library(aricode)

#################chargement des donnÃ©es #########################

jaffe=readMat("C:/Users/Ellite Informatique/Desktop/Nadif_or/Data-partie-1/DATA_MATLAB - Projet-master-MLDS/jaffe.mat")

optdegit= readMat("C:/Users/Ellite Informatique/Desktop/Nadif_or/Data-partie-2/Optdigits.mat")





####################Qustion 2 ##############################

mclust_jaffe_debut=Mclust(jaffe$X)


plot(mclust_jaffe_debut)
#nombre de cluster =9 BIC= -1238334 VEI (diagonal, equal shape) and EEI     
summary(mclust_jaffe)

Z=matrix(0,nrow = 213, ncol = 6)
mdn=c("EII","VII","EEI","VEI","EVI","VVI")

j=1
for(i in c(1:6)){
  
  
  mclust_jaffe=Mclust(jaffe$X,modelNames = mdn[j])
  Z[,i]=mclust_jaffe$classification
  j=j+1
  
}


## NMI=0.8891035 de mclust avec la realité
NMI(mclust_jaffe_debut$classification,as.vector(jaffe$y))
NMI(mclust_jaffe$classification,as.vector(jaffe$y))
# ARI=0.8094428 de mclust avec la realité
ARI(mclust_jaffe_debut$classification,as.vector(jaffe$y))
ARI(mclust_jaffe$classification,as.vector(jaffe$y))
#avec le model EII= 0.8989303 ************************************************
NMI(Z[,1],as.vector(jaffe$y))
#???avec le model VII = 0.88206
NMI(Z[,2],as.vector(jaffe$y))
#???avec le model EEI = 0.8671167
NMI(Z[,3],as.vector(jaffe$y))
#???avec le model VEI = 0.8891035 *************************************
NMI(Z[,4],as.vector(jaffe$y))
#???avec le model EVI = 0.6460854
NMI(Z[,5],as.vector(jaffe$y))
#???avec le model VVI = 0.6460854
NMI(Z[,6],as.vector(jaffe$y))
#


ari1=ARI(Z[,1],as.vector(jaffe$y))
ari2=ARI(Z[,2],as.vector(jaffe$y))
ari3=ARI(Z[,3],as.vector(jaffe$y))
ari4=ARI(Z[,4],as.vector(jaffe$y))
ari5=ARI(Z[,5],as.vector(jaffe$y))
ari6=ARI(Z[,6],as.vector(jaffe$y))


######################### Qustion3 #############################
##on obtient la meilleure repartition on applique Rmixmod 

consensus_jaffe=mixmodCluster(as.data.frame(type.convert(Z)),9,dataType="qualitative",models=mixmodMultinomialModel(),criterion=c("BIC", "ICL"))
consensus_jaffe@bestResult@partition

##### NMI=0.8891035 le consensus avec la realité
NMI(consensus_jaffe@bestResult@partition,as.vector(jaffe$y))
ARI(consensus_jaffe@bestResult@partition,as.vector(jaffe$y))
# reponse a la question 3: la NMI correspond aux meilleurs modeles qui sont: "VEI","EII" 

####################### question 4 ###############################???

# on prend seulement VVI et EVI VEI#####
### NMI= 0.8671167 ##### 
model= c("VEI","EII")
matrice=matrix(0,nrow = 213, ncol = 2)
j=1
for(i in c(1:2)){
  
  
  mclust_jaffe1=Mclust(jaffe$X,modelNames = model)
  matrice[,i]=mclust_jaffe1$classification
  j=j+1
  
}

consensus_jaffe1=mixmodCluster(as.data.frame(type.convert(matrice)),9,dataType="qualitative",models=mixmodMultinomialModel(),criterion=c("BIC", "ICL"))
NMI(consensus_jaffe1@bestResult@partition,as.vector(jaffe$y))
ARI(consensus_jaffe1@bestResult@partition,as.vector(jaffe$y))

#on applique kmeans sur la matrice Z1 nmi= 0.8891035
#ajouter les résultat de kmeans

km= kmeans(matrice, centers=9, iter.max = 10, nstart = 1)
NMI(km$cluster,as.vector(jaffe$y))
ARI(km$cluster,as.vector(jaffe$y))
matrice[,3]=as.vector(km$cluster)

consensus_jaffe2=mixmodCluster(as.data.frame(type.convert(matrice)),9,dataType="qualitative",models=mixmodMultinomialModel(),criterion=c("BIC", "ICL"))
h2=consensus_jaffe2@bestResult@partition

NMI(h2,as.vector(jaffe$y))
ARI(h2,as.vector(jaffe$y))


###### Deuxieme jeu de données : optdegit  ###########

mclust_optdegit=Mclust(optdegit$X)
mclust_optdegit$classification
plot(mclust_optdegit)
#nombre de cluster = 9 VII (spherical, varying volume) and EII
summary(mclust_optdegit)


Z1=matrix(0,nrow = 5620, ncol = 2)
mdn1=c("VII","EII")
j=1
for(i in c(1:2)){
  
  
  mclust_optdegit=Mclust(optdegit$X,modelNames = mdn1[j])
  Z1[,i]=mclust_optdegit$classification
  j=j+1
  
}


## NMI=0.69 de mclust avec la realité ARI=0.59

NMI(mclust_optdegit$classification,as.vector(optdegit$y))
ARI(mclust_optdegit$classification,as.vector(optdegit$y))

#avec le model VII= 0.7067953 ************************************************
NMI(Z1[,1],as.vector(optdegit$y))
#???avec le model EII = 0.6896295
NMI(Z1[,2],as.vector(optdegit$y))

#c("VII","EII")
# 0.62  0.59
ari8=ARI(Z1[,1],as.vector(optdegit$y))
ari9=ARI(Z1[,2],as.vector(optdegit$y))



consensus_optdegit=mixmodCluster(as.data.frame(type.convert(Z1)),9,dataType="qualitative",models= mixmodMultinomialModel() ,criterion=c("BIC", "ICL"))

h2=consensus_optdegit@bestResult@partition


##### NMI=0.67le consensus avec la realité
NMI(consensus_optdegit@bestResult@partition,as.vector(optdegit$y))
ARI(consensus_optdegit@bestResult@partition,as.vector(optdegit$y))

##avec kmeans
km= kmeans(Z1, centers=9, iter.max = 10, nstart = 1)
NMI(km$cluster,as.vector(optdegit$y))
ARI(km$cluster,as.vector(optdegit$y))
