install.packages("robustbase")
install.packages("DetMCD")
install.packages("rrcov")
install.packages("robust")
install.packages("ggplot2")
install.packages("colorspace")
install.packages("ellipsis")
install.packages("matlib")
install.packages("mixtools")
install.packages("ellipse")
install.packages("mvoutlier")
library(ggplot2)
library(ellipse)
library(robustbase)
library(mvoutlier)
library(rrcov)

tab <- data.frame(read.csv(file.choose())) #cortex
m4prott <- tab[,c(2:41,82)]
m4prot <- na.omit(m4prott)
m4prot

c <- sqrt(qchisq(0.975,df=40))
sqrt(qchisq(0.975,df=40))
#--------- CLASSE 1 -------------
class1p4 <- m4prot[m4prot[,41]=="c-CS-m",1:40]
class1p4
miu1p4 <- colMeans(class1p4)
sig1p4 <- cov(class1p4)
resmcd1p4 <- covMcd(x=class1p4)
resmve1p4 <- CovMve(x=class1p4)
#DISTÂNCIAS
r1p4 <- sqrt(mahalanobis(class1p4,miu1p4,sig1p4))
rmcd1p4 <- sqrt(mahalanobis(class1p4,resmcd1p4$center,resmcd1p4$cov))
rmve1p4 <- sqrt(mahalanobis(class1p4,resmve1p4$center,resmve1p4$cov))
#OUTLIERS
r1p4out <- r1p4[r1p4>c]
rmcd1p4out <- rmcd1p4[rmcd1p4>c]
rmve1p4out <- rmve1p4[rmve1p4>c]
length(r1p4out)
length(rmcd1p4out)
length(rmve1p4out)
#GRÁFICO
samples <- length(class1p4[,1])
ind1c <- data.frame(cbind(c(1:samples),r1p4,Class="c-CS-m"))
colnames(ind1c)[1:2] <- c("Index","Distance")
ind1mcd <- data.frame(cbind(c(1:samples),rmcd1p4,Class="c-CS-m"))
colnames(ind1mcd)[1:2] <- c("Index","Distance")
ind1mve <- data.frame(cbind(c(1:samples),rmve1p4,Class="c-CS-m"))
colnames(ind1mve)[1:2] <- c("Index","Distance")
before <- samples
r1p4

#------------ CLASSE 2 --------------
class2p4 <- m4prot[m4prot[,41]=="c-CS-s",1:40]
miu2p4 <- colMeans(class2p4)
sig2p4 <- cov(class2p4)
resmcd2p4 <- covMcd(x=class2p4)
resmve2p4 <- CovMve(x=class2p4)
#DISTÂNCIAS
r2p4 <- sqrt(mahalanobis(class2p4,miu2p4,sig2p4))
rmcd2p4 <- sqrt(mahalanobis(class2p4,resmcd2p4$center,resmcd2p4$cov))
rmve2p4 <- sqrt(mahalanobis(class2p4,resmve2p4$center,resmve2p4$cov))
#OUTLIERS
r2p4out <- r2p4[r2p4>c]
rmcd2p4out <- rmcd2p4[rmcd2p4>c]
rmve2p4out <- rmve2p4[rmve2p4>c]
length(r2p4out)
length(rmcd2p4out)
length(rmve2p4out)
#GRÁFICO
samples <- samples + length(class2p4[,1])
ind2c <- data.frame(cbind(c((before+1):samples),r2p4,Class="c-CS-s"))
colnames(ind2c)[1:2] <- c("Index","Distance")
ind2mcd <- data.frame(cbind(c((before+1):samples),rmcd2p4,Class="c-CS-s"))
colnames(ind2mcd)[1:2] <- c("Index","Distance")
ind2mve <- data.frame(cbind(c((before+1):samples),rmve2p4,Class="c-CS-s"))
colnames(ind2mve)[1:2] <- c("Index","Distance")
before <- samples

#------------- CLASSE 3 ---------------
class3p4 <- m4prot[m4prot[,41]=="c-SC-s",1:40]
miu3p4 <- colMeans(class3p4)
sig3p4 <- cov(class3p4)
resmcd3p4 <- covMcd(x=class3p4)
resmve3p4 <- CovMve(x=class3p4)
#DISTÂNCIAS
r3p4 <- sqrt(mahalanobis(class3p4,miu3p4,sig3p4))
rmcd3p4 <- sqrt(mahalanobis(class3p4,resmcd3p4$center,resmcd3p4$cov))
rmve3p4 <- sqrt(mahalanobis(class3p4,resmve3p4$center,resmve3p4$cov))
#OUTLIERS
r3p4out <- r3p4[r3p4>c]
rmcd3p4out <- rmcd3p4[rmcd3p4>c]
rmve3p4out <- rmve3p4[rmve3p4>c]
length(r3p4out)
length(rmcd3p4out)
length(rmve3p4out)
#GRÁFICO
samples <- samples + length(class3p4[,1])
ind3c <- data.frame(cbind(c((before+1):samples),r3p4,Class="c-SC-s"))
colnames(ind3c)[1:2] <- c("Index","Distance")
ind3mcd <- data.frame(cbind(c((before+1):samples),rmcd3p4,Class="c-SC-s"))
colnames(ind3mcd)[1:2] <- c("Index","Distance")
ind3mcd
ind3mve <- data.frame(cbind(c((before+1):samples),rmve3p4,Class="c-SC-s"))
colnames(ind3mve)[1:2] <- c("Index","Distance")
before <- samples

#------------ CLASSE 4 ----------------
class4p4 <- m4prot[m4prot[,41]=="c-SC-m",1:40]
miu4p4 <- colMeans(class4p4)
sig4p4 <- cov(class4p4)
resmcd4p4 <- covMcd(x=class4p4)
resmve4p4 <- CovMve(x=class4p4)
#DISTÂNCIAS
r4p4 <- sqrt(mahalanobis(class4p4,miu4p4,sig4p4))
rmcd4p4 <- sqrt(mahalanobis(class4p4,resmcd4p4$center,resmcd4p4$cov))
rmve4p4 <- sqrt(mahalanobis(class4p4,resmve4p4$center,resmve4p4$cov))
#OUTLIERS
r4p4out <- r4p4[r4p4>c]
rmcd4p4out <- rmcd4p4[rmcd4p4>c]
rmve4p4out <- rmve4p4[rmve4p4>c]
length(r4p4out)
length(rmcd4p4out)
length(rmve4p4out)
#GRÁFICO
samples <- samples + length(class4p4[,1])
ind4c <- data.frame(cbind(c((before+1):samples),r4p4,Class="c-SC-m"))
colnames(ind4c)[1:2] <- c("Index","Distance")
ind4mcd <- data.frame(cbind(c((before+1):samples),rmcd4p4,Class="c-SC-m"))
colnames(ind4mcd)[1:2] <- c("Index","Distance")
ind4mve <- data.frame(cbind(c((before+1):samples),rmve4p4,Class="c-SC-m"))
colnames(ind4mve)[1:2] <- c("Index","Distance")
before <- samples

#------------ CLASSE 5 -----------------
class5p4 <- m4prot[m4prot[,41]=="t-CS-s",1:40]
miu5p4 <- colMeans(class5p4)
sig5p4 <- cov(class5p4)
resmcd5p4 <- covMcd(x=class5p4)
resmve5p4 <- CovMve(x=class5p4)
#DISTÂNCIAS
r5p4 <- sqrt(mahalanobis(class5p4,miu5p4,sig5p4))
rmcd5p4 <- sqrt(mahalanobis(class5p4,resmcd5p4$center,resmcd5p4$cov))
rmve5p4 <- sqrt(mahalanobis(class5p4,resmve5p4$center,resmve5p4$cov))
#OUTLIERS
r5p4out <- r5p4[r5p4>c]
rmcd5p4out <- rmcd5p4[rmcd5p4>c]
rmve5p4out <- rmve5p4[rmve5p4>c]

length(r5p4out)
length(rmcd5p4out)
length(rmve5p4out)
#GRÁFICO
samples <- samples + length(class5p4[,1])
ind5c <- data.frame(cbind(c((before+1):samples),r5p4,Class="t-CS-s"))
colnames(ind5c)[1:2] <- c("Index","Distance")
ind5mcd <- data.frame(cbind(c((before+1):samples),rmcd5p4,Class="t-CS-s"))
colnames(ind5mcd)[1:2] <- c("Index","Distance")
ind5mve <- data.frame(cbind(c((before+1):samples),rmve5p4,Class="t-CS-s"))
colnames(ind5mve)[1:2] <- c("Index","Distance")
before <- samples

#------------- CLASSE 6 -------------------
class6p4 <- m4prot[m4prot[,41]=="t-CS-m",1:40]
miu6p4 <- colMeans(class6p4)
sig6p4 <- cov(class6p4)
resmcd6p4 <- covMcd(x=class6p4)
resmve6p4 <- CovMve(x=class6p4)
#DISTÂNCIAS
r6p4 <- sqrt(mahalanobis(class6p4,miu6p4,sig6p4))
rmcd6p4 <- sqrt(mahalanobis(class6p4,resmcd6p4$center,resmcd6p4$cov))
rmve6p4 <- sqrt(mahalanobis(class6p4,resmve6p4$center,resmve6p4$cov))
#OUTLIERS
r6p4out <- r6p4[r6p4>c]
rmcd6p4out <- rmcd6p4[rmcd6p4>c]
rmve6p4out <- rmve6p4[rmve6p4>c]
length(r6p4out)
length(rmcd6p4out)
length(rmve6p4out)
#GRÁFICO
samples <- samples + length(class6p4[,1])
ind6c <- data.frame(cbind(c((before+1):samples),r6p4,Class="t-CS-m"))
colnames(ind6c)[1:2] <- c("Index","Distance")
ind6mcd <- data.frame(cbind(c((before+1):samples),rmcd6p4,Class="t-CS-m"))
colnames(ind6mcd)[1:2] <- c("Index","Distance")
ind6mve <- data.frame(cbind(c((before+1):samples),rmve6p4,Class="t-CS-m"))
colnames(ind6mve)[1:2] <- c("Index","Distance")
before <- samples

#------------ CLASSE 7 --------------------
class7p4 <- m4prot[m4prot[,41]=="t-SC-s",1:40]
miu7p4 <- colMeans(class7p4)
sig7p4 <- cov(class7p4)
resmcd7p4 <- covMcd(x=class7p4)
resmve7p4 <- CovMve(x=class7p4)
#DISTÂNCIAS
r7p4 <- sqrt(mahalanobis(class7p4,miu7p4,sig7p4))
rmcd7p4 <- sqrt(mahalanobis(class7p4,resmcd7p4$center,resmcd7p4$cov))
rmve7p4 <- sqrt(mahalanobis(class7p4,resmve7p4$center,resmve7p4$cov))
#OUTLIERS
r7p4out <- r7p4[r7p4>c]
rmcd7p4out <- rmcd7p4[rmcd7p4>c]
rmve7p4out <- rmve7p4[rmve7p4>c]
length(r7p4out)
length(rmcd7p4out)
length(rmve7p4out)
#GRÁFICO
samples <- samples + length(class7p4[,1])
ind7c <- data.frame(cbind(c((before+1):samples),r7p4,Class="t-SC-s"))
colnames(ind7c)[1:2] <- c("Index","Distance")
ind7mcd <- data.frame(cbind(c((before+1):samples),rmcd7p4,Class="t-SC-s"))
colnames(ind7mcd)[1:2] <- c("Index","Distance")
ind7mve <- data.frame(cbind(c((before+1):samples),rmve7p4,Class="t-SC-s"))
colnames(ind7mve)[1:2] <- c("Index","Distance")
before <- samples

#------------- CLASSE 8 --------------------
class8p4 <- m4prot[m4prot[,41]=="t-SC-m",1:40]
miu8p4 <- colMeans(class8p4)
sig8p4 <- cov(class8p4)
resmcd8p4 <- covMcd(x=class8p4)
resmve8p4 <- CovMve(x=class8p4)
#DISTÂNCIAS
r8p4 <- sqrt(mahalanobis(class8p4,miu8p4,sig8p4))
rmcd8p4 <- sqrt(mahalanobis(class8p4,resmcd8p4$center,resmcd8p4$cov))
rmve8p4 <- sqrt(mahalanobis(class8p4,resmve8p4$center,resmve8p4$cov))
#OUTLIERS
r8p4out <- r8p4[r8p4>c]
rmcd8p4out <- rmcd8p4[rmcd8p4>c]
rmve8p4out <- rmve8p4[rmve8p4>c]
length(r8p4out)
length(rmcd8p4out)
length(rmve8p4out)
#GRÁFICO
samples <- samples + length(class8p4[,1])
ind8c <- data.frame(cbind(c((before+1):samples),r8p4,Class="t-SC-m"))
colnames(ind8c)[1:2] <- c("Index","Distance")
ind8mcd <- data.frame(cbind(c((before+1):samples),rmcd8p4,Class="t-SC-m"))
colnames(ind8mcd)[1:2] <- c("Index","Distance")
ind8mve <- data.frame(cbind(c((before+1):samples),rmve8p4,Class="t-SC-m"))
colnames(ind8mve)[1:2] <- c("Index","Distance")
before <- samples

#----------- GRÁFICOS -------------
allclassc <- rbind(ind1c,ind2c,ind3c,ind4c,ind5c,ind6c,ind7c,ind8c)
allclassc
allclassc[,1] <- as.numeric(allclassc[,1])
allclassc[,2] <- as.numeric(allclassc[,2])
allclassc$Class <- factor(allclassc$Class,levels=c("c-CS-m","c-CS-s","c-SC-s","c-SC-m","t-CS-s","t-CS-m","t-SC-s","t-SC-m"))
dpcla <- ggplot(allclassc,aes(x=Index,y=Distance,col=Class))+geom_point(pch=19,cex=1.2)+scale_colour_manual(values = c("#003f5c","#2f4b7c","#665191","#a05195","#d45087","#f95d6a","#ff7c43","#ffa600"))+ggtitle("Classic Mahalanobis distance - Separate classes - 40 proteins")+geom_hline(yintercept = c)
dpcla

allclassc
allclassmcd <- rbind(ind1mcd,ind2mcd,ind3mcd,ind4mcd,ind5mcd,ind6mcd,ind7mcd,ind8mcd)
allclassmcd[,1] <- as.numeric(allclassmcd[,1])
allclassmcd[,2] <- as.numeric(allclassmcd[,2])
allclassmcd$Class <- factor(allclassmcd$Class,levels=c("c-CS-m","c-CS-s","c-SC-s","c-SC-m","t-CS-s","t-CS-m","t-SC-s","t-SC-m"))
dpmcd <- ggplot(allclassmcd,aes(x=Index,y=Distance,col=Class))+geom_point(pch=19,cex=1.2)+scale_colour_manual(values = c("#003f5c","#2f4b7c","#665191","#a05195","#d45087","#f95d6a","#ff7c43","#ffa600"))+ggtitle("Robust Mahalanobis distance (MCD) - Separate classes - 40 proteins")+geom_hline(yintercept = c)
dpmcd

allclassmve <- rbind(ind1mve,ind2mve,ind3mve,ind4mve,ind5mve,ind6mve,ind7mve,ind8mve)
allclassmve[,1] <- as.numeric(allclassmve[,1])
allclassmve[,2] <- as.numeric(allclassmve[,2])
allclassmve$Class <- factor(allclassmve$Class,levels=c("c-CS-m","c-CS-s","c-SC-s","c-SC-m","t-CS-s","t-CS-m","t-SC-s","t-SC-m"))
dpmve <- ggplot(allclassmve,aes(x=Index,y=Distance,col=Class))+geom_point(pch=19,cex=1.2)+scale_colour_manual(values = c("#003f5c","#2f4b7c","#665191","#a05195","#d45087","#f95d6a","#ff7c43","#ffa600"))+ggtitle("Robust Mahalanobis distance (MVE) - Separate classes - 40 proteins")+geom_hline(yintercept = c)
dpmve

allp4 <- m4prot[,1:40]
miuallp4 <- colMeans(allp4)
sigallp4 <- cov(allp4)
resmcdallp4 <- covMcd(x=allp4)
resmveallp4 <- CovMve(x=allp4)
resmcdallp4
#DISTÂNCIAS
rallp4 <- sqrt(mahalanobis(allp4,miuallp4,sigallp4))
rmcdallp4 <- sqrt(mahalanobis(allp4,resmcdallp4$center,resmcdallp4$cov))
rmveallp4 <- sqrt(mahalanobis(allp4,resmveallp4$center,resmveallp4$cov))
#OUTLIERS
rallp4out <- rallp4[rallp4>c]
rmcdallp4out <- rmcdallp4[rmcdallp4>c]
rmveallp4out <- rmveallp4[rmveallp4>c]
length(rallp4out)
length(rmcdallp4out)
length(rmveallp4out)
#GRÁFICO
n <- length(allp4[,1])
indallc <- as.data.frame(cbind(c(1:n),rallp4))
indallmcd <- as.data.frame(cbind(c(1:n),rmcdallp4))
indallmve <- as.data.frame(cbind(c(1:n),rmveallp4))
distplotclaall <- ggplot(indallc,aes(x=V1,y=rallp4))+geom_point(pch=19,cex=1.2,col="#003f5c")+geom_hline(yintercept = c)+ylab("Classic Distance")+xlab("Index")+ggtitle("Classic Mahalanobis distance - Conjoined classes - 40 proteins")
distplotmcdall <- ggplot(indallmcd,aes(x=V1,y=rmcdallp4))+geom_point(pch=19,cex=1.2,col="#003f5c")+geom_hline(yintercept = c)+ylab("Robust Distance MCD")+xlab("Index")+ggtitle("Robust Mahalanobis distance (MCD) - Conjoined classes - 40 proteins")
distplotmveall <- ggplot(indallmve,aes(x=V1,y=rmveallp4))+geom_point(pch=19,cex=1.2,col="#003f5c")+geom_hline(yintercept = c)+ylab("Robust Distance MVE")+xlab("Index")+ggtitle("Robust Mahalanobis distance (MVE) - Conjoined classes - 40 proteins")

distplotclaall
distplotmcdall
distplotmveall