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

tab <- read.csv(file.choose()) #genres
m <- data.frame(tab[1:length(tab[,1]),c(1,2,6,7,8,9,10)])
length(m[,1])
# m[,1] - danceability
# m[,2] - energy
# m[,3] - speechiness
# m[,4] - acousticness
# m[,5] - instrumentalness
# m[,6] - liveness
# m[,7] - valence

c <- sqrt(qchisq(0.975,df=7))
c
miu <- colMeans(m)
cov <- cov(m)
resmcd <- covMcd(x=m)
res2mcd <- covMcd(x=m,alpha=0.75)
resmve <- CovMve(x=m)

length(m[,1])

ind <- c(1:length(m[,1]))
ind
sco <- rowMeans(m)
sco
allm <- rbind(miu,resmcd$center,resmve$center)
allm

score <- data.frame(cbind(Index=ind,Score=sco))
score
score
order(score$Score,decreasing = TRUE)
score[1937,] #minimo
score[4201,] #maximo
m[1937,]
splot <- ggplot(score,aes(x=Index,y=Score))+geom_point(pch=19,cex=0.8)+ggtitle("Plot of Scores")
splot
#DADO
mcla <- sqrt(mahalanobis(m,miu,cov))
mcla[1937]

t <- sum(mahalanobis(m,miu,cov))
t2 <- length(m[1,])*(length(m[,1])-1)
t
t2

pmcla <- data.frame(cbind(c(1:length(m[,1])),mcla))
mmcd <- sqrt(mahalanobis(m,resmcd$center,resmcd$cov))
mmcd[1937]
pmmcd <- data.frame(cbind(c(1:length(m[,1])),mmcd))
mmcd2 <- sqrt(mahalanobis(m,res2mcd$center,res2mcd$cov))
pmmcd2 <- data.frame(cbind(c(1:length(m[,1])),mmcd2))
mmve <- sqrt(mahalanobis(m,resmve$center,resmve$cov))
mmve[1937]
pmmve <- data.frame(cbind(c(1:length(m[,1])),mmve))

#GRÁFICO
pclaplot <- ggplot(pmcla,aes(x=V1,y=mcla))+geom_point(pch=19,cex=0.8)+geom_hline(yintercept = c,col="red",cex=0.7)+ylab("Classic Distance")+xlab("Index")+ggtitle("Classic Distance Plot")
pmcdplot <- ggplot(pmmcd,aes(x=V1,y=mmcd))+geom_point(pch=19,cex=0.8)+geom_hline(yintercept = c,col="red",cex=0.7)+ylab("Robust Distance (MCD)")+xlab("Index")+ggtitle("Robust (MCD) Distance Plot")
pmveplot <- ggplot(pmmve,aes(x=V1,y=mmve))+geom_point(pch=19,cex=0.8)+geom_hline(yintercept = c,col="red",cex=0.7)+ylab("Robust Distance (MVE)")+xlab("Index")+ggtitle("Robust (MVE) Distance Plot")
pclaplot
pmcdplot
pmveplot

pmcd2plot <- ggplot(pmmcd2,aes(x=V1,y=mmcd))+geom_point(pch=19,cex=0.8)+geom_hline(yintercept = c,col="red",cex=0.7)+ylab("Robust Distance (MCD)")+xlab("Index")+ggtitle("Robust (MCD) Distance Plot, alpha=0.75")
pmcd2plot
#OUTLIER
noutcla <- length(mcla[mcla>c])
noutmcd <- length(mmcd[mmcd>c])
noutmve <- length(mmve[mmve>c])
noutcla
noutmcd
noutmve
length(mmcd[mmcd<=c])
m[m[,7]<0.05,7]
#----------------- HISTOGRAMAS -----------------

hist(m[,4],
     main="Acousticness",
     freq=TRUE,
     xlab="Value",
     ylab="Frequency",
     xlim=c(0,1),
     ylim=c(0,20000),
     breaks=seq(0, 1, 0.05),
     col="lightsalmon")

hist(m[,6],
     main="Speachiness",
     freq=TRUE,
     xlab="Value",
     ylab="Frequency",
     xlim=c(0,1),
     ylim=c(0,20000),
     breaks=seq(0,1,0.05),
     col="lightsalmon")

hist(m[,5],
     main="Instrumentalness",
     freq=TRUE,
     xlab="Value",
     ylab="Frequency",
     xlim=c(0,1),
     ylim=c(0,20000),
     breaks=seq(0,1,0.05),
     col="lightsalmon")

hist(m[,1],
     main="Danceability (original)",
     freq=TRUE,
     xlab="Value",
     ylab="Frequency",
     xlim=c(0,1),
     ylim=c(0,20000),
     breaks=seq(0,1,0.05),
     col="lightsalmon")

#------------------- ANÁLISE ------------------------
resmcd
hist(rnorm(42305,resmcd$center[1],resmcd$cov[1,1]),
     main="Danceability (MCD, alpha=0.5)",
     freq=TRUE,
     xlab="Value",
     ylab="Frequency",
     xlim=c(0,1),
     ylim=c(0,20000),
     breaks=seq(0,1,0.05),
     col="lightsalmon")

hist(rnorm(42305,res2mcd$center[1],res2mcd$cov[1,1]),
     main="Danceability (MCD, alpha=0.75)",
     freq=TRUE,
     xlab="Value",
     ylab="Frequency",
     xlim=c(0,1),
     ylim=c(0,20000),
     breaks=seq(0,1,0.05),
     col="lightsalmon")




hist(rnorm(42305,resmve$center[1],resmve$cov[1,1]),
     main="Danceability (MVE)",
     freq=TRUE,
     xlab="Value",
     ylab="Frequency",
     xlim=c(0,1),
     ylim=c(0,6000),
     breaks=seq(0,1,0.05),
     col="lightsalmon")
newm <- m[,c(1,6)]
newm
newrmcd <- covMcd(x=newm)
hist(rnorm(42305,newrmcd$center[1],newrmcd$cov[1,1]),
     main="Danceability (New MCD)",
     freq=TRUE,
     xlab="Value",
     ylab="Frequency",
     xlim=c(0,1),
     ylim=c(0,20000),
     breaks=seq(0,1,0.05),
     col="lightsalmon")

hist(rnorm(42305,newrmcd$center[1],newrmcd$cov[1,1]),
     main="Danceability (New MCD)",
     freq=TRUE,
     xlab="Value",
     ylab="Frequency",
     xlim=c(0,1),
     ylim=c(0,20000),
     breaks=seq(0,1,0.05),
     col="lightsalmon")
