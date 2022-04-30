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

tab <- read.csv(file.choose()) #fish
n <- tab[tab[,1]=="Perch",]
m <- data.frame(n[1:length(n[,1]),c(2,6)])
m
c <- sqrt(qchisq(0.975,df=2))
cnew <- sqrt(qchisq(0.975,df=20))
cnew
init <- ggplot(m,aes(x=Weight,y=Height))+geom_point(size=1.5)+ggtitle("Weight - Height graph of Perch")
init <- init + geom_hline(yintercept = 0, color="azure3") + geom_vline(xintercept=0,color="azure3")
init
# ------- ELIPSE CLÁSSICA -----------
miu <- colMeans(m)
sig <- cov(m)
miu
mcla <- sqrt(mahalanobis(m,miu,sig))
length(mcla[mcla>c])
log(det(sig))
ellip <- data.frame(ellipse(sig,centre=miu,level=0.975,t=sqrt(qchisq(0.975,df=2)),npoints=400),Method="Classic")
# ------- ELIPSE ROBUSTA MCD -----------
res <- covMcd(x=m)
res
ellip2 <- data.frame(ellipse(res$cov,centre=res$center,level=0.975,t=sqrt(qchisq(0.975,df=2)),npoints=400),Method="MCD")
mmcd <- sqrt(mahalanobis(m,res$center,res$cov))
length(mmcd[mmcd>c])
# ------- ELIPSE ROBUSTA MVE -----------
res2 <- CovMve(x=m)
res2
log(det(res2$cov))
ellip3 <- data.frame(ellipse(res2$cov,centre=res2$center,level=0.975,t=sqrt(qchisq(0.975,df=2)),npoints=400),Method="MVE")
mmve <- sqrt(mahalanobis(m,res2$center,res2$cov))
length(mmve[mmve>c])

ellip2
ellip3
compare <- cbind(ellip2,ellip3)
compare
allellip <- rbind(ellip,ellip2,ellip3)
allellip[,1]
p <- ggplot(m,aes(x=Weight,y=Height))+geom_point(size=1.5)+ggtitle("Weight - Height graph of Perch")
p <- p + geom_hline(yintercept = 0, color="azure3") + geom_vline(xintercept=0,color="azure3")
p <- p + geom_path(data=allellip,size=1,aes(colour = Method,linetype=Method))+scale_linetype_manual(values=c(5,1,2))+scale_colour_manual(values = c("blue", "red", "green"))
p <- p + geom_point(mapping=aes(x=res$center[1],y=res$center[2]),pch=19,col="red",size=3.5) 
p <- p + geom_point(mapping=aes(x=res2$center[1],y=res2$center[2]),pch=19,col="green",size=2.5)
p <- p + geom_point(mapping=aes(x=miu[1],y=miu[2]),pch=19,col="blue",size=3)
p

# ------- ELIPSES MCD COM VARIAÇÃO DE ALPHA -----------
res5 <- covMcd(x=m,alpha=0.5)
ellip5 <- data.frame(ellipse(res5$cov,centre=res5$center,level=0.975,t=sqrt(qchisq(0.975,df=2)),npoints=600),alpha="0.5")
res7 <- covMcd(x=m,alpha=0.7)
r7mcd <- sqrt(mahalanobis(m,res7$center,res7$cov))
length(r7mcd[r7mcd>c])
ellip7 <- data.frame(ellipse(res7$cov,centre=res7$center,level=0.975,t=sqrt(qchisq(0.975,df=2)),npoints=600),alpha="0.7")
res9 <- covMcd(x=m,alpha=0.9)
r9mcd <- sqrt(mahalanobis(m,res9$center,res9$cov))
length(r9mcd[r9mcd>c])
ellip9 <- data.frame(ellipse(res9$cov,centre=res9$center,level=0.975,t=sqrt(qchisq(0.975,df=2)),npoints=600),alpha = "0.9")

allellip2 <- rbind(ellip5,ellip7,ellip9)

p2 <- ggplot(m,aes(x=Weight,y=Height))+geom_point(size=1.5)+ggtitle("Weight - Height graph of Perch")
p2 <- p2 + geom_hline(yintercept = 0, color="azure3") + geom_vline(xintercept=0,color="azure3")
p2 <- p2 + geom_path(data=allellip2,size=1,aes(colour = alpha))+scale_colour_manual(values = c("#e80000", "#ff5757", "#ff9e9e"))
p2 <- p2 + geom_point(mapping=aes(x=res5$center[1],y=res5$center[2]),pch=19,col="#e80000",size=3) 
p2 <- p2 + geom_point(mapping=aes(x=res7$center[1],y=res7$center[2]),pch=19,col="#ff5757",size=3)
p2 <- p2 + geom_point(mapping=aes(x=res9$center[1],y=res9$center[2]),pch=19,col="#ff9e9e",size=3)
p2

res5
res7
res9