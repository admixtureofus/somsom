
######################################################################################
# 5  영향력 관측치와 이상치outlier &influential obs
######################################################################################

set.seed(123)
testdata <- data.frame(x=1:10,y=-1:-10+rnorm(10))
lmod <- lm(y ~ x, testdata)
p1 <- c(5.5,12)
lmod1 <- lm(y ~ x, rbind(testdata, p1))
plot(y ~ x, rbind(testdata, p1))
points(5.5,12,pch=4,cex=2)
abline(lmod)
abline(lmod1, lty=2)

p2 <- c(1,0.7)
lmod2 <- lm(y ~ x, rbind(testdata, p2))
plot(y ~ x, rbind(testdata, p2))
points(1,1,pch=4,cex=2)
abline(lmod)
abline(lmod2,lty=2)


p3 <- c(15,5.1)
lmod3 <- lm(y ~ x, rbind(testdata, p3))
plot(y ~ x, rbind(testdata, p3))
points(15,5.1,pch=4,cex=2)
abline(lmod)
abline(lmod3,lty=2)


library(faraway)
data(savings)
head(savings)  ## 50's country
help(savings)


fit<-lm(sr~pop15+pop75+dpi+ddpi, data=savings)
hh<-hatvalues(fit)
head(hh)
sum(hh)
sr<-rstandard(fit)
sr2<-rstudent(fit)

par(mfrow=c(1,1))

index<-1:50

##  y's outlier : using standardized residual"
country.name<-row.names(savings)
plot(rstandard(fit), main="standardized residual")
points(index[abs(sr)>2], sr[abs(sr)>2], pch="*")
abline(h=-2); abline(h=2)

country.name[abs(sr)>2]
sr[abs(sr)>2]
savings[abs(sr)>2,1]
mean(sr)

### x's outlier : using leverage
p=length(fit$coef); n=nrow(savings)
ctr=2*p/(n)
ctr
country.name[hh>ctr]
hh[hh>ctr]
plot(hh, main="leverage") 
abline(h=ctr) 
points(index[hh>ctr], hh[hh>ctr], pch="*" )

x1<-savings[,-1]
apply(x1,2,mean)
cbind(x1[hh>ctr,],hh[hh>ctr])

########################################################

set.seed(123)
testdata0<- data.frame(x=5:14, y=-1:-10+rnorm(10))
p1=c(1,7); p2<-c(1,-1); p3<-c(20,-16+0.1); p4=c(21,-17-0.1)
testdata<-rbind(p1,p2,p3,p4,testdata0)

plot(y ~ x, testdata, type="p", pch=16)
text(1.5,7, "p1")
text(1.5,-1, "p2")
text(20,-15, "p3")
text(21,-16, "p4")

fit<-lm(y~x, data=testdata)
fit1<-lm(y~x, data=testdata[-1,])
fit2<-lm(y~x, data=testdata[-2,])
fit12<-lm(y~x, data=testdata[-c(1:2),])
fit3<-lm(y~x, data=testdata[-3,])
fit4<-lm(y~x, data=testdata[-4,])
fit34<-lm(y~x, data=testdata[-c(3:4),])


###########################################

head(usedcars)
carfit<-lm(price~year+mileage+cc+automatic, data=usedcars)
library(lattice)

splom(usedcars)
summary(carfit)
hatcar<-hatvalues(carfit)
head(hatcar)
sum(hatcar)
sr<-rstandard(carfit)
sr2<-rstudent(carfit)
plot(sr,sr2)


##  y's outlier : using standardized residual"
index<-1:30
plot(rstandard(carfit), main="standardized residual")
points(index[abs(sr)>2], sr[abs(sr)>2], pch="*")
index[abs(sr)>2]
abline(h=-2); abline(h=2)


### x's outlier : using leverage
p=length(carfit$coef); n=nrow(usedcar)
ctr=2*p/(n)
ctr

index[hatcar>ctr]
hatcar[hatcar>ctr]
plot(hatcar, main="leverage") 
abline(h=ctr) 
points(index[hatcar>ctr], hatcar[hatcar>ctr], pch="*" )


### influence measures

influence.measures(carfit)

par(mfrow=c(2,2))
plot(carfit)

plot(carfit, which=4)
plot(covratio(carfit))
abline(h=1.5)

plot(dffits(carfit))
abline(h=0.816)
plot(index, dfbetas(carfit)[,2], ylab="year")
abline(h=0.365); abline(h=-0.365)


