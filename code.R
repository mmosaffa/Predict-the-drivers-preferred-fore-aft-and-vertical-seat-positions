seat = read.csv('C:/Seatdata.csv')
mf<-lm(SeatX~Stature+Sitting.Height+SHS+BMI+Weight+L11+H17,data=seat)
mr<-lm(SeatX~Sitting.Height+BMI+Weight+L11+H17,data=seat) 
anova(mr,mf)
mr<-lm(SeatX~I(Stature+Sitting.Height)+SHS+BMI+Weight+L11+H17,data=seat) 
anova(mr,mf)
t=(summary(mf)$coef[2,1]-0.6)/summary(mf)$coef[2,2] 
t
mr<lm(SeatX~offset(0.6*Stature)+Sitting.Height+SHS+BMI+Weight+L11+H17,data=seat)
mr<lm(SeatX~offset(0.6*Stature)+Sitting.Height+SHS+BMI+Weight+L11+H17,data=seat)

head(seat)
y=seat[,8]
x=seat[,1:7]

f=NA
mforg=lm(y~as.matrix(x))
for (i in 1:1000){
p=sample(length(y),length(y),replace = F)
yperm=y[p]
mf=lm(yperm~as.matrix(x))
mr=lm(yperm~1)
h=anova(mr,mf)
f[i]=h$F[2]
}
hist(f)
quantile(f,0.95)
mforg=lm(y~as.matrix(x))
mrorg=lm(y~1)
anova(mrorg,mforg)


mf<-lm(SeatX~Stature+SHS+BMI+L11+H17,data=seat) 
plot(mf$fitted.values, mf$residuals,xlab="Fitted Values",ylab="Residuals") 
abline(h=mean(mf$residuals),col="red") 
abline(v=mean(mf$fitted.values),col="red")
plot(mf$fitted.values,abs(mf$residuals))
mf=lm(abs(mf$residual) ~ mf$fitted)
summary(mf)


plot(mf$residual[1:397],mf$residual[3:399],xlab="e(i-1)",ylab="e(i)") 
acf(mf$residuals)
plot(mf$residual[1:390],mf$residual[10:399],xlab="e(i-1)",ylab="e(i)")


library(faraway) 
mf<-lm(SeatX~Stature+SHS+BMI+L11+H17,data=seat)
halfnorm(influence(mf)$hat, nlab = 2, ylab="Leverages") 


resid <- residuals(mf) 
sigma <- summary(mf)$sigma 
hi <- influence(mf)$hat 
stud.res <- resid/(sigma * sqrt(1-hi))
plot(stud.res, fitted.values(mf),ylab="Fitted Values",xlab="Studentized Residuals") 
points(stud.res[320],fitted.values(mf)[320],col="red") 
points(stud.res[239],fitted.values(mf)[239],col="red")
