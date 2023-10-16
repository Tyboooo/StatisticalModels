#Q1.a.i 
homeowner <- scan()


# Set up a matrix containing the data
homeowner.mat = matrix(homeowner, ncol=5, byrow=T)

# Set up a data frame containing the data
homeowner.df = data.frame(homeowner.mat,row.names=1)
names(homeowner.df) <-c ("policies","medinc","aged", "area")
homeowner.df
attach(homeowner.df)

#Plot policies against medinc, distinguising between North, Central and South
plot(policies~medinc, area="n")
points(policies[area=='1']~medinc[area=='1'])
points(policies[area=='2']~medinc[area=='2'],pch=3)
points(policies[area=='3']~medinc[area=='3'],pch=15)

#Q1.a.ii

# Plot policies against aged, distinguising between North, Central and South
plot(policies~aged, area="n")
points(policies[area=='1']~aged[area=='1'])
points(policies[area=='2']~aged[area=='2'], pch=3)
points(policies[area=='3']~aged[area=='3'], pch=15)

#Q1.b.
# Set the 'area' variable to be treated as a factor
area <-as.factor(area)

mod1=lm(policies~medinc);summary(mod1);
summary.aov(mod1)

mod2=lm(policies~aged);summary(mod2);
summary.aov(mod2)

mod3=lm(policies~area);summary(mod3);
summary.aov(mod3)

# Fit linear model with three explanatory variables, no interaction term
mod4=lm(policies~medinc+aged+area);summary(mod4);
summary.aov(mod4)
par(mfrow=c(2,2))
plot(mod4)


plot(policies~medinc+aged+area, type='n')
points(y[type=='1']~x[type=='1'])
points(y[type=='2']~x[type=='2'], pch=3)
lines(fitted(mod4)[type==1]~x[type==1])
lines(fitted(mod4)[type==2]~x[type==2])


#Q2.a
mortality=scan()

# Set up a matrix containing the data
mortality.mat = matrix(mortality, ncol=4, byrow=T)

# Set up a data frame containing the data
mortality.df = data.frame(mortality.mat,row.names=1)
names(mortality.df) <-c ("Age","Deaths","Sex")
mortality.df
attach(mortality.df)

#Plot Deaths against Sex, distinguising between Male and Female
plot(Deaths~Age)
points(Deaths[Sex=='1']~Age[Sex=='1'])
points(Deaths[Sex=='2']~Age[Sex=='2'], pch=3)

#Q2.b

mod5 = glm(Deaths~Sex*Age, family = poisson)
summary(mod5)
1-pchisq(34.042,df=20)
summary(fitted.values(mod5))
summary(residuals(mod5))

pi.mean = sum(Deaths)/sum(24)
plot(Deaths~Sex*Age, ylab = "pi", pch=19)
lines(Sex*Age, fitted(mod5), col="red", lwd=1.8)
lines(Sex*Age, rep(pi.mean,length(Sex*Age)), col="blue", lwd=2.0, lty=2)
