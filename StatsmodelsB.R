# Read in data set
times = scan("C:/Users/User/Documents/web_times_data.txt")
print(times)
#Q1

summary(times)
sd(times)
hist(times, xlab = "Number of observations for times between successive visits at a popular website during a particular period of a day", col= "blue" , main ="")
boxplot(times,  xlab = "Number of observations for times between successive visits at a popular website during a particular period of a day", col= "blue", main="")
#Q2
qqnorm(times, main="Normal Q-Q Plot for Times", pch=19 , col="red")
qqline(times)
qqnorm(times^0.5, main =" Normal Q-Q Plot for Square Root of Times" , pch=19 , col="red")
qqline(times^0.5)
#Q3
mean(times)
log.times <- log(times)
sum(log.times)
alpha_hat <- 0.5/(log(mean(times))- (sum(log.times)/92))
beta_hat <- alpha_hat / mean(times)

q.ga <- qgamma(ppoints(92), 0.9754937 , 0.09976812); q.ga
qqplot(q.ga , times , main=" QQ plot of times")
alphabar = 0.9754937
betabar = 0.09976812
abline(0,1)

## Use cells of length 10
times.breaks = seq(0,55,by=5.5); times.breaks  ##define breaks
times.cut = cut(times, breaks = times.breaks, right=F); ##bin data
times.table = table(times.cut); times.table ##count data in bins

## Compute expected frequencies
prob = numeric(10);  exp.f = numeric(10)
for (i in 1:(length(times.breaks)-1)) {
  prob[i] = pgamma(times.breaks[i+1], alphabar, betabar) - pgamma(times.breaks[i], alphabar, betabar)
  exp.f[i] <- prob[i] * length(times)
}
exp.f

## Extract observed frequencies
obs.f = numeric(10)
for (i in 1:10) obs.f[i] = times.table[[i]]
obs.f 
# or
obs.f = hist(times, breaks=times.breaks, plot=F, right=F)$counts

exp.f1 <- c(exp.f[1:4] , sum(exp.f[5:10]))
obs.f1 <- c(obs.f[1:4] , sum(obs.f[5:10]))

## Compute chi-sq statistic
x2 = sum( (obs.f1 - exp.f1)^2 / exp.f1); x2

## Calculate pvalue; 
pval = 1 - pchisq(x2, (5-2-1))
pval


#Q4
B = 1000
mean.b = numeric(B)
for(i in 1:B){ mean.b[i] = mean(sample(times, size=92, replace=TRUE)) }
summary(mean.b)
par(mfrow=c(2,1))
hist(mean.b, main="Histogram of bootstrap means", col="cyan")
boxplot(mean.b, main="Boxplot of bootstrap means", col="cyan", horizontal=TRUE)

#non-parametric bootstrap 90% confidence interval
bootstrap_CI = quantile(mean.b,c(0.05,0.95))
bootstrap_CI

#asymptotic theory 90% confidence interval 

asymp_CI = c(mean(times)-(1.645*(sd(times)/sqrt(92))),mean(times)+(1.645*(sd(times)/sqrt(92))))
asymp_CI

#Q5
B = 10000
sd.times = 10
boot.sd.H0 = numeric(B)  # Create vector for bootstrap values
for (i in 1: B){
  y.boot = rgamma(92,1 , 0.1) # Notice this is under H0
  boot.sd.H0[i] = sd(y.boot)
}

# Obtain sampling distribution under H0 and p-value

sd.hat = sd(times)
abline(v = sd.hat, col="red", lwd=2.0)

boot.pval = (1 + length(boot.sd.H0[boot.sd.H0 <= sd.hat])) / (B + 1) 
boot.pval

#Q6

df <- 91
alpha <- 0.05
sigma <-1
varTest(times,alternative="two.sided",sigma.squared = sigma^2, conf.level= (1-alpha))
