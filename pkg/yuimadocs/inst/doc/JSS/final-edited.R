### R code from vignette source 'final-edited.Rnw'
### Encoding: UTF-8


##################################
##################################
##################################
### Sec 3. Model specification ###
##################################
##################################
##################################



library("yuima")
mod1 <- setModel(drift = "-3*x", diffusion = "1/(1+x^2)")
str(mod1)
set.seed(123)
X <- simulate(mod1)
plot(X)
str(X,vec.len=2)

mod1b <- setModel(drift = "-3*s*y", diffusion = "1/(1+y^2)",
state.var="y", time.var="s")
str(mod1b)

mod2 <- setModel(drift = "-theta*x", diffusion = "1/(1+x^gamma)")
str(mod2)
set.seed(123)
X <- simulate(mod2,true.param=list(theta=1,gamma=3))
plot(X)

# 2-dim Models

sol <- c("x1","x2") # variable for numerical solution
a <- c("-3*x1","-x1-2*x2")   # drift vector 
b <- matrix(c("1","x1","0","3","x2","0"),2,3)  #  diffusion matrix
mod3 <- setModel(drift = a, diffusion = b, solve.variable = sol)
set.seed(123)
X <- simulate(mod3)
plot(X, plot.type="single",lty=1:2)

# Non liner models
mu <- 0.1
sig <- 0.2
rho <- -0.7
g <- function(t) {0.4 + (0.1 + 0.2*t)* exp(-2*t)}

f1 <- function(t, x1, x2, x3) {
    ret <- 0
    if(x1 > 0 && x2 > 0) ret <- x2*exp(log(x1)*2/3)
    return(ret)
}

f2 <- function(t, x1, x2, x3) {
     ret <- 0
     if(x3 > 0) ret <- rho*sig*x3
     return(ret)
}

f3 <- function(t, x1, x2, x3) {
     ret <- 0
     if(x3 > 0) ret <- sqrt(1-rho^2)*sig*x3
     return(ret)
}

diff.coef.matrix <- matrix(c("f1(t,x1,x2,x3)",
 "f2(t,x1,x2,x3) * g(t)", "f2(t,x1,x2,x3)", "0", 
     "f3(t,x1,x2,x3)*g(t)", "f3(t,x1,x2,x3)"),  3, 2)

sabr.mod <- setModel(drift = c("0", "mu*g(t)*x3", "mu*x3"), 
diffusion = diff.coef.matrix, state.variable = c("x1", "x2", "x3"),
    solve.variable = c("x1", "x2", "x3"))
str(sabr.mod@parameter)



f2 <- function(t, x1, x2, x3, rho, sig) {
     ret <- 0
     if(x3 > 0) ret <- rho*sig*x3
     return(ret)
 }

 f3 <- function(t, x1, x2, x3, rho, sig) {
     ret <- 0
     if(x3 > 0) ret <- sqrt(1-rho^2)*sig*x3
     return(ret)
 }

 diff.coef.matrix <- matrix(c("f1(t,x1,x2,x3)", 
 "f2(t,x1,x2,x3,rho, sig) * g(t)", "f2(t,x1,x2,x3,rho,sig)", 
 "0", "f3(t,x1,x2,x3,rho,sig)*g(t)", "f3(t,x1,x2,x3,rho,sig)"),  3, 2)

 sabr.mod <- setModel(drift = c("0", "mu*g(t)*x3", "mu*x3"), 
 diffusion = diff.coef.matrix, state.variable = c("x1", "x2", "x3"), 
 solve.variable = c("x1", "x2", "x3"))
str(sabr.mod@parameter)

# Fractional SDE

mod4A <- setModel(drift="3*y", diffusion=1, hurst=0.3, solve.var="y")
mod4B <- setModel(drift="3*y", diffusion=1, hurst=0.7, solve.var="y")
set.seed(123)
X1 <- simulate(mod4A,sampling=setSampling(n=1000))
X2 <- simulate(mod4B,sampling=setSampling(n=1000))
par(mfrow=c(2,1))
par(mar=c(2,3,1,1))
plot(X1,main="H=0.3")
plot(X2,main="H=0.7")
str(mod4A)


# JUMP SDE

mod5 <- setModel(drift=c("-theta*x"), diffusion="sigma",
 jump.coeff="1", measure=list(intensity="10", df=list("dnorm(z, 0, 1)")),
 measure.type="CP", solve.variable="x")
set.seed(123)
X <- simulate(mod5, true.p=list(theta=1,sigma=3),sampling=setSampling(n=1000))
plot(X)

mod6 <- setModel(drift="-x", xinit=1, jump.coeff="1",
  measure.type="code", measure=list(df="rIG(z, 1, 0.1)"))
set.seed(123)
X <- simulate(mod6, sampling=setSampling(Terminal=10, n=10000)) 
plot(X)


###################################################
###################################################
###################################################
### Sec 4. Simulation, sampling and subsampling ###
###################################################
###################################################
###################################################


sol <- c("x1","x2") # variable for numerical solution
b <- c("-theta*x1","-x1-gamma*x2")   # drift vector 
s <- matrix(c("1","x1","0","delta","x2","0"),2,3)  #  diffusion matrix
mymod <- setModel(drift = b, diffusion = s, solve.variable = sol)
samp <- setSampling(Terminal=3, n=3000)
str(samp)
set.seed(123)
X2 <- simulate(mymod, sampling=samp)
str(X2@sampling)

# RANDOM SAMPLING
newsamp <- setSampling(
random=list(rdist=c( function(x) rexp(x, rate=10), 
function(x) rexp(x, rate=20))) )
str(newsamp)
newdata <- subsampling(X2, sampling=newsamp)
plot(X2,plot.type="single", lty=c(1,3),ylab="X2")
points(get.zoo.data(newdata)[[1]],col="red")
points(get.zoo.data(newdata)[[2]],col="green",pch=18)

# SUBSAMPLING

newsamp <- setSampling(delta=c(0.1,0.2))
newdata <- subsampling(X2, sampling=newsamp)
plot(X2,plot.type="single", lty=c(1,3),ylab="X2")
points(get.zoo.data(newdata)[[1]],col="red")
points(get.zoo.data(newdata)[[2]],col="green", pch=18)


set.seed(123)
Y.sub <- simulate(mymod,sampling=setSampling(delta=0.001,n=1000),
subsampling=setSampling(delta=0.01,n=100))
set.seed(123)
Y <- simulate(mymod, sampling=setSampling(delta=0.001,n=1000))
plot(Y, plot.type="single")
points(get.zoo.data(Y.sub)[[1]],col="red")
points(get.zoo.data(Y.sub)[[2]],col="green",pch=18)
plot(Y.sub, plot.type="single")



###################################
###################################
###################################
### Sec 5. Asymptotic expansion ###
###################################
###################################
###################################


###################################################
### code chunk number 42: final-edited.Rnw:991-1000
###################################################
model <- setModel(drift = "x", diffusion = matrix( "x*e", 1,1))
T <- 1
xinit <- 150
K <- 100
f <- list( expression(x/T), expression(0))
F <- 0
e <- 0.5
yuima <- setYuima(model = model, sampling = setSampling(Terminal=T, n=1000))
yuima <- setFunctional( yuima, f=f,F=F, xinit=xinit,e=e)
str(yuima@functional)

F0 <- F0(yuima)
F0
rho <- expression(0)
epsilon <- e  # noise level
g <- function(x) {
  tmp <- (F0 - K) + (epsilon * x) 
 tmp[(epsilon * x) < (K-F0)] <- 0
 tmp
}

asymp <- asymptotic_term(yuima, block=10, rho, g)
asymp
asy1 <- asymp$d0 + e * asymp$d1  # 1st order asymp. exp. of asian call price
asy1
asy2 <- asymp$d0 + e * asymp$d1+ e^2* asymp$d2  # 2nd order asymp. exp. of asian call price
asy2

# PRICING ASIAN OPTIONS

library("fExoticOptions")
levy <- LevyAsianApproxOption(TypeFlag = "c", S = xinit, SA = xinit, X = K, 
      Time = 1, time = 1, r = 0.0, b = 1, sigma = e)@price
levy

a <- 0.9
e <- 0.4
Terminal <- 3

xinit <- 1
K <- 10

drift <- "a * x"
diffusion <- "e * sqrt(x)"

model <- setModel(drift=drift,diffusion=diffusion)

n <- 1000*Terminal
yuima <- setYuima(model = model,sampling = setSampling(Terminal=Terminal,n=n))

f <- list(c(expression(0)),c(expression(0)))
F <- expression(x)

yuima.ae <- setFunctional(yuima,f=f,F=F,xinit=xinit,e=e)
rho <- expression(0)
F1 <- F0(yuima.ae)


get_ge <- function(x,epsilon,K,F0){
	tmp <- (F0 - K) + (epsilon * x[1])
	tmp[(epsilon * x[1]) > (K - F0)] <- 0
	return( - tmp )
}


g <- function(x){
	return(get_ge(x,e,K,F1))
}

time1 <- proc.time()
asymp <- asymptotic_term(yuima.ae,block=100,rho,g)
time2 <- proc.time()



ae.value0 <- asymp$d0
ae.value0
ae.value1 <- asymp$d0 + e * asymp$d1
ae.value1
ae.value2 <- as.numeric(asymp$d0 + e * asymp$d1 + e^2 * asymp$d2)
ae.value2
ae.time <- time2 - time1
ae.time


#################################################
#################################################
#################################################
### Sec 6. Inference for stochastic processes ###
#################################################
#################################################
#################################################


@ GETTING DATA INTO YUIMA 

data <- read.csv("http://chart.yahoo.com/table.csv?s=IBM&g=d&x=.csv")
x <- setYuima(data=setData(data$Close))
str(x@data)


# QMLE ESTIMATION
ymodel <- setModel(drift="(2-theta2*x)", diffusion="(1+x^2)^theta1")
n <- 750
ysamp <- setSampling(Terminal = n^(1/3), n = n)
yuima <- setYuima(model = ymodel, sampling = ysamp)
set.seed(123)
yuima <- simulate(yuima, xinit = 1, 
true.parameter = list(theta1 = 0.2, theta2 = 0.3))

param.init <- list(theta2=0.5,theta1=0.5)
low.par <-  list(theta1=0, theta2=0)
upp.par <-  list(theta1=1, theta2=1)
mle1 <- qmle(yuima, start = param.init,  lower = low.par, upper = upp.par)
summary(mle1)

# BAYES ESTIMATION
prior <- list(theta2=list(measure.type="code",df="dunif(theta2,0,1)"),
 theta1=list(measure.type="code",df="dunif(theta1,0,1)"))
bayes1 <- adaBayes(yuima, start=param.init, prior=prior)

coef(summary(bayes1))
coef(summary(mle1))



n <- 500
ysamp <- setSampling(Terminal = n^(1/3), n = n)
yuima <- setYuima(model = ymodel, sampling = ysamp)
set.seed(123)
yuima <- simulate(yuima, xinit = 1, 
true.parameter = list(theta1 = 0.2, theta2 = 0.3))
param.init <- list(theta2=0.5,theta1=0.5)
mle2 <- qmle(yuima, start =param.init , 
lower = list(theta1=0, theta2=0), 
upper = list(theta1=1, theta2=1))
bayes2 <- adaBayes(yuima, start=param.init, prior=prior)
coef(summary(bayes2))
coef(summary(mle2))



# ASYNCH COV ESTIMATION

# diffusion coefficient for process 1
diff.coef.1 <- function(t,x1=0, x2=0) sqrt(1+t)
# diffusion coefficient for process 2
diff.coef.2 <- function(t,x1=0, x2=0) sqrt(1+t^2)
# correlation
cor.rho <- function(t,x1=0, x2=0) sqrt(1/2)
# coefficient matrix for diffusion term
diff.coef.matrix <- matrix( c( "diff.coef.1(t,x1,x2)", 
"diff.coef.2(t,x1,x2) * cor.rho(t,x1,x2)", "", 
"diff.coef.2(t,x1,x2) * sqrt(1-cor.rho(t,x1,x2)^2)"),2,2)
# Model SDE using yuima.model
cor.mod <- setModel(drift = c("",""), diffusion = diff.coef.matrix,
 solve.variable=c("x1","x2"))


CC.theta <- function( T, sigma1, sigma2, rho)
{
 	tmp <- function(t) return( sigma1(t) * sigma2(t) * rho(t) )
 	integrate(tmp,0,T)
}

set.seed(123)
Terminal <- 1
n <- 1000
# Cumulative Covariance
theta <- CC.theta(T=Terminal, sigma1=diff.coef.1, 
sigma2=diff.coef.2, rho=cor.rho)$value
cat(sprintf("theta=%5.3f\n",theta))

yuima.samp <- setSampling(Terminal=Terminal,n=n)
yuima <- setYuima(model=cor.mod, sampling=yuima.samp)
X <- simulate(yuima)
cce(X)
plot(X,main="complete data")

# RANDOM SAMPLING
p1 <- 0.2
p2 <- 0.3
newsamp <- setSampling(
 random=list(rdist=c( function(x) rexp(x, rate=p1*n/Terminal), 
  function(x) rexp(x, rate=p1*n/Terminal))) )
Y <- subsampling(X, sampling=newsamp)
cce(Y)
plot(Y,main="asynchronous data")

b1 <- function(x,y) y
b2 <- function(x,y) -x
s1 <- function(t,x,y) sqrt(abs(x)*(1+t))
s2 <- function(t,x,y) sqrt(abs(y))
cor.rho <- function(t,x,y) 1/(1+x^2)
diff.mat <- matrix(c("s1(t,x,y)", "s2(t,x,y) * cor.rho(t,x,y)","",
 "s2(t,x,y) * sqrt(1-cor.rho(t,x,y)^2)"), 2, 2) 
cor.mod <- setModel(drift = c("b1","b2"), diffusion = diff.mat,
solve.variable = c("x", "y"),state.var=c("x","y"))

## Generate a path of the process
set.seed(111) 
Terminal <- 1
n <- 10000
yuima.samp <- setSampling(Terminal = Terminal, n = n) 
yuima <- setYuima(model = cor.mod, sampling = yuima.samp) 
yuima <- simulate(yuima, xinit=c(2,3)) 


p1 <- 0.2
p2 <- 0.3
newsamp <- setSampling(
random=list(rdist=c( function(x) rexp(x, rate=p1*n/Terminal), 
function(x) rexp(x, rate=p1*n/Terminal))) )
Y <- subsampling(yuima, sampling = newsamp)
plot(Y,main="asynchronous data (non linear case)")
cce(yuima)
cce(Y)

# CHANGE POINT

diff.matrix <- matrix(c("theta1.k*x1","0*x2","0*x1","theta2.k*x2"), 2, 2)
drift.c <- c("sin(x1)", "3-x2")
drift.matrix <- matrix(drift.c, 2, 1)
ymodel <- setModel(drift=drift.matrix, diffusion=diff.matrix, 
time.variable="t", state.variable=c("x1", "x2"), 
solve.variable=c("x1", "x2"))

n <- 1000

set.seed(123)

t0 <- list(theta1.k=0.5, theta2.k=0.3)
T <- 10
tau <- 4
pobs <- tau/T
ysamp1 <- setSampling(n=n*pobs, Initial=0, delta=0.01)
yuima1 <- setYuima(model=ymodel, sampling=ysamp1)
yuima1 <- simulate(yuima1, xinit=c(3, 3), true.parameter=t0)

x1 <- yuima1@data@zoo.data[[1]]
x1 <- as.numeric(x1[length(x1)])
x2 <- yuima1@data@zoo.data[[2]]
x2 <- as.numeric(x2[length(x2)])

t1 <- list(theta1.k=0.2, theta2.k=0.4)
ysamp2 <- setSampling(Initial=n*pobs*0.01, n=n*(1-pobs), delta=0.01)
yuima2 <- setYuima(model=ymodel, sampling=ysamp2)
yuima2 <- simulate(yuima2, xinit=c(x1, x2), true.parameter=t1)
yuima <- yuima1
yuima@data@zoo.data[[1]] <- c(yuima1@data@zoo.data[[1]], 
yuima2@data@zoo.data[[1]][-1])
yuima@data@zoo.data[[2]] <- c(yuima1@data@zoo.data[[2]], 
yuima2@data@zoo.data[[2]][-1])
plot(yuima)



# Model without drift
noDriftModel <- setModel(drift=c("0", "0"), diffusion=diff.matrix,
 time.variable="t", state.variable=c("x1", "x2"), 
 solve.variable=c("x1", "x2"))
noDriftModel <- setYuima(noDriftModel, data=yuima@data)
noDriftModel@model@drift
t.est <- CPoint(yuima,param1=t0,param2=t1)
t.est$tau
t.est2 <- CPoint(noDriftModel,param1=t0,param2=t1)
t.est2$tau


# 2 STAGE ESTIMATION
qmleL(noDriftModel, t=2, start=list(theta1.k=0.1, theta2.k=0.1),
lower=list(theta1.k=0, theta2.k=0), upper=list(theta1.k=1, theta2.k=1), 
method="L-BFGS-B") -> estL
qmleR(noDriftModel, t=8, start=list(theta1.k=0.1, theta2.k=0.1),
lower=list(theta1.k=0, theta2.k=0), upper=list(theta1.k=1, theta2.k=1), 
method="L-BFGS-B") -> estR
t0.est <- coef(estL)
t1.est <- coef(estR)
t.est3 <- CPoint(noDriftModel,param1=t0.est,param2=t1.est)
t.est3

qmleL(noDriftModel, t=t.est3$tau, start=list(theta1.k=0.1, theta2.k=0.1),
lower=list(theta1.k=0, theta2.k=0), upper=list(theta1.k=1, theta2.k=1), 
method="L-BFGS-B") -> estL
qmleR(noDriftModel, t=t.est3$tau, start=list(theta1.k=0.1, theta2.k=0.1),
lower=list(theta1.k=0, theta2.k=0), upper=list(theta1.k=1, theta2.k=1), 
method="L-BFGS-B") -> estR
t02s.est <- coef(estL)
t12s.est <- coef(estR)
t2s.est3 <- CPoint(noDriftModel,param1=t02s.est,param2=t12s.est)
t2s.est3


# LASSO ESTIMATION

library("Ecdat")
data("Irates")
rates <- Irates[,"r1"]
plot(rates)
X <- window(rates, start=1964.471, end=1989.333)
mod <- setModel(drift="alpha+beta*x", diffusion=matrix("sigma*x^gamma",1,1))
yuima <- setYuima(data=setData(X), model=mod)
lambda10 <- list(alpha=10, beta =10, sigma =10, gamma =10)
start <- list(alpha=1, beta =-.1, sigma =.1, gamma =1)
low <- list(alpha=-5, beta =-5, sigma =-5, gamma =-5)
upp <- list(alpha=8, beta =8, sigma =8, gamma =8)

lasso10 <- lasso(yuima, lambda10, start=start, lower=low, upper=upp,
   method="L-BFGS-B")

round(lasso10$mle, 2)
round(lasso10$lasso, 2)



#######################################################
#######################################################
#######################################################
### Sec 7. Miscellanea and roadmap of YUIMA project ###
#######################################################
#######################################################
#######################################################




# Yuima 2 LaTeX
a <- c("-3*x1","-x1-2*x2")
b <- matrix(c("1","x1","0","3","x2","0"),2,3)
modtex <- setModel(drift = a, diffusion = b, solve.variable = c("x1","x2"))
toLatex(modtex)


