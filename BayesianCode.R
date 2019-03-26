source("analset.R")

filename<-datasetname
path1<-paste(path,filename,sep="")
data <- read.csv(file=path1, header=TRUE)

refvar<-data[[refvarname]]
testvar<-data[[testvarname]]

testvarcut<-c()
testvarcut[testvar<=5]<-0
testvarcut[testvar>5]<-1

length(refvar)
length(testvarcut)
testvarcut

table(refvar,testvarcut)
refvar<-refvar[testvarcut==1]
sum(refvar)


# David Foxcroft routines

prev<-0.085
#score #10
ypfit<-163.1019
ynfit<-1147.08
dp<-2391.951
dn<-25222.2


#P(B|A) AND ERRORs
sdnp<-sqrt((ypfit*(dp-ypfit)/dp))
sdnn<-sqrt((ynfit*(dn-ynfit/dn))
z<-(ypfit/dp*prev)/(ypfit/dp*prev+ynfit/dn*(1-prev))


tprev <- diagtable_out$tprev
tseprev <- diagtable_out$tseprev

tprev<-.13
        
tseprev<-.025
           
           

######## Avril Coghlan code https://github.com/avrilcoghlan/LittleBookofRBayesianStatistics/blob/master/src/bayesianstats.rst
quantile1 <- list(p=0.5, x=tprev)    # we believe the median of the prior is 0.85
quantile2 <- list(p=0.995,x=tprev+(tseprev*2.58)) # we believe the 99.5 percentile of the prior is ...
quantile3 <- list(p=0.05,x=tprev-(tseprev*2.58)) # we believe the 0.05 percentile of the prior is  ... 


findBeta <- function(quantile1,quantile2,quantile3)
{
  # find the quantiles specified by quantile1 and quantile2 and quantile3
  quantile1_p <- quantile1[[1]]; quantile1_q <- quantile1[[2]]
  quantile2_p <- quantile2[[1]]; quantile2_q <- quantile2[[2]]
  quantile3_p <- quantile3[[1]]; quantile3_q <- quantile3[[2]]
  
  # find the beta prior using quantile1 and quantile2
  priorA <- beta.select(quantile1,quantile2)
  priorA_a <- priorA[1]; priorA_b <- priorA[2]
  
  # find the beta prior using quantile1 and quantile3
  priorB <- beta.select(quantile1,quantile3)
  priorB_a <- priorB[1]; priorB_b <- priorB[2]
  
  # find the best possible beta prior
  diff_a <- abs(priorA_a - priorB_a); diff_b <- abs(priorB_b - priorB_b)
  step_a <- diff_a / 100; step_b <- diff_b / 100
  if (priorA_a < priorB_a) { start_a <- priorA_a; end_a <- priorB_a }
  else                     { start_a <- priorB_a; end_a <- priorA_a }
  if (priorA_b < priorB_b) { start_b <- priorA_b; end_b <- priorB_b }
  else                     { start_b <- priorB_b; end_b <- priorA_b }
  steps_a <- seq(from=start_a, to=end_a, length.out=1000)
  steps_b <- seq(from=start_b, to=end_b, length.out=1000)
  max_error <- 10000000000000000000
  best_a <- 0; best_b <- 0
  for (a in steps_a)
  {
    for (b in steps_b)
    {
      # priorC is beta(a,b)
      # find the quantile1_q, quantile2_q, quantile3_q quantiles of priorC:
      priorC_q1 <- qbeta(c(quantile1_p), a, b)
      priorC_q2 <- qbeta(c(quantile2_p), a, b)
      priorC_q3 <- qbeta(c(quantile3_p), a, b)
      priorC_error <- abs(priorC_q1-quantile1_q) +
        abs(priorC_q2-quantile2_q) +
        abs(priorC_q3-quantile3_q)
      if (priorC_error < max_error)
      {
        max_error <- priorC_error; best_a <- a; best_b <- b
      }
    }
  }
  #print(paste("The best beta prior has a=",best_a,"b=",best_b))
  beta_out<-list(best_a=best_a,best_b=best_b)
}

           
#library("LearnBayes")
#beta_out<-findBeta(quantile1,quantile2,quantile3)
#curve(dbeta(x, beta_out$best_a, beta_out$best_b)) # plot the prior

calcPosteriorForProportion <- function(successes, total, a, b)
{
  # Adapted from triplot() in the LearnBayes package
  # Plot the prior, likelihood and posterior:
  likelihood_a = successes + 1; likelihood_b = total - successes + 1
  posterior_a = a + successes;  posterior_b = b + total - successes
  theta = seq(0.005, 0.995, length = 500)
  prior = dbeta(theta, a, b)
  likelihood = dbeta(theta, likelihood_a, likelihood_b)
  posterior  = dbeta(theta, posterior_a, posterior_b)
  m = max(c(prior, likelihood, posterior))
  #plot(theta, posterior, type = "l", ylab = "Density", lty = 2, lwd = 3,
#       main = paste("beta(", a, ",", b, ") prior, B(", total, ",", successes, ") data,",
 #                   "beta(", posterior_a, ",", posterior_b, ") posterior"), ylim = c(0, m), col = "red")
  #lines(theta, likelihood, lty = 1, lwd = 3, col = "blue")
  #lines(theta, prior, lty = 3, lwd = 3, col = "green")
  #legend(x=0.8,y=m, c("Prior", "Likelihood", "Posterior"), lty = c(3, 1, 2),
   #      lwd = c(3, 3, 3), col = c("green", "blue", "red"))
  # Print out summary statistics for the prior, likelihood and posterior:
  calcBetaMode <- function(aa, bb) { BetaMode <- (aa - 1)/(aa + bb - 2); return(BetaMode); }
  calcBetaMean <- function(aa, bb) { BetaMean <- (aa)/(aa + bb); return(BetaMean); }
  calcBetaSd   <- function(aa, bb) { BetaSd <- sqrt((aa * bb)/(((aa + bb)^2) * (aa + bb + 1))); return(BetaSd); }
  prior_mode      <- calcBetaMode(a, b)
  likelihood_mode <- calcBetaMode(likelihood_a, likelihood_b)
  posterior_mode  <- calcBetaMode(posterior_a, posterior_b)
  prior_mean      <- calcBetaMean(a, b)
  likelihood_mean <- calcBetaMean(likelihood_a, likelihood_b)
  posterior_mean  <- calcBetaMean(posterior_a, posterior_b)
  prior_sd        <- calcBetaSd(a, b)
  likelihood_sd   <- calcBetaSd(likelihood_a, likelihood_b)
  posterior_sd    <- calcBetaSd(posterior_a, posterior_b)
  #print(paste("mode for prior=",prior_mode,", for likelihood=",likelihood_mode,", for posterior=",posterior_mode))
  #print(paste("mean for prior=",prior_mean,", for likelihood=",likelihood_mean,", for posterior=",posterior_mean))
  #print(paste("sd for prior=",prior_sd,", for likelihood=",likelihood_sd,", for posterior=",posterior_sd))
  
  calcPosteriorForProportion_out <- list(posterior_mode=posterior_mode,posterior_mean=posterior_mean,posterior_sd=posterior_sd)
}

#calcPosteriorForProportion(pos, postot, beta_out$best_a, beta_out$best_b)




############# http://stats.stackexchange.com/questions/12232/calculating-the-parameters-of-a-beta-distribution-using-the-mean-and-variance


estBetaParams <- function(mu, var) {
  alpha <- ((1 - mu) / var - 1 / mu) * mu ^ 2
  beta <- alpha * (1 / mu - 1)
  return(params = list(a = alpha, b = beta))
}

prior.dist = estBetaParams(.14,.02)



############### http://www.r-bloggers.com/the-beta-prior-likelihood-and-posterior/

##########################################################
## Take a look at only the prior
##########################################################
curve(dbeta(x,prior.dist$a,prior.dist$b)) # plot the prior
abline(v=Q$prior[1])





##########################################################
## Take a look at only the likelihood with given successes
##########################################################
calcLikelihood = function(successes, total){
  curve(dbinom(successes,total,x)) # plot the likelihood
}

#table(refvar)

calcLikelihood(sum(refvar), length(refvar)) ## e.g. 45/50 sucessescalc

## calculate some properties of the Beta distribution
calcBetaMode = function(aa, bb) {
  beta.mode = (aa - 1)/(aa + bb - 2)
  return(beta.mode)
}
calcBetaMean = function(aa, bb) {
  beta.mean = (aa)/(aa + bb)
  return(beta.mean)
}
calcBetaVar = function(aa, bb) {
  beta.var = (aa * bb)/(((aa + bb)^2) * (aa + bb + 1))
  return(beta.var)
}
calcBetaMedian = function(aa, bb) {
  beta.med = (aa-1/3)/(aa+bb-2/3)
  return(beta.med)
}
calcBetaSkew = function(aa, bb) {
  beta.skew = ( 2*(bb-aa)*sqrt(aa+bb+1) ) /( (aa+bb+2)/sqrt(aa+bb) )
  return(beta.skew)
}

##########################################################
## Take a look at the prior, likelihood, and posterior
##########################################################
priorToPosterior = function(successes, total, a, b) {
  ## Note the rule of succession
  likelihood.a = successes + 1
  likelihood.b = total - successes + 1
  
  ## Create posterior
  posterior.a = a + successes;
  posterior.b = b + total - successes
  theta = seq(0.005, 0.995, length = 500)
  
  ## Calc density
  prior = dbeta(theta, a, b)
  likelihood = dbeta(theta, likelihood.a, likelihood.b)
  posterior = dbeta(theta, posterior.a, posterior.b)
  
  ## Plot prior, likelihood, and posterior
  
  ## Can be used to scale down the graph if desired.
  ## However, the density is different for each prior, likelihood, posterior
  m.orig = apply( cbind(prior, likelihood, posterior), 2, max)
  m = max(c(prior, likelihood, posterior))
  
  plot(theta, posterior, type = "l", ylab = "Density", lty = 2, lwd = 3,
       main = paste("Prior: beta(", round(a,2), ",", round(b,2), "); Data: B(", total, ",", successes, "); ",
                    "Posterior: beta(", round(posterior.a,2), ",", round(posterior.b,2), ")", sep=""), ylim = c(0, m), col = 1)
  lines(theta, likelihood, lty = 1, lwd = 3, col = 2)
  lines(theta, prior, lty = 3, lwd = 3, col = 3)
  legend("topleft",y=m, c("Prior", "Likelihood", "Posterior"), lty = c(3, 1, 2),
         lwd = c(3, 3, 3), col = c(3, 2, 1))
  
  prior.mode = calcBetaMode(a, b)
  likelihood.mode = calcBetaMode(likelihood.a, likelihood.b)
  posterior.mode = calcBetaMode(posterior.a, posterior.b)
  prior.mean = calcBetaMean(a, b)
  likelihood.mean = calcBetaMean(likelihood.a, likelihood.b)
  posterior.mean = calcBetaMean(posterior.a, posterior.b)
  prior.med = calcBetaMedian(a, b)
  likelihood.med = calcBetaMedian(likelihood.a, likelihood.b)
  posterior.med = calcBetaMedian(posterior.a, posterior.b)
  prior.var = calcBetaVar(a, b)
  likelihood.var = calcBetaVar(likelihood.a, likelihood.b)
  posterior.var = calcBetaVar(posterior.a, posterior.b)
  prior.skew = calcBetaSkew(a, b)
  likelihood.skew = calcBetaSkew(likelihood.a, likelihood.b)
  posterior.skew = calcBetaSkew(posterior.a, posterior.b)
  
  print(paste("Mode: prior=",prior.mode,"; Likelihood=",likelihood.mode,"; Posterior=",posterior.mode))
  print(paste("Mean: prior=",prior.mean,"; Likelihood=",likelihood.mean,"; Posterior=",posterior.mean))
  print(paste("~Approx Median (for a and b > 1): prior=",prior.med,"; Likelihood=",likelihood.med,", for Posterior=",posterior.med))
  print(paste("Var: prior=",prior.var,"; Likelihood=", likelihood.var,"; Posterior=",posterior.var))
  print(paste("Skewness: prior=",prior.skew,"; Likelihood=",likelihood.skew,"; Posterior=",posterior.skew))
  return(list(a=posterior.a,b=posterior.b))
}
#table(refvar)
posterior.out = priorToPosterior(210,length(refvar), prior.dist$a, prior.dist$b) # 25/50 is current data
beta.sim = rbeta(1000000,posterior.out$a, posterior.out$b)
abline(v=quantile(beta.sim, prob=c(.05/2, 1-.05/2)), col='#000000', lwd=2)
abline(v=quantile(beta.sim, prob=c(.01/2, 1-.01/2)), col='#EEEEEE', lwd=2)



############# http://www.biostat.jhsph.edu/~pmurakam/epi_tests.html
################################################################################
## Copyright (C) 2010 Peter Murakami <pmurakam@jhsph.edu>
## 
## This program is free software: you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation, either version 3 of the License, or
## (at your option) any later version.
## 
## This program is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
## 
## You should have received a copy of the GNU General Public License
## along with this program.  If not, see <http://www.gnu.org/licenses/>.
################################################################################

epi.tests.bayes <- function(x,y,level=0.95,dig=3, method="hpd", a=1, b=1, plotDensities=TRUE) { 
  ## Checking and preprocessing:
  if(!inherits(level,c("numeric","integer"))) stop("level must be number between 0 and 1.")
  if(level<=0 | level>=1) stop("level must be number between 0 and 1.")
  if(!is.vector(x) | !is.vector(y)) stop("x and y must be logical vectors or a numeric vectors of zeros for FALSE and 1 for TRUE.")  
  if(length(x)!=length(y))  stop("lengths of x and y differ.")
  if(inherits(x,c("character","factor"))) stop("x must be 0,1 or FALSE,TRUE.")
  if(inherits(y,c("character","factor"))) stop("y must be 0,1 or FALSE,TRUE.")
  rm = is.na(x) | is.na(y)
  x  = x[!rm]
  y  = y[!rm]    
  if(is.numeric(x) | is.integer(x)){ if(!all(x%in%c(0,1))) stop("x must be 0,1 or FALSE,TRUE.")}
  if(is.integer(y) | is.integer(y)){ if(!all(y%in%c(0,1))) stop("y must be 0,1 or FALSE,TRUE.")}
  y = as.logical(y)
  x = as.numeric(x)
  
  ## Calculations:
  sens =  x[y]           #mean of this is the sensitivity estimate
  spec = !x[!y]          #mean of this is the specificity estimate
  dacc = c(x[y],!x[!y])  #mean of this is the diagnostic accuracy estimate
  
  if(plotDensities) {
      par(mfrow=c(1,3), mar=c(5,4,4,.6))
      plotDensities(v=sens,a=a,b=b,s=s,f=f,main="sensitivity",ylab="Density")
      plotDensities(v=spec,a=a,b=b,s=s,f=f,main="specificity",ylab="")
      plotDensities(v=dacc,a=a,b=b,s=s,f=f,main="diagnostic accuracy",ylab="")        
  }
  
  if(method=="hpd") {
    require(TeachingDemos)
    sensit = hpd(qbeta, shape1=sum(sens)+1, shape2=sum(!sens)+1, conf=level)
    specif = hpd(qbeta, shape1=sum(spec)+1, shape2=sum(!spec)+1, conf=level)
    diagac = hpd(qbeta, shape1=sum(dacc)+1, shape2=sum(!dacc)+1, conf=level)        
  } else {
    sensit = qbeta(p=c((1-level)/2,1-(1-level)/2), shape1=sum(sens)+1, shape2=sum(!sens)+1)
    specif = qbeta(p=c((1-level)/2,1-(1-level)/2), shape1=sum(spec)+1, shape2=sum(!spec)+1)
    diagac = qbeta(p=c((1-level)/2,1-(1-level)/2), shape1=sum(dacc)+1, shape2=sum(!dacc)+1)
  }
  
  sensit = c(sensit[1],mean(sens),sensit[2],length(sens))
  specif = c(specif[1],mean(spec),specif[2],length(spec))
  diagac = c(diagac[1],mean(dacc),diagac[2],length(dacc))
  out = round(rbind(sensit,specif,diagac), dig)
  colnames(out) = c("lower","estimate","upper","n")
  rownames(out) = c("sensitivity","specificity","diagnostic_accuracy")
  out
}

plotDensities <- function(v,a,b,s,f,...) {
  p = seq(0,1,length=600)
  s = sum(v)
  f = sum(!v)
  prior = dbeta(p,a,b)
  like  = dbeta(p,s+1,f+1)
  post  = dbeta(p,a+s,b+f)
  plot(p,like,type="l",lty=1,lwd=3,...)
  lines(p,prior,lty=3,lwd=3)
  lines(p,post,lty=2,lwd=3,col="red")
  rug(mean(v))
  legend("topright",c("Prior","Likelihood","Posterior"), lty=c(3,1,2), lwd=c(3,3,3), col=c("black","black","red"))
}

epi.tests.bayes(refvar,testvarcut,a=prior.dist$a,b=prior.dist$b)








############# https://github.com/SupplyFrame/EmpiricalBayesR



BetaBinoMLE = function (success, trials, start = NULL, optim.method = "default", 
                        lower = 0, upper = Inf) {
  #################################################################################
  # MLE estimate of Beta-Binomial parameters
  #
  # Args:  
  #      success: vector of #success; trials:=vector of #trials; 
  #      start: initial parameters (must be a list with name shape1, shape2)
  #      optim.method: optimization methods in optim(){stats}
  #      lower(upper): lower(upper) bound for parameters
  #
  # Returns: 
  #         $estimate: MLE estimate for beta parameters   
  #         $convergence: convergence code from optim(). 0 means good.
  #         $loglik: Loglikelihood with estimated parameters
  #         $starting: initial parameters from the method of moments
  #
  # Dependent package: VGAM
  # 
  # Note: The structure of the function heavily relies on 
  #       mledist(){fitdistrplus} by Marie Laure Delignette-Muller.
  #
  # Summer2013 @Supplyframe
  #################################################################################
  
  if(!is.element("VGAM", installed.packages()[,1])){
    stop("Please install and load package VGAM before using this function.")
  }
  
  require(VGAM)
  
  distname <- "betabinom.ab"
  ddistname <- paste("d", distname, sep = "")
  
  if (is.null(start)) {
    if (distname == "betabinom.ab") {
      if (any(success/trials < 0) | any(success/trials > 1)) {
        stop("Proportion must be in [0-1] to fit a betabinom distribution")
      } 
      start.mu <- mean(success/trials)
      start.var <- var(success/trials)
      start.a <- ((1 - start.mu) / start.var - 1 / start.mu) * start.mu ^ 2
      start.b <- start.a * (1 / start.mu - 1)
      start <- list(shape1 = start.a, shape2 = start.b)
    } 
    if (!is.list(start)) 
      stop("'start' must be defined as a named list for this distribution")
  }
  
  vstart <- unlist(start)
  argddistname <- names(formals(ddistname))
  m <- match(names(start), argddistname)
  
  if (any(is.na(m)) || length(m) == 0) 
    stop("'start' must specify names which are arguments to 'distr'")
  
  fnobj <- function(par, x, n, ddistnam) {
    -sum(do.call(ddistnam, c(list(x), list(n), par, log = TRUE)))
  }
  
  if (optim.method == "default") {
    if (is.infinite(lower) && is.infinite(upper)) {
      if (length(vstart) > 1) 
        meth <- "Nelder-Mead"
      else meth <- "BFGS"
    }
    else meth <- "L-BFGS-B"
  }
  else meth <- optim.method
  
  opttryerror <- try(opt <- optim(par = start, fn = fnobj, 
                                  x = success, n = trials, ddistnam = ddistname, 
                                  hessian = TRUE, method = meth, lower = lower, 
                                  upper = upper), silent = TRUE)
  
  if (inherits(opttryerror, "try-error")) {
    warnings("The function optim encountered an error and stopped")
    print(opttryerror)
    return(list(estimate = rep(NA, length(vstart)), convergence = 100, 
                loglik = NA, hessian = NA))
  }
  if (opt$convergence > 0) {
    warnings("The function optim failed to converge, with the error code ", 
             opt$convergence)
    return(list(estimate = rep(NA, length(vstart)), convergence = opt$convergence, 
                loglik = NA, hessian = NA, message = opt$message))
  }
  
  res <- list(estimate = opt$par, convergence = opt$convergence, 
              loglik = -opt$value, initial = vstart)
  
  return(res)
}



EbPropEstor = function(success, trials){
  # Empirical Bayes estimator for binomial proportion
  # 
  # Args: success: a vector of #success
  #       trials: a vector of #trials
  #        
  # Returns: a vector of estimated proportion.
  #        
  # Dependent Function: BetaBinoMLE()
  # Summer2013 @Supplyframe
  
  est = BetaBinoMLE(success, trials)$estimate
  a = est[1]
  b = est[2]
  proportion = (a + success)/(a + b + trials)
  return(proportion)  
  
}

table(refvar,testvarcut)
EbPropEstor(success=refvar,trials=length(refvar))   




############# http://cran.r-project.org/web/packages/LearnBayes/LearnBayes.pdf

# person believes the median of the prior is 0.25
# and the 90th percentile of the prior is 0.45
require(LearnBayes)
quantile1=list(p=.5,x=0.14)
quantile2=list(p=.9,x=0.20)
beta.select(quantile1,quantile2)


############## http://cran.r-project.org/web/packages/binom/binom.pdf
require(binom)
binom.bayes(x = refvar, n = length(refvar), prior.shape1=9.66, prior.shape2=57.63, tol = 1e-9)

length(refvar)


############### http://www.r-bloggers.com/the-beta-prior-likelihood-and-posterior/

library(LearnBayes)
Q = data.frame(
  quantile=c(
    median=0.5,
    maximum=0.99999,
    minimum=0.00001),
  prior=c(
    median=0.14,
    maximum=0.20,
    minimum=0.08)
)

optimalBeta = function(Q) {
  q1q = Q$quantile[1]
  q1p = Q$prior[1]
  q2q = Q$quantile[2]
  q2p = Q$prior[2]
  q3q = Q$quantile[3]
  q3p = Q$prior[3]
  
  # find the beta prior using quantile1 and quantile2
  q.med = list(p=q1q, x=q1p)
  q.max = list(p=q2q, x=q2p)
  q.min = list(p=q3q, x=q3p)
  
  # prior parameters using median and max, and median and min
  prior.A = beta.select(q.med,q.max)
  prior.B = beta.select(q.med,q.min)
  
  prior.Aa = prior.A[1]
  prior.Ab = prior.A[2]
  
  prior.Ba = prior.B[1]
  prior.Bb = prior.B[2]
  
  ## find the best possible beta prior
  ## Set a start and stop point range to find the best parameters
  if (prior.Aa < prior.Ba) {
    start.a = prior.Aa
    stop.a = prior.Ba
  } else {
    start.a = prior.Ba
    stop.a = prior.Aa
  }
  
  if (prior.Ab < prior.Bb) {
    start.b = prior.Ab
    stop.b = prior.Bb
  } else {
    start.b = prior.Bb
    stop.b = prior.Ab
  }
  seq.a = seq(from=start.a, to=stop.a, length.out=1000)
  seq.b = seq(from=start.b, to=stop.b, length.out=1000)
  
  seq.grid = expand.grid(seq.a, seq.b)
  
  prior.C.q1 = qbeta(q1q, seq.grid[,1], seq.grid[,2])
  prior.C.q2 = qbeta(q2q, seq.grid[,1], seq.grid[,2])
  prior.C.q3 = qbeta(q3q, seq.grid[,1], seq.grid[,2])
  
  ## Different distance measurements, manhattan, euclidean, or otherwise.
  ## It would be interesting to run a simulation to measure a variety of distance measurements.
  prior.C.delta = abs(prior.C.q1 - q1p) + abs(prior.C.q2 - q2p) + abs(prior.C.q3 - q3p)
  ## prior.C.delta = sqrt( (prior.C.q1 - q1p)^2 + (prior.C.q2 - q2p)^2 + (prior.C.q3 - q3p)^2 )
  
  optimize.seq = cbind(seq.grid, prior.C.q1, prior.C.q2, prior.C.q3, prior.C.delta)
  
  ## Minimize the delta, if the min-delta is not unique then choose the first occurence
  best.a = optimize.seq[,1][ optimize.seq[,6]==min(optimize.seq[,6])][1]
  best.b = optimize.seq[,2][ optimize.seq[,6]==min(optimize.seq[,6])][1]
  
  return(list(a=best.a,b=best.b))
}

prior.dist = optimalBeta(Q)

##########################################################
## Take a look at only the prior
##########################################################
curve(dbeta(x,prior.dist$a,prior.dist$b)) # plot the prior
abline(v=Q$prior[1])





