# Test Case
old_distinct<-data.frame(Id=1:100, converted=rbinom(100, 1, 0.52))
new_distinct<-data.frame(Id=1:100, converted=rbinom(100, 1, 0.483))

SampleTest <- BayesianSample$new(old_distinct=old_distinct, new_distinct=new_distinct,rows_per_subset = 10,
                           alpha_prior_old=1,
                           alpha_prior_new=1,
                           beta_prior_old=1,
                           beta_prior_new=1)

tryCatch(
  expr = {
    res <-SampleTest$calculateMean()
    cat("calculateMean from BayesianSample Class executed successfully.", "\n")
  },
  error= function(err){
      cat("Error in Function calculateMean from BayesianSample Class:", conditionMessage(err), "\n")
    }
)

tryCatch(
  expr = {
    res <-SampleTest$PlotCI()
    cat("PlotCI from BayesianSample Class executed successfully.", "\n")
  },
  error= function(err){
    cat("Error in Function PlotCI from BayesianSample Class:", conditionMessage(err), "\n")
  }
)

# True model
a<-10
b<-0.5
sigmasq<-1/rgamma(1, shape=a, rate=b)

m<-c(0,0)
V<-matrix(c(10,3,4,6), nrow = length(m), byrow = TRUE)
betas<-MASS::mvrnorm(n=1, mu=m, Sigma = sigmasq*V)

params_true <- data.frame(beta0=betas[1], beta1=betas[2], sigmasq=sigmasq)

n<-1000
x<-matrix(c(rep(1,n), rnorm(n, mean = 12.5, sd=2)), ncol = length(m), byrow = FALSE)
e<-rnorm(n, mean=0, sd=sqrt(sigmasq) )
y<-x%*%betas+e 

train_data<-data.frame(Response=y, Predictor=x[,2])
test_data<-data.frame(Response=y*runif(1, 0.8, 0.9), Predictor=x[,2]*runif(1, 0.7, 0.8))

tryCatch(
  expr = {
    RegressionTest <- BayesianRegression$new(train_data, "Response", "Predictor", c(10,0.5), list(m=c(0,0), V=matrix(c(10,3,4,6), nrow = length(m), byrow = TRUE)))
    cat("PosteriorSample from BayesianRegression Class executed successfully.", "\n")
  },
  error= function(err){
    cat("Error in Function PosteriorSample from BayesianRegression Class:", conditionMessage(err), "\n")
  }
)

tryCatch(
  expr = {
    RegressionTest <- BayesianRegression$new(data, "Response", "Predictor", c(10,0.5), list(m=c(0,0), V=matrix(c(10,3,4,6), nrow = length(m), byrow = TRUE)))
    res<-RegressionTest$PosteriorPlot()
    cat("PosteriorPlot from BayesianRegression Class executed successfully.", "\n")
  },
  error= function(err){
    cat("Error in Function PosteriorPlot from BayesianRegression Class:", conditionMessage(err), "\n")
  }
)

tryCatch(
  expr = {
    RegressionTest <- BayesianRegression$new(data, "Response", "Predictor", c(10,0.5), list(m=c(0,0), V=matrix(c(10,3,4,6), nrow = length(m), byrow = TRUE)))
    res<-RegressionTest$PredictionPlot(test_data)
    cat("PredictionPlot from BayesianRegression Class executed successfully.", "\n")
  },
  error= function(err){
    cat("Error in Function PosteriorPlot from BayesianRegression Class:", conditionMessage(err), "\n")
  }
)

