# Test Case
set.seed(100)
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
    res <-all(res%>%colMeans()%>%round(7)==data.frame(size=55, 
                                                      value=0.4121150,
                                                      posterior_old_mean=0.5239404,
                                                      posterior_new_mean= 0.4933655, 
                                                      posterior_old_se=0.0002347, 
                                                      posterior_new_se= 0.0002376))
    cat("calculateMean from BayesianSample Class executed successfully.", "\n")
    cat("Calculated params are expected", res, "\n")
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
set.seed(100)
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
    res<-all(RegressionTest$posteriorSample%>%group_by(param)%>%summarise(m=round(mean(value),7))%>%
               data.frame()%>%select(m) == data.frame(m=c(1.1260214, 0.7556874, 0.0614320)))
    cat("PosteriorSample from BayesianRegression Class executed successfully.", "\n")
    cat("Calculated params are expected", res, "\n")
  },
  error= function(err){
    cat("Error in Function PosteriorSample from BayesianRegression Class:", conditionMessage(err), "\n")
  }
)

tryCatch(
  expr = {
    RegressionTest <- BayesianRegression$new(train_data, "Response", "Predictor", c(10,0.5), list(m=c(0,0), V=matrix(c(10,3,4,6), nrow = length(m), byrow = TRUE)))
    res<-RegressionTest$PosteriorPlot()
    cat("PosteriorPlot from BayesianRegression Class executed successfully.", "\n")
  },
  error= function(err){
    cat("Error in Function PosteriorPlot from BayesianRegression Class:", conditionMessage(err), "\n")
  }
)

tryCatch(
  expr = {
    RegressionTest <- BayesianRegression$new(train_data, "Response", "Predictor", c(10,0.5), list(m=c(0,0), V=matrix(c(10,3,4,6), nrow = length(m), byrow = TRUE)))
    res<-RegressionTest$PredictionPlot(test_data)
    cat("PredictionPlot from BayesianRegression Class executed successfully.", "\n")
  },
  error= function(err){
    cat("Error in Function PosteriorPlot from BayesianRegression Class:", conditionMessage(err), "\n")
  }
)

# TestCate
test_cat <- data.frame(
  resp = c(rep(0,178),rep(0, 138),rep(0,108),rep(1, 570),rep(1, 648),rep(1,442),rep(2, 138),rep(2, 252),rep(2, 252)),
  Var1 = c(rep(0,178),rep(1, 138),rep(2,108),rep(0, 570),rep(1, 648),rep(2,442),rep(0, 138),rep(1, 252),rep(2, 252)))%>% 
  mutate(
    resp = factor(resp),
    Var1 = factor(Var1))

tryCatch(
  expr = {
    CategoryTest <- CategoryDependence$new(test_cat,predictor_x=c("Var1"), response_y="resp")
    res<-(CategoryTest$p_value[2,] == "69.16")
    cat("chi_square_test from CategoryDependence Class executed successfully.", "\n")
    cat("Calculated p-values are expected: ", res)
  },
  error= function(err){
    cat("Error in Function chi_square_test from CategoryDependence Class:", conditionMessage(err), "\n")
  }
)