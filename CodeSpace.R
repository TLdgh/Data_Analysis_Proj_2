BayesianRegression<-setRefClass(
  "RegressionClass",
  fields = list(data="data.frame", responseV="character", predictorV="character", prior_sigma="numeric", prior_beta="list"),
  methods = list(
    initialize=function(data, responseV, predictorV, prior_sigma, prior_beta){
      .self$posteriorSample<-.self$PosteriorSample(data, responseV, predictorV, prior_sigma, prior_beta)
      .self$df_vline<-distinct(posteriorSample, labels, params_est)
    },
    
    PosteriorSample=function(data, responseV, predictorV, prior_sigma, prior_beta, sim_n=100000){
      # Get data matrix
      x<-data%>%select(all_of(predictorV))%>%mutate(Intercept=1, .before=1)%>%as.matrix()
      y<-data%>%select(all_of(responseV))%>%as.matrix(ncol=1)
      n<-nrow(x)
      
      # Set up priors
      a<-prior_sigma[1]
      b<-prior_sigma[2]
      m<-prior_beta[["m"]]
      V<-prior_beta[["V"]]
      
      # Calculate the parameters for the posterior
      mu<-solve(t(x)%*%x + solve(V))%*%(t(x)%*%y + solve(V)%*%m)
      CovM<-solve(t(x)%*%x + solve(V))
      a_star<-n/2+a
      b_star<-0.5*(t(m)%*%solve(V)%*%m -t(mu)%*%solve(CovM)%*%mu + t(y)%*%y +2*b)
      
      # Sample from the posterior
      sigma_sim<-1/rgamma(sim_n, shape=a_star, rate=b_star)
      #pairwise multiplication
      betas_sim<-sqrt(sigma_sim)*MASS::mvrnorm(n = sim_n, mu = rep(0, length(m)), CovM) + matrix(rep(c(mu), each = sim_n), ncol=length(m), byrow = FALSE)
      
      params<-data.frame(betas_sim, sigma_sim)
      colnames(params)<-c(colnames(x), "sigmasq")
      
      
      # Loss for betas: chosen to be the mean squared residual
      loss <- function(x, y){mean((y - x)^2)}
      bs <- params%>%select(-sigmasq)%>%apply(MARGIN = 2, function(x){optimize(function(y) loss(y, x), interval = c(min(x), max(x)))$minimum})
      
      # Loss for sigma: chosen to be the mean of the abs(residual)
      loss_sigma <- function(x, y) {mean(abs(y - x))}
      s <- params%>%select(sigmasq)%>%apply(MARGIN = 2, function(x){optimize(function(y) loss_sigma(y, x),interval = c(min(x), max(x)))$minimum}) 
      
      
      params_est <- data.frame(val=c(bs,s))%>%t()
      params<-gather(params, key="param", value="value")
      params$labels<-factor(params$param, labels =c(colnames(x), 'sigma^2'))
      params$params_est<-as.numeric(params_est[, match(params$param, colnames(params_est))])
      
      return(params)
    }
    
    
  )
)

BayesianRegression$new(train_data, c("LOSdays"), c("NumTransfers"), prior_sigma=c(8,10), prior_beta=list(m=c(0,0), V=matrix(c(10,3,4,6), nrow=2, byrow = TRUE)) )
