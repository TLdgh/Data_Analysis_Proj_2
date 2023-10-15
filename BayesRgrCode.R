set.seed(100)


# Simulate data by giving true parameters
a<-10
b<-0.5
sigmasq<-1/rgamma(1, shape=a, rate=b)

m<-c(0,0,5)
V<-matrix(c(10,0,3, 0,4,6, 2,5,10), nrow = length(m), byrow = TRUE)
betas<-MASS::mvrnorm(n=1, mu=m, Sigma = sigmasq*V)

params_true <- data.frame(beta0=betas[1], beta1=betas[2], beta2=betas[3], sigmasq=sigmasq)


n<-2000
x<-matrix(c(rep(1,n), rnorm(n, mean = 12.5, sd=2), rbeta(n, 12.5, 2)), ncol = length(m), byrow = FALSE)
e<-rnorm(n, mean=0, sd=sqrt(sigmasq) )
y<-x%*%betas+e # This is the true model


# The posterior distribution sampling function
PosteriorSample<-function(x, y, a, b, m, V, sim_n=100000){
  #Get the priors
  sigmasq<-1/rgamma(1, shape=a, rate=b) #the variance
  betas<-MASS::mvrnorm(n=1, mu=m, Sigma = sigmasq*V) #the betas
  
  
  # Calculate the parameters for the posterior
  mu<-solve(t(x)%*%x + solve(V))%*%(t(x)%*%y + solve(V)%*%m)
  CovM<-solve(t(x)%*%x + solve(V))
  a_star<-n/2+a
  b_star<-0.5*(t(m)%*%solve(V)%*%m -t(mu)%*%solve(CovM)%*%mu + t(y)%*%y +2*b)
  
  
  # Sample from the posterior
  sigma_sim<-1/rgamma(sim_n, shape=a_star, rate=b_star) #simulate variance
  #pairwise multiplication
  betas_sim<-sqrt(sigma_sim)*MASS::mvrnorm(n = sim_n, mu = rep(0, length(m)), CovM) + matrix(rep(c(mu), each = sim_n), ncol=length(m), byrow = FALSE)
  
  params<-data.frame(betas_sim, sigma_sim)
  colnames(params)<-c(paste0("beta", seq(0,length(m)-1)), "sigmasq")
  
  params<-gather(params, key="param", value="value")
  params$labels<-factor(params$param, labels =c(paste0("beta", seq(0,length(m)-1)), 'sigma^2'))
  
  return(params)
}

#under correct model specification
params_sim_correct<- PosteriorSample(x, y, a=10, b=0.5, m=c(0,0,5), V=matrix(c(10,0,3, 0,4,6, 2,5,10), nrow = ncol(x), byrow = TRUE) ) 
params_sim_correct$true_param <- as.numeric(params_true[, match(params_sim_correct$param, names(params_true))])
df_vline<-distinct(params_sim_correct, labels, true_param)

p1<-params_sim_correct%>%ggplot(aes(x=value)) +
  geom_histogram(aes(y=..density..), bins = 50, fill="skyblue", color="black") +
  geom_density(colour='blue', linewidth = 0.5) +
  geom_vline(data=df_vline, aes(xintercept = true_param), linetype = 'dashed', size = 1)+
  geom_text(data=df_vline, aes(x= true_param, label = paste("True Param:", round(true_param,4)), y=Inf),size = 4,hjust=0, vjust=2) + 
  facet_wrap(~labels, scales = 'free',labeller = label_parsed) + theme_bw() +
  labs(x = 'Parameters', y = 'Density',title = 'Posterior Density')


#under incorrect model specification
params_sim_incorrect<- PosteriorSample(x, y, a=2, b=10, m=c(-2,-10, 28), V=matrix(c(100,10,37, 20,47,61, 2,58,150), nrow=ncol(x), byrow = TRUE) ) 
params_sim_incorrect<- params_sim_incorrect%>%mutate(true_param = case_when(
  param == "beta0" ~ params_true$beta0,
  param == "beta1" ~ params_true$beta1,
  param == "sigmasq" ~ params_true$sigmasq
))

p2<-params_sim_incorrect%>%ggplot(aes(x=value)) +
  geom_histogram(aes(y=..density..), bins = 50, fill="skyblue", color="black") +
  geom_density(colour='blue', linewidth = 0.5) +
  geom_vline(data=df_vline, aes(xintercept = true_param), linetype = 'dashed', size = 1)+
  geom_text(data=df_vline, aes(x= true_param, label = paste("True Param:", round(true_param,4)), y=Inf),size = 4,hjust=0, vjust=2) + 
  facet_wrap(~labels, scales = 'free',labeller = label_parsed) + theme_bw() +
  labs(x = 'Parameters', y = 'Density',title = 'Posterior Density')


print(p1)
print(p2)

