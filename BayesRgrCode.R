set.seed(100)


# Simulate data by giving true parameters
a<-10
b<-0.5
sigmasq<-1/rgamma(1, shape=a, rate=b)

m<-c(0,0)
V<-matrix(c(10,3,4,6), nrow = length(m), byrow = TRUE)
betas<-MASS::mvrnorm(n=1, mu=m, Sigma = sigmasq*V)

params_true <- data.frame(beta0=betas[1], beta1=betas[2], sigmasq=sigmasq)


n<-40000
x<-matrix(c(rep(1,n), rnorm(n, mean = 12.5, sd=2)), ncol = length(m), byrow = FALSE)
e<-rnorm(n, mean=0, sd=sqrt(sigmasq) )
y<-x%*%betas+e # This is the true model


# The posterior distribution sampling function
PosteriorSample<-function(x, y, a, b, m, V, sim_n=100000){
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
  
  # Loss for betas: chosen to be the mean squared residual
  loss <- function(x, y){mean((y - x)^2)}
  bs <- params%>%select(-sigmasq)%>%apply(MARGIN = 2, function(x){optimize(function(y) loss(y, x), interval = c(min(x), max(x)))$minimum})
  
  # Loss for sigma: chosen to be the mean of the abs(residual)
  loss_sigma <- function(x, y) {mean(abs(y - x))}
  s <- params%>%select(sigmasq)%>%apply(MARGIN = 2, function(x){optimize(function(y) loss_sigma(y, x),interval = c(min(x), max(x)))$minimum}) 
  
  
  params_est <- data.frame(val=c(bs,s))%>%t()
  params<-gather(params, key="param", value="value")
  params$labels<-factor(params$param, labels =c(paste0("beta", seq(0,length(m)-1)), 'sigma^2'))
  
  params$true_param<-as.numeric(params_est[, match(params$param, colnames(params_est))])
  
  return(params)
}

#under correct model specification
inds<-1:32000
x_train<-x[inds,]
y_train<-y[inds,]
x_test<-x[-inds,]
y_test<-y[-inds,]

params_sim_correct<- PosteriorSample(x, y, a=10, b=0.5, m=c(0,0), V=matrix(c(10,3,4,6), nrow = length(m), byrow = TRUE)) 
df_vline<-distinct(params_sim_correct, labels, true_param)

p1<-params_sim_correct%>%ggplot(aes(x=value)) +
  geom_histogram(aes(y=..density..), bins = 50, fill="skyblue", color="black") +
  geom_density(colour='blue', linewidth = 0.5) +
  geom_vline(data=df_vline, aes(xintercept = true_param), linetype = 'dashed', size = 1)+
  geom_text(data=df_vline, aes(x= true_param, label = paste("True Param:", round(true_param,4)), y=Inf),size = 4,hjust=0, vjust=2) + 
  facet_wrap(~labels, scales = 'free',labeller = label_parsed) + theme_bw() +
  labs(x = 'Parameters', y = 'Density',title = 'Posterior Density')
print(p1)

#Prediction
betas_star<-params_sim_correct%>%group_by(param)%>%summarise(value=mean(value))%>%select(value)%>%as.matrix()

y_star<-x%*%betas_star[1:2]
p3<-data.frame(y=y_test, y_star=as.numeric(y_star))%>%gather(key="data", value = "value")%>%
  ggplot(aes(value, col=data))+
  geom_histogram(aes(y=..count..), bins=50)
ggplotly(p3)

data.frame(y=y, y_star=as.numeric(y_star))%>%gather(key="data", value = "value")%>%
  plot_ly(x = ~value, color=~data, type = "histogram", nbinsx=70)%>% 
  layout(
    title = "Histogram Plot",
    xaxis = list(title = "X-Axis Label"),
    yaxis = list(title = "Frequency"),
    bargap = 0.5
  )

#under incorrect model specification
params_sim_incorrect<- PosteriorSample(x_train, y_train, a=0.5, b=1, m=c(0,0), V=diag(10,2,2)) 
df_vline<-distinct(params_sim_incorrect, labels, true_param)

p2<-params_sim_incorrect%>%ggplot(aes(x=value)) +
  geom_histogram(aes(y=..density..), bins = 50, fill="skyblue", color="black") +
  geom_density(colour='blue', linewidth = 0.5) +
  geom_vline(data=df_vline, aes(xintercept = true_param), linetype = 'dashed', size = 1)+
  geom_text(data=df_vline, aes(x= true_param, label = paste("True Param:", round(true_param,4)), y=Inf),size = 4,hjust=0, vjust=2) + 
  facet_wrap(~labels, scales = 'free',labeller = label_parsed) + theme_bw() +
  labs(x = 'Parameters', y = 'Density',title = 'Posterior Density')
print(p2)

#Prediction
betas_star<-params_sim_incorrect%>%group_by(param)%>%summarise(value=mean(value))%>%select(value)%>%as.matrix()

y_star<-x_test%*%betas_star[1:2]
data.frame(y=y_test, y_star=as.numeric(y_star))%>%gather(key="data", value = "value")%>%
  plot_ly(x = ~value, color=~data, type = "histogram", nbinsx=70)%>% 
  layout(
    title = "Histogram Plot",
    xaxis = list(title = "X-Axis Label"),
    yaxis = list(title = "Frequency"),
    bargap = 0.5
  )


a<-8
b<-100
plot(seq(1,50,length.out=1000), b^a/gamma(a)*(seq(1,50,length.out=1000))^(-a-1)*exp(-b/seq(1,50,length.out=1000)))

