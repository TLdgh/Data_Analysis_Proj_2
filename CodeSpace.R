BayesianSample <- setRefClass(
  "BayesianSample",
  fields = list(old_distinct= "data.frame",new_distinct = "data.frame",rows_per_subset="numeric",
                alpha_prior_old="numeric", alpha_prior_new="numeric", beta_prior_old="numeric", beta_prior_new="numeric",
                value="numeric",size="numeric",posteriors="list",se_mean_old = "numeric",
                se_mean_new = "numeric"),
  methods = list(
    initialize = function(old_distinct,new_distinct,alpha_prior_old,alpha_prior_new,beta_prior_old,beta_prior_new,rows_per_subset){
      .self$alpha_prior_old<-alpha_prior_old
      .self$alpha_prior_new<-alpha_prior_new
      .self$beta_prior_old<-beta_prior_old
      .self$beta_prior_new<-beta_prior_new
      .self$value<-numeric(0)
      .self$size<-numeric(0)
      .self$se_mean_old <- numeric(0)  # 添加初始化值
      .self$se_mean_new <- numeric(0)
      .self$posteriors<-.self$GetPosteriors(old_distinct,new_distinct,rows_per_subset)
      .self$se_mean_old <- se_mean_old
      .self$se_mean_new <- se_mean_new
    },
    
    #take the sample of the data
    takeSample=function(old_distinct,new_distinct,rows_per_subset,i){
      start_row <- (i - 1) * rows_per_subset + 1
      end_row <- i * rows_per_subset
      old_sample<-old_distinct[seq(from=start_row, to=end_row), ]
      new_sample<-new_distinct[seq(from=start_row, to=end_row), ]
      size<<-c(size, i*rows_per_subset)
      return(list(old_sample=old_sample, new_sample=new_sample))
    },
    
    #get the posteriors using subsets of data
    GetPosteriors=function(old_distinct,new_distinct,rows_per_subset){
      posterior_old<-list()
      posterior_new<-list()
      
      #loop through the subsets defined by rows_per_subset
      for(i in 1:floor(min(nrow(old_distinct), nrow(new_distinct))/rows_per_subset) ){
        samples<-.self$takeSample(old_distinct,new_distinct,rows_per_subset,i)
        visitor_to_old <- nrow(samples$old_sample)
        visitor_to_new <- nrow(samples$new_sample)
        conversion_old <- sum(samples$old_sample$converted)
        conversion_new <- sum(samples$new_sample$converted)
        posterior_old[[i]] <- rbeta(100000,alpha_prior_old+conversion_old,beta_prior_old+visitor_to_old-conversion_old)
        posterior_new[[i]] <- rbeta(100000,alpha_prior_new+conversion_new,beta_prior_new+visitor_to_new-conversion_new)
        value<<-c(value, mapply(function(x, y) {x>y}, posterior_new[[i]], posterior_old[[i]])%>%mean())
        
        alpha_prior_old<<-alpha_prior_old+conversion_old
        alpha_prior_new<<-alpha_prior_new+conversion_new
        beta_prior_old<<-beta_prior_old+visitor_to_old-conversion_old
        beta_prior_new<<-beta_prior_new+visitor_to_new-conversion_new
      }
      return(list(posterior_old=posterior_old,posterior_new=posterior_new))
    },
    
    #calculate the posterior mean
    calculateMean=function(){
      posterior_mean<-map(posteriors, function(x){sapply(x, FUN=mean)})
      posterior_se<-map(posteriors, function(x){sapply(x, function(x){sd(x)/sqrt(length(x))})})
      
      frame<-data.frame(size,value,posterior_mean%>%as.data.frame(col.names=paste0(names(posterior_mean), "_mean")),
                        posterior_se%>%as.data.frame(col.names=paste0(names(posterior_mean), "_se")))
      return(frame)
    },
    
    PlotCI=function(){
      posterior_mean<-map(posteriors, function(x){sapply(x, FUN=mean)})
      names(posterior_mean)<-paste0(names(posterior_mean), ".mean")
      
      posteriorCI<-map(posteriors, function(x) do.call(rbind, lapply(x, function(y){data.frame(Q25=quantile(y, 0.025), Q975=quantile(y,0.975))})))
      
      frame<-cbind(data.frame(size),posterior_mean%>%as.data.frame(), posteriorCI%>%as.data.frame())%>%
        pivot_longer(cols=starts_with(c("posterior_old", "posterior_new")), names_to=c("Type",".value"), names_sep = "\\.")
      
      p<-ggplot(frame, aes(x = size, y = mean)) +
        geom_line(color = "blue") +
        geom_ribbon(aes(ymin = Q25,  ymax = Q975), fill = "blue", alpha = 0.2) +
        facet_wrap(~Type,scales = 'free',labeller = label_parsed) + theme_bw() +
        labs(title = "Mean Comparison with Confidence Intervals",
             x = "Sample Size",
             y = "Mean")
      return(p)
    }
  )
)


BayesianRegression<-setRefClass(
  "RegressionClass",
  fields = list(train_data="data.frame", responseV="character", predictorV="character", 
                prior_sigma="numeric", prior_beta="list", posteriorSample="data.frame"),
  methods = list(
    initialize=function(train_data, responseV, predictorV, prior_sigma, prior_beta){
      .self$responseV<-responseV
      .self$predictorV<-predictorV
      .self$prior_sigma<-prior_sigma
      .self$prior_beta<-prior_beta
      .self$posteriorSample<-.self$PosteriorSample(train_data)
    },
    
    PosteriorSample=function(train_data, sim_n=100000){
      # Get data matrix
      x<-train_data%>%select(all_of(predictorV))%>%mutate(Intercept=1, .before=1)%>%as.matrix()
      y<-train_data%>%select(all_of(responseV))%>%as.matrix(ncol=1)
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
    },
    
    PosteriorPlot=function(){
      df_vline<-distinct(posteriorSample, labels, params_est)
      
      p<-posteriorSample%>%ggplot(aes(x=value)) +
        geom_histogram(aes(y=after_stat(density)), bins = 50, fill="skyblue", color="black") +
        geom_density(colour='blue', linewidth = 0.5) +
        geom_vline(data=df_vline, aes(xintercept = params_est), linetype = 'dashed', size = 1)+
        geom_text(data=df_vline, aes(x= params_est, label = paste("Param_Est:", round(params_est,4)), y=Inf),size = 2,hjust=0, vjust=2) + 
        facet_wrap(~labels, scales = 'free',labeller = label_parsed) + theme_bw() +
        labs(x = 'Parameters', y = 'Density',title = 'Posterior Density')
      
      return(p)
    },
    
    PredictionPlot=function(test_data){
      betas<-posteriorSample%>%select(param, value)%>%group_by(param)%>%summarise(betas=mean(value))
      
      #Prediction
      test_data<-test_data%>%select(all_of(c(predictorV, responseV)))
      x<-test_data%>%select(all_of(predictorV))%>%mutate(Intercept=1)%>%as.matrix()
      y<-test_data%>%select(all_of(responseV))%>%as.matrix(ncol=1)

      betas<-betas%>%arrange(match(param, colnames(x)))%>%filter(param!="sigmasq")%>%column_to_rownames(var = "param") %>% data.matrix()

      y_star<-x%*%betas
      
      p<-data.frame(y=as.numeric(y), y_star=as.numeric(y_star))%>%gather(key="data", value = "value")%>%mutate(value=ifelse(value>=0, value, 0))%>%
        plot_ly(x = ~value, color=~data, type = "histogram",nbinsx=200)%>% 
        layout(
          title = "Histogram Plot",
          xaxis = list(title = "X-Axis Label"),
          yaxis = list(title = "Frequency"),
          bargap = 0.5
        )
      return(p)
    }
  )
)
