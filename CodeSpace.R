CategoryDependence <- setRefClass(
  "CategoryDependence",
  fields = list(
    df = "data.frame",
    p_value = "list"
  ),
  # create a list contain pairwise comparsion between cate variables and LOSdays_less2
  # create df1 containing  column 1  and column 2, put df1 into first place in a list 
  # create df2 containing column 1 and column 3, put df2 into 2nd of the list, 
  # create df3 containing column 1 and 4, put df3 into 3rd of the list, 
  # create df4 containing  column 1  and 5, put df4 into 4th in list, 
  # create df5 containing column 1 and 6, put df5 into 5rd of the list.
  methods = list(
    initialize = function(df) {
      .self$df <- df
      .self$p_value <- .self$chi_square_test()
    },
    create_list = function() {
      my_list <- list()
      for (i in 2:ncol(df)) {
        df_name <- colnames(.self$df)[i]
        new_df <- data.frame(.self$df[[1]], 
                             .self$df[[i]])
        my_list[[df_name]] <- new_df
      }
      return(my_list)
    },
    # Do chi square test through the list
    chi_square_test = function() {
      p_value <- list()
      for (i in 2:ncol(.self$df)) {
        v <- colnames(.self$df)[i]
        my_df <- .self$create_list()[[v]]
        chi_square_table <- table(my_df)
        chi_square_result <- chisq.test(chi_square_table)
        # Perform pvalue into df
        p_value[[v]] <- chi_square_result$p.value
      }
      return(p_value)
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
        geom_histogram(aes(y=..density..), bins = 50, fill="skyblue", color="black") +
        geom_density(colour='blue', linewidth = 0.5) +
        geom_vline(data=df_vline, aes(xintercept = params_est), linetype = 'dashed', size = 1)+
        geom_text(data=df_vline, aes(x= params_est, label = paste("Param_Est:", round(params_est,4)), y=Inf),size = 2,hjust=0, vjust=2) + 
        facet_wrap(~labels, scales = 'free',labeller = label_parsed) + theme_bw() +
        labs(x = 'Parameters', y = 'Density',title = 'Posterior Density')
      
      print(p)
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
      print(p)
    }
  )
)
