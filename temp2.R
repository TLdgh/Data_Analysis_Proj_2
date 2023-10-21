cate_data <- mimic3d %>% transmute(
  LOSdays_Less2 = factor(ifelse(LOSdays <= 2, "1", "0")), 
  gender = factor(gender, levels = c("F", "M"), labels = c(0, 1)), 
  admit_type = factor(admit_type, levels = c("ELECTIVE","EMERGENCY","NEWBORN","URGENT"), labels = c(0,1,2,3)),
  insurance = factor(insurance, levels = c("Government","Medicaid","Medicare","Private","Self Pay"), labels = c(0,1,2,3,4)),
  factor(ExpiredHospital),factor(LOSgroupNum)
)
colnames(cate_data) <- c("LOSdays_Less2","gender","admit_type","insurance","ExpiredHospital","LOSgroupNum")
str(cate_data)


CategoryDependence <- setRefClass(
  "CategoryDependence",
  fields = list(
    df = "data.frame"
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
    },
    create_list = function() {
      my_list <- list()
      for (i in 2:ncol(df)) {
        df_name <- colnames(df)[i]
        new_df <- data.frame(df[, 1], df[, i])
        my_list[[df_name]] <- new_df
      }
      return(my_list)
    },
    # Do chi square test through the list
    chi_square_test = function() {
      for (v in colnames(df)[2:length(colnames(df))]) {
        my_df <- create_list()[[v]]
        chi_square_table <- table(my_df)
        chi_square_result <- chisq.test(chi_square_table)
        cat("\nP-value of LOSdays and ", v, " is ", chi_square_result$p.value, "\n")
      }
    }
  )
)




IndepTest<-setRefClass(
  "IndepTest",
  fields = list(data="data.frame", variable_x="character", variable_y="character",pval_df="data.frame",ContingencyTbl="list" ),
  methods=list(
    initialize=function(data, variable_x, variable_y){
      #make a contingency table for the variable X and Y
      .self$ContingencyTbl<-.self$create_list(data, variable_x, variable_y)
      
      #calculate the pvalues and make a data frame
      .self$pval_df<- .self$chi_square_test(my_list=ContingencyTbl)
    },
    
    create_list=function(data, variable_x, variable_y){
      my_list <- list()
      for(x in variable_x){
        my_list[[x]]<-data%>%select(all_of(c(x, variable_y)))%>%na.omit()%>%
          group_by(across(everything(), as.factor))%>%tally()%>%spread(key=variable_y, value=n)}
      return(my_list)
    }, 
    
    chi_square_test=function(my_list){
      #do the chi-squared test
      pvalues<-map(my_list, function(x){
        if(any(is.na(x))==FALSE){
          y<-ungroup(x)%>%select(-any_of(group_vars(x)))%>%as.matrix()%>%chisq.test()
          return(y$p.value)
        }else{return("Has no values in Contingency Table")}
      })
      
      #format it nicely
      pvalues<-pvalues%>%data.frame(row.names = "p-value")%>%
        mutate(across(where(is.numeric), .fns = ~as.character(signif(.,4))))
      
      return(pvalues)}
  )
)

my_instance<-IndepTest$new(data=cate_data, variable_x=c("gender","admit_type","insurance","ExpiredHospital","LOSgroupNum"), variable_y="LOSdays_Less2")
my_instance$ContingencyTbl
my_instance$pval_df




