libpath<-.libPaths()
lib<-c("sqldf","jsonlite","gtools","reshape","doBy","reshape2","data.table","plyr","e1071")
lapply(lib, require, character.only=T)
library(stringr,lib.loc=libpath)
library(caret,lib.loc=libpath)

setwd("../conf/files")
#setwd("/Users/prithwirajmukherjee/Documents/sandbox/workspace/23-12-2015/prm-model-testbench/conf/files")
args<-commandArgs(T)
data<-args[1]
acknowledgement<-as.character(args[2])


library(jsonlite)
listjson<-fromJSON(data)

#setwd("/home/hduser/Documents/TEST1/prm-model-testbench/conf/files")#

#listjson<-fromJSON("94d95380-e69a-4e40-9134-c8c3e588f35a_filename.json")

parameters<-listjson$params
response_file<-listjson$responseDumpFile
intercept<-listjson$interceptValue
required_tests<-listjson$testCases$name
#required_tests1<-c("ks","dw")


Linear_Regression_Champion<-function(parameters,response_file,intercept,required_tests){
  parms<-parameters
  parms$IndCols<-paste(parms$entityName,parms$attributeName,sep="_")
  parms<-subset(parms,select=-c(entityName,attributeName))
  indColumns<-paste(parms$IndCols,collapse=",")
  indColumns<-gsub("[[:space:]]","",indColumns)
  
  ################################################# RESPONSE CREATION ##################################################
  
  resfile<-response_file
  resdata<-read.csv(resfile,header=T)
  resdata<-subset(resdata,select=-X)
  AllIndCols<-sqldf(paste("select ", indColumns,"from resdata"))
  AllIndCols<-eval(parse(text=paste("subset(resdata,select=c(",indColumns,"))")))
  print("resfile")
  indp_data<-AllIndCols
  ####################################### RESPONSE AND INDEPENDENT COLUMNS PREPARATION END ###########################
  
  
  PRM<-AllIndCols
  
  response<-subset(resdata,select=Observed_Outcome)
  names(response)="response"
  PRM<-cbind(PRM,response)
  val_data<-PRM
  response_data<-response$response
  
  ############ COEFFICIENT TABLE PREPARATION ###########
  
  coeff_table<-data.frame(row.names(parms),parms)
  names(coeff_table)=c("rownames","coefficientValue","coefficients")
  coeff_table<-subset(coeff_table,select=-rownames)
  coeff_table<-data.frame(coeff_table$coefficients,coeff_table$coefficientValue)
  names(coeff_table)=c("coefficients","coefficientValue")
  params<-data.frame(intercept<-1,bad<-"response",ord<-10)
  names(params)=c("intercept","bad","ord")
  intercept<-params[1,1] # THIS FLAG IS 1 IF INTERCEPT IS THERE IN THE MODEL
  bad<-params[1,2] # THE NAME OF TARGET VARIABLE IN THE DATASET
  ord<-as.numeric(params[1,3]) # NUMBER OF GROUPS TO BE USED IN KS CALCULATION
  
  
  
  ############################################ DATA PREPARATION #############################################################
  #listjson$interceptValue<-ifelse(listjson$interceptValue!=NULL,listjson$interceptValue,0)
  interceptvalue<-data.frame(intercept)
  interceptvalue<-ifelse(length(intercept)==0,0,intercept)
  names(interceptvalue)="interceptvalue"
  
  ############################################# coeff table preparation #####################################################
  
  library(gtools)
  
  t_coeff<-t(coeff_table)
  n<-data.frame(t_coeff[1,])
  names(n)="name"
  n$name<-as.character(n$name)
  d_coeff<-data.frame(t_coeff)
  library(data.table)
  for(i in 1:nrow(n)){
    setnames( d_coeff,names(d_coeff)[i], n[i,])
  }
  coeff_table<-d_coeff[-1,]
  coeff_table<-cbind(interceptvalue,coeff_table)
  coeff_with_val_data<-smartbind(coeff_table,val_data)
  for (i in 2:ncol(coeff_with_val_data))
  {
    coeff_with_val_data[,i]<-as.numeric(coeff_with_val_data[,i])
  }
  coeff_with_val_data[,1]<-as.numeric(as.character(coeff_with_val_data[,1]))
  validation_temp<-coeff_with_val_data[((nrow(coeff_table)+1):nrow(coeff_with_val_data)),]
  coefficients<-coeff_with_val_data[(1:nrow(coeff_table)),]
  row1<-nrow(validation_temp) # NO OF OBSERVATIONS
  col1<-ncol(validation_temp) # TOTAL NUMBER OF VARIABLES IN DATA (INCLUDING INTERCEPT)
  
  print(col1)
  #FOLLWOING CODE REMOVES ALL NON-NUMERIC VARIABLES
  validation<-validation_temp
  if (intercept==1) {validation<-validation_temp[,(2:col1)]}
  
  #NOTE: FOR INTERCEPT COEFFICIENT TABLE WILL HAVE ONE MORE COLUMN
  
  j<-1
  while(j <=ncol(validation))
  {
    k<-j
    if (intercept==1) k<-k+1
    if (class(validation[,j])!="numeric")
    {
      validation[,j]<-NULL
      coefficients[,k]<-NULL
      #NOTE: FOR INTERCEPT COEFFICIENT TABLE WILL HAVE ONE MORE COLUMN
      j<-j-1
    }
    j<-j+1
  }
  
  # COEFFICIENTS FOR EXTRA NUMERIC VARIABLES ARE ASSIGEND TO ZERO
  for (i in 1:ncol(coefficients))
  {
    if (is.na(coefficients[1,i]) == "TRUE")
    {
      coefficients[1,i]<-0 # COEFFICIENT
      coefficients[2,i]<-0 # MISSING VALUE INPUT
    } 
  }
  
  
  lnodd<-array(0,dim=row1)
  if (intercept==1) 
  {
    for (i in 1:row1)
    {
      lnodd[i]<-as.numeric(coefficients[1,1])
      for (j in 1:ncol(validation))
      {
        lnodd[i]<-lnodd[i]+as.numeric(validation[i,j])*as.numeric(coefficients[1,(j+1)])}
    }
  }
  if (intercept!=1) 
  {
    for (i in 1:row1)
    {
      for (j in 1:ncol(validation))
      {
        lnodd[i]<-lnodd[i]+as.numeric(validation[i,j])*as.numeric(coefficients[1,j])}
    }
  }
  
  Fitted_values<-lnodd
  
  
  
  ####################### Residual #############################
  Response_mean<-mean(response_data)
  Residuals<-response_data-Fitted_values
  obs_err<-Residuals^2
  mean_err<-(response_data-Response_mean)^2
  
  
  
  #setwd("/home/dbadmin/Documents/Linear Regression")
  #acknowledgement="AA"
  summary_file<-paste(acknowledgement,"summary.txt",sep="_") 
  output_file<-paste(acknowledgement,"output.txt",sep="_")
  
  ########### R Square ########
  if("R_Square" %in% required_tests==TRUE)
  {
    R_sq<-abs(round(1-(sum(obs_err)/sum(mean_err)),10))
    R_sq_heading<-sprintf("R-Square value: %.2f.",R_sq)
    R_sq_threshold<-listjson$testCases["threshold"][which(listjson$testCases=='R_Square') ,1]
    diff_R_Sq<-ifelse( R_sq_threshold < 1  ,(abs((R_sq/100)- R_sq_threshold)*100), (R_sq - R_sq_threshold))
    R_Sq_interpret<-if(R_sq > R_sq_threshold ){
      sprintf("Based on below output percent variation expalined is %.2f percent and is %.2f percent more than threshold value.",R_sq,diff_R_Sq)
    }else{
      sprintf("Based on below output percent variation expalined is %.2f percent and is %.2f percent less than threshold value.",R_sq,diff_R_Sq)}
    if("R_Square" %in% required_tests==TRUE)
      R_Sq_sum<-data.frame(testName="R_Square",heading=R_sq_heading,interpretation=R_Sq_interpret)
    R_Sq_sum1<-data.frame(Testcases="R_Square",Values=R_sq)#######
    if(summary_file %in% list.files()==TRUE) {write.table(R_Sq_sum,summary_file,sep='|',row.names=F,quote=F,append=T,col.names=F)} else
    {write.table(R_Sq_sum,summary_file,sep='|',row.names=F,append=T,quote=F,col.names=T)} 
    write.table(R_Sq_sum1,paste(acknowledgement,"R_Square.txt",sep="_"),row.names=F,quote=F,sep='|')
    
    R_Square_output<-data.frame(testName="R_Square",csv_path=paste(acknowledgement,csv_path=c('R_Square.txt'),sep="_"),graph=c('no'),heading="",conclusion='',interpretation='',yAxisLabel=c(''))
    if(output_file %in% list.files()==TRUE) {write.table(R_Square_output,output_file,sep='|',quote=F,row.names=F,append=T,col.names=F)} else
    {write.table(R_Square_output,output_file,sep='|',row.names=F,quote=F,append=T,col.names=T)}
    
  }
  
  if("Adj_R_Square" %in% required_tests==TRUE)
  {
    ########## Adjusted_R_Sq #########
    
    adj<-(nrow(indp_data)-1)/(nrow(indp_data)-(ncol(indp_data)-1)-1)
    
    Adj_R_sq<-1-(1-R_sq)*adj
    Adj_R_sq_heading<-sprintf("Adjusted-R-Square value: %.2f.",Adj_R_sq)
    Adj_R_sq_threshold<-listjson$testCases["threshold"][which(listjson$testCases=='Adj_R_Square') ,1]
    diff_Adj_R_Sq<-ifelse(  Adj_R_sq_threshold < 1  ,(abs((Adj_R_sq/100)- Adj_R_sq_threshold)*100), (Adj_R_sq - Adj_R_sq_threshold))
    Adj_R_Sq_interpret<-if(Adj_R_sq > Adj_R_sq_threshold ){
      sprintf("Based on below output Adjusted R-Square is %.2f percent and is %.2f percent more than threshold value.",Adj_R_sq,diff_Adj_R_Sq)
    }else{
      sprintf("Based on below output Adjusted R-Square is %.2f percent and is %.2f percent less than threshold value.",Adj_R_sq,diff_Adj_R_Sq)}
    if("Adj_R_Square" %in% required_tests==TRUE)
      Adj_R_Sq_sum<-data.frame(testName="Adj_R_Square",heading=Adj_R_sq_heading,interpretation=Adj_R_Sq_interpret)
    Adj_R_Sq_sum1<-data.frame(Testcases="Adj_R_Square",Value=Adj_R_sq)
    
    if(summary_file %in% list.files()==TRUE) {write.table(Adj_R_Sq_sum,summary_file,sep='|',quote=F,row.names=F,append=T,col.names=F)} else
    {write.table(Adj_R_Sq_sum,summary_file,sep='|',row.names=F,append=T,quote=F,col.names=T)} 
    write.table(Adj_R_Sq_sum1,paste(acknowledgement,"Adj_R_Square.txt",sep="_"),row.names=F,quote=F,sep='|')
    
    Adj_R_Square_output<-data.frame(testName="Adj_R_Square",csv_path=paste(acknowledgement,csv_path=c('Adj_R_Square.txt'),sep="_"),graph=c('no'),heading="",conclusion='',interpretation='',yAxisLabel=c(''))
    if(output_file %in% list.files()==TRUE) {write.table(Adj_R_Square_output,output_file,sep='|',quote=F,row.names=F,append=T,col.names=F)} else
    {write.table(Adj_R_Square_output,output_file,sep='|',row.names=F,quote=F,append=T,col.names=T)}
    
  }
  
  if("RMSE" %in% required_tests==TRUE)
  {
    ######### RMSE ############
    
    RMSE<- round(sqrt(sum(obs_err)/nrow(indp_data)),2)
    RMSE_heading<-sprintf("RMSE value: %.2f.",RMSE)
    RMSE_threshold<-listjson$testCases["threshold"][which(listjson$testCases=='RMSE') ,1]
    diff_RMSE<-ifelse(  RMSE_threshold < 1  ,(abs((RMSE/100)- RMSE_threshold)*100), (RMSE - RMSE_threshold))
    RMSE_interpret<-if(RMSE > RMSE_threshold ){
      sprintf("Observed RMSE is %.2f more than desired threshold value.",diff_RMSE)
    }else{
      sprintf("Observed RMSE is %.2f less than desired threshold value.",diff_RMSE)}
    #RMSE_interpret<-sprintf("RMSE value is %.2f and it measures the average squared difference between the estimator and what is estimated",RMSE)
    if("RMSE" %in% required_tests==TRUE)
      RMSE_sum<-data.frame(testName="RMSE",heading=RMSE_heading,interpretation=RMSE_interpret)
    RMSE_sum1<-data.frame(Testcases="RMSE",Value=RMSE)
    
    if(summary_file %in% list.files()==TRUE) {write.table(RMSE_sum,summary_file,sep='|',row.names=F,quote=F,append=T,col.names=F)} else
    {write.table(RMSE_sum,summary_file,sep='|',row.names=F,append=T,quote=F,col.names=T)} 
    write.table(RMSE_sum1,paste(acknowledgement,"RMSE.txt",sep="_"),quote=F,row.names=F,sep='|')
    
    RMSE_output<-data.frame(testName="RMSE",csv_path=paste(acknowledgement,csv_path=c('RMSE.txt'),sep="_"),graph=c('no'),heading="",conclusion='',interpretation='',yAxisLabel=c(''))
    if(output_file %in% list.files()==TRUE) {write.table(RMSE_output,output_file,sep='|',row.names=F,quote=F,append=T,col.names=F)} else
    {write.table(RMSE_output,output_file,sep='|',row.names=F,quote=F,append=T,col.names=T)}
    
  }
  
  if("MAPE" %in% required_tests==TRUE)
  {
    ######### MAPE ##########
    
    Ratio<- abs(Residuals/response_data)
    APE<-Ratio[is.finite(Ratio)]
    MAPE<-round((sum(APE)/length(APE))*100,3)
    MAPE_heading<-sprintf("MAPE value: %.2f.",MAPE)
    MAPE_threshold<-listjson$testCases["threshold"][which(listjson$testCases=='MAPE'),1]
    diff_MAPE<-ifelse(  MAPE_threshold < 1  ,(abs((MAPE/100)- MAPE_threshold)*100), (MAPE - MAPE_threshold))
    
    MAPE_interpret<-if(MAPE > MAPE_threshold ){
      sprintf("Mean Absolute Percent Deviation is %.2f percent more than desired threshold value.",diff_MAPE)
    }else{
      sprintf("Mean Absolute Percent Deviation is %.2f percent more than desired threshold value.",diff_MAPE)}
    #MAPE_interpret<-sprintf("MAPE value is %.2f and it is calculated as the average of the unsigned percentage error ",MAPE)
    
    if("MAPE" %in% required_tests==TRUE)
      MAPE_sum<-data.frame(testName="MAPE",heading=MAPE_heading,interpretation=MAPE_interpret)
    MAPE_sum1<-data.frame(Testcases="MAPE",Value=MAPE)
    
    if(summary_file %in% list.files()==TRUE) {write.table(MAPE_sum,summary_file,sep='|',row.names=F,quote=F,append=T,col.names=F)} else
    {write.table(MAPE_sum,summary_file,sep='|',row.names=F,append=T,quote=F,col.names=T)} 
    write.table(MAPE_sum1,paste(acknowledgement,"MAPE.txt",sep="_"),row.names=F,quote=F,sep='|')
    
    MAPE_output<-data.frame(testName="MAPE",csv_path=paste(acknowledgement,csv_path=c('MAPE.txt'),sep="_"),graph=c('no'),heading="",conclusion='',interpretation='',yAxisLabel=c(''))
    if(output_file %in% list.files()==TRUE) {write.table(MAPE_output,output_file,sep='|',row.names=F,quote=F,append=T,col.names=F)} else
    {write.table(MAPE_output,output_file,sep='|',row.names=F,append=T,quote=F,col.names=T)}
    
  }
  
  
  ########### Performing Test ############
  
  ######### KS test of Residuals ##############
  
  try(if("ks" %in% required_tests==TRUE)
  {
    
    K_S<-ks.test(Residuals, "pnorm", alternative="two.sided")
    KS_Test_Stat<-matrix(round(K_S$statistic,4))[1,1]
    KS_Test_P_Value<-round(K_S$p.value,5)
    KS_Test_Heading<-sprintf("Kolmogorov-Smirnov Test P-value is: %.2f.",KS_Test_P_Value)
    #Test<-"K-S"
    #ks_test_res<-data.frame(Test,KS_Test_Stat,KS_Test_P_Value)
    #names(ks_test_res)=c("Tests","Statistic","P-value")
    #ks_threshold<-listjson$testCases["threshold"][which(listjson$testCases=='ks'),1]
    ks_threshold<-0.01
    ks_Interpret<-if(KS_Test_P_Value < ks_threshold){
      sprintf("Since P-value of Kolmogorov-Smirnov Test is less than desired level of significance, hence we would accept the Null Hypothesis that the sample comes from Normal Population at level %.2f.",ks_threshold)
    }else{
      sprintf("Since P-value of Kolmogorov-Smirnov Test is less than desired level of significance, hence we would accept the Null Hypothesis that the sample comes from Normal Population at level %.2f.",ks_threshold)}
    #KS_Interpret1<-sprintf("Taking alpha=0.05 i.e.to test Null Hypothesis that the sample comes from Normal Population at level 0.05, Since  and is greater than 0.05 then we wouldnot reject Null with this test",KS_Test_P_Value)
    #KS_interpret2<-sprintf("Taking alpha=0.05 i.e.to test Null Hypothesis that the sample comes from Normal Population at level of 0.05, Since P-value is %.2f and is less than 0.05 then we wouldnot accept Null with this test",KS_Test_P_Value)
    if("ks" %in% required_tests==TRUE)
      ks_sum<-data.frame(testName="Kolmogorov-Smirnov",heading=KS_Test_Heading,interpretation=ks_Interpret)
    ks_sum1<-data.frame(Testcases="Kolmogorov-Smirnov",Value=KS_Test_P_Value)
    
    if(summary_file %in% list.files()==TRUE) {write.table(ks_sum,summary_file,sep='|',row.names=F,quote=F,append=T,col.names=F)} else
    {write.table(ks_sum,summary_file,sep='|',row.names=F,append=T,quote=F,col.names=T)} 
    write.table(ks_sum1,paste(acknowledgement,"ks.txt",sep="_"),row.names=F,quote=F,sep='|')
    
    ks_output<-data.frame(testName="Kolmogorov-Smirnov",csv_path=paste(acknowledgement,csv_path=c('ks.txt'),sep="_"),graph=c('no'),heading="",conclusion='',interpretation='',yAxisLabel=c(''))
    if(output_file %in% list.files()==TRUE) {write.table(ks_output,output_file,sep='|',row.names=F,quote=F,append=T,col.names=F)} else
    {write.table(ks_output,output_file,sep='|',row.names=F,append=T,quote=F,col.names=T)}
    
    
  },silent = T)
  
  if("dw" %in% required_tests==TRUE)
  {
    dw1<-0
    
    y<-(Residuals[1])**2
    for(i in 2:nrow(Residuals)){
      x<-(Residuals[i]-Residuals[i-1])**2
      z<-Residuals[i]**2
      dw1<-(dw1+x)
      y<-y+z
    }
    
    Dur_Wat<-dw1/y
    Rho<-round(1-(Dur_Wat/2),3)
    DW_Heading<-sprintf("Durbin-Watson Test Statistic is %.2f .",Dur_Wat)
    #dw_threshold<-listjson$testCases["threshold"][which(listjson$testCases=='dw'),1]
    dw_threshold<-2
    dw_Interpret<-if(Dur_Wat < dw_threshold){
      sprintf("Positive 1st order lagged autocorrelation between Residuals of the Regression Model, and the correlation value is %.2f.",Rho)
    }else{
      sprintf("Negative 1st order lagged autocorrelation between Residuals of the Regression Model, and the correlation value is %.2f.",Rho)}
    if("dw" %in% required_tests==TRUE)
      DW_sum<-data.frame(testName="Durbin-Watson",heading=DW_Heading,interpretation=dw_Interpret)
    DW_sum1<-data.frame(Testcases="Durbin-Watson",Value=Dur_Wat)
    
    if(summary_file %in% list.files()==TRUE) {write.table(DW_sum,summary_file,sep='|',row.names=F,append=T,quote=F,col.names=F)} else
    {write.table(DW_sum,summary_file,sep='|',row.names=F,append=T,quote=F,col.names=T)}
    write.table(DW_sum1,paste(acknowledgement,"DW.txt",sep="_"),row.names=F,quote=F,sep='|')
    
    dw_output<-data.frame(testName="Durbin-Watson",csv_path=paste(acknowledgement,csv_path=c('DW.txt'),sep="_"),graph=c('no'),heading="",conclusion='',interpretation='',yAxisLabel=c(''))
    if(output_file %in% list.files()==TRUE) {write.table(dw_output,output_file,sep='|',row.names=F,append=T,quote=F,col.names=F)} else
    {write.table(dw_output,output_file,sep='|',row.names=F,append=T,col.names=T,quote=F)}
  }
  
  
}



run_details<-data.frame(RunId='2',acknowledgement_id=acknowledgement,rag='GREEN')
write.table(run_details,paste(paste(acknowledgement,"run_details",sep='_'),"txt",sep='.'),sep="|",row.names=F,quote=F)


#acknowledgement<-'test'

#library(rjson)
Linear_Regression_Champion(parameters,response_file,intercept,required_tests)
