################################################### LIBRARIES ##########################################################
libpath<-.libPaths()
lib<-c("sqldf","jsonlite","gtools","reshape","doBy","reshape2","data.table","plyr","e1071")
lapply(lib, require, character.only=T)
library(stringr,lib.loc=libpath)
library(caret,lib.loc=libpath)


################################################# CONNECTION PART ###########################################

# setwd("/Users/prithwirajmukherjee/Documents/Pabitra\ Database/prm-model-testbench/conf/files")
setwd("../conf/files")
args<-commandArgs(T)
data<-args[1]

acknowledgement<-as.character(args[2])

#data<-'{"ivDumpFile":"65f10d66-6983-451a-9fc3-5eb972d4d865_iv_dump.csv","testCases":[{"name":"ginny","threshold":0.125},{"name":"ks","threshold":0.25},{"name":"sen_spec","threshold":0.25}],"technique":"Logistic Regression","responseDumpFile":"65f10d66-6983-451a-9fc3-5eb972d4d865_response_dump.csv","functionalClassification":"PD","params":[{"attrId":"465ebbbb-6654-4293-80b5-8a59b5c1b1f0","name":"RaboBankData_Duration","entityId":"51648edc-6418-4082-a3c3-2a2a57124508"},{"attrId":"7f6ccce2-6b25-412b-a01e-5cca4af09fa1","name":"RaboBankData_TransactionCount","entityId":"51648edc-6418-4082-a3c3-2a2a57124508"},{"attrId":"7862bb7c-3269-4912-9488-e915b3794355","name":"RaboBankData_nflag1","entityId":"51648edc-6418-4082-a3c3-2a2a57124508"},{"attrId":"ce907450-4797-492e-8457-bcb6f1c873e3","name":"RaboBankData_LiquidAssests","entityId":"51648edc-6418-4082-a3c3-2a2a57124508"},{"attrId":"f1a584f8-9eca-4584-812e-7aabc3af8858","name":"RaboBankData_student2flag1","entityId":"51648edc-6418-4082-a3c3-2a2a57124508"},{"attrId":"ce907450-4797-492e-8457-bcb6f1c873e3","name":"RaboBankData_LiquidAssests","entityId":"51648edc-6418-4082-a3c3-2a2a57124508"},{"attrId":"465ebbbb-6654-4293-80b5-8a59b5c1b1f0","name":"RaboBankData_Duration","entityId":"51648edc-6418-4082-a3c3-2a2a57124508"},{"attrId":"09e71aad-c5ed-4639-bb71-f5cea9e4a9f5","name":"RaboBankData_homeflag1","entityId":"51648edc-6418-4082-a3c3-2a2a57124508"},{"attrId":"d1781f31-cb44-4982-ad09-b5ea61e69242","name":"RaboBankData_NegativeBalance","entityId":"51648edc-6418-4082-a3c3-2a2a57124508"},{"attrId":"ec373b5d-a40c-45e8-a26f-1b9a43fe78f2","name":"RaboBankData_CustomerID","entityId":"51648edc-6418-4082-a3c3-2a2a57124508"},{"attrId":"09e71aad-c5ed-4639-bb71-f5cea9e4a9f5","name":"RaboBankData_homeflag1","entityId":"51648edc-6418-4082-a3c3-2a2a57124508"},{"attrId":"0896a9c9-fd1b-4fa9-9202-34d75b2565c4","name":"RaboBankData_student1flag1","entityId":"51648edc-6418-4082-a3c3-2a2a57124508"},{"attrId":"5d47abf9-0b19-4075-af87-926589d9576d","name":"RaboBankData_kindflag1","entityId":"51648edc-6418-4082-a3c3-2a2a57124508"},{"attrId":"7f6ccce2-6b25-412b-a01e-5cca4af09fa1","name":"RaboBankData_TransactionCount","entityId":"51648edc-6418-4082-a3c3-2a2a57124508"},{"attrId":"d1781f31-cb44-4982-ad09-b5ea61e69242","name":"RaboBankData_NegativeBalance","entityId":"51648edc-6418-4082-a3c3-2a2a57124508"},{"attrId":"5d47abf9-0b19-4075-af87-926589d9576d","name":"RaboBankData_kindflag1","entityId":"51648edc-6418-4082-a3c3-2a2a57124508"},{"attrId":"ec373b5d-a40c-45e8-a26f-1b9a43fe78f2","name":"RaboBankData_CustomerID","entityId":"51648edc-6418-4082-a3c3-2a2a57124508"},{"attrId":"7862bb7c-3269-4912-9488-e915b3794355","name":"RaboBankData_nflag1","entityId":"51648edc-6418-4082-a3c3-2a2a57124508"},{"attrId":"39ba0f49-a16c-4e6a-bf5f-5cb28b205351","name":"RaboBankData_pdflag","entityId":"51648edc-6418-4082-a3c3-2a2a57124508"},{"attrId":"39ba0f49-a16c-4e6a-bf5f-5cb28b205351","name":"RaboBankData_pdflag","entityId":"51648edc-6418-4082-a3c3-2a2a57124508"},{"attrId":"0896a9c9-fd1b-4fa9-9202-34d75b2565c4","name":"RaboBankData_student1flag1","entityId":"51648edc-6418-4082-a3c3-2a2a57124508"},{"attrId":"f1a584f8-9eca-4584-812e-7aabc3af8858","name":"RaboBankData_student2flag1","entityId":"51648edc-6418-4082-a3c3-2a2a57124508"}],"calibrationValues":[],"requestId":"65f10d66-6983-451a-9fc3-5eb972d4d865","industries":[],"challenge":false,"schemeDumpFile":"65f10d66-6983-451a-9fc3-5eb972d4d865_scheme_dump.csv","interceptValue":"-2.318","dataSource":null,"linkedColumn":"RaboBankData_CustomerID","dataSrc":{"password":"tectes","driver":"com.mysql.jdbc.Driver","dbname":"StagingDB","port":"3306","columns":null,"host":"jdbc:mysql://127.0.0.1:3306","user":"root","table":"StagingTable"}}'
# data<-"fd948267-ffe6-4f9e-86d2-1609ac9c1bc0_filename.json"
print(data)
listjson<-fromJSON(data)
detach(package:jsonlite, unload = TRUE)
model<-listjson$technique   # TYPES: 1.Rgression 2.LogisticRegression
reqd_tests<-listjson$testCases$name
################################################# RUNID AND DATASET INFO EXTRACT ################################################

table<-listjson$schemeDumpFile



################################################# INDEPENDENT VARIABLES EXTRACT ##########################################

parms<-listjson$params
parms$IndCols<-paste(parms$entityName,parms$attributeName,sep="_")


parms<-subset(parms,select=-c(entityName,attributeName))
indColumns<-paste(parms$IndCols,collapse=",")
indColumns<-gsub("[[:space:]]","",indColumns)



################################################# RESPONSE CREATION ##################################################

resfile<-listjson$responseDumpFile
resdata<-read.csv(resfile,header=T)
resdata<-subset(resdata,select=-X)
AllIndCols<-eval(parse(text=paste("subset(resdata,select=c(",indColumns,"))")))


####################################### RESPONSE AND INDEPENDENT COLUMNS PREPARATION END ###########################


PRM<-AllIndCols

response<-subset(resdata,select=Observed_Outcome)
names(response)="response"
PRM<-cbind(PRM,response)
val_data<-PRM


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
interceptvalue<-data.frame(listjson$interceptValue)
interceptvalue<-ifelse(length(listjson$interceptValue)==0,0,listjson$interceptValue)
names(interceptvalue)="interceptvalue"

############################################# coeff table preparation #####################################################



t_coeff<-t(coeff_table)
n<-data.frame(t_coeff[1,])
names(n)="name"
n$name<-as.character(n$name)
d_coeff<-data.frame(t_coeff)
for(i in 1:nrow(n)){
  setnames(d_coeff, names(d_coeff)[i], n[i,])
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


######################################## MISSING VALUE TREATMENT #########################################################

for (i in 1:row1)
{
  for (j in 1:ncol(validation))
  {
    k<-j
    if (intercept==1) k<-k+1
    if (is.na(validation[i,j]) == "TRUE")
    {
      validation[i,j]=coefficients[2,k]
    }
  }
}
validation[is.na(validation)]<-0

#################################### FINAL SCORE COMPUTATION ###################################################


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



######################################### COMMON PART FOR REGRESSION AND LOGISTIC REGRESSION ##############################################

print(validation$response[1:100])

validation2<-data.frame(PRM,lnodd)
validation2$odd=exp(validation2$lnodd)
validation2$pred_prob<-validation2$odd/(1+validation2$odd) #FINAL SCORES

PredProb<-validation2$pred_prob
ActualOutcome<-validation$response

Goodness_of_fit<-function(PredProb,ActualOutcome,acknowledgement,reqd_tests)
{
  
  main_table<-function(PredProb,ActualOutcome,ord){
    
    library(reshape)
    smalldata<-data.frame(PredProb,ActualOutcome)
    names(smalldata)=c('pred_prob','bad')
    library(doBy)
    ################### RANKING DATA BASED ON SCORE ###################
    
    # THIS PART RANKS BASED ON SCORE IN DESCENDING ORDER BY BREAKING THE TIES
    smalldata$all_rank<-rank(-smalldata$pred_prob,ties.method="random")
    obs_cnt<-nrow(smalldata)
    #GROUPING THE VARIABLES BASED ON RANKS AND HENCE SCORES
    smalldata$rank<-ceiling(as.numeric(smalldata$all_rank)*as.numeric(ord)/obs_cnt)
    # RECALL: 'ord' IS AN INPUT, NUMBER OF GROUPS
    
    ################### PREPARATION OF KS TABLE ###################
    
    #COUNT OF OBSERVATIONS IN EACH GROUP (SHOULD BE ALMOST SAME ACROSS GROUPS
    mytable <- table(smalldata$rank)
    smalldata$bad<-as.integer(as.character(smalldata$bad))
    smalldata$rank<-as.integer(smalldata$rank)
    ##COUNTING OBSERVED BAD (BAD=1) IN EACH GROUP##
    actual_table<-summaryBy(bad ~rank, data = smalldata,
                            FUN = function(x) { c(tot=sum(x)) } )
    
    ##COUNTING PREDICTED BAD (BAD=1) IN EACH GROUP##
    pred_table<-summaryBy(pred_prob ~rank, data = smalldata,
                          FUN = function(x) { c(min=round(min(x),digits=5),max=round(max(x),digits=5), avg=round(mean(x),digits=5)) } )
    
    
    
    ################################### KS TABLE PREPARATION START ###################################################
    
    
    ##FIRST LEVEL OF KS TABLE##
    ks_table<-cbind(t(mytable),actual_table,pred_table)
    
    ks_table$Var1<-ks_table$rank<-NULL # REMOVING UNNECESSERY VARIABLES
    ks_table$rank<-NULL
    ks_table$good.tot<-ks_table$Freq-ks_table$bad.tot  #OBSERVED GOOD
    ks_table$bad.rate<-ks_table$bad.tot/ks_table$Freq #OBSERVED BAD RATE
    
    ##CALCULATION OF OBSERVED CUMULITIVE BAD, GOOD AND RANK ORDERING FLAG##
    ks_table$cum.bad.tot<-ks_table$bad.tot
    ks_table$cum.good.tot<-ks_table$good.tot
    ks_table$cum.tot<-ks_table$Freq
    ks_table$rank.check<-1
    for (i in 2:nrow(ks_table))
    {
      
      ks_table$cum.bad.tot[i]<-ks_table$cum.bad.tot[i-1]+ks_table$bad.tot[i]
      ks_table$cum.good.tot[i]<-ks_table$cum.good.tot[i-1]+ks_table$good.tot[i]
      ks_table$cum.tot[i]<-ks_table$cum.tot[i-1]+ks_table$Freq[i]
      if (ks_table$bad.rate[i]>ks_table$bad.rate[i-1]) ks_table$rank.check[i]=0
    }
    
    
    ##OBSERVED CUM BAD & GOOD RATE##
    ks_table$cum.bad.rate<-ks_table$cum.bad.tot/(ks_table$cum.bad.tot+ks_table$cum.good.tot)
    ks_table$cum.good.rate<-1- ks_table$cum.bad.rate
    
    ##OBSERVED BAD CAPTURE AND CALCULATION OF KS##
    tot.good<-max(ks_table$cum.good.tot)
    tot.bad<-max(ks_table$cum.bad.tot)
    tot<-max(ks_table$cum.tot)
    ks_table$cum.bad.capture<-ks_table$cum.bad.tot/tot.bad
    ks_table$cum.good.capture<-ks_table$cum.good.tot/tot.good
    ks_table$cum.tot.capture<-ks_table$cum.tot/tot
    ks_table$ks<-ks_table$cum.bad.capture-ks_table$cum.good.capture
    
    rank_check<-min(ks_table$rank.check) #CHECKS RANK ORDERING
    if (rank_check==0)
    {
      temp1<-ks_table[which(ks_table$rank.check==0),]
      rank_break<-min(as.numeric(temp1$Var2)) #GIVES THE DECILE WHERE RANK ORDER BREAKS
    }
    return(ks_table)
  }
  
  
  ks_table<-main_table(PredProb,ActualOutcome,ord)
  max_ks<-max(ks_table$ks)
  table_ks<-round(max_ks*100,digit=4) #KS OF THE TABLE
  temp2<-ks_table[which(ks_table$ks==max_ks),]
  bad_capture_at_max_ks<-round(temp2$cum.bad.capture[1]*100,digit=4) # BAD CAPTURE AT MAX KS
  good_capture_at_max_ks<-round(temp2$cum.good.capture[1]*100,digit=4)# GOOD CAPTURE AT MAX KS
  max_ks_at_decile<-temp2$Var2[1]# MAX KS ATTAINED AT DECILE
  bad_rate_till_max_ks<-round(temp2$cum.bad.rate[1]*100,digit=4) # CUM BAD RATE TILL MAX KS DECILE
  
  tot.good<-max(ks_table$cum.good.tot)
  tot.bad<-max(ks_table$cum.bad.tot)
  
  summary_file<-paste(acknowledgement,"summary.txt",sep="_")
  
  
  if ("ks" %in% reqd_tests==TRUE)
  {
    
    
    
    ##########################################################################
    ################### GETTING READY FOR OUTPUT TABLES ######################
    ##########################################################################
    
    ####### KS TABLE #######
    ks_output_table<-ks_table
    ks_output_table$cum.bad.tot<-NULL
    ks_output_table$cum.good.tot<-NULL
    ks_output_table$rank.check<-NULL
    ks_output_table$cum.good.rate<-NULL
    ks_output_table$cum.good.capture<-NULL
    ks_output_table$cum.tot.capture<-NULL
    ks_output_table$cum.tot<-NULL
    ks_output_table$ginni<-NULL
    
    
    ks_output_table$pred_prob.avg<-round((ks_output_table$pred_prob.avg*100),digits=4)
    ks_output_table$bad.rate<-round((ks_output_table$bad.rate*100),digits=4)
    ks_output_table$cum.bad.rate<-round((ks_output_table$cum.bad.rate*100),digits=4)
    ks_output_table$cum.bad.capture<-round((ks_output_table$cum.bad.capture*100),digits=4)
    ks_output_table$ks<-round((ks_output_table$ks*100),digits=4)
    ks_output_table$good.tot<-NULL
    
    ks_output_table <- rename(ks_output_table, c(Var2="Score Group No", Freq= "No. Of Cases",
                                                 bad.tot="No. Of Bads",
                                                 pred_prob.min="Minimum Score",
                                                 pred_prob.max="Maximum Score",
                                                 pred_prob.avg="Expected % Bad" ,
                                                 bad.rate="Observed % Bad" ,
                                                 cum.bad.rate="Observed Cumulitive % Bad" ,
                                                 cum.bad.capture="% Bad Captured" ,
                                                 ks="KS Measure (in %)"
    ))
    
    #setwd("/Users/prithwirajmukherjee/Desktop/new\ output\ format")
    
    write.table(ks_output_table,paste(acknowledgement,"ks.txt",sep="_"),row.names=F,sep='|',quote=FALSE)
    
    ##########################################################################
    ######################### CREATING THE GRAPHS ############################
    ##########################################################################
    
    ### KS GRAPH ###
    Var2<-0
    cum.bad.capture<-0
    cum.good.capture<-0
    tempdata<-data.frame(Var2,cum.bad.capture,cum.good.capture)
    tokeep<-c('Var2','cum.bad.capture','cum.good.capture')
    ks1<-ks_table[tokeep]
    ks_temp<-rbind(tempdata,ks1)
    
    graph_table<-data.frame(ks_temp$Var2,ks_temp$cum.bad.capture*100,ks_temp$cum.good.capture*100)
    names(graph_table)=c("Score Group No","%Bad Captured","%Good Captured")
    
    write.table(graph_table,paste(acknowledgement,"ks_g.txt",sep="_"),row.names=F,quote=F,sep='|')
    #################################### KS OUTPUT JSON ##################################################
    
    summary_tab<-ks_output_table
    
    cond1<-ifelse(sum(ks_table$rank.check)<nrow(ks_table),"not","")
    summary1<-paste("1.KS Table does",cond1,"rank orders with",nrow(summary_tab),"groups")
    brk_grp<-summary_tab[ which(ks_table$rank.check=='0'),]
    
    try(if(cond1!=""){
      summary2<-paste("Interpretation:","Rank ordering breaks at groups",paste(brk_grp$"Score Group No",collapse=','),"from top as the Observed Percent of Bad has increased from the previous group")
    }
    else{
      summary2<-""
    }
    ,silent=TRUE)
    
    grp<-summary_tab[summary_tab$"KS Measure (in %)"==max(summary_tab$"KS Measure (in %)"),]
    summary3<-paste("2.Maximum KS is",paste(max(summary_tab$"KS Measure (in %)"),"%",sep=""),"which is attained at group",grp$"Score Group No","from top")[1]
    tot_bad_percent<-sum(summary_tab$"No. Of Bads")
    max_ks_data<-summary_tab[1:grp$"Score Group No",]
    tot_bad_percent_max_ks<-sum(max_ks_data$"No. Of Bads")
    max_ks_percent<-round((tot_bad_percent_max_ks/tot_bad_percent)*100,2)
    #ifelse(max(summary_tab$"KS Measure(in %)")>40,"satisfactory","not satisfactory")
    cond_summ3<-max(summary_tab$"KS Measure (in %)")
    summary3_interpret1<-paste("Interpretation: Since the maximum KS is greater than 40 percent,the model performance (discriminating power) is satisfactory on validation sample")
    summary3_interpret2<-paste("Interpretation: Since the maximum KS is less than 40 percent,the model performance (discriminating power) is not satisfactory on validation sample")
    summary3_interpret<-ifelse(cond_summ3>40,summary3_interpret1,summary3_interpret2)
    
    summary4<-paste("3.Bad capture at group",paste(grp$ScoreGroupNumber,collapse=","),"from top is",paste(max_ks_percent,"%",sep=""))
    ks<-max(summary_tab$"KS Measure (in %)")
    
    heading1<-summary1
    interpret1<-summary2
    heading2<-summary3
    interpret2<-summary3_interpret
    heading3<-summary4
    
    
    ks_summary<-data.frame(testname="ks",heading=c(heading1,heading2),interpretation=c(interpret1,interpret2))
    
    if(summary_file %in% list.files()==TRUE) {write.table(ks_summary,summary_file,quote=FALSE,sep='|',row.names=F,append=T,col.names=F)} else
    {write.table(ks_summary,summary_file,quote=F,sep='|',row.names=F,append=T,col.names=T)}
    
    
    
    ks_output<-data.frame(testName="ks",csv_path=paste(acknowledgement,csv_path=c('ks.txt','ks_g.txt'),sep="_"),graph=c('no','yes'),heading=c('KS TABLE','Bad-Good Capture Graph'),conclusion='',interpretation='',yAxisLabel=c('',''))
    
    output_file<-paste(acknowledgement,"output.txt",sep="_")
    
    if(output_file %in% list.files()==TRUE) {write.table(ks_output,output_file,quote=F,sep='|',row.names=F,append=T,col.names=F)} else
    {write.table(ks_output,output_file,quote=F,sep='|',row.names=F,append=T,col.names=T)}
    
    
  }
  
  
  
  if("ROC" %in% reqd_tests==TRUE){
    
    ks_table$ginni<-0
    ginni_sum<-0
    for (i in 1:nrow(ks_table))
    {
      if(i==1)
      {
        ks_table$ginni[i]<-((ks_table$cum.bad.capture[i])*(ks_table$cum.good.capture[i]))/2
      }
      else
      {
        ks_table$ginni[i]<-((ks_table$cum.bad.capture[i]+ks_table$cum.bad.capture[i-1])*(ks_table$cum.good.capture[i]-ks_table$cum.good.capture[i-1]))/2
      }
      ginni_sum<-ginni_sum+ks_table$ginni[i]
    }
    
    ginni_index<-ginni_sum - 0.50 #FINAL GINNI INDEX
    
    
    tot.good<-max(ks_table$cum.good.tot)
    tot.bad<-max(ks_table$cum.bad.tot)
    
    ks_table$pecentgood<-ks_table$good.tot/tot.good
    ks_table$percentbad<-ks_table$bad.tot/tot.bad
    
    
    
    
    vars_to_keep<-c("pred_prob.max","cum.bad.tot","cum.good.tot")
    temp3<-ks_table[vars_to_keep]
    pred_prob.max<-c(0)
    cum.bad.tot<-c(0)
    cum.good.tot<-c(0)
    temp4<-data.frame(pred_prob.max, cum.bad.tot, cum.good.tot)
    
    specificity_data<-rbind(temp3,temp4)
    specificity_data$TP<-0
    specificity_data$FP<-0
    specificity_data$Precision<-0
    
    for (i in 2:nrow(specificity_data))
    {
      specificity_data$TP[i]<-specificity_data$cum.bad.tot[i-1]
      specificity_data$FP[i]<-specificity_data$cum.good.tot[i-1]
      specificity_data$Precision[i]<-round((((specificity_data$TP[i])/(specificity_data$TP[i]+specificity_data$FP[i]))*100),digits=4)
      
    }
    specificity_data$TN<-tot.good-specificity_data$FP
    specificity_data$FN<-tot.bad-specificity_data$TP
    specificity_data$sensitivity<-round((specificity_data$TP/tot.bad*100),digits=4)
    specificity_data$specificity<-round(((1-specificity_data$FP/tot.good)*100),digits=4)
    
    specificity_data$accuracy<-round((((specificity_data$TP+specificity_data$TN)/(tot.bad+tot.good))*100),digits=4)
    
    ## Incorporating ROC value in specificity data
    
    ROC<-data.frame(matrix(nrow=10,ncol=4))
    ROC$height = ((specificity_data$sensitivity[-1]+specificity_data$sensitivity[-length(specificity_data$sensitivity)])/2)
    ROC$width = -diff(specificity_data$specificity)
    ROC$diff = diff(specificity_data$sensitivity)
    ROC$AUC<- ROC$height*ROC$width/(100*100)
    vars_to_keep<-c("diff","width","AUC")
    ROC<-ROC[,vars_to_keep]
    
    roc<-sum(ROC$AUC)
    
    
    ROC<- rename(ROC, c(diff="Difference_in_Sensitivity(in %)",width="Difference_in_Specificity(in %)",AUC="Area_Under_the_BIN"))
    
    write.table(ROC,paste(acknowledgement,"ROC.txt",sep="_"),row.names=F,quote=F,sep='|')
    
    
    graph_table<-data.frame(ROC$"Difference_in_Sensitivity(in %)",ROC$"Difference_in_Specificity(in %)",ROC$"Area_Under_the_BIN")
    names(graph_table)=c("Difference_in_Sensitivity(in %)","Difference_in_Specificity(in %)","Area_Under_the_BIN")
    write.table(graph_table,paste(acknowledgement,"ROC_g.txt",sep="_"),row.names=F,quote=F,sep='|')
    
    ROC_output<-data.frame(testName="ROC",paste(acknowledgement,csv_path=c('ROC.txt','ROC_g.txt'),sep="_"),graph=c('no','yes'),heading=c('Difference in Sensitivity(in %), Difference in Specificity( in %) and Area under the curve for each BIN'),conclusion='',interpretation='',yAxisLabel=c('',''))
    output_file<-paste(acknowledgement,"output.txt",sep="_")
    
    
    if(output_file %in% list.files()==TRUE) {write.table(ROC_output,output_file,quote=F,sep='|',row.names=F,append=T,col.names=F)} else
    {write.table(ROC_output,output_file,quote=F,sep='|',row.names=F,append=T,col.names=T)}
    
    
    
    summary<-data.frame(testname=c("ROC","GINI"),heading=c("",""),interpretation=c(sprintf(" Current Performance Gini Index based on KS table is %.2f",ginni_index),
                                                                                   sprintf(" Area under ROC curve on ROC table is %.2f",roc)))
    
    
    if(summary_file %in% list.files()==TRUE) {write.table(summary,summary_file,quote=F,sep='|',row.names=F,append=T,col.names=F)} else
    {write.table(summary,summary_file,quote=F,sep='|',row.names=F,append=T,col.names=T)}
    
    
  }
  
  
  if("sen_spec" %in% reqd_tests==TRUE){
    
    
    tot.good<-max(ks_table$cum.good.tot)
    tot.bad<-max(ks_table$cum.bad.tot)
    vars_to_keep<-c("pred_prob.max","cum.bad.tot","cum.good.tot")
    temp3<-ks_table[vars_to_keep]
    pred_prob.max<-c(0)
    cum.bad.tot<-c(0)
    cum.good.tot<-c(0)
    temp4<-data.frame(pred_prob.max, cum.bad.tot, cum.good.tot)
    
    specificity_data<-rbind(temp3,temp4)
    specificity_data$TP<-0
    specificity_data$FP<-0
    specificity_data$Precision<-0
    
    for (i in 2:nrow(specificity_data))
    {
      specificity_data$TP[i]<-specificity_data$cum.bad.tot[i-1]
      specificity_data$FP[i]<-specificity_data$cum.good.tot[i-1]
      specificity_data$Precision[i]<-round((((specificity_data$TP[i])/(specificity_data$TP[i]+specificity_data$FP[i]))*100),digits=4)
      
    }
    specificity_data$TN<-tot.good-specificity_data$FP
    specificity_data$FN<-tot.bad-specificity_data$TP
    specificity_data$sensitivity<-round((specificity_data$TP/tot.bad*100),digits=4)
    specificity_data$specificity<-round(((1-specificity_data$FP/tot.good)*100),digits=4)
    
    specificity_data$accuracy<-round((((specificity_data$TP+specificity_data$TN)/(tot.bad+tot.good))*100),digits=4)
    
    
    
    specificity_data$cum.bad.tot<-specificity_data$cum.good.tot<-NULL
    specificity_data$FP<-specificity_data$FN<-NULL
    specificity_data$TP<-specificity_data$TN<-NULL
    
    ##########################################################################
    ################### GETTING READY FOR OUTPUT TABLES ######################
    ##########################################################################
    
    ####### SPECIFICITY TABLE #######
    specificity_data <- rename(specificity_data, c(pred_prob.max="Score",
                                                   specificity= "Specificity (in %)",
                                                   sensitivity="Sensitivity (in %)",
                                                   accuracy="Accuracy (in %)",
                                                   Precision="Precision (in %)"
    ))
    
    write.table(specificity_data,paste(acknowledgement,"sen_spec.txt",sep="_"),row.names=F,quote=F,sep='|')
    
    
    graph_table<-data.frame(specificity_data$Score,specificity_data$"Sensitivity (in %)",specificity_data$"Specificity (in %)",specificity_data$"Accuracy (in %)")
    names(graph_table)=c("Score","Sensitivity (in %)","Specificity (in %)","Accuracy (in %)")
    
    write.table(graph_table,paste(acknowledgement,"sen_spec_g.txt",sep="_"),quote=F,row.names=F,sep='|')
    
    #sen_spec_summary<-data.frame(testname="",heading='',interpretation='')
    
    sen_spec_output<-data.frame(testName="sen_spec",paste(acknowledgement,csv_path=c('sen_spec.txt','sen_spec_g.txt'),sep="_"),graph=c('no','yes'),heading=c('Specificity, Sensitivity, Accuracy,Precesion  by Score Cut-Offs','Specificity, Sensityvity,Accuracy and ROC Graph'),conclusion='',interpretation='',yAxisLabel=c('',''))
    
    output_file<-paste(acknowledgement,"output.txt",sep="_")
    
    if(output_file %in% list.files()==TRUE) {write.table(sen_spec_output,output_file,quote=F,sep='|',row.names=F,append=T,col.names=F)} else
    {write.table(sen_spec_output,output_file,quote=F,sep='|',row.names=F,append=T,col.names=T)}
    
  }
  
}
Goodness_of_fit(PredProb,ActualOutcome,acknowledgement,reqd_tests)

run_details<-data.frame(RunId='2',acknowledgement_id=acknowledgement,rag='GREEN')
write.table(run_details,paste(paste(acknowledgement,"run_details",sep='_'),"txt",sep='.'),quote=F,sep="|",row.names=FALSE)

