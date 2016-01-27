#####################################################################################################################


# CHAID CHAMPION



libpath<-.libPaths()
lib<-c("sqldf","jsonlite","gtools","reshape","doBy","reshape2","data.table","plyr","e1071")
lapply(lib, require, character.only=T)
library(stringr,lib.loc=libpath)
library(caret,lib.loc=libpath)


################################################# CONNECTION PART ###########################################
setwd("../conf/files")
#setwd("/home/hduser/Documents/Pabitra/R Files")
print(getwd())
#list.files()
#  listjson<-fromJSON("45e3d47e-f23a-4386-9955-6f2bb0247644_filename.json")
args<-commandArgs(T)
data<-args[1]
print(data)
listjson<-fromJSON(data)
print(listjson)
acknowledgement<-as.character(args[2])

#print(listjson)

#listjson<-fromJSON(data)
#detach(package:jsonlite, unload = TRUE)


#####################################################################################################################


ord=10 
#reqd_tests<-c("ks_table","ROC","Sen_Spec") 
#acknowledgement<-"Chaid_Champion_Testing" 
reqd_tests<-listjson$testCases$name

#chaid_pred(json,ord,reqd_tests,acknowledgement) 
#setwd("/Users/prithwirajmukherjee/Documents/sandbox/workspace/23-12-2015/prm-model-testbench/conf/files")


#json<-'{"ivDumpFile":"2c8bc583-74ba-4864-9b63-e96eee578e4b_iv_dump.csv","testCases":[{"name":"ginny","threshold":0.125},{"name":"ks","threshold":0.25},{"name":"sen_spec","threshold":0.25}],"technique":"CHAID","responseDumpFile":"2c8bc583-74ba-4864-9b63-e96eee578e4b_response_dump.csv","functionalClassification":"PD","params":[{"predicate":"DecisionTree_negative_balance >= 831","percentage":".52"},{"predicate":"DecisionTree_Trans_count < 21 AND DecisionTree_negative_balance < 831","percentage":".27"},{"predicate":"DecisionTree_negative_balance < 472 AND DecisionTree_Trans_count >= 21 AND DecisionTree_negative_balance < 831","percentage":".71"},{"predicate":"DecisionTree_Trans_count < 38 AND DecisionTree_negative_balance >= 472 AND DecisionTree_Trans_count >= 21 AND DecisionTree_negative_balance < 831","percentage":".36"},{"predicate":"DecisionTree_Trans_count >= 38 AND DecisionTree_negative_balance >= 472 AND DecisionTree_Trans_count >= 21 AND DecisionTree_negative_balance < 831","percentage":".64"}],"calibrationValues":[],"requestId":"2c8bc583-74ba-4864-9b63-e96eee578e4b","industries":[],"challenge":false,"ivParams":[{"attrId":"5ccfbd23-a350-40e9-ab8b-9451b500d955","name":"DecisionTree_Trans_count","entityId":"73fad7b3-3441-40aa-88b5-0880a517d782"},{"attrId":"d570c619-fd17-407b-9d59-9ffb06a30fc5","name":"DecisionTree_negative_balance","entityId":"73fad7b3-3441-40aa-88b5-0880a517d782"},{"attrId":"fab1f35b-2ca7-4a62-ba6d-5da84ca6da9d","name":"DecisionTree_student1_flag","entityId":"73fad7b3-3441-40aa-88b5-0880a517d782"},{"attrId":"4783e11b-e916-4d59-948a-bc662f164484","name":"DecisionTree_id","entityId":"73fad7b3-3441-40aa-88b5-0880a517d782"},{"attrId":"9c211738-2a6b-4335-a1fb-2d71b8aec259","name":"DecisionTree_pd_flag","entityId":"73fad7b3-3441-40aa-88b5-0880a517d782"},{"attrId":"1333f604-3b0c-41c6-a608-2f62730d0fce","name":"DecisionTree_model_id","entityId":"73fad7b3-3441-40aa-88b5-0880a517d782"},{"attrId":"48b4382c-c710-4556-a2d2-72483d6df217","name":"DecisionTree_home_flag","entityId":"73fad7b3-3441-40aa-88b5-0880a517d782"},{"attrId":"28a6ba73-a173-425d-921e-67c252efdacf","name":"DecisionTree_duration","entityId":"73fad7b3-3441-40aa-88b5-0880a517d782"}],"schemeDumpFile":"2c8bc583-74ba-4864-9b63-e96eee578e4b_scheme_dump.csv","interceptValue":null,"usedVariables":["DecisionTree_negative_balance","DecisionTree_Trans_count"],"dataSource":null,"linkedColumn":"DecisionTree_id","dataSrc":{"password":"tectes","dbname":"StagingDB","driver":"com.mysql.jdbc.Driver","port":"3306","columns":null,"host":"jdbc:mysql://127.0.0.1:3306","user":"root","table":"StagingTable"}}'



#library(jsonlite) 
#listjson<-fromJSON(json) 


print("Hiiiiii")
chaid_pred<-function(listjson,ord,reqd_tests,acknowledgement)  # use data.frame in data_frame object in double quote  
{
  print("First Function Line")
  rules<-listjson$params 
  
  resfile<-listjson$responseDumpFile
  
  resdata<-read.csv(resfile,header=T)
  response<-subset(resdata,select=Observed_Outcome)
  names(response)="Response"
  rm(resdata)
  library(plyr) 
  library(sqldf) 
  ord<-10
  work_data<-read.csv(listjson$schemeDumpFile,header=TRUE)
  print(str(work_data))
#       work_data$DecisionTree_Trans_count<-as.numeric(as.character(work_data$DecisionTree_Trans_count))
#       work_data$DecisionTree_negative_balance<-as.numeric(as.character(work_data$DecisionTree_negative_balance))
  output_data<-data.frame() 
  for(i in 1:nrow(rules)) 
  { 
    q<-paste("select * from work_data where" ,rules[i,1],sep=" ") 
    rule_data<-sqldf(q) 
    
    rule_data$percentage<-as.numeric(as.character(rules[i,2])) 
    output_data<-rbind(output_data,rule_data) 
  }
  
  print("First Function Line 2")
  output_data<-subset(output_data,select=-X)
  
  #output<-chaid_pred(data,listjson) 
  ActualOutcome<-response$Response 
  PredProb<-output_data$percentage 
  
  
  
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
