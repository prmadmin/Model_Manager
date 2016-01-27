# setwd("/home/hduser/Documents/TEST1/prm-model-testbench/conf/files")
#list.files()
setwd("../conf/files")
#getwd()
library(sqldf)
args<-commandArgs(T)
req_json<-args[1]
acknowledgement<-args[2]

#  req_json<-'{"ivDumpFile":"33d77a25-1429-47ef-b36a-c0a9db2d308e_iv_dump.csv","requestId":"33d77a25-1429-47ef-b36a-c0a9db2d308e","schemeDumpFile":"33d77a25-1429-47ef-b36a-c0a9db2d308e_scheme_dump.csv","params":[{"attrId":"5ccfbd23-a350-40e9-ab8b-9451b500d955","name":"DecisionTree_Trans_count","entityId":"73fad7b3-3441-40aa-88b5-0880a517d782"},{"attrId":"d570c619-fd17-407b-9d59-9ffb06a30fc5","name":"DecisionTree_negative_balance","entityId":"73fad7b3-3441-40aa-88b5-0880a517d782"},{"attrId":"fab1f35b-2ca7-4a62-ba6d-5da84ca6da9d","name":"DecisionTree_student1_flag","entityId":"73fad7b3-3441-40aa-88b5-0880a517d782"},{"attrId":"4783e11b-e916-4d59-948a-bc662f164484","name":"DecisionTree_id","entityId":"73fad7b3-3441-40aa-88b5-0880a517d782"},{"attrId":"9c211738-2a6b-4335-a1fb-2d71b8aec259","name":"DecisionTree_pd_flag","entityId":"73fad7b3-3441-40aa-88b5-0880a517d782"},{"attrId":"1333f604-3b0c-41c6-a608-2f62730d0fce","name":"DecisionTree_model_id","entityId":"73fad7b3-3441-40aa-88b5-0880a517d782"},{"attrId":"48b4382c-c710-4556-a2d2-72483d6df217","name":"DecisionTree_home_flag","entityId":"73fad7b3-3441-40aa-88b5-0880a517d782"},{"attrId":"28a6ba73-a173-425d-921e-67c252efdacf","name":"DecisionTree_duration","entityId":"73fad7b3-3441-40aa-88b5-0880a517d782"}],"linkedColumn":"DecisionTree_id"}'

library(sqldf)
library(jsonlite)
#req_json<-"25a4e3cd-e995-45e6-96c6-308386139737_filename.json"
listjson<-fromJSON(req_json)

print("Test 1")

#####       IMPORTANT: 
#####                  1. Use R version >= 3.1.0
#####                  2. Open R Studio with "Run as Administrator" option - To be able to "install new packages"
#####                  3. Read through carefully to change:
#####                     i.   Input data path
#####                     ii.  Response/Target variable name
#####                     iii. Continuous & Categorical variable list in Flatfile OR Direct input names
#####                     iv.  Install "woe" package (one time) and include library
#####                     v.   Install "sqldf" package (one time) and include library
#####                     vi.  Output data path



ivdump<-listjson$ivDumpFile
schemedump<-listjson$schemeDumpFile
print(schemedump)
print("Test 2")
print(ivdump)


ivfile<-read.csv(ivdump,header=T)
ivfile<-subset(ivfile,select=c(Linked_Column,Observed_Outcome))
schemefile<-read.csv(schemedump,header=T)
schemefile<-subset(schemefile,select=-X)

#####   Change Input data path:
merged_file<-merge(schemefile,ivfile,by.x=listjson$linkedColumn,by.y="Linked_Column")
# names(merged_file)=gsub('\\.','_',names(merged_file))
query<-paste("select",paste(c(unique(listjson$ivParams$name),"Observed_Outcome"),collapse=','), "from merged_file")
indat<-sqldf(query)


linked_column<-listjson$linkedColumn
indat[,linked_column]<-as.character(indat[,linked_column])


# res_data<-read.csv("C:\\Users\\prm\\Desktop\\iv data\\75dfb018-0408-4f0a-a13f-93c7144b74db_iv_dump.csv",header = TRUE, sep = ",")
# res_data<-subset(res_data,select=-X)
  indat = indat[sample(nrow(indat), 1000), ]

#####   Response/Target variable name in the "Input data"
respvar = c("Observed_Outcome")

varlist<-data.frame()
for(i in 1:ncol(indat))
{
  lst<-data.frame(Name=names(indat)[i],Type=ifelse(class(indat[,i])==c("character")|class(indat[,i])==c("factor"),2,1))
  varlist<-rbind(varlist,lst)
  varlist<-unique(varlist)
}

print("Test 3")

#####   Input Variables from Flat file: File contains Variable Name & Type (1=Continuous AND 2=Categorical)
#varlist <- read.csv("C:\\Users\\prm\\Desktop\\iv data\\varlist.csv",header = TRUE, sep = ",")
cont = subset(varlist,Type == '1')
contvars = as.vector(cont$Name)
catg = subset(varlist,Type == '2')
catgvars = as.vector(catg$Name)

#############################################   OR   ######################################################

#####   Input Variables from Direct inputname: Continuous AND Categorical, two separate lists
# contvars = c("age","negative_balance")
# catgvars = c("coop_bank","local_market_current","mnths_18yr_bin","years_enrolment_bin")

#####   Install "woe" package (one time) and include library
#install.packages("woe")
library(woe)

#indat<-indat[1:1000,]

#####   For reference/input understanding only: No need to uncomment this portion

# woe(Data, Independent, Continuous, Dependent, C_Bin, Bad, Good)
# Data : Name of Data Set
# Independent : Name of the Independent Variable
# Continuous : True if the variable is continuous, False if variable is Ordinal or Nominal
# Dependent : Name of the Targer Variable
# C_Bin : Count of Bins to be computed -- Default 10 works
# Bad : Which categorical variable do you want to be bad
# Good : Which categorical variable do you want to be good

#####   Continuous variable loop: Binning and WOE & IV calculation
for (i in 1:length(contvars))
{  
  if(contvars[i] != respvar)
  {
  var_working = contvars[i]
  
  }


  eval(parse(text=paste(paste("indat$",var_working,sep="")," = ",paste("ifelse(is.na(indat$",var_working,"),0,indat$",var_working,")",sep=""),sep="")))
  eval(parse(text=paste(paste("indat$",var_working,sep="")," = ",paste("as.numeric(as.character(indat$",var_working,"))",sep=""))))
 
  out <- woe(Data=indat,var_working,TRUE,respvar,11,Bad=0,Good=1)
  
  out$Type = "Continuous"
  out$Varname = var_working
  colnames(out)[which(names(out) == "IV")] <- "MIV"
  if (i == 1)
  {
    out_woe = out
  } else {
    out_woe = rbind(out_woe,out)
  }
  out_woe$MIV = ifelse(is.na(out_woe$WOE)|out_woe$WOE == Inf|out_woe$WOE == -Inf,0,out_woe$MIV)
  out_woe$WOE = ifelse(is.na(out_woe$WOE)|out_woe$WOE == Inf|out_woe$WOE == -Inf,0,out_woe$WOE)
}

#####   Categorical variable loop: WOE & IV calculation
for (i in 1:length(catgvars))
{  
  var_working = catgvars[i]
  
  eval(parse(text=paste(paste("indat$",var_working,sep="")," = ",paste("as.character(indat$",var_working,")",sep=""))))
  
  out <- woe(Data=indat,var_working,FALSE,respvar,10,Bad=0,Good=1)
  out$MIN = 0                 #To match continuous output
  out$MAX = 0                 #To match continuous output
  out$Type = "Categorical"
  out$Varname = var_working
  colnames(out)[which(names(out) == "IV")] <- "MIV"
  if (length(contvars) == 0)  #This is intentional!! -- If no Continuous variables are present then this loop will be executed!
  {
    out_woe = out
  } else {
    out_woe = rbind(out_woe,out)
  }
  out_woe$MIV = ifelse(is.na(out_woe$WOE)|out_woe$WOE == Inf|out_woe$WOE == -Inf,0,out_woe$MIV)
  out_woe$WOE = ifelse(is.na(out_woe$WOE)|out_woe$WOE == Inf|out_woe$WOE == -Inf,0,out_woe$WOE)
}

print("Test 4")

# R> df[ , -which(names(df) %in% c("z","u"))]
# R> subset(df, select=-c(z,u))

#####   Install "sqldf" package (one time) and include library
#install.packages("sqldf")
library(sqldf)

out_iv <- sqldf(' select Varname,Type,sum(MIV) as IV
                from out_woe
                group by Varname,Type
                order by Type,Varname
                ')
out_iv$Importance = ifelse(out_iv$IV > 0.3,"High",ifelse((0.2 < out_iv$IV & out_iv$IV <= 0.3),"Medium","Low"))
out_iv<-subset(out_iv,select=-Type)


output_iv<-merge(listjson$ivParams,out_iv,by.x="name",by.y="Varname",all.x=T)
output_iv<-sqldf("select distinct attrId,name,entityId,IV,Importance from output_iv")
names(output_iv)[c(4,5)]=c("iv","importance")

write.csv(output_iv,paste(acknowledgement,"iv_output.csv",sep='_'),row.names=F,quote=F)

print("Test 5")

# 
# library(jsonlite)
# js<-toJSON(list(list(acknowledgement_id=acknowledgement),output_iv))
# 
# 
# library(RCurl)
# 
# 
# fileup<-fileUpload(filename="output.json",contents=js)
# jsonpost<-postForm("127.0.0.1:9000/saveModelResponse",content=fileup,.opts=list(verbose=TRUE,header=TRUE))
# 
# 
