####################################  SUMMARY CALCULATION ##############
library(jsonlite)
setwd("../conf/files")
print(getwd())
args<-commandArgs(T)
data<-args[1]
print(data)
listjson<-fromJSON(data)
acknowledgement<-as.character(args[2])

work_data<-read.csv(listjson$schemeDumpFile,header=TRUE)
independent_tab<-work_data

num_summary<-data.frame(1:8)
names(num_summary)="row_num"
for (i in 1:ncol(independent_tab))
{
  if(class(independent_tab[,i])=="numeric" |class(independent_tab[,i])=="integer" )
    
  {
    
    summry<-data.frame(t(summary(independent_tab[,i])))[,2:3]
    names(summry)=c("Measure",paste(names(independent_tab)[i],"Values",sep="_"))  
    sd<-data.frame(Measure="sd",values=sd(independent_tab[,i]))
    nmiss<-data.frame(Measure="nmiss",values=sum(is.na(independent_tab[,i])))
    nmiss<-rbind(sd,nmiss)
    names(nmiss)=c("Measure",paste(names(independent_tab)[i],"Values",sep="_"))
    summry<-rbind(summry,nmiss)
    summry_measure<-subset(summry,select=1)
    num_summary<-cbind(num_summary,subset(summry,select=2))

  }
  
}
num_summary<-data.frame(t(subset(num_summary,select=-1)))
names(num_summary)=c(t(summry_measure))
ColumnNames<-row.names(num_summary)
ColumnNames<-data.frame(ColumnNames)
num_summary<-data.frame(ColumnNames,num_summary)
names(num_summary)=c("Variable","Minimum","1st Quartile","Median","Average","3rd Quartile","Maximum","Standard Deviation","No. of Missing")
write.table(num_summary,paste(acknowledgement,"summary_dump.txt",sep="_"),quote=F,sep="|",row.names=F)
