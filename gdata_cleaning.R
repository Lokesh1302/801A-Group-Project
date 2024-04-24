

library(dplyr)

setwd("C:/Users/Jack/OneDrive/Documents/ACC 822/Project Data")

# 1.1 Read in data
EKPO=readxl::read_xlsx("EKPO_01012022_31032022.xlsx")
EKKO=readxl::read_xlsx("EKKO_01012022_31032022.xlsx")
EKBE=readxl::read_xlsx("EKBE_01012022_31032022.xlsx")
EBAN=readxl::read_xlsx("EBAN_01012022_31032022.xlsx")
EKET=readxl::read_xlsx("EKET_01012022_31032022.xlsx")
Activity=readxl::read_xlsx("Activity_01012022_31032022.xlsx")

# Valuable Columns; Their Data Frame; Their Meaning

#LIFNR;   Vendor Account Number

### EKBE
#SHKZG;   Debit/Card Indicator

### EKPO
#LGORT;   Storage Location

### EBAN
#LFDAT;   Item Delivery Date (Expected)
#FRGDT;   Purchase Requisition Release Date (Approval Date)
#BADAT;   Requisition Date 

### EKET
#EINDT;   Scheduled Delivery Date

# 1.2. cleanup data

# rename activity$_CASE_KEY to X_CASE_KEY
#Activity=Activity%>%rename(X_CASE_KEY="_CASE_KEY")
Activity=Activity[,!(colnames(Activity) %in% c("MANDT"))]
Activity=Activity%>%select(-starts_with("CHANGED"))
# Convert EVENTTIME to POSIXlt format
Activity$time <- as.POSIXlt(Activity$EVENTTIME, format = "%b %d %Y %I:%M %p")
Activity=Activity%>%arrange(EBELN,EBELP,time)%>%
            group_by(X_CASE_KEY,EBELN,EBELP)%>%
            mutate(create=if_else(ACTIVITY_EN=="Create Purchase Order Item",1,0),
                  receive=if_else(ACTIVITY_EN=="Record Goods Receipt",1,0),
                  changeconfirmeddeliverydate=if_else(ACTIVITY_EN=="Change Confirmed Delivery Date",1,0),
                  changecontract=if_else(ACTIVITY_EN=="Change Contract",1,0),
                  changecurrency=if_else(ACTIVITY_EN=="Change Currency",1,0),
                  changedeliveryindicator=if_else(ACTIVITY_EN=="Change Delivery Indicator",1,0),
                  changefinalinvoiceindicator=if_else(ACTIVITY_EN=="Change Final Invoice Indicator",1,0),
                  changeoutwarddeliveryindicator=if_else(ACTIVITY_EN=="Change Outward Delivery Indicator",1,0),
                  changeprice=if_else(ACTIVITY_EN=="Change Price",1,0),
                  changequantity=if_else(ACTIVITY_EN=="Change Quantity",1,0),
                  changerequesteddeliverydate=if_else(ACTIVITY_EN=="Change requested delivery date",1,0),
                  changestoragelocation=if_else(ACTIVITY_EN=="Change Storage Location",1,0))%>%
            mutate(receive=cumsum(receive)*receive) #use 1,2,3 to index the record goods receipt events

ActivitySummary=Activity%>%group_by(X_CASE_KEY,EBELN,EBELP)%>%
                   summarize(
                   #get the number of each type of change-item events 
                   changeconfirmeddeliverydate=sum(changeconfirmeddeliverydate), 
                   changecontract=sum(changecontract),
                   changecurrency=sum(changecurrency),
                   changedeliveryindicator=sum(changedeliveryindicator),
                   changefinalinvoiceindicator=sum(changefinalinvoiceindicator),
                   changeoutwarddeliveryindicator=sum(changeoutwarddeliveryindicator),
                   changeprice=sum(changeprice),
                   changequantity=sum(changequantity),
                   changerequesteddeliverydate=sum(changerequesteddeliverydate),
                   changestoragelocation=sum(changestoragelocation),
                   numdelivery=max(receive))

createTime=Activity%>%filter(create==1)%>%
      select(X_CASE_KEY,EBELN,EBELP,time)%>%
      rename(createtime=time)
#86402 cases with create purchase
#dim(createTime)
  
firstReceiveTime=Activity%>%filter(receive==1)%>%
      select(X_CASE_KEY,EBELN,EBELP,time)%>%
      rename(firstreceivetime=time)
#76412 cases with first receive goods
#dim(firstReceiveTime)

#get GD days with 2 decimal digits
options(scipen = 999)
data1=createTime%>%inner_join(firstReceiveTime,by=c("X_CASE_KEY","EBELN","EBELP"))%>%
                  inner_join(ActivitySummary,by=c("X_CASE_KEY","EBELN","EBELP"))%>%
                  mutate(GDdays=round(100*as.numeric(difftime(firstreceivetime,createtime,units="days")))/100)

# save data1 to activity_summary.csv
write.table(data1,"activity_summary.csv",sep=",",row.names=FALSE,col.names=TRUE)

############################################

# load activity_summary.csv to data
data=read.csv("activity_summary.csv")

data$EBELP=paste0("000", data$EBELP)
data$X_CASE_KEY=paste0("100",data$EBELN,data$EBELP)

data$EBELP=as.character(data$EBELP)

# clean EKPO table
EKPO=EKPO[,(colnames(EKPO) %in% c("NETPR","EBELP","EBELN","BUKRS",
                                  "MATNR","MATKL","PSTYP",
                                  "WERKS", "LGORT"))]
# clean EBAN table
EBAN=EBAN[,(colnames(EBAN) %in% c("EBELP","EBELN","ERNAM","LFDAT", 
                                  "FRGDT", "ESTKZ"))]
EBAN=EBAN[!(duplicated(EBAN)),]

# clean EKKO table
EKKO=EKKO[,(colnames(EKKO) %in% c("EBELN","BSART","LIFNR"))]

# clean EKBE table
EKBE=EKBE[,(colnames(EKBE) %in% c("EBELP","EBELN","SHKZG"))]

# clean EKET table
EKET=EKET[,(colnames(EKET) %in% c("EBELP","EBELN","EINDT"))]

#join tables
data=data%>%left_join(EKPO,by=c("EBELP","EBELN"))%>%
            left_join(EBAN,by=c("EBELP","EBELN"))%>%
            left_join(EKKO,by=c("EBELN"))%>%
            left_join(EKBE,by=c("EBELP","EBELN"))%>%
            left_join(EKET,by=c("EBELP","EBELN"))

# Remove duplicates in data
data=data[!(duplicated(data)),]

# convert columns from POSIXct to POSIXlt format
data$LFDAT <- as.POSIXlt(data$LFDAT, format = "%Y-%m-%d %H:%M:%S")
data$EINDT <- as.POSIXlt(data$EINDT, format = "%Y-%m-%d %H:%M:%S")
data$FRGDT <- as.POSIXlt(data$FRGDT, format = "%Y-%m-%d %H:%M:%S")

# make new column: deiliverydaysexpected = difference between LFDAT and createtime
data=data%>%mutate(deliverydaysexpected=1+round(100*as.numeric(difftime(LFDAT,createtime,units="days")))/100)

# make new column: scheduledshipdays = difference between EINDT and createtime
data=data%>%mutate(scheduledshipdays=1+round(100*as.numeric(difftime(EINDT,createtime,units="days")))/100)

# make new column: approvaldays = difference between FRGDT and createtime
data=data%>%mutate(approvaldays=1+round(100*as.numeric(difftime(FRGDT,createtime,units="days")))/100)

# make new column: lateGR = 1 if scheduledshipdays-GDdays > 0, 0 otherwise
data=data%>%mutate(lateGR=if_else(scheduledshipdays-GDdays>0,1,0))

# count non-NA values in each column of data
na_counts <- colSums(!is.na(data))

# output GD data
write.table(data,"GData2.csv",sep=",",row.names=FALSE,col.names=TRUE)


