


print("co-dispenses of PGx ,defined as >= 2 overlaps of more than 14 days of supply")

#Load MAPB list

MAPB_List=read.csv("MAPB_NDCs_2020_0106-1.txt",sep = "\t",colClasses = "character")
head(MAPB_List)

#Include FDA biomarker and therapeutic area
MAPB_List$FDA_Therapeutic_Area=NA
MAPB_List$FDA_Biomarker=NA
FDA_infor=read.csv('FDAPGx_BioMarker_InDrugLabeling - Sheet1.csv')
FDA_infor$Therapeutic.Area.=as.character(FDA_infor$Therapeutic.Area.)
FDA_infor$Biomarker.=as.character(FDA_infor$Biomarker.)

for (i in (1:nrow(MAPB_List))){
  MAPB_List$FDA_Therapeutic_Area[i]=paste0(FDA_infor$Therapeutic.Area.[grepl(pattern = MAPB_List$MAPB_name[i],x = FDA_infor$Drug,ignore.case = T)],collapse = ",")
  MAPB_List$FDA_Biomarker=paste0(FDA_infor$Biomarker.[grepl(pattern = MAPB_List$MAPB_name[i],x = FDA_infor$Drug,ignore.case = T)],collapse = ",")

  }
head(MAPB_List)
dim(MAPB_List)
#
#
#
#Load PharmacyClaimInfor and Member
Data_Dir="/n/scratch2/DL_temp/PGx"

X=load(file.path(Data_Dir,"PGx_Cohort_PharmacyClaim_2016_2019.Rdata"))
PGx_Cohort_PharmacyClaim=get(X)
head(PGx_Cohort_PharmacyClaim)

X=load(file.path(Data_Dir,"PGx_Cohort_Members_2016_2019.Rdata"))
PGx_Cohort_Members=get(X)
head(PGx_Cohort_Members)


##only keep those records with MAPBs
PGx_Cohort_PharmacyClaim=PGx_Cohort_PharmacyClaim[PGx_Cohort_PharmacyClaim$NationalDrugCode%in%MAPB_List$NDC,]

dim(PGx_Cohort_PharmacyClaim)

head(PGx_Cohort_PharmacyClaim)



# #First of all, only people with more than one drug can be kept and drug days of supplies >=14 days
OnDrugTable=table(PGx_Cohort_PharmacyClaim$MemberId)
sum(OnDrugTable>1)
PGx_Cohort_PharmacyClaim=PGx_Cohort_PharmacyClaim[PGx_Cohort_PharmacyClaim$MemberId%in%names(OnDrugTable)[OnDrugTable>1],]

PGx_Cohort_PharmacyClaim=PGx_Cohort_PharmacyClaim[PGx_Cohort_PharmacyClaim$DaysSupply>=14,]

dim(PGx_Cohort_PharmacyClaim)
sum(table(PGx_Cohort_PharmacyClaim$MemberId)>1)

#over lap period
head(PGx_Cohort_PharmacyClaim)

class(PGx_Cohort_PharmacyClaim$DispenseDate)

library(dplyr)
Member_MAPB=left_join(PGx_Cohort_PharmacyClaim,MAPB_List[,c("NDC","MAPB_name")],by = c("NationalDrugCode" = "NDC"))
sum(table(Member_MAPB$MemberId)>1)
head(Member_MAPB)

Member_MAPB$DispenseDate=as.Date(Member_MAPB$DispenseDate)
Member_MAPB$EndDate=Member_MAPB$DispenseDate+Member_MAPB$DaysSupply

Member_MAPB=Member_MAPB[,c("MemberId","MAPB_name","DispenseDate","EndDate")]
#Member_MAPB=split(Member_MAPB,f = as.factor(Member_MAPB$MemberId))

#function to get over lap periiod between MAPBs
#install.packages("lubridate")
library("lubridate")

Get_overLap=function(OnDrug){

OnDrug$Intervals=new_interval(OnDrug$DispenseDate, OnDrug$EndDate, tzone="UTC")

#head(OnDrug)

OnDrug=OnDrug[,c("MemberId","MAPB_name","Intervals")]

OnDrug2=OnDrug
names(OnDrug2)=c("MemberId","MAPB_name2","Intervals2")

Merged=inner_join(OnDrug,OnDrug2,by = "MemberId")#much faster (10X) than merge function

Merged=Merged[Merged$MAPB_name!=Merged$MAPB_name2,]
Merged$OverLap=intersect(Merged$Intervals,Merged$Intervals2)

Merged$OverLap_number_days=day(as.period(intersect(Merged$Intervals,Merged$Intervals2), "days"))

Merged=Merged[!is.na(Merged$OverLap_number_days),]
Merged=Merged[Merged$OverLap_number_days>=14,]#be careful the resulted output is symetric (therefore number of pair should/2)

return(Merged)
}


#loop through multiple chunk ( otherwise too big to operate on)

AllMembers=unique(Member_MAPB$MemberId)
VecMatch=match(AllMembers,Member_MAPB$MemberId)


Vec_Save=NULL

TaskID=as.integer(commandArgs(trailingOnly = T)[[1]][1])
print(TaskID)

for (i in  (((TaskID-1)*100+1):(TaskID*100))){
  print(paste0("task ",TaskID," from member ",max(ceiling(length(AllMembers)/1000)*(i-1),1)," to member ",ceiling(length(AllMembers)/1000)*(i)))
  
  Index=max(ceiling(length(AllMembers)/1000)*(i-1),1):ceiling(length(AllMembers)/1000)*(i)
  
  Vec=Get_overLap(Member_MAPB[which(Member_MAPB$MemberId%in%AllMembers[Index]),])
  
  Vec_Save=rbind(Vec_Save,Vec)
}


save(Vec_Save,file = paste0(Data_Dir,'/Co_dispenses_',TaskID,'.Rdata'))
