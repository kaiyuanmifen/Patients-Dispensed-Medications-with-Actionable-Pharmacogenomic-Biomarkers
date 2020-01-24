


print("co-dispenses of PGx ,defined as >= 1 overlaps of at least 30 days of supply")

#Load MAPB list

MAPB_List=read.csv("MAPB_NDCs_2020_0106-1.txt",sep = "\t",colClasses = "character")
head(MAPB_List)

#Include FDA biomarker and therapeutic area
# MAPB_List$FDA_Therapeutic_Area=NA
# MAPB_List$FDA_Biomarker=NA
# FDA_infor=read.csv('FDAPGx_BioMarker_InDrugLabeling - Sheet1.csv')
# FDA_infor$Therapeutic.Area.=as.character(FDA_infor$Therapeutic.Area.)
# FDA_infor$Biomarker.=as.character(FDA_infor$Biomarker.)
# 
# for (i in (1:nrow(MAPB_List))){
#   MAPB_List$FDA_Therapeutic_Area[i]=paste0(FDA_infor$Therapeutic.Area.[grepl(pattern = MAPB_List$MAPB_name[i],x = FDA_infor$Drug,ignore.case = T)],collapse = ",")
#   MAPB_List$FDA_Biomarker=paste0(FDA_infor$Biomarker.[grepl(pattern = MAPB_List$MAPB_name[i],x = FDA_infor$Drug,ignore.case = T)],collapse = ",")
# 
#   }
# head(MAPB_List)
# dim(MAPB_List)
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



# #First of all, only people with more than one drug can be kept and if drug days of supplies <30 days ,serveral dispenses combine might be > 30 days 
#OnDrugTable=table(PGx_Cohort_PharmacyClaim$MemberId)
#sum(OnDrugTable>1)
#PGx_Cohort_PharmacyClaim=PGx_Cohort_PharmacyClaim[PGx_Cohort_PharmacyClaim$MemberId%in%names(OnDrugTable)[OnDrugTable>1],]

PGx_Cohort_PharmacyClaim$DispenseID=1:nrow(PGx_Cohort_PharmacyClaim)# assign a dispense ID to each dispense to deal two MAPBs dispensed together 

#PGx_Cohort_PharmacyClaim=PGx_Cohort_PharmacyClaim[PGx_Cohort_PharmacyClaim$DaysSupply>=14,]

dim(PGx_Cohort_PharmacyClaim)
#sum(table(PGx_Cohort_PharmacyClaim$MemberId)>1)

#over lap period
head(PGx_Cohort_PharmacyClaim)

class(PGx_Cohort_PharmacyClaim$DispenseDate)


library(dplyr)
Member_MAPB=left_join(PGx_Cohort_PharmacyClaim,MAPB_List[,c("NDC","MAPB_name")],
                      by = c("NationalDrugCode" = "NDC"))#be careful some drug may have >1 MAPB label 
sum(table(Member_MAPB$MemberId)>1)
head(Member_MAPB)

Member_MAPB$DispenseDate=as.Date(Member_MAPB$DispenseDate)
Member_MAPB$EndDate=Member_MAPB$DispenseDate+Member_MAPB$DaysSupply

Member_MAPB=Member_MAPB[,c("MemberId","MAPB_name",'NationalDrugCode',"DispenseDate","EndDate","DispenseID")]


OnDrugTable=table(Member_MAPB$MemberId)
Member_MAPB=Member_MAPB[Member_MAPB$MemberId%in%names(OnDrugTable)[OnDrugTable>1],]
#Member_MAPB=split(Member_MAPB,f = as.factor(Member_MAPB$MemberId))

#function to get over lap periiod between MAPBs
#install.packages("lubridate")
library("lubridate")




library(dplyr) # for %>%, arrange, bind_rows

interval_union <- function(input) {#function to union time intervals 
  if (nrow(input) == 1) {
    return(input)
  }
  input <- input %>% arrange(start)
  output = input[1, ]
  for (i in 2:nrow(input)) {
    x <- input[i, ]
    if (output$stop[nrow(output)] < x$start) {
      output <- bind_rows(output, x)
    } else if (output$stop[nrow(output)] == x$start) {
      output$stop[nrow(output)] <- x$stop
    }
    if (x$stop > output$stop[nrow(output)]) {
      output$stop[nrow(output)] <- x$stop
    }
  }
  return(output)
}




Get_overLap=function(OnDrug){

#generate interval

names(OnDrug)[c(4,5)]=c('start',"stop")

AllCo_Dispenses=NULL
On_3_MAPB=NULL
On_4_MAPB=NULL
On_5_MAPB=NULL


for (Memberid in unique(OnDrug$MemberId)){
  
    
  
    #combine dispenses with the same NDC  
    VecMember_onDrug=OnDrug[OnDrug$MemberId==Memberid,]
    
    if(length(unique(VecMember_onDrug$MAPB_name))>=2){
    VecMember_onDrug=split(VecMember_onDrug,f =VecMember_onDrug$NationalDrugCode )
    VecMember_onDrug=lapply(VecMember_onDrug,FUN=interval_union)
    VecMember_onDrug=Reduce(VecMember_onDrug,f=rbind)
    VecMember_onDrug$Intervals=new_interval(VecMember_onDrug$start, VecMember_onDrug$stop, tzone="UTC")
    
    #Look at co-dispenses 
    
    Vec_Overlap=VecMember_onDrug[,c("MemberId","MAPB_name","NationalDrugCode","Intervals")]
    names(Vec_Overlap)=c("MemberId","MAPB_name2","NationalDrugCode2","Intervals2")
    
    
    Merged=inner_join(VecMember_onDrug,Vec_Overlap,by = "MemberId")#much faster (10X) than merge function
    #Merged=Merged[Merged$NationalDrugCode!=Merged$NationalDrugCode2,]# it is ok if the MAPBs are from the same NDC
    Merged=Merged[Merged$MAPB_name!=Merged$MAPB_name2,]#careful, this is symetric
    Merged$OverLap_Period=intersect(Merged$Intervals,Merged$Intervals2)
    Merged$OverLap_number_days=day(as.period(Merged$OverLap_Period, "days"))
    Merged=Merged[Merged$OverLap_number_days>=30,]#only overlaps >=30 days count
    Merged=Merged[!is.na(Merged$OverLap_number_days),]
    
    Co_dispenses=table(Merged[,c("MAPB_name","MAPB_name2")])
    Vec_Index=which((Co_dispenses>=1)&(lower.tri(Co_dispenses)),arr.ind =T)
    
    if (length(Vec_Index)>0){
    Co_dispenses=data.frame(MAPB1= rownames(Co_dispenses)[Vec_Index[,1]],
                            MAPB2= colnames(Co_dispenses)[Vec_Index[,2]])
    Co_dispenses$Memberid=Memberid
    
    AllCo_Dispenses=rbind(AllCo_Dispenses,Co_dispenses)
    }
    
    
    #look at number co-dispense with 2,3,4,>=5 MAPBs
    
      #3 MAPBs 
    
    if (nrow(Vec_Index)>=2){# 3 MAPB co-dispenses can only be possible if there are at least two pairs of MAPB co-dispenses
      Vec_Overlap=VecMember_onDrug[,c("MemberId","MAPB_name","NationalDrugCode","Intervals")]
      names(Vec_Overlap)=c("MemberId","MAPB_name3","NationalDrugCode3","Intervals3")
      
      Merged=inner_join(Merged,Vec_Overlap,by = "MemberId")#look at further overlaps 
      #Merged=Merged[(Merged$NationalDrugCode3!=Merged$NationalDrugCode)&(Merged$NationalDrugCode3!=Merged$NationalDrugCode2),]
      Merged=Merged[(Merged$MAPB_name3!=Merged$MAPB_name)&(Merged$MAPB_name3!=Merged$MAPB_name2),]#careful, this is symetric
      Merged$OverLap_Period2=intersect(Merged$OverLap_Period,Merged$Intervals3)
      Merged$OverLap_number_days2=day(as.period(Merged$OverLap_Period2, "days"))
      Merged=Merged[Merged$OverLap_number_days2>=30,]#only overlaps >=30 days count
      Merged=Merged[!is.na(Merged$OverLap_number_days2),]
      
      Co_dispenses_3_MAPB=table(Merged[,c("MAPB_name","MAPB_name2","MAPB_name3")])
      if(any(Co_dispenses_3_MAPB>=1)){
        On_3_MAPB=c(On_3_MAPB,Memberid)
        
        #continue the condition , because if there are no 3 MAPB co-dispenses, there won't be any 4,5 MAPB-co-dispenses
      
      #4 MAPBs 
      Vec_Overlap=VecMember_onDrug[,c("MemberId","MAPB_name","NationalDrugCode","Intervals")]
      names(Vec_Overlap)=c("MemberId","MAPB_name4","NationalDrugCode4","Intervals4")
      
      Merged=inner_join(Merged,Vec_Overlap,by = "MemberId")#look at further overlaps 
      #Merged=Merged[(Merged$NationalDrugCode4!=Merged$NationalDrugCode)&(Merged$NationalDrugCode4!=Merged$NationalDrugCode2)&(Merged$NationalDrugCode4!=Merged$NationalDrugCode3),]
      Merged=Merged[(Merged$MAPB_name4!=Merged$MAPB_name)&(Merged$MAPB_name4!=Merged$MAPB_name2)&(Merged$MAPB_name4!=Merged$MAPB_name3),]#careful, this is symetric
      Merged$OverLap_Period3=intersect(Merged$OverLap_Period2,Merged$Intervals4)
      Merged$OverLap_number_days3=day(as.period(Merged$OverLap_Period3, "days"))
      Merged=Merged[Merged$OverLap_number_days3>=30,]#only overlaps >=30 days count
      Merged=Merged[!is.na(Merged$OverLap_number_days3),]
      
      Co_dispenses_4_MAPB=table(Merged[,c("MAPB_name","MAPB_name2","MAPB_name3","MAPB_name4")])
      if(any(Co_dispenses_4_MAPB>=1)){
        On_4_MAPB=c(On_4_MAPB,Memberid)
      }
      
      #5 MAPBs 
      Vec_Overlap=VecMember_onDrug[,c("MemberId","MAPB_name","NationalDrugCode","Intervals")]
      names(Vec_Overlap)=c("MemberId","MAPB_name5","NationalDrugCode5","Intervals5")
      
      Merged=inner_join(Merged,Vec_Overlap,by = "MemberId")#look at further overlaps 
      #Merged=Merged[(Merged$NationalDrugCode5!=Merged$NationalDrugCode)&(Merged$NationalDrugCode5!=Merged$NationalDrugCode2)&(Merged$NationalDrugCode5!=Merged$NationalDrugCode3)&(Merged$NationalDrugCode5!=Merged$NationalDrugCode4),]
      Merged=Merged[(Merged$MAPB_name5!=Merged$MAPB_name)&(Merged$MAPB_name5!=Merged$MAPB_name2)&(Merged$MAPB_name5!=Merged$MAPB_name3)&(Merged$MAPB_name5!=Merged$MAPB_name4),]#careful, this is symetric
      Merged$OverLap_Period4=intersect(Merged$OverLap_Period3,Merged$Intervals5)
      Merged$OverLap_number_days4=day(as.period(Merged$OverLap_Period4, "days"))
      Merged=Merged[Merged$OverLap_number_days4>=30,]#only overlaps >=30 days count
      Merged=Merged[!is.na(Merged$OverLap_number_days4),]
      
      Co_dispenses_5_MAPB=table(Merged[,c("MAPB_name","MAPB_name2","MAPB_name3","MAPB_name4","MAPB_name5")])
      if(any(Co_dispenses_5_MAPB>=1)){
        On_5_MAPB=c(On_5_MAPB,Memberid)
        }
    
      }#end of condition loop (4,5 MAPBS)
      
      }#end of condition loop (3 MAPBS)
      
    }##end of condition loop (2 MAPBS)
    
}#end of member loop


print(paste(length(unique(AllCo_Dispenses$Memberid)), "co-dispensed MAPB",
            length(On_3_MAPB)," people got co-dispensed 3 MAPBs",
            length(On_4_MAPB)," people got co-dispensed 4 MAPBs",
            length(On_5_MAPB)," people got co-dispensed 5 MAPBs"))

return(list(AllCo_Dispenses=AllCo_Dispenses,
            On_3_MAPB=On_3_MAPB,On_4_MAPB=On_4_MAPB,On_5_MAPB=On_5_MAPB))
}






#loop through multiple chunk ( otherwise too big to operate on)

AllMembers=unique(Member_MAPB$MemberId)
#VecMatch=match(AllMembers,Member_MAPB$MemberId)


To_Save_AllCo_Dispenses=NULL
To_Save_On_3_MAPB=NULL
To_Save_On_4_MAPB=NULL
To_Save_On_5_MAPB=NULL


TaskID=as.integer(commandArgs(trailingOnly = T)[[1]][1])
print(TaskID)

AllSeq=floor(seq(1,length(AllMembers),length.out = 2000))#segments to run on different nodes in cluster

for (i in  (((TaskID-1)*40+1):(TaskID*40))){
  #divide the members into 1000 shares and each taskid run 100 shares ( about 100*1000 people)
  
  print(paste0("task ",TaskID," from member ",max(AllSeq[i-1],1)," to member ",AllSeq[i]))
  
  Index=max(AllSeq[i-1],1):AllSeq[i]
  
  Vec_Output=Get_overLap(Member_MAPB[which(Member_MAPB$MemberId%in%AllMembers[Index]),])
  
  #save the data 
  To_Save_AllCo_Dispenses=rbind(To_Save_AllCo_Dispenses,Vec_Output$AllCo_Dispenses)
  To_Save_On_3_MAPB=c(To_Save_On_3_MAPB,Vec_Output$On_3_MAPB)
  To_Save_On_4_MAPB=c(To_Save_On_4_MAPB,Vec_Output$On_4_MAPB)
  To_Save_On_5_MAPB=c(To_Save_On_5_MAPB,Vec_Output$On_5_MAPB)
  
}


save(To_Save_AllCo_Dispenses,To_Save_On_3_MAPB,To_Save_On_4_MAPB,To_Save_On_5_MAPB,file = paste0(Data_Dir,'/Co_dispenses_50Shares_',TaskID,'.Rdata'),compress = F)
print("Finish")