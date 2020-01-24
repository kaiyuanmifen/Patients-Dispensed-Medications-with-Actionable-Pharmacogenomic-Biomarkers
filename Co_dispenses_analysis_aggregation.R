

Data_Dir="/n/scratch2/DL_temp/PGx"

All_OL=NULL

All_To_Save_On_3_MAPB=NULL

All_To_Save_On_4_MAPB=NULL

All_To_Save_On_5_MAPB=NULL

for (TaskID in 1:50){
  
  X=load(paste0(Data_Dir,'/Co_dispenses_50Shares_',TaskID,'.Rdata'))
  Vec=get(X)
  All_OL=rbind(All_OL,To_Save_AllCo_Dispenses)
  All_To_Save_On_3_MAPB=c(All_To_Save_On_3_MAPB,To_Save_On_3_MAPB)
  
  All_To_Save_On_4_MAPB=c(All_To_Save_On_4_MAPB,To_Save_On_4_MAPB)
  
  All_To_Save_On_5_MAPB=c(All_To_Save_On_5_MAPB,To_Save_On_5_MAPB)
  
  
  print(paste0("task ",TaskID))
 
}
head(All_OL)

save(All_OL,All_To_Save_On_3_MAPB,All_To_Save_On_4_MAPB,All_To_Save_On_5_MAPB,file = "AllPeople_Co_dispenses.Rdata")
load("AllPeople_Co_dispenses.Rdata")
head(All_OL)

names(All_OL)=c("MAPB1","MAPB2","MemberId")

length(unique(All_OL$MemberId))


##Get age group 
X=load(file.path(Data_Dir,"PGx_Cohort_Members_2016_2019.Rdata"))
PGx_Cohort_Members=get(X)



#left join 
library(dplyr)
names(All_OL)=c("MAPB1","MAPB2","MemberId")
All_OL=left_join(All_OL,PGx_Cohort_Members[,c("MemberId","BirthYear")],by = c("MemberId" = "MemberId"))
All_OL$AgeGroup="NA"
All_OL$AgeGroup[(All_OL$BirthYear>=2002)&(All_OL$BirthYear<=2016)]="0-17"
All_OL$AgeGroup[(All_OL$BirthYear>=1955)&(All_OL$BirthYear<=1998)]="18-64"
sum(All_OL$AgeGroup=="NA")
All_OL=All_OL[!All_OL$AgeGroup=="NA",]
head(All_OL)
dim(All_OL)


#Load MAPB list 
source('Get_GeneticBiomarkers_therapeuticArea.R')
MAPB_List=Get_MAPBList_with_biomarker_therapeutic_are(Split_item = T)#MAPB with multiple Therapeutic areas/biomarkes wer split into differnet rows


#Update the co-dispenses with biomarker information and therapeutic area information 

All_OL$FDA_Therapeutic_Area_1=MAPB_List$FDA_Therapeutic_Area[match(All_OL$MAPB1,MAPB_List$MAPB_name)]
All_OL$Biomarker_1=MAPB_List$Biomarker[match(All_OL$MAPB1,MAPB_List$MAPB_name)]

All_OL$FDA_Therapeutic_Area_2=MAPB_List$FDA_Therapeutic_Area[match(All_OL$MAPB2,MAPB_List$MAPB_name)]
All_OL$Biomarker_2=MAPB_List$Biomarker[match(All_OL$MAPB2,MAPB_List$MAPB_name)]


#results  to save 

NumberOfChildrens=sum((PGx_Cohort_Members$BirthYear>=2002)&(PGx_Cohort_Members$BirthYear<=2016))
NumberOfAdults=sum((PGx_Cohort_Members$BirthYear>=1955)&(PGx_Cohort_Members$BirthYear<=1998))

Vec=unique(All_OL[,c("MemberId","AgeGroup")])

Vec=table(Vec$AgeGroup )
NumberOfChildrens_Co_dispenses=Vec[1]
NumberOfAdults_Co_dispenses=Vec[2]

All_OL$MAPB_pair=paste0(All_OL$MAPB1,'-',All_OL$MAPB2)
Vec=unique(All_OL[,c("MemberId","MAPB_pair","AgeGroup")])
MAPB_pair_Co_dispenses=table(Vec$MAPB_pair)

All_OL$Same_Biomarker=All_OL$Biomarker_1==All_OL$Biomarker_2
Vec=All_OL[,c("MemberId","Biomarker_1","Same_Biomarker","AgeGroup")]
Vec=Vec[Vec$Biomarker_1!="",]
Vec=unique(Vec[Vec$Same_Biomarker==TRUE,])
On_the_same_Biomarker=Vec

All_OL$Same_FDA_Therapeutic_Area=All_OL$FDA_Therapeutic_Area_1==All_OL$FDA_Therapeutic_Area_2
Vec=All_OL[,c("MemberId","FDA_Therapeutic_Area_1","Same_FDA_Therapeutic_Area","AgeGroup")]
Vec=Vec[Vec$FDA_Therapeutic_Area_1!="",]
Vec=unique(Vec[Vec$Same_FDA_Therapeutic_Area==TRUE,])
On_the_Same_FDA_Therapeutic_Area=Vec


NumberOfPeople_co_dispenses_3_MAPBs=length(unique(All_To_Save_On_3_MAPB))
NumberOfPeople_co_dispenses_4_MAPBs=length(unique(All_To_Save_On_4_MAPB))
NumberOfPeople_co_dispenses_5_MAPBs=length(unique(All_To_Save_On_5_MAPB))


save(All_OL,NumberOfChildrens,NumberOfAdults,NumberOfChildrens_Co_dispenses,NumberOfAdults_Co_dispenses,MAPB_pair_Co_dispenses,On_the_same_Biomarker,On_the_Same_FDA_Therapeutic_Area,
     NumberOfPeople_co_dispenses_3_MAPBs,NumberOfPeople_co_dispenses_4_MAPBs,NumberOfPeople_co_dispenses_5_MAPBs,
     file = paste0('Co_dispenses_Aggregated.Rdata'))
