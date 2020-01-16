

Data_Dir="/n/scratch2/DL_temp/PGx"

Vec_Save=NULL
for (TaskID in 1:10){
  
  X=load(paste0(Data_Dir,'/Co_dispenses_',TaskID,'.Rdata'))
  Vec=get(X)
  Vec_Save=rbind(Vec_Save,Vec)
  
  print(paste0("task ",TaskID))
 
}
head(Vec_Save)

##Get age group 
X=load(file.path(Data_Dir,"PGx_Cohort_Members_2016_2019.Rdata"))
PGx_Cohort_Members=get(X)

#left join 
library(dplyr)
Vec_Save=left_join(Vec_Save,PGx_Cohort_Members[,c("MemberId","BirthYear")],by = c("MemberId" = "MemberId"))
Vec_Save$AgeGroup="NA"
Vec_Save$AgeGroup[(Vec_Save$BirthYear>=2002)&(Vec_Save$BirthYear<=2016)]="0-17"
Vec_Save$AgeGroup[(Vec_Save$BirthYear>=1955)&(Vec_Save$BirthYear<2002)]="18-64"
sum(Vec_Save$AgeGroup=="NA")


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
  MAPB_List$FDA_Biomarker[i]=paste0(FDA_infor$Biomarker[grepl(pattern = MAPB_List$MAPB_name[i],x = as.character(FDA_infor$Drug),ignore.case = T)],collapse = ",")
  
}
head(MAPB_List)


#Update the co-dispenses with biomarker information and therapeutic area information 
names(Vec_Save)=c("MemberId" ,"MAPB_name1","Intervals1" , "MAPB_name2","Intervals2" ,"OverLap","OverLap_number_days","BirthYear","AgeGroup")

Vec_Save$FDA_Therapeutic_Area_1=MAPB_List$FDA_Therapeutic_Area[match(Vec_Save$MAPB_name1,MAPB_List$MAPB_name)]
Vec_Save$FDA_Biomarker_1=MAPB_List$FDA_Biomarker[match(Vec_Save$MAPB_name1,MAPB_List$MAPB_name)]

Vec_Save$FDA_Therapeutic_Area_2=MAPB_List$FDA_Therapeutic_Area[match(Vec_Save$MAPB_name2,MAPB_List$MAPB_name)]
Vec_Save$FDA_Biomarker_2=MAPB_List$FDA_Biomarker[match(Vec_Save$MAPB_name2,MAPB_List$MAPB_name)]

#results  to save 

NumberOfChildrens=sum((PGx_Cohort_Members$BirthYear>=2002)&(PGx_Cohort_Members$BirthYear<=2016))
NumberOfAdults=sum((PGx_Cohort_Members$BirthYear>=1955)&(PGx_Cohort_Members$BirthYear<=1998))

Vec=unique(Vec_Save[,c("MemberId","AgeGroup")])

Vec=table(Vec_Save$AgeGroup)
NumberOfChildrens_Co_dispenses=Vec[1]
NumberOfAdults_Co_dispenses=Vec[2]

Vec_Save$MAPB_pair=paste0(Vec_Save$MAPB_name1,'-',Vec_Save$MAPB_name2)
Vec=unique(Vec_Save[,c("MemberId","MAPB_pair","AgeGroup")])
MAPB_pair_Co_dispenses=table(Vec$MAPB_pair)

Vec_Save$Same_Biomarker=Vec_Save$FDA_Biomarker_1==Vec_Save$FDA_Biomarker_2
Vec=Vec_Save[,c("MemberId","FDA_Biomarker_1","Same_Biomarker","AgeGroup")]
Vec=Vec[Vec$FDA_Biomarker_1!="",]
Vec=unique(Vec[Vec$Same_Biomarker==TRUE,])
On_the_same_Biomarker=Vec

Vec_Save$Same_FDA_Therapeutic_Area=Vec_Save$FDA_Therapeutic_Area_1==Vec_Save$FDA_Therapeutic_Area_1
Vec=Vec_Save[,c("MemberId","FDA_Therapeutic_Area_1","Same_FDA_Therapeutic_Area","AgeGroup")]
Vec=Vec[Vec$FDA_Therapeutic_Area_1!="",]
Vec=unique(Vec[Vec$Same_FDA_Therapeutic_Area==TRUE,])
On_the_Same_FDA_Therapeutic_Area=Vec


#Biomarker vs. number of people in each age group 


save(Vec_Save,NumberOfChildrens,NumberOfAdults,NumberOfChildrens_Co_dispenses,NumberOfAdults_Co_dispenses,MAPB_pair_Co_dispenses,On_the_same_Biomarker,On_the_Same_FDA_Therapeutic_Area,file = paste0('Co_dispenses_Aggregated.Rdata'))
