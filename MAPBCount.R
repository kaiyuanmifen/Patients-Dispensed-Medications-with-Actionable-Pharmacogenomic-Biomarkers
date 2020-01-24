
print("count of PGx usage")

#Load MAPB list with genetic biomarker and therapeutic areas 

source('Get_GeneticBiomarkers_therapeuticArea.R')
MAPB_List=Get_MAPBList_with_biomarker_therapeutic_are()



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



#Number of people on MAPB

Number_Of_People_on_MAPB=length(unique(PGx_Cohort_PharmacyClaim$MemberId))
Total_Number_of_People=length(unique(PGx_Cohort_Members$MemberId))
print(paste0(Number_Of_People_on_MAPB/Total_Number_of_People," people on MAPB"))


#Age group 

range(PGx_Cohort_Members$BirthYear)
PGx_Cohort_Members$ON_MAPB=0
PGx_Cohort_Members$ON_MAPB[PGx_Cohort_Members$MemberId%in%PGx_Cohort_PharmacyClaim$MemberId]=1

PGx_Cohort_Members$AgeGroup[(PGx_Cohort_Members$BirthYear<=1998)&(PGx_Cohort_Members$BirthYear>=1955)]="18-64"
PGx_Cohort_Members$AgeGroup[(PGx_Cohort_Members$BirthYear>=2002)&(PGx_Cohort_Members$BirthYear<=2016)]="0-17"

AgeTable=table(PGx_Cohort_Members[,c("AgeGroup","ON_MAPB")])

#Gender 
SexTable=table(PGx_Cohort_Members[,c("Gender","ON_MAPB")])




#By Each MAPB
library(dplyr)
Member_MAPB=left_join(PGx_Cohort_PharmacyClaim,MAPB_List[,c("NDC","MAPB_name")],by = c("NationalDrugCode" = "NDC"))
Member_MAPB=Member_MAPB[,c("MemberId","MAPB_name")]
Member_MAPB=Member_MAPB[!duplicated(Member_MAPB),]
Vec=table(Member_MAPB$MemberId)
Mean_MAPB_Count_per_person=mean(Vec)
sd_MAPB_Count_per_person=sd(Vec)

Member_MAPB=left_join(Member_MAPB,PGx_Cohort_Members[,c("MemberId",'AgeGroup',"Gender")],by=c("MemberId"="MemberId"))

#By Agegroup 

AgeGroup_Count_In_Cohort=table(PGx_Cohort_Members$AgeGroup)


AgeGroup_On_MAPB_headCount_Table=table(Member_MAPB[,c("MAPB_name",'AgeGroup')])


#by gender 

Gender_Count_In_Cohort=table(PGx_Cohort_Members$Gender)

Gender_On_MAPB_headCount_Table=table(Member_MAPB[,c("MAPB_name",'Gender')])



#By therapeutic area and Age group 
MAPB_List=Get_MAPBList_with_biomarker_therapeutic_are(Split_item=T)#MAPB with multiple Therapeutic areas/biomarkes wer split into differnet rows

Member_MAPB=left_join(Member_MAPB,MAPB_List[,c("MAPB_name","FDA_Therapeutic_Area","Biomarker")],by = c("MAPB_name" = "MAPB_name"))

Vec=Member_MAPB[,c("MemberId","AgeGroup","FDA_Therapeutic_Area")]
library(dplyr)
Vec=distinct(Vec)
Member_Therapeutic_area_agegroup_table=table(Vec[,c("AgeGroup","FDA_Therapeutic_Area")])

#By Biomarker and Age group
Vec=Member_MAPB[,c("MemberId","AgeGroup","Biomarker")]
Vec=distinct(Vec)#remove repeats 
Member_Biomarker_agegroup_table=table(Vec[,c("AgeGroup","Biomarker")])


# by Gender and Age group 
Vec=Member_MAPB[,c("MemberId","AgeGroup","Gender")]
Vec=distinct(Vec)#remove repeats 
Member_Gender_agegroup_table=table(Vec[,c("AgeGroup","Gender")])




#Save the results 

save(list =c("Number_Of_People_on_MAPB","Total_Number_of_People",
     "AgeTable","SexTable","Mean_MAPB_Count_per_person","sd_MAPB_Count_per_person",
     "AgeGroup_Count_In_Cohort","AgeGroup_On_MAPB_headCount_Table",
     "Gender_Count_In_Cohort","Gender_On_MAPB_headCount_Table",
     "Member_Therapeutic_area_agegroup_table",
     "Member_Biomarker_agegroup_table","Member_Gender_agegroup_table"),file = "OutPut.Rdata")