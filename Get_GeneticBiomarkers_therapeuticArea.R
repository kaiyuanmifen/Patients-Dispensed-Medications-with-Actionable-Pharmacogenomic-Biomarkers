


Get_MAPBList_with_biomarker_therapeutic_are=function(Split_item=F){

MAPB_List=read.csv("MAPB_NDCs_2020_0106-1.txt",sep = "\t",colClasses = "character")
head(MAPB_List)
length(unique(MAPB_List$MAPB_name))


#FDA therapeutic area 
FDA_infor=read.csv('Table of Pharmacogenomic Biomarkers in Drug Labeling  FDA.csv',skip = 1)#data from Sep 2019
MAPB_List$FDA_Therapeutic_Area=NA
FDA_infor$Therapeutic.Area.=as.character(FDA_infor$Therapeutic.Area.)
FDA_infor$Drug=as.character(FDA_infor$Drug)

for (i in (1:nrow(MAPB_List))){
  Areas=paste0(unique(FDA_infor$Therapeutic.Area.[grepl(pattern = MAPB_List$MAPB_name[i],x = FDA_infor$Drug,ignore.case = T)]),collapse = ",")
  MAPB_List$FDA_Therapeutic_Area[i]=Areas
  
}

unique(MAPB_List$FDA_Therapeutic_Area)
sum(MAPB_List$FDA_Therapeutic_Area=="")
print(paste("these MAPBs do not have a FDA therapeutic area assigned :",unique(MAPB_List$MAPB_name[MAPB_List$FDA_Therapeutic_Area==""])))
#MAPB_List$Missing_FDA_Therapeutic_Area=""
#MAPB_List$Missing_FDA_Therapeutic_Area[MAPB_List$FDA_Therapeutic_Area==""]="Y"

# 
# #FDA version of biomarker 
# 
# FDA_infor=read.csv('SFK_PharmGKB_12_2019.xlsx')
# 
# FDA_infor$Biomarker.=as.character(FDA_infor$Biomarker.)
# 
# for (i in (1:nrow(MAPB_List))){
#   
#   MAPB_List$FDA_Biomarker[i]=paste0(FDA_infor$Biomarker.[grepl(pattern = MAPB_List$MAPB_name[i],x = FDA_infor$Drug,ignore.case = T)],collapse = ",")
#   
# }
# sum(MAPB_List$FDA_Biomarker=="")
# sum(is.na(MAPB_List$FDA_Biomarker))
# 


#PharmGKB genetic biomarkers 

PharmGKB_drugLabels=read.csv('drugLabels/drugLabels.tsv',sep = '\t')
head(PharmGKB_drugLabels)
names(PharmGKB_drugLabels)


PharmGKB_drugLabels$Name=as.character(PharmGKB_drugLabels$Name)
Genetic_biomarker=data.frame(Drug=unlist(lapply(strsplit(split = "for|and ",PharmGKB_drugLabels$Name,),function(x){x[2]})),
                             Genetic_biomarker=unlist(lapply(strsplit(split = "for|and ",PharmGKB_drugLabels$Name),function(x){x[3]})),
                             Source=PharmGKB_drugLabels$Source)
Genetic_biomarker$Genetic_biomarker=gsub(Genetic_biomarker$Genetic_biomarker,pattern = " ",replacement = "")
Genetic_biomarker$Drug=gsub(Genetic_biomarker$Drug,pattern = " ",replacement = "")


MAPB_List$Biomarker=unlist(lapply(MAPB_List$MAPB_name,function(x){paste0(unique(Genetic_biomarker$Genetic_biomarker[which(grepl(pattern = x,Genetic_biomarker$Drug))]),collapse = ',')}))
MAPB_List$Source=unlist(lapply(MAPB_List$MAPB_name,function(x){paste0(unique(Genetic_biomarker$Source[which(grepl(pattern = x,Genetic_biomarker$Drug))]),collapse = ',')}))
unique(MAPB_List$MAPB_name[MAPB_List$Biomarker==""])

#Those without genetic biomarker from PharmGKB should be from CPIC


#Karen's CPIC data

CPIC_drugLabels=read.csv('SFK_PharmGKB_12_2019.csv')
CPIC_drugLabels$Drug=as.character(CPIC_drugLabels$Drug)
CPIC_drugLabels$Gene..CPIC.=as.character(CPIC_drugLabels$Gene..CPIC.)
VecWhich=which((MAPB_List$Biomarker=="")&(MAPB_List$MAPB_name%in%as.character(CPIC_drugLabels$Drug[CPIC_drugLabels$Gene..CPIC.!=""])))

MAPB_List$Source[VecWhich]="CPIC"
MAPB_List$Biomarker[VecWhich]=CPIC_drugLabels$Gene..CPIC.[match(MAPB_List$MAPB_name[VecWhich],CPIC_drugLabels$Drug)]

unique(MAPB_List$MAPB_name[MAPB_List$Biomarker==""])

#Still several biomarker missing add them manually 
MAPB_List$Biomarker[MAPB_List$MAPB_name=="ethinyl estradiol" ]="F2,F5,MTHFR,PROC,PROS1,SERPINC1"
MAPB_List$Source[MAPB_List$MAPB_name=="ethinyl estradiol" ]="European Medicines Agency,Health Canada Santé Canada"

MAPB_List$Biomarker[MAPB_List$MAPB_name=="sodium phenylbutyrate" ]="ASS1,CPS1,OTC"
MAPB_List$Source[MAPB_List$MAPB_name=="sodium phenylbutyrate" ]="U.S. Food and Drug Administration"

MAPB_List$Biomarker[MAPB_List$MAPB_name=="cholic acid" ]="AKR1D1,AMACR,CYP27A1,CYP7A1,DHCR7,HSD3B7"
MAPB_List$Source[MAPB_List$MAPB_name=="cholic acid" ]="U.S. Food and Drug Administration"

MAPB_List$Biomarker[MAPB_List$MAPB_name=="ado-trastuzumab emtansine" ]="ERBB2"
MAPB_List$Source[MAPB_List$MAPB_name=="ado-trastuzumab emtansine" ]="U.S. Food and Drug Administration"

MAPB_List$Biomarker[MAPB_List$MAPB_name=="mafenide topical" ]="G6PD"
MAPB_List$Source[MAPB_List$MAPB_name=="mafenide topical" ]="U.S. Food and Drug Administration"

MAPB_List$Biomarker[MAPB_List$MAPB_name=="elosulfase alfa"]="GALNS"
MAPB_List$Source[MAPB_List$MAPB_name=="elosulfase alfa"]="U.S. Food and Drug Administration"


print(paste("still missing :",unique(MAPB_List$MAPB_name[MAPB_List$Biomarker==""])))

#Quality control
MAPB_List$Biomarker[MAPB_List$MAPB_name=="acetaminophen"]
MAPB_List$Biomarker[MAPB_List$MAPB_name=="codeine"]

MAPB_List$Biomarker[MAPB_List$MAPB_name=="codeine"]="CYP2D6"#codeine has only CYP2D6 as biomarker according to PharmGKB

MAPB_List$Biomarker[MAPB_List$MAPB_name=="oxycodone"]
MAPB_List$Biomarker[MAPB_List$MAPB_name=="ethinyl estradiol"]
MAPB_List$Biomarker[MAPB_List$MAPB_name=="ondansetron"]
MAPB_List$Biomarker[MAPB_List$MAPB_name=="ciprofloxacin"]
MAPB_List$Biomarker[MAPB_List$MAPB_name=="sulfamethoxazole"]
MAPB_List$Biomarker[MAPB_List$MAPB_name=="meloxicam"]
MAPB_List$Biomarker[MAPB_List$MAPB_name=="tramadol"]
MAPB_List$Biomarker[MAPB_List$MAPB_name=="nitrofurantoin"]

MAPB_List$Biomarker[MAPB_List$MAPB_name=="dextromethorphan"]
MAPB_List$Biomarker[MAPB_List$MAPB_name=="sertraline"]
MAPB_List$Biomarker[MAPB_List$MAPB_name=="sertraline"]="CYP2C19,CYP2D6"#correct using information from CPIC
MAPB_List$Biomarker[MAPB_List$MAPB_name=="oxycodone"]
MAPB_List$Biomarker[MAPB_List$MAPB_name=="fluoxetine"]
MAPB_List$Biomarker[MAPB_List$MAPB_name=="escitalopram"]
MAPB_List$Biomarker[MAPB_List$MAPB_name=="escitalopram"]="CYP2C19,CYP2D6"#correct using PharmGKB information




#Split therapeutic areas and biomark for analysis 
if (Split_item==T){
  
  #Therapeutic area 
  Vec=NULL
  for (i in which(grepl(MAPB_List$FDA_Therapeutic_Area,pattern = ','))){
    for (j in 1:length(strsplit(MAPB_List$FDA_Therapeutic_Area[i],split = ',')[[1]])){
      VecRow=MAPB_List[i,]
      VecRow$FDA_Therapeutic_Area=strsplit(MAPB_List$FDA_Therapeutic_Area[i],split = ',')[[1]][j]
      Vec=rbind(Vec,VecRow)
    }
  }
  MAPB_List=MAPB_List[-which(grepl(MAPB_List$FDA_Therapeutic_Area,pattern = ',')),]
  MAPB_List=rbind(MAPB_List,Vec)
  
  
  #Biomarker 
  
  Vec=NULL
  for (i in which(grepl(MAPB_List$Biomarker,pattern = ','))){
    for (j in 1:length(strsplit(MAPB_List$Biomarker[i],split = ',')[[1]])){
      VecRow=MAPB_List[i,]
      VecRow$Biomarker=strsplit(MAPB_List$Biomarker[i],split = ',')[[1]][j]
      Vec=rbind(Vec,VecRow)
    }
  }
  MAPB_List=MAPB_List[-which(grepl(MAPB_List$Biomarker,pattern = ',')),]
  MAPB_List=rbind(MAPB_List,Vec)
  MAPB_List=unique(MAPB_List)
  
}
#MAPB_List=MAPB_List[order(MAPB_List$MAPB_name),]
#write.csv(MAPB_List,file ="MAPB_Biomarker_therapeutic_area.csv")

return(MAPB_List)
}
