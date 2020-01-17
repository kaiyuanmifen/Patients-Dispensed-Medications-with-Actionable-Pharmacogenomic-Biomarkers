load("OutPut.Rdata")

load("Co_dispenses_Aggregated.Rdata")

FileDir="../../figures"

#general count ( not part of the paper)
write.csv(AgeTable,file = file.path(FileDir,"On_MAPB_count_by_Age_group.csv"))

print(paste("number of adults:",NumberOfAdults))
print(paste("number of children:",NumberOfChildrens))
print(paste("total number of people in the cohort:",Total_Number_of_People))
print(paste(100*Number_Of_People_on_MAPB/Total_Number_of_People,"% of people in the cohort:"))



print(paste(100*(AgeTable[1,]/sum(AgeTable[1,]))[2],"% of children on MAPB"))

print(paste(100*(AgeTable[2,]/sum(AgeTable[2,]))[2],"% of adult on MAPB"))

print(paste("number of people on MAPB:",Number_Of_People_on_MAPB))
print(paste("% of people on MAPB:",100*Number_Of_People_on_MAPB/Total_Number_of_People))

print(paste("number of female:",sum(SexTable[,1])))
print(paste("%female:",sum(SexTable[,1])/Total_Number_of_People))
print(paste("number of male:",sum(SexTable[,2])))

print(paste(100*(SexTable[1,]/sum(SexTable[1,]))[2],"% of female on MAPB"))
print(paste(100*(SexTable[2,]/sum(SexTable[2,]))[2],"% of male on MAPB"))


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


#Table 1 

AgeGroup_On_MAPB_headCount_Table=as.data.frame(AgeGroup_On_MAPB_headCount_Table)
print(paste(length(unique(rownames(AgeGroup_On_MAPB_headCount_Table)))," different MAPBs were found in the data "))
head(AgeGroup_On_MAPB_headCount_Table)

Col=AgeGroup_On_MAPB_headCount_Table[AgeGroup_On_MAPB_headCount_Table$AgeGroup=="0-17",]
Col=Col[order(Col$Freq,decreasing = T),]
Col=Col[1:10,c(1,3)]

Table1=data.frame(Children_MAPB=Col$MAPB_name,
                  Children_Count=Col$Freq,
                  Children_Percentage=100*signif(Col$Freq/AgeGroup_Count_In_Cohort[1],3))

Col=AgeGroup_On_MAPB_headCount_Table[AgeGroup_On_MAPB_headCount_Table$AgeGroup=="18-64",]
Col=Col[order(Col$Freq,decreasing = T),]
Col=Col[1:10,c(1,3)]

Table1$Adult_MAPB=Col$MAPB_name
Table1$Adult_Count=Col$Freq
Table1$Adult_Percentage=100*signif(Col$Freq/AgeGroup_Count_In_Cohort[2],3)

Table1$Biomarker_Children=MAPB_List$FDA_Biomarker[match(Table1$Children_MAPB,MAPB_List$MAPB_name)]
Table1$Biomarker_Adult=MAPB_List$FDA_Biomarker[match(Table1$Adult_MAPB,MAPB_List$MAPB_name)]
Table1$source_Children=""
Table1$source_Adult=""
names(Table1)
Table1=Table1[,c("Children_MAPB" ,"Children_Count","Children_Percentage","Biomarker_Children","source_Children",
                 "Adult_MAPB", "Adult_Count","Adult_Percentage","Biomarker_Adult","source_Adult")]
write.csv(Table1,file = file.path(FileDir,"Table1.csv"))



#Supplementary table 1
load("OutPut.Rdata")#reload to set object type 
Supp_Table_1=data.frame(MAPB=rownames(AgeGroup_On_MAPB_headCount_Table),
                        Children_Count=AgeGroup_On_MAPB_headCount_Table[,1],
                        Adult_Count=AgeGroup_On_MAPB_headCount_Table[,2])

Supp_Table_1$Difference=Supp_Table_1$Adult_Count-Supp_Table_1$Children_Count



Supp_Table_1$Percentage_Children=signif(100*Supp_Table_1$Children_Count/NumberOfChildrens,2)
Supp_Table_1$Percentage_Adults=signif(100*Supp_Table_1$Adult_Count/NumberOfAdults,2)
Supp_Table_1$Percentage_Total=signif(100*(Supp_Table_1$Adult_Count+Supp_Table_1$Children_Count)/(NumberOfAdults+NumberOfChildrens),2)
Supp_Table_1=Supp_Table_1[order(abs(Supp_Table_1$Percentage_Total),decreasing = T),]
names(Supp_Table_1)
Supp_Table_1=Supp_Table_1[,c("MAPB" ,"Children_Count","Adult_Count","Percentage_Children","Percentage_Adults","Percentage_Total")]


Test_difference=function(Group1_ON,Group1_off,Group2_on,Group2_off){
  ##function to test association of beneficiaries group and  dispenses of each MAPB
  Vec_Test=data.frame(Group1_ON,Group1_off,Group2_on,Group2_off)
  VecP=NULL
  for (i in 1:nrow(Supp_Table_1)){
    Vec_Contigency_table=matrix(data = unlist(Vec_Test[i,]),nrow = 2,ncol = 2,byrow = T,
                                dimnames = list(c("Group1","Group2"),c("On","Off")))
    Vec=chisq.test(Vec_Contigency_table)
    VecP=c(VecP,Vec$p.value)
  }
  
  return(VecP)
}

Supp_Table_1$P_val=Test_difference(Group1_ON=Supp_Table_1$Children_Count,
                                    Group1_off=NumberOfChildrens-Supp_Table_1$Children_Count,
                                    Group2_on=Supp_Table_1$Adult_Count,
                                    Group2_off=NumberOfAdults-Supp_Table_1$Adult_Count)

Supp_Table_1$P_val=p.adjust(Supp_Table_1$P_val,method =  "bonferroni")# adjust for multiple tests 
Supp_Table_1$P_val=signif(Supp_Table_1$P_val,digits = 2)
head(Supp_Table_1)

write.csv(Supp_Table_1,file = file.path(FileDir,"Supp_Table_1.csv"))

##look at which MAPB has largest gap between age group 
VecDiff=data.frame(MAPB=Supp_Table_1$MAPB,diff=abs(Supp_Table_1$Percentage_Adults-Supp_Table_1$Percentage_Children))
head(Supp_Table_1[order(VecDiff$diff,decreasing = T),])
head(VecDiff[order(VecDiff$diff,decreasing = T),])

#Supplementary table 2
load("OutPut.Rdata")#reload to set object type 
Gender_On_MAPB_headCount_Table
Supp_Table_2=data.frame(MAPB=rownames(Gender_On_MAPB_headCount_Table),
                        Female_Count=Gender_On_MAPB_headCount_Table[,1],
                        Male_Count=Gender_On_MAPB_headCount_Table[,2])
head(Supp_Table_2)
Supp_Table_2$Difference=Supp_Table_2$Female_Count-Supp_Table_2$Male_Count
#Supp_Table_2=Supp_Table_2[order(abs(Supp_Table_2$Difference),decreasing = T),]

Supp_Table_2$Percentage_Female=signif(100*Supp_Table_2$Female_Count/Gender_Count_In_Cohort[1],2)
Supp_Table_2$Percentage_Male=signif(100*Supp_Table_2$Male_Count/Gender_Count_In_Cohort[2],2)
Supp_Table_2$Percentage_Total=signif(100*(Supp_Table_2$Female_Count+Supp_Table_2$Male_Count)/(sum(Gender_Count_In_Cohort[c(1,2)])),2)
Supp_Table_2=Supp_Table_2[order(abs(Supp_Table_2$Percentage_Total),decreasing = T),]
names(Supp_Table_2)
Supp_Table_2=Supp_Table_2[,c("MAPB","Female_Count","Male_Count","Percentage_Female","Percentage_Male","Percentage_Total")]

Supp_Table_2$P_val=Test_difference(Group1_ON=Supp_Table_2$Female_Count,
                                   Group1_off=sum(SexTable[1,])-Supp_Table_2$Female_Count,
                                   Group2_on=Supp_Table_2$Male_Count,
                                   Group2_off=sum(SexTable[2,])-Supp_Table_2$Male_Count)

Supp_Table_2$P_val=p.adjust(Supp_Table_2$P_val,method =  "bonferroni")# adjust for multiple tests 
head(Supp_Table_2)


write.csv(Supp_Table_2,file = file.path(FileDir,"Supp_Table_2.csv"))




#Figure 1 
library(ggplot2)
#A
PlotVec=Member_Therapeutic_area_agegroup_table
PlotVec=PlotVec[,order(apply(PlotVec,2,sum),decreasing = T)]
PlotVec=PlotVec[,1:10]
PlotVec=as.data.frame(PlotVec)
PlotVec=PlotVec[PlotVec$FDA_Therapeutic_Area!='',]
PlotVec$Percentage=100*PlotVec$Freq/(NumberOfAdults+NumberOfChildrens)
names(PlotVec)[2]='Therapeutic area'
PlotVec$AgeGroup=as.character(PlotVec$AgeGroup)
PlotVec$AgeGroup[PlotVec$AgeGroup=="0-17"]="Children"
PlotVec$AgeGroup[PlotVec$AgeGroup=="18-64"]="Adults"  
By_Therapeutic_A_C<-ggplot(data=PlotVec, aes(x=reorder(`Therapeutic area`,Percentage), y=Percentage)) +
  geom_bar(stat="identity",aes(fill = AgeGroup))+scale_fill_manual(values=c("grey", "black"))+xlab("Therapeutic area")+ylab('Percentage in cohort')+
  theme(axis.text.x=element_text(hjust=1),text = element_text(size=20),legend.position = "bottom",
        panel.background = element_blank(),panel.grid.major = element_line(colour = "grey", size = 0.5),
        plot.margin=margin(t = 1.5, r = 1.5, b = 1.5, l = 1.5, unit = "cm"))+
  coord_flip()


#B

PlotVec=Member_Biomarker_agegroup_table
PlotVec=PlotVec[,order(apply(PlotVec,2,sum),decreasing = T)]
PlotVec=PlotVec[,names(PlotVec[1,])!=""]#remove those do not have biomarker from FDA
PlotVec=PlotVec[,1:10]
PlotVec=as.data.frame(PlotVec)
PlotVec$Percentage=100*PlotVec$Freq/(NumberOfAdults+NumberOfChildrens)
names(PlotVec)[2]='BioMarker'
PlotVec$AgeGroup=as.character(PlotVec$AgeGroup)
PlotVec$AgeGroup[PlotVec$AgeGroup=="0-17"]="Children"
PlotVec$AgeGroup[PlotVec$AgeGroup=="18-64"]="Adults"  

By_Biomarer_A_C<-ggplot(data=PlotVec, aes(x=reorder(BioMarker,Percentage), y=Percentage)) +
  geom_bar(stat="identity",aes(fill = AgeGroup))+scale_fill_manual(values=c("grey", "black"))+xlab("Genetic Biomarker")+ylab('Percentage in cohort')+
  theme(axis.text.x=element_text(hjust=1),text = element_text(size=20),
        panel.background = element_blank(),panel.grid.major = element_line(colour = "grey", size = 0.5),
        plot.margin=margin(t = 1.5, r = 1.5, b = 1.5, l = 1.5, unit = "cm"),legend.position = "bottom")+
  coord_flip()


print(paste(sum(PlotVec$Percentage[PlotVec$BioMarker=="CYP2D6"]), "% people on CYP2D6 MAPB"))
print(paste(sum(PlotVec$Percentage[PlotVec$BioMarker=="CYP2C19"]), "% people on CYP2C19 MAPB"))


#C

PlotVec=Member_Gender_agegroup_table
PlotVec=PlotVec[,order(apply(PlotVec,2,sum),decreasing = T)]
PlotVec=as.data.frame(PlotVec)
PlotVec$Percentage=100*PlotVec$Freq/(NumberOfAdults+NumberOfChildrens)
names(PlotVec)[2]='Sex'
PlotVec$AgeGroup=as.character(PlotVec$AgeGroup)
PlotVec$AgeGroup[PlotVec$AgeGroup=="0-17"]="Children"
PlotVec$AgeGroup[PlotVec$AgeGroup=="18-64"]="Adults"  

AgeNGender=ggplot(data=PlotVec, aes(x=reorder(Sex,Percentage), y=Percentage)) +
  geom_bar(stat="identity",aes(fill = AgeGroup))+scale_fill_manual(values=c("grey", "black"))+xlab("Sex")+ylab('Percentage in cohort')+
  theme(axis.text.x=element_text(hjust=1),text = element_text(size=20),
        panel.background = element_blank(),panel.grid.major = element_line(colour = "grey", size = 0.5),
        plot.margin=margin(t = 1.5, r = 1.5, b = 1.5, l = 1.5, unit = "cm"),legend.position = "bottom")+
  coord_flip()






#Co_dispenses

#load("Co_dispenses_Aggregated.Rdata")

print(paste0("people co-dispensed PGx ",NumberOfAdults_Co_dispenses+NumberOfChildrens_Co_dispenses))

print(paste0("among people on MAPB, Percentage of people co-dispensed PGx ",signif(100*((NumberOfAdults_Co_dispenses+NumberOfChildrens_Co_dispenses)/(Number_Of_People_on_MAPB)),2),"%"))

print(paste0("Percentage of people co-dispensed PGx ",signif(100*((NumberOfAdults_Co_dispenses+NumberOfChildrens_Co_dispenses)/(NumberOfAdults+NumberOfChildrens)),2),"%"))
print(paste0("Percentage of adults co-dispensed PGx ",signif(100*(NumberOfAdults_Co_dispenses/NumberOfAdults),2),"%"))
print(paste0("Percentage of adults co-dispensed PGx ",signif(100*(NumberOfChildrens_Co_dispenses/NumberOfChildrens),2),"%"))

##most frequently- codispensed MAPB pairs 
print(MAPB_pair_Co_dispenses[order(MAPB_pair_Co_dispenses,decreasing = T)][1])

#co-dispenses on the same biomarker 
#% dispensed on the same Biomarker
signif(100*table(On_the_same_Biomarker$AgeGroup)/c(NumberOfChildrens,NumberOfAdults),digits = 2)
table(On_the_same_Biomarker$AgeGroup)
Vec=table(On_the_same_Biomarker[,c("FDA_Biomarker_1","AgeGroup")])
Vec=Vec[order(Vec[,1]+Vec[,2],decreasing = T),]
Vec/c(NumberOfChildrens,NumberOfAdults)

#co-dispenses on the same therapeutic area
#% dispensed on the same therpeutic area 
signif(100*table(On_the_Same_FDA_Therapeutic_Area$AgeGroup)/c(NumberOfChildrens,NumberOfAdults),digits = 2)
print(paste(length(unique(On_the_Same_FDA_Therapeutic_Area$MemberId[On_the_Same_FDA_Therapeutic_Area$AgeGroup=="0-17"]))," children co-dispensed with MAPB from same therapeutic area"))
print(paste(length(unique(On_the_Same_FDA_Therapeutic_Area$MemberId[On_the_Same_FDA_Therapeutic_Area$AgeGroup=="18-64"]))," children co-dispensed with MAPB from same therapeutic area"))

Vec=table(On_the_Same_FDA_Therapeutic_Area[,c("FDA_Therapeutic_Area_1","AgeGroup")])
Vec=Vec[order(Vec[,1]+Vec[,2],decreasing = T),]
Vec/c(NumberOfChildrens,NumberOfAdults)


#Co-dispenses by genetic biomarker and age group in population percentage (figue 1 D)
VecPlot=All_OL[All_OL$Same_Biomarker==T,]
VecPlot$FDA_Biomarker_1=gsub(",.*$", "",VecPlot$FDA_Biomarker_1)
VecPlot=VecPlot[VecPlot$FDA_Biomarker_1!='',]
dim(VecPlot)
head(VecPlot)
VecPlot=unique(VecPlot[,c("MemberId","AgeGroup","FDA_Biomarker_1")])
VecPlot=VecPlot[,c("AgeGroup","FDA_Biomarker_1")]
VecPlot=data.frame(table(VecPlot))
VecPlot$TotalPercentage=100*VecPlot$Freq/sum(c(NumberOfChildrens,NumberOfAdults))
VecPlot$AgeGroup=as.character(VecPlot$AgeGroup)
VecPlot=VecPlot[VecPlot$AgeGroup!="NA",]
VecPlot$AgeGroup[VecPlot$AgeGroup=="0-17"]="Children"
VecPlot$AgeGroup[VecPlot$AgeGroup=="18-64"]="Adults"

names(VecPlot)= c("AgeGroup","GeneticBiomarker","Freq" , "TotalPercentage")
Co_Hit_biomarker_A_C<-ggplot(data=VecPlot, aes(x=reorder(GeneticBiomarker,TotalPercentage), y=TotalPercentage)) +
  geom_bar(stat="identity",aes(fill = AgeGroup))+scale_fill_manual(values=c("grey", "black"))+
  xlab("Genetic biomarker shared in co-dispenses")+ylab('Percentage in cohort')+
  theme(axis.text.x=element_text(angle = 45,hjust=1),text = element_text(size=20),legend.position = "bottom",
        panel.background = element_blank(),panel.grid.major = element_line(colour = "grey", size = 0.5),
        plot.margin=margin(t = 1.5, r = 1.5, b = 1.5, l = 1.5, unit = "cm"))+
  coord_flip()



#Supplementary figure 1
head(All_OL)
VecPlot=All_OL$MemberId[All_OL$AgeGroup=='0-17']
VecPlot=table(VecPlot)/2#symetric 
VecPlot=data.frame(VecPlot)

names(VecPlot)[2]="Number of co-dispensed MAPB pairs"
Children=ggplot(VecPlot, aes(x=`Number of co-dispensed MAPB pairs`)) + geom_histogram()+ylab('Head count')+
  theme(axis.text.x=element_text(hjust=1),text = element_text(size=20),legend.position = "bottom",
        panel.background = element_blank(),panel.grid.major = element_line(colour = "grey", size = 0.5),
        plot.margin=margin(t = 1.5, r = 1.5, b = 1.5, l = 1.5, unit = "cm"))

VecPlot=All_OL$MemberId[All_OL$AgeGroup=='18-64']
VecPlot=table(VecPlot)/2 #Symetric 
VecPlot=data.frame(VecPlot)
names(VecPlot)[2]="Number of co-dispensed MAPB pairs"
Adult=ggplot(VecPlot, aes(x=`Number of co-dispensed MAPB pairs`)) + geom_histogram()+ylab('Head count')+
  theme(axis.text.x=element_text(hjust=1),text = element_text(size=20),legend.position = "bottom",
   panel.background = element_blank(),panel.grid.major = element_line(colour = "grey", size = 0.5),
   plot.margin=margin(t = 1.5, r = 1.5, b = 1.5, l = 1.5, unit = "cm"))



library(data.table)
library(ggplot2)
library(ggpubr)



Figure=ggarrange(Children,
                 Adult,
                 nrow = 1,ncol=2,labels = c("A", "B"))

ggsave("../../figures/SupplementaryFigure1.png", scale=0.5)
ggsave(Figure, file="../../figures/SupplementaryFigure1.eps", device="eps",dpi = 900,width = 11.1,height = 10.1)




#figure 1


Figure=ggarrange(By_Therapeutic_A_C,
                 By_Biomarer_A_C,
                 AgeNGender,
                 Co_Hit_biomarker_A_C,
                 nrow = 2,ncol=2,labels = c("A", "B","C","D"))



ggsave("../../figures/CombinedFigure.png", scale=0.5)
ggsave(Figure, file="../../figures/CombinedFigure.eps", device="eps",dpi = 900,width = 12.1,height = 12.1)






#a little bit of quality control of the data 
Total_Number_of_People==NumberOfAdults+NumberOfChildrens
sum(AgeGroup_Count_In_Cohort)==Total_Number_of_People

Number_Of_People_on_MAPB


sum(Number_Of_People_on_MAPB)/Total_Number_of_People



Table1$Children_Count
Supp_Table_1$Children_Count[match(Table1$Children_MAPB,Supp_Table_1$MAPB)]==Table1$Children_Count
Supp_Table_1$Adult_Count[match(Table1$Adult_MAPB,Supp_Table_1$MAPB)]==Table1$Adult_Count


sum(Supp_Table_2$Male_Count+Supp_Table_2$Female_Count)>Number_Of_People_on_MAPB
sum(Gender_On_MAPB_headCount_Table)>Number_Of_People_on_MAPB

length(unique(All_OL$MemberId))
NumberOfChildrens_Co_dispenses+NumberOfAdults_Co_dispenses==length(unique(All_OL$MemberId))


