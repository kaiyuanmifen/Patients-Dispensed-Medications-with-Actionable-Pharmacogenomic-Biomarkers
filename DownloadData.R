
print("Downloading data ")
## This library provides connectivity to the DB server using an open source JDBC driver
library(SqlServerJtds)

## This library contains some tools for pulling large chunks of data more efficiently than
## using DBI::dbGetQuery(...)
library(SqlTools)

## This library contains functions to pivot a tall 3-column table to a design matrix
library(FactToCube)




FileFolder="/n/scratch2/DL_temp/PGx/"

 
 
 
 
 
 #member
 members=dbGetQuery(cn2, 	paste0("
                                 SELECT
                                 *
                                 FROM
                                 ",
                                 "PGx_Cohort_Members_2016_2019"
 )
 )
 
 
 save(members,file = paste0(FileFolder,"PGx_Cohort_Members_2016_2019",".Rdata"),compress=F)
 #write.csv(members,paste0("../../../AetnaData/PsyChosis_ML_Member",".csv"))
 print("download PGx_Cohort_Members_2016_2019 finished")
 
 

 
 #  #Medication
 
 Target=dbGetQuery(cn2, 	paste0("
                                SELECT
                                *
                                FROM
                                ",
                                "PGx_Cohort_PharmacyClaim_2016_2019"
 )
 )
 
 
 save(Target,file = paste0(FileFolder,"PGx_Cohort_PharmacyClaim_2016_2019",".Rdata"),compress=F)
 #write.csv(Target,file = paste0("../../../AetnaData/PsyChosis_ML_Med_Cohort",".csv"))
 print("download PGx_Cohort_PharmacyClaim_2016_2019 finished")
 
 
 
 
 