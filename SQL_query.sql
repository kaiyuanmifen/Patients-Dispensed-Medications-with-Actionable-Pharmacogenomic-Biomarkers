

--Select members with with full pharmacy enrollment

use AetnaDataWarehouse;

select * from INFORMATION_SCHEMA.TABLES


--Select members with full enrollment between Jun 30 2016 and Jun 30 2020 
--select TOP 3 memberid from (select memberid, COUNT(*) AS 'num' from Enrollment where (EffectiveDate between '2014-04-01' and '2017-06-01') and (DrugIndicator='Y') group by memberid) a where num=36;

--select members with full enrollment and <=age 64 in 2019 (>65 got Medicare problem)

drop table DLiu..PGx_Cohort_Members_2016_2019

select * into DLiu..PGx_Cohort_Members_2016_2019 from Members where (MemberId in (select  memberid from (select memberid, COUNT(Distinct EffectiveDate) AS 'NumOfMonths' from Enrollment where (EffectiveDate between '2016-04-01' and '2019-03-31') and (DrugIndicator='Y') and (MedicalIndicator='Y') group by memberid) a where NumOfMonths=36) ) and (((BirthYear >=2002) and (BirthYear <= 2016)) or ((BirthYear >=1955) and (BirthYear <=1998)))

select count (distinct MemberId) from DLiu..PGx_Cohort_Members_2016_2019;



--Select pharmacy records

drop table DLiu..PGx_Cohort_PharmacyClaim_2016_2019 

select TOP 3 * from PharmacyClaims;

select PaidDate,DispenseDate,DaysSupply,MemberId, NationalDrugCode, EmployeeZipCode, NdcDescription
into DLiu..PGx_Cohort_PharmacyClaim_2016_2019 from PharmacyClaims
where (memberid in (select memberid from DLiu..PGx_Cohort_Members_2016_2019 )) and
(DispenseDate between '2016-04-01' and '2019-03-31') and (ExpandedRxClaimId not in (select ExpandedRxClaimId from PharmacyClaimsDeletes));




