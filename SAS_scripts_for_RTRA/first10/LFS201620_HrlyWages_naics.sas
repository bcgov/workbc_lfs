
/*************************************************************************************************/
/*  Research Question: this program estimates employment by NAICS and B.C.                  */
/*************************************************************************************************/

Data work.HrlyWages;
Set RTRAdata.LFS201620 (keep= ID AGE HRLYEARN PROV LFSSTAT NAICS_5 SYEAR GENDER);

/*Province B.C.*/
if PROV=59;

/*age group*/
if AGE>= 15;

/* employed only*/
if LFSSTAT in (1,2);


/* WAGE */
HRLYEARN_NUM = INPUT(HRLYEARN, 6.2);

/*4 digit NAICS categories*/
if missing(NAICS_5)then NAICS_5="missing";
run;

%RTRAMean(
                InputDataset=work.HrlyWages,
                OutputName=HrlyWages_1620,
                ClassVarList=SYEAR NAICS_5 GENDER,
                AnalysisVarList= HRLYEARN_NUM,
                UserWeight=FINALWT);
run;
