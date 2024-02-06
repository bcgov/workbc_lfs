/*************************************************************************/
/* Research Question: Labour Force Statistics for 4-digit NAICS          */
/*************************************************************************/

DATA work.lfstats4digNAICS;
SET RTRAdata.LFS202125 (keep = ID PROV AGE LFSSTAT NAICS_5 SYEAR);

/* Province B.C.*/
if PROV = 59;

/* Age 15 or over */
if AGE >= 15;

/*Labour Force Status*/

length lf_stat $20;

if LFSSTAT IN (1,2) then lf_stat = 'Employed';
else if LFSSTAT in (3,4,5) then lf_stat = 'Unemployed';
else lf_stat = 'Unknown';

/*deal with missing naics*/
if missing(NAICS_5)then NAICS_5="missing";
run;

%RTRAFreq(
     InputDataset=work.lfstats4digNAICS,
     OutputName=lfsstat2125,
     ClassVarList= SYEAR lf_stat NAICS_5,
     UserWeight=FINALWT);
run;
