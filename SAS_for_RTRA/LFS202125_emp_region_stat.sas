/*************************************************************************************************/
/*  Research Question: this program estimates employment by NAICS and B.C.	                 */ 
/*************************************************************************************************/

Data work.EMP_NAICS;
Set RTRAdata.LFS202125 (keep= ID LFSSTAT PROV NAICS_5 SYEAR ERTAB);

/*Labour Force Status*/

length lf_stat $20;

if LFSSTAT IN (1,2) then lf_stat = 'Employed';
else if LFSSTAT in (3,4,5) then lf_stat = 'Unemployed';
else lf_stat = 'Unknown';

/*Province B.C.*/
if PROV=59;

/*for 8 economic regions*/
length Region $ 30;
IF   ERTAB IN ("5910") THEN Region="Vancouver Island and Coast";
ELSE IF ERTAB IN ("5920") THEN Region="Lower Mainland-Southwest";
ELSE IF ERTAB IN ("5930") THEN Region="Thompson-Okanagan";
ELSE IF ERTAB IN ("5940") THEN Region="Kootenay";
ELSE IF ERTAB IN ("5950") THEN Region="Cariboo";
ELSE IF ERTAB IN ("5960") THEN Region="North Coast";
ELSE IF ERTAB IN ("5970") THEN Region="Nechako";
ELSE IF ERTAB IN ("5980") THEN Region="Northeast";
ELSE Region="Other";  

/*4 digit NAICS categories*/
if missing(NAICS_5)then NAICS_5="missing";
run;

%RTRAFreq(
	InputDataset=work.EMP_NAICS,
	OutputName=EMP_REG_STAT_2125,
	ClassVarList=SYEAR lf_stat REGION NAICS_5,
	UserWeight=FINALWT);
run;
