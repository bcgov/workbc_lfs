/*************************************************************************************************/
/*  Research Question: this program estimates employment by NAICS and B.C.	                 */
/*************************************************************************************************/

Data work.EMPFTPT_NAICS;
Set RTRAdata.LFS201620 (keep= ID LFSSTAT FTPTMAIN NAICS_5 SYEAR ERTAB PROV);

/*Job Status employed in labour force*/
If LFSSTAT IN (1,2);

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

/* Part-time and Full-time status */

length ftpt_main $ 20;

if FTPTMAIN = '1' then ftpt_main =      'Full-time';
else if FTPTMAIN = '2' then ftpt_main = 'Part-time';
else ftpt_main = 'Unknown';

/*4 digit NAICS categories*/
if missing(NAICS_5)then NAICS_5="missing";
run;

%RTRAFreq(
	InputDataset=work.EMPFTPT_NAICS,
	OutputName=ftpt_region1620,
	ClassVarList=SYEAR NAICS_5 Region ftpt_main,
	UserWeight=FINALWT);
run;
