/*************************************************************************************************/
/*  Research Question: this program estimates employment by NAICS and B.C.	                 */ 
/*************************************************************************************************/

Data work.EMPFTPT_NAICS;
Set RTRAdata.LFS202125 (keep= ID LFSSTAT FTPTMAIN NAICS_5 SYEAR PROV);

/*Job Status employed in labour force*/
If LFSSTAT IN (1,2);

/*Province B.C.*/
if PROV=59;

/* Part-time and Full-time status */
if ftptmain = '1' then ftpt =      'Full-time';
else if ftptmain = '2' then ftpt = 'Part-time';
else ftpt = 'Unknown';


/*4 digit NAICS categories*/
if missing(NAICS_5)then NAICS_5="missing";
run;

%RTRAFreq(
	InputDataset=work.EMPFTPT_NAICS,
	OutputName=empftpt_naics2125,
	ClassVarList=SYEAR NAICS_5 FTPT,
	UserWeight=FINALWT);
run;
