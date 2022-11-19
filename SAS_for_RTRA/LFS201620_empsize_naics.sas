/*************************************************************************************************/
/*  Research Question: this program estimates employment by NAICS and B.C.	                 */ 
/*************************************************************************************************/

Data work.EMPSIZE_NAICS;
Set RTRAdata.LFS201620 (keep= ID LFSSTAT FIRMSIZE NAICS_5 SYEAR PROV);

/*Job Status employed in labour force*/
If LFSSTAT IN (1,2);

/*Province B.C.*/
if PROV=59;

/* Firm Size. Total number of persons employed at all locations */
if FIRMSIZE = '1' then size = 'Less than 20 employees';
else if '2'<=FIRMSIZE<='4' then size = 'Between 20 and over 500 employees';
else size = 'Unknown';


/*4 digit NAICS categories*/
if missing(NAICS_5)then NAICS_5="missing";
run;

%RTRAFreq(
	InputDataset=work.EMPSIZE_NAICS,
	OutputName=empsize_naics1620,
	ClassVarList=SYEAR NAICS_5 SIZE,
	UserWeight=FINALWT);
run;
