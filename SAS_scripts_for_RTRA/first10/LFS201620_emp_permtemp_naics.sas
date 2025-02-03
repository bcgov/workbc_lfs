/*************************************************************************************************/
/*  Research Question: this program estimates employment by NAICS and B.C.	                 */ 
/*************************************************************************************************/

Data work.EMPFTPT_NAICS;
Set RTRAdata.LFS201620 (keep= ID LFSSTAT PERMTEMP NAICS_5 SYEAR PROV);

/*Job Status employed in labour force*/
If LFSSTAT IN (1,2);

/*Province B.C.*/
if PROV=59;

/* Part-time and Full-time status */
length temp $ 15;
if permtemp in (1) then temp = 'Permanent';
else if permtemp in (2,3,4,5,6) then temp = 'Temporary';
else temp = 'Unknown';


/*4 digit NAICS categories*/
if missing(NAICS_5)then NAICS_5="missing";
run;

%RTRAFreq(
	InputDataset=work.EMPFTPT_NAICS,
	OutputName=emp_permtemp_1620,
	ClassVarList=SYEAR NAICS_5 TEMP,
	UserWeight=FINALWT);
run;
