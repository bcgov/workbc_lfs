/*************************************************************************************************/
/*  Research Question: this program estimates employment by NAICS and B.C.	                 */
/*************************************************************************************************/

Data work.EMPCOW_NAICS;
Set RTRAdata.LFS201620 (keep= ID LFSSTAT PROV NAICS_5 SYEAR COWMAIN);

/*Job Status employed in labour force*/
If LFSSTAT IN (1,2);

/*Province B.C.*/
if PROV=59;

/* Class of worker, main job*/
IF COWMAIN=1 THEN CLASS="Public employee";
ELSE IF COWMAIN=2 THEN CLASS="Private employee";
ELSE IF 3<=COWMAIN<=7 THEN CLASS="Self-employed";
else CLASS = 'Unknown';

/*4 digit NAICS categories*/
if missing(NAICS_5)then NAICS_5="missing";
run;

%RTRAFreq(
	InputDataset=work.EMPCOW_NAICS,
	OutputName=EMPCOW_NAIC1620,
	ClassVarList=SYEAR NAICS_5 CLASS,
	UserWeight=FINALWT);
run;
