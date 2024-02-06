/*************************************************************************************************/
/*  Research Question: this program estimates employment by NAICS and B.C.	                 */ 
/*************************************************************************************************/

Data work.EMP_NAICS;
Set RTRAdata.LFS201620 (keep= ID LFSSTAT AGE PROV NAICS_5 SYEAR SEX);


/*Job Status employed in labour force*/
If LFSSTAT IN (1,2);

/*Province B.C.*/
if PROV=59;

/* gender*/
IF SEX=1 THEN GENDER="Male";
ELSE IF SEX=2 THEN GENDER="Female";
Else GENDER="Missing";

/* creating age groups*/
length AGEGRP $15;
IF 0<=AGE<=14 THEN AGEGRP= "14andunder";
ElSE If 15<=AGE<=24 THEN AGEGRP="Between15and24";
ELSE IF 25<=AGE<=34 THEN AGEGRP="Between25and34";
ELSE IF 35<=AGE<=44 THEN AGEGRP="Between35and44";
ELSE IF 45<=AGE<=54 THEN AGEGRP="Between45and54";
ELSE IF 55<=AGE<=64 THEN AGEGRP="Between55and64";
ELSE IF AGE>=65 THEN AGEGRP="65andover";
ELSE AGEGRP="Other";

/*4 digit NAICS categories*/
if missing(NAICS_5)then NAICS_5="missing";
run;

%RTRAFreq(
	InputDataset=work.EMP_NAICS,
	OutputName=EMP_NAICS_1620,
	ClassVarList=SYEAR NAICS_5 AGEGRP SEX,
	UserWeight=FINALWT);
run;
