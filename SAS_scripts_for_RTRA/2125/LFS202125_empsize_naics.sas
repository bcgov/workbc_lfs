/*************************************************************************************************/
/*  number of employed in BC by establishment size and naics*/
/*************************************************************************************************/

Data work.EMPSIZE_NAICS;
Set RTRAdata.LFS202125 (keep= ID LFSSTAT COWMAIN ESTSIZE NAICS_5 SYEAR PROV);

/*Job Status employed in labour force*/
If LFSSTAT IN (1,2);

/*Public and Private, no self employed*/
If COWMAIN IN (1,2);

/*Province B.C.*/
if PROV=59;

/* Firm Size. Total number of persons employed at all locations */
if ESTSIZE = 1 then size = 'Less than 20 employees';
else if ESTSIZE = 2 then size = '20 to 99 employees';
else if ESTSIZE = 3 then size = '100 to 500 employees';
else if ESTSIZE = 4 then size = 'More than 500 employees';
else size = 'Unknown';

/*4 digit NAICS categories*/
if missing(NAICS_5) then NAICS_5="missing";
run;

%RTRAFreq(
	InputDataset=work.EMPSIZE_NAICS,
	OutputName=empsize_naics_2125,
	ClassVarList=SYEAR NAICS_5 size,
	UserWeight=FINALWT);
run;
