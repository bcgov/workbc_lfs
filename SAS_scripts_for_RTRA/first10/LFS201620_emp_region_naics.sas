Data work.EMP_NAICS;
Set RTRAdata.LFS201620 (keep= ID LFSSTAT PROV NAICS_5 SYEAR ERTAB);

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

/*4 digit NAICS categories*/
if missing(NAICS_5)then NAICS_5="missing";
run;

%RTRAFreq(
	InputDataset=work.EMP_NAICS,
	OutputName=EMP_REGION_1620,
	ClassVarList=SYEAR REGION NAICS_5,
	UserWeight=FINALWT);
run;
