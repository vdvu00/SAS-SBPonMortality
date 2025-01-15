*/ Vanessa Vu
BS852 Final Project;
/*********************************************/


*/ LOAD IN DATA;
proc import datafile='/home/u63978239/BS852 stats in epi/framdat4.csv'
	out=framdat
	DBMS=csv replace;
	getnames=yes;
	datarow=2;
run;

/*********************************************/


*/ UNIVARIATE ANALYSIS with outcome variable;
proc phreg data=framdat zph;
	model SURV*DTH(0) = SPF4 / rl ties=efron;
run;
*/ SPF significant;
proc phreg data=framdat zph;
	class SEX(ref='1');  
	model SURV*DTH(0) = SEX / rl ties=efron;
run;
*/ SEX significant;
proc phreg data=framdat zph;
	model SURV*DTH(0) = AGE4 / rl ties=efron;
run;
*/ AGE significant;
proc phreg data=framdat zph; 
	model SURV*DTH(0) = CHOL4 / rl ties=efron;
run;
*/ CHOL4 significant;
proc phreg data=framdat zph; 
	class SMOKE(ref='0');  
	model SURV*DTH(0) = SMOKE / rl ties=efron;
run;
*/ SMOKE NOT significant;
proc phreg data=framdat zph;  
	model SURV*DTH(0) = DPF4 / rl ties=efron;
run;
*/ DPF4 significant;
proc phreg data=framdat zph;  
	model SURV*DTH(0) = WGT4 / rl ties=efron;
run;
*/ WGT4 significant;
proc phreg data=framdat zph;  
	model SURV*DTH(0) = FVC4 / rl ties=efron;
run;
*/ FVC4 significant;
proc phreg data=framdat zph;  
	model SURV*DTH(0) = BMI4 / rl ties=efron;
run;
*/ BMI4 significant;
proc phreg data=framdat zph;  
	model SURV*DTH(0) = HTN4 / rl ties=efron;
run;
*/ HTN4 significant but fails PH assumption;
proc phreg data=framdat zph;
	class CHD(ref='0');    
	model SURV*DTH(0) = CHD / rl ties=efron;
run;
*/ CHD significant but fails PH assumption;
proc phreg data=framdat zph;  
	class T2D(ref='0'); 
	model SURV*DTH(0) = T2D / rl ties=efron;
run;
*/ T2D not significant;

/*********************************************/


*/ CORRELATION ANALYSIS;
*/ pearson correlation - continuous covariates and main predictor;
proc corr data=framdat;
var AGE4 CHOL4 CIGS4 WGT4 FVC4 BMI4 DPF4 SPF4;
run;
*/ correlation w chisq - categorical covariates;
proc freq data=framdat;
tables SEX SMOKE HTN4 CHD T2D;
run;
proc freq data=framdat;
tables SEX*SMOKE / chisq;
run;
proc freq data=framdat;
tables SEX*HTN4 / chisq;
run;
proc freq data=framdat;
tables SEX*CHD / chisq;
run;
proc freq data=framdat;
tables SEX*T2D / chisq;
run;
proc freq data=framdat;
tables SMOKE*HTN4 / chisq;
run;
proc freq data=framdat;
tables SMOKE*CHD / chisq;
run;
proc freq data=framdat;
tables SMOKE*T2D / chisq;
run;
proc freq data=framdat;
tables HTN4*CHD / chisq;
run;
proc freq data=framdat;
tables HTN4*T2D / chisq;
run;
proc freq data=framdat;
tables CHD*T2D / chisq;
run;
*/ point biserial correlation;
proc corr data=framdat;
var AGE4 CHOL4 CIGS4 WGT4 FVC4 BMI4 DPF4 SPF4;
with SEX;
run;
proc corr data=framdat;
var AGE4 CHOL4 CIGS4 WGT4 FVC4 BMI4 DPF4 SPF4;
with SMOKE;
run;
proc corr data=framdat;
var AGE4 CHOL4 CIGS4 WGT4 FVC4 BMI4 DPF4 SPF4;
with HTN4;
run;
proc corr data=framdat;
var AGE4 CHOL4 CIGS4 WGT4 FVC4 BMI4 DPF4 SPF4;
with CHD;
run;
proc corr data=framdat;
var AGE4 CHOL4 CIGS4 WGT4 FVC4 BMI4 DPF4 SPF4;
with T2D;
run;

/*********************************************/

*/ VARIABLES SELECTED: SEX, AGE4, CHOL4, CIGS4, SPF4, FVC4, BMI4, SURV, DTH;

/*********************************************/


*/ MISSING VALUES - complete cases only;
data mainfram; set framdat;
if nmiss(SEX, AGE4, CHOL4, CIGS4, SPF4, FVC4, BMI4, SURV, DTH) = 0;
run;
*/ double check there are no missing values;
proc means data=mainfram n nmiss;
var SEX AGE4 CHOL4 CIGS4 SPF4 FVC4 BMI4 SURV DTH;
run;
*/ summary of new dataset;
proc summary data=mainfram print;
var SEX AGE4 CHOL4 CIGS4 SPF4 FVC4 BMI4 SURV DTH;
run;

/*********************************************/


*/ INITIAL MODEL;
proc phreg data=mainfram zph;
	class SEX(ref='1'); 
	model SURV*DTH(0) = SPF4 SEX AGE4 CHOL4 CIGS4 FVC4 BMI4 / rl ties=efron;
run;
*/ BMI4 and CHOL4 removed from model because not significant;

*/ FINAL MODEL;
proc phreg data=mainfram zph;
	class SEX(ref='1'); 
	model SURV*DTH(0) = SPF4 SEX AGE4 CIGS4 FVC4 / rl ties=efron;
run;
*/ summary of final model variables;
proc summary data=mainfram print;
var DTH SURV SPF4 SEX AGE4 CIGS4 FVC4;
run;

/*********************************************/


*/ INTERACTION ANALYSIS;
*/ Question 3 - is the association different between males vs females?;
proc phreg data=mainfram zph;
	class SEX(ref='1'); 
	model SURV*DTH(0) = SPF4 SEX AGE4 CIGS4 FVC4 SPF4*SEX / rl ties=efron;
run;
proc phreg data=mainfram zph;
	class SEX(ref='1'); 
	model SURV*DTH(0) = SPF4 SEX AGE4 CIGS4 FVC4 SPF4*SEX / rl ties=efron;
	hazardratio SPF4 / at (SEX='1');
	hazardratio SPF4 / at (SEX='2');
run;
*/ The association does not differ between males vs females;

/*********************************************/


*/ CONFOUNDING ANALYSIS;
*/ base model;
proc phreg data=mainfram zph;
	model SURV*DTH(0) = SPF4 / rl ties=efron;
run;
*/ fully adjusted model;
proc phreg data=mainfram zph;
	class SEX(ref='1'); 
	model SURV*DTH(0) = SPF4 SEX AGE4 CIGS4 FVC4 / rl ties=efron;
run;
*/ is sex a confounder?;
proc phreg data=mainfram zph;
	model SURV*DTH(0) = SPF4 AGE4 CIGS4 FVC4 / rl ties=efron;
run;
*/ is age a confounder?;
proc phreg data=mainfram zph;
	class SEX(ref='1'); 
	model SURV*DTH(0) = SPF4 SEX CIGS4 FVC4 / rl ties=efron;
run;
*/ is cigs a confounder?;
proc phreg data=mainfram zph;
	class SEX(ref='1'); 
	model SURV*DTH(0) = SPF4 SEX AGE4 FVC4 / rl ties=efron;
run;
*/ is fvc a confounder?;
proc phreg data=mainfram zph;
	class SEX(ref='1'); 
	model SURV*DTH(0) = SPF4 SEX AGE4 CIGS4 / rl ties=efron;
run;

/*********************************************/


*/ FOREST PLOT;
proc phreg data=mainfram zph;
	class SEX(ref='1'); 
	model SURV*DTH(0) = SPF4 SEX AGE4 CIGS4 FVC4 / rl ties=efron;
	ods output parameterestimates=hazards;
run;
title "Forest Plot of Hazard Ratios from Final Model";
proc sgplot data=hazards;
scatter x=HazardRatio y=Parameter / 
	xerrorlower=HRLowerCL
	xerrorupper=HRUpperCL
	markerattrs=or(symbol=DiamondFilled size=10);
refline 1 / axis=x;
text x=HazardRatio y=Parameter text=HazardRatio / 
position=bottom textattrs=(size=8 color='red');
xaxis label="Hazard Ratio (HR) with 95% CI " min=0;
yaxis label="Predictors";
keylegend / exclude=("Parameter");
run; 




