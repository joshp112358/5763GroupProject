%macro regBoot(NumberOfLoops, DataSet, XVariable, YVariable);

/*Sample my data with replacement*/
	proc surveyselect data=&DataSet out=bootData(rename=(Replicate=SampleNo)) seed=-23434 method=urs noprint samprate=1 reps=&NumberOfLoops;
	run;

/*Conduct a regression on this randomised dataset and get parameter estimates*/
	proc reg data=bootData outest=ParameterEstimates  noprint;
	by SampleNo;
	freq NumberHits;
	Model &YVariable=&XVariable;
	run;
	quit;

/*Extract just the columns for slope and intercept for storage*/
	data ResultHolder;
	set ParameterEstimates;
	keep Intercept &XVariable;
	rename Intercept=RandomIntercept &XVariable=RandomSlope;
	run;

/*mean estimates & 95%confint*/
ods rtf file="/folders/myfolders/output.rtf";
title "Analysis of Variables";
proc univariate data=resultholder cibasic;
var randomintercept randomslope;
run;

/*plot distribution*/
ods graphics / reset width=6.4in height=4.8in imagemap;
title "Bootstrap Distribution of Y Variable";
proc sgplot data=WORK.RESULTHOLDER;
	histogram RandomIntercept / fillattrs=(color=CX177fce transparency=0.5);
	density RandomIntercept;
	xaxis label= "Y Variable";
	yaxis grid;
run;

ods graphics / reset;

ods graphics / reset width=6.4in height=4.8in imagemap;
title "Bootstrap Distribution of X Variable";
proc sgplot data=WORK.RESULTHOLDER;
	histogram RandomSlope / fillattrs=(color=CXbd3de4 transparency=0.5);
	density RandomSlope;
	xaxis label= "X Variable";
	yaxis grid;
run;

ods graphics / reset;

ods rtf close;

%mend;

options nonotes;
/*Run the macro with timer*/
%let _timer_start = %sysfunc(datetime());
%regBoot(NumberOfLoops=100, DataSet=work.fitness, XVariable=age, YVariable=oxygen);
data _null_;
  dur = datetime() - &_timer_start;
  put 30*'-' / ' TOTAL DURATION:' dur time13.2 / 30*'-';
run;

