%macro regBoot(NumberOfLoops, DataSet, XVariable, YVariable);

%let _timer_start = %sysfunc(datetime());

/*Generate all bootstrap samples*/
	proc surveyselect data=&DataSet out=bootData(rename=(Replicate=SampleNo)) seed=23434 method=urs noprint samprate=1 reps=&NumberOfLoops;
	run;

/*Conduct a regression on each randomised dataset and get parameter estimates*/
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

data _null_;
  dur = datetime() - &_timer_start;
  put 30*'-' / ' TOTAL DURATION:' dur time13.2 / 30*'-';
run;

%mend;

options nonotes;

/*Run the macro*/
%regBoot(NumberOfLoops=100, DataSet=work.fitness, XVariable=age, YVariable=oxygen);

