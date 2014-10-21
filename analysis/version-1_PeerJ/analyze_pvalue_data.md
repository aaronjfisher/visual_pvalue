Supplemental Code
========================================================

This report was created using the knitr package (http://yihui.name/knitr/) in R (http://www.r-project.org/)


```r
#Initial Setup
#Note: Output from this code section has been masked for the sake of saving space.

library(plotrix) 
library(lme4)

opts_chunk$set(dev = 'pdf')

#The code book for the two files below is included in the readMe files of the github repository.
x_full<-read.csv('coursera_user_responses_tidy.csv',header=TRUE) #user responses to coursera questions.
load('data_for_1plots_coursera.RData')  #objects describing the library of plots shown to users.

logit<-function(x) log(x/(1-x))
invLogit<-function(x) {
  out<-exp(x)/(1+exp(x)) #maintains the ability of the function to accept vectors
  out[exp(x)==Inf]<-1
	return(out)
}
```


```r
#Output from this section has been masked

#Process x_full

#re-order the style factor's levels so the reference category is first
uStyle<-unique(c('n100ref',as.character(x_full$style)))
x_full$style<-factor(as.character(x_full$style),levels=uStyle)
x_full$datVer<- as.factor(as.numeric(x_full$datVer))
x_full$id<- as.factor(as.numeric(x_full$id))
x_full$attemptNumFactor<- as.factor(as.numeric(x_full$attemptNum))

#preview x_full
dim(x_full) #x_full includes everything, including missing data
head(x_full) #note: styleNum values 2 & 3 are actually the same question style
lapply(x_full[1,],class)
N<-sum(!is.na(x_full$guessSig))
n<-sum(!duplicated(x_full$id[!is.na(x_full$guessSig)]))
K<-length(uStyle)
N #total # responses
n #total # users
K #total # of question types

#Make a version without missing data
x<- x_full[!is.na(x_full$guessSig),]

#Get number of answers for each question type
nStyle<-c(table(x$style))[uStyle]
nStyle


uStyle
pretty_style_labels<-c('Reference', 'Smaller n', 'Larger n', 'Best Fit','Axis Scale',
		'Axis Label', 'Outlier', 'Lowess') #for use in plots


######### How many users finished their first attempts of the quiz
    ## this section is commented out, as it runs fairly slowly.
# userNames<-unique(x$id)
# num_of_first_try_by_user<-rep(NA,n)
# for(i in 1:n){
#   num_of_first_try_by_user[i]<- sum(x$id==userNames[i] & x$attemptNum==1)
# }
# mean(num_of_first_try_by_user==9) #=.944
#########
```

Models for Baseline Accuracy, and Effect of Plot Presentation Style
-----------------


```r
#Just look at first attempts of the survey
attemptNum1Data<-x[x$attemptNum==1,]


#Using 2 separate models, one for sensitivity, one for specificity (sense & spec)
sum(attemptNum1Data$trueSig) # number of responses used in sense model = 9063
```

```
## [1] 9063
```

```r
sum(!attemptNum1Data$trueSig) # number of responses used in spec model = 9032
```

```
## [1] 9032
```

```r
glmmSense = glmer(correct ~ 1 + (1|id) + style, data=attemptNum1Data[attemptNum1Data$trueSig,],
	family="binomial") #Sensitivity model
glmmSpec = glmer(correct ~ 1 + (1|id) + style, data=attemptNum1Data[!attemptNum1Data$trueSig,],
	family="binomial") #Specificity model

#Show basic output
print(glmmSense,correlation=FALSE)
```

```
## Generalized linear mixed model fit by maximum likelihood (Laplace
##   Approximation) [glmerMod]
##  Family: binomial  ( logit )
## Formula: correct ~ 1 + (1 | id) + style
##    Data: attemptNum1Data[attemptNum1Data$trueSig, ]
##      AIC      BIC   logLik deviance df.resid 
##    12099    12163    -6041    12081     9054 
## Random effects:
##  Groups Name        Std.Dev.
##  id     (Intercept) 0.63    
## Number of obs: 9063, groups:  id, 2036
## Fixed Effects:
##    (Intercept)        stylen35       stylen200    stylebestFit  
##         -0.103          -0.789          -0.116           0.480  
## styleaxesScale  styleaxesLabel    styleoutlier     stylelowess  
##          0.279           0.017           1.013           0.230
```

```r
print(glmmSpec,correlation=FALSE)
```

```
## Generalized linear mixed model fit by maximum likelihood (Laplace
##   Approximation) [glmerMod]
##  Family: binomial  ( logit )
## Formula: correct ~ 1 + (1 | id) + style
##    Data: attemptNum1Data[!attemptNum1Data$trueSig, ]
##      AIC      BIC   logLik deviance df.resid 
##    11120    11184    -5551    11102     9023 
## Random effects:
##  Groups Name        Std.Dev.
##  id     (Intercept) 0.765   
## Number of obs: 9032, groups:  id, 2032
## Fixed Effects:
##    (Intercept)        stylen35       stylen200    stylebestFit  
##          1.076           0.522          -1.139          -0.511  
## styleaxesScale  styleaxesLabel    styleoutlier     stylelowess  
##         -0.400          -0.209          -0.529          -0.357
```

```r
#Get odds ratios and CIs for odds ratios
getORCIs<-function(model){ 
  #logit(E(Y))=X*Beta; 
  #odds(E(Y))=exp(X*Beta)
  modelCoef<-fixef(model)
  modelSe<-sqrt(diag(vcov(model))) #std error
  fitted_OR<-exp(modelCoef) 
  li<- exp(modelCoef - qnorm(.975) * modelSe)
  ui<- exp(modelCoef + qnorm(.975) * modelSe)

  out<- signif(cbind(fitted_OR, li, ui),3)
  rownames(out)<- pretty_style_labels
  kable(out)
}
```

Show odds ratios, CIs for odds ratios, and variance explained by random intercepts.


```r
# 95% Confidence intervals = (li, ui)
getORCIs(glmmSense)
```



|           | fitted_OR|    li|    ui|
|:----------|---------:|-----:|-----:|
|Reference  |     0.903| 0.820| 0.993|
|Smaller n  |     0.454| 0.384| 0.538|
|Larger n   |     0.891| 0.759| 1.040|
|Best Fit   |     1.620| 1.370| 1.900|
|Axis Scale |     1.320| 1.120| 1.560|
|Axis Label |     1.020| 0.867| 1.190|
|Outlier    |     2.750| 2.330| 3.260|
|Lowess     |     1.260| 1.070| 1.480|

```r
getORCIs(glmmSpec)
```



|           | fitted_OR|    li|    ui|
|:----------|---------:|-----:|-----:|
|Reference  |     2.930| 2.630| 3.270|
|Smaller n  |     1.690| 1.390| 2.040|
|Larger n   |     0.320| 0.269| 0.381|
|Best Fit   |     0.600| 0.506| 0.713|
|Axis Scale |     0.670| 0.565| 0.795|
|Axis Label |     0.811| 0.680| 0.969|
|Outlier    |     0.589| 0.495| 0.701|
|Lowess     |     0.700| 0.588| 0.833|

```r
#Show variance explained by the random intercepts in each model
#Works specifically for binomial models
get_var_explained_by_rand_int<-function(model){
	G<-attr(VarCorr(model)$id,'stddev')^2
	return( G/(G+(pi^2)/3) )
}
# get_var_explained_by_rand_int(glmmInt)
get_var_explained_by_rand_int(glmmSense)
```

(Intercept) 
     0.1077 

```r
get_var_explained_by_rand_int(glmmSpec)
```

(Intercept) 
     0.1511 


Recreate two examples of plots shown to users.


```r
par(mar=c(3,3.5,3,0),oma=c(1,.5,0,1),mfcol=c(1,2))

#Make example plots from the reference category, one significant and one not
plotSigRefInd<-which(pres=='n100ref'&pvals<.05)[4]
plotNotSigRefInd<-which(pres=='n100ref'&pvals>=.05)[4]

plot(xes[plotSigRefInd,],yes[plotSigRefInd,],xlab='',ylab='')
mtext(paste0('Example: Truly Significant Plot\n(p=',round(pvals[plotSigRefInd],3),')'),
  3,line=.4,font=2,cex=1.2)
mtext('X',1,line=2)
mtext('Y',2,line=2.5)
mtext('A)',3,line=1.5,adj=0,font=2,cex=1.2)

plot(xes[plotNotSigRefInd,],yes[plotNotSigRefInd,],xlab='',ylab='')
mtext(paste0('Example: Non-significant Plot\n(p=',round(pvals[plotNotSigRefInd],3),')'),3,line=.4,
  font=2,cex=1.2)
mtext('X',1,line=2)
mtext('B)',3,line=1.5,adj=0,font=2,cex=1.2)
```

![plot of chunk example_ref_plots](figure/example_ref_plots.pdf) 

Get confidence intervals on accuracy scale, as opposed to odds ratio scale, and plot results.


```r
#Function for generating confidence intervals for the fitted accuracy rates for each plot style;
#This function is meant to be used separately, for both the sensitivity and specificity models
getCIs<-function(model=glmmSense, plotInd=1:K, ci_width_scalar=1.96, plotIt=TRUE,
	axisLab=TRUE, cex.axis=1, offset=0, ref_lwd=2, ...){ #y can also be specificity
  #diag(vcov(model))
	#fixef(model)
	
	coefNames<-rep(NA,K)
	coefNames[1]<-'(Intercept)'
	coefNames[2:K]<-levels(x$style)[-1]
	modelCoef<-fixef(model)
	names(modelCoef)<-coefNames

	ui<- #upper CI (on probability/accuracy scale)
	li<- # Lower CI
	center<-rep(NA,8)
	for(k in 1:K){
		#let a be the vector such that crossprod(a,modelCoef) = intercept + coefficient[k]
		#abbreviate this crossproduct as 'af'
		a<-rep(0,K)
		names(a)<-coefNames
		a['(Intercept)']<-1
		if(k>1) a[coefNames[k]]<-1
		var_af<- t(a) %*% vcov(model) %*% a
		se_af<-sqrt(as.numeric(var_af))
		center_logOdds <- t(a)%*%modelCoef
		ui[k]<- invLogit( center_logOdds + ci_width_scalar*se_af)
		li[k]<- invLogit( center_logOdds - ci_width_scalar*se_af)
		center[k]<- invLogit(center_logOdds)
	}	

	if(plotIt){
		plotCI(x=center[plotInd]*100,y=offset+(length(plotInd)):1,ui=ui[plotInd]*100,
			li=li[plotInd]*100,pch=19,cex=.5,yaxt='n',err='x',ylab='', ...)
		if(axisLab) axis(2, at=1:(length(plotInd)), labels=pretty_style_labels[plotInd][(length(plotInd)):1],
			cex.axis=cex.axis,las=2) #need to reorder labels so they go down, not up
		abline(v=center[1]*100,lty=2,lwd=ref_lwd)
	}
	
	out<- cbind(center,ui,li)
	rownames(out)<- pretty_style_labels
	return(kable(out))
}


#plot confidence intervals for fitted accuracy
par(oma=c(0,4,0,0))
plotInd4CIfig_pre<-c(1,order(fixef(glmmSense)[-1])+1) #order plots by sense coef, not including ref category
plotInd4CIfig<-plotInd4CIfig_pre[plotInd4CIfig_pre!=7] #dropping outlier from plot
getCIs(glmmSense,plotInd=plotInd4CIfig,col=c("darkblue"),main='',xlab='',xlim=c(0,100),
	cex.axis=1.1,lwd=2,offset=.125,ylim=c(.8,7.2))
```



|           | center|     ui|     li|
|:----------|------:|------:|------:|
|Reference  | 0.4744| 0.4982| 0.4507|
|Smaller n  | 0.2907| 0.3215| 0.2618|
|Larger n   | 0.4456| 0.4789| 0.4128|
|Best Fit   | 0.5934| 0.6261| 0.5598|
|Axis Scale | 0.5440| 0.5780| 0.5096|
|Axis Label | 0.4786| 0.5122| 0.4453|
|Outlier    | 0.7132| 0.7418| 0.6827|
|Lowess     | 0.5319| 0.5651| 0.4984|

```r
mtext('% Accuracy', side=1, line=2,cex=1.1)
mtext('Accuracy of Significance Classifications', side=3, line=.4,cex=1.2,font=2)
getCIs(glmmSpec,plotInd=plotInd4CIfig,col=c("darkred"),axisLab=FALSE,main='',xlab='',
	xlim=c(0,100),lwd=2,add=TRUE,offset=-.125)
```



|           | center|     ui|     li|
|:----------|------:|------:|------:|
|Reference  | 0.7457| 0.7660| 0.7242|
|Smaller n  | 0.8317| 0.8541| 0.8066|
|Larger n   | 0.4842| 0.5193| 0.4492|
|Best Fit   | 0.6376| 0.6698| 0.6042|
|Axis Scale | 0.6627| 0.6937| 0.6303|
|Axis Label | 0.7041| 0.7342| 0.6720|
|Outlier    | 0.6334| 0.6663| 0.5992|
|Lowess     | 0.6723| 0.7035| 0.6395|

```r
abline(v=seq(0,100,by=20),col='darkgray',lty=2,lwd=2)
legend('bottomleft',c('Sensitivity', 'Specificity'),col=c('darkblue','darkred'),pch=19,bg='white')
```

![plot of chunk accuracy_CIs](figure/accuracy_CIs.pdf) 




Models for Learning
---------------



```r
#select users who did the survey at least twice, but exclude questions they saw twice.
users_with_multiple_tries<- unique(x$id[x$attemptNum>1])
multi_try_data_ind<-x$id %in% users_with_multiple_tries 
multi_try_data<-x[multi_try_data_ind& x$firstTry,]

#Calculate the percent of second attempts were discarded because users saw a duplicate plot
1-sum(multi_try_data_ind& x$firstTry)/sum(multi_try_data_ind)
```

```
## [1] 0.1279
```

```r
########  Of users with multiple attempts, how many compeleted their first & second attempts?
userNames2<-unique(x[multi_try_data_ind,'id'])
multi_users_1st_tries<-
multi_users_2nd_tries<-rep(NA,length(userNames2))
for(i in 1:length(userNames2)){
  multi_users_2nd_tries[i]<- sum(x[multi_try_data_ind,'id']==userNames2[i] &
  	x[multi_try_data_ind,'attemptNum']==2)
  multi_users_1st_tries[i]<- sum(x[multi_try_data_ind,'id']==userNames2[i] & 
  	x[multi_try_data_ind,'attemptNum']==1)
}
mean(multi_users_1st_tries==9) #=.92
```

```
## [1] 0.9208
```

```r
mean(multi_users_2nd_tries==9) #=.99
```

```
## [1] 0.9901
```

```r
########


cut=2 #cutoff point for max # of tries in our model (only look at first and second attempts)
multi_try_data_sense_leq_cut<- multi_try_data[multi_try_data$attemptNum<=cut & multi_try_data$trueSig,]
multi_try_data_spec_leq_cut <- multi_try_data[multi_try_data$attemptNum<=cut & !multi_try_data$trueSig,]

#Fit random intercept model
dim(multi_try_data_sense_leq_cut) #846 = # responses in model
```

```
## [1] 846  15
```

```r
glmmSenseLearn_rIntercept = glmer(correct~ 1 +  (1|id) + attemptNumFactor*style,
  data=multi_try_data_sense_leq_cut, family="binomial") # Fit model with interaction terms
```

```
## Warning: Model failed to converge with max|grad| = 0.0116144 (tol = 0.001,
## component 13)
```

```r
print(glmmSenseLearn_rIntercept,correlation=FALSE)
```

```
## Generalized linear mixed model fit by maximum likelihood (Laplace
##   Approximation) [glmerMod]
##  Family: binomial  ( logit )
## Formula: correct ~ 1 + (1 | id) + attemptNumFactor * style
##    Data: multi_try_data_sense_leq_cut
##      AIC      BIC   logLik deviance df.resid 
##   1131.1   1211.7   -548.6   1097.1      829 
## Random effects:
##  Groups Name        Std.Dev.
##  id     (Intercept) 0.383   
## Number of obs: 846, groups:  id, 101
## Fixed Effects:
##                      (Intercept)                 attemptNumFactor2  
##                           -0.475                             1.656  
##                         stylen35                         stylen200  
##                           -1.238                             0.416  
##                     stylebestFit                    styleaxesScale  
##                            0.211                             0.393  
##                   styleaxesLabel                      styleoutlier  
##                            0.222                             0.945  
##                      stylelowess        attemptNumFactor2:stylen35  
##                            0.725                            -0.150  
##      attemptNumFactor2:stylen200    attemptNumFactor2:stylebestFit  
##                           -1.308                            -0.561  
## attemptNumFactor2:styleaxesScale  attemptNumFactor2:styleaxesLabel  
##                           -0.975                            -1.522  
##   attemptNumFactor2:styleoutlier     attemptNumFactor2:stylelowess  
##                           -1.694                            -2.049
```

```r
dim(multi_try_data_spec_leq_cut) #859 = # responses in model
```

```
## [1] 859  15
```

```r
glmmSpecLearn_rIntercept = glmer(correct ~ 1 +  (1|id) + attemptNumFactor*style,
	data=multi_try_data_spec_leq_cut, family="binomial") #
print(glmmSpecLearn_rIntercept,correlation=FALSE) 
```

```
## Generalized linear mixed model fit by maximum likelihood (Laplace
##   Approximation) [glmerMod]
##  Family: binomial  ( logit )
## Formula: correct ~ 1 + (1 | id) + attemptNumFactor * style
##    Data: multi_try_data_spec_leq_cut
##      AIC      BIC   logLik deviance df.resid 
##   1151.6   1232.5   -558.8   1117.6      842 
## Random effects:
##  Groups Name        Std.Dev.
##  id     (Intercept) 0.668   
## Number of obs: 859, groups:  id, 101
## Fixed Effects:
##                      (Intercept)                 attemptNumFactor2  
##                          0.77087                          -0.02719  
##                         stylen35                         stylen200  
##                          0.92746                          -1.43888  
##                     stylebestFit                    styleaxesScale  
##                         -0.72145                          -0.82359  
##                   styleaxesLabel                      styleoutlier  
##                         -0.13382                          -0.26011  
##                      stylelowess        attemptNumFactor2:stylen35  
##                         -0.80865                          -1.78792  
##      attemptNumFactor2:stylen200    attemptNumFactor2:stylebestFit  
##                          0.83449                           0.35148  
## attemptNumFactor2:styleaxesScale  attemptNumFactor2:styleaxesLabel  
##                          0.00191                          -0.61431  
##   attemptNumFactor2:styleoutlier     attemptNumFactor2:stylelowess  
##                         -0.72382                           0.08577
```

```r
#Show variance explained by random intercepts
get_var_explained_by_rand_int(glmmSenseLearn_rIntercept)
```

```
## (Intercept) 
##     0.04263
```

```r
get_var_explained_by_rand_int(glmmSpecLearn_rIntercept)
```

```
## (Intercept) 
##      0.1195
```

```r
#Get CIs for learning models
# Get confidence intervals for the fitted accuracy rates for each combination of style and 
# attempt number in the learning model. 
# Also get confidence intervals for odds ratios for the learning effect in each category.
# To get interpretation of effect of attemptNum, we need to add attemptNum coeff to interaction terms. 
# Negative interaction just means that the learning effect is less strong than in the reference category.
# The function below returns a list of CI matrixes for each style, with rows for attemptNum.
getCIlearn<-function(model,ci_width_scalar=1.96,test_type='two_sided'){    
	
	modelCoef<-fixef(model)
	coefNames<-names(modelCoef)

	#find value of 'cut' (max attemptNum in model)
	cut=length(coefNames)/K #K = number of categories for each attemptNum

	#list output by style
	CImats<-list()

	for(k in 1:K){
		k_mat<-matrix(NA,cut,8)
		colnames(k_mat)<-c('centerProb','liProb','uiProb','centerOR','liOR',
			'uiOR','zstat','p_value') #we'll add this to CImats later
		#attemptNum is the row index of k_mat
		for(no in 1:cut){
      # let a be the vector such that a'modelCoef = intercept + coefficient k
			#abbreviate this t(a)%*%modelCoef as af
      
			a<-rep(0,times=length(coefNames))
			names(a)<-coefNames
			a['(Intercept)']<-1
			if(k>1) a[paste0('style',uStyle[k])] <-1
			if(no>1) a[paste0('attemptNumFactor',no)]<-1
			if(k>1 & no>1) a[paste0('attemptNumFactor',no,':style',uStyle[k])]	<-1

			var_af<- t(a) %*% vcov(model) %*% a
			se_af<-sqrt(as.numeric(var_af))

			center_logOdds <- crossprod(a,modelCoef)
			k_mat[no,'uiProb']<- invLogit( center_logOdds + ci_width_scalar*se_af)
			k_mat[no,'liProb']<- invLogit( center_logOdds - ci_width_scalar*se_af)
			k_mat[no,'centerProb']<- invLogit(center_logOdds)
    

			# pvalues need to be calculated the same way, but without the intercept term,
			# and without the baseline style term.
			# Get dist of "wf"=t(w)%*%coefficients, where w is a vector similar to "a", above.
			if(no==1) next #This not relevant if a attemptNum=1
      
			w<-rep(0,times=length(coefNames))
      names(w)<-coefNames
      w[paste0('attemptNumFactor',no)]<-1
			if(k>1) w[paste0('attemptNumFactor',no,':style',uStyle[k])]	<-1

			var_wf<-t(w) %*% vcov(model) %*% w
			se_wf<-sqrt(as.numeric(var_wf))
      
      k_mat[no,'centerOR']<- exp(crossprod(w,modelCoef))
      k_mat[no,'uiOR']<- exp( crossprod(w,modelCoef) + ci_width_scalar*se_wf)
  		k_mat[no,'liOR']<- exp( crossprod(w,modelCoef) - ci_width_scalar*se_wf)
      
			zstat<- t(w)%*%modelCoef / se_wf
			if(test_type=="two_sided")      p_value<- 2*(1-pnorm(abs(zstat)))
			if(test_type=="one_sided_up")   p_value<-   (1-pnorm(zstat))
			if(test_type=="one_sided_down") p_value<-      pnorm(zstat)
			k_mat[no,'zstat']<- zstat
			k_mat[no,'p_value']<- p_value

		}
		CImats[[ uStyle[k] ]]<-k_mat
	}
	return(CImats)
}

#Show confidence intervals for odds ratios regarding the learning effect, for each category.
getCIlearn(glmmSenseLearn_rIntercept) 
```

```
## $n100ref
##      centerProb liProb uiProb centerOR  liOR  uiOR zstat   p_value
## [1,]     0.3834 0.2912 0.4849       NA    NA    NA    NA        NA
## [2,]     0.7652 0.6532 0.8493    5.239 2.651 10.36 4.765 1.891e-06
## 
## $n35
##      centerProb liProb uiProb centerOR  liOR  uiOR zstat  p_value
## [1,]     0.1528 0.0796 0.2734       NA    NA    NA    NA       NA
## [2,]     0.4486 0.3127 0.5927     4.51 1.776 11.45 3.167 0.001539
## 
## $n200
##      centerProb liProb uiProb centerOR   liOR  uiOR  zstat p_value
## [1,]     0.4853 0.3506 0.6222       NA     NA    NA     NA      NA
## [2,]     0.5718 0.4113 0.7184    1.416 0.6068 3.304 0.8044  0.4212
## 
## $bestFit
##      centerProb liProb uiProb centerOR  liOR  uiOR zstat p_value
## [1,]     0.4343 0.2985 0.5807       NA    NA    NA    NA      NA
## [2,]     0.6965 0.5512 0.8109     2.99 1.274 7.017 2.516 0.01187
## 
## $axesScale
##      centerProb liProb uiProb centerOR   liOR  uiOR zstat p_value
## [1,]     0.4795 0.3509 0.6108       NA     NA    NA    NA      NA
## [2,]     0.6455 0.4923 0.7736    1.976 0.8713 4.482  1.63   0.103
## 
## $axesLabel
##      centerProb liProb uiProb centerOR   liOR  uiOR  zstat p_value
## [1,]     0.4371 0.3011 0.5834       NA     NA    NA     NA      NA
## [2,]     0.4703 0.3125 0.6343    1.143 0.4708 2.776 0.2956  0.7675
## 
## $outlier
##      centerProb liProb uiProb centerOR   liOR  uiOR    zstat p_value
## [1,]     0.6153 0.4645 0.7468       NA     NA    NA       NA      NA
## [2,]     0.6064 0.4617 0.7345   0.9631 0.4155 2.233 -0.08757  0.9302
## 
## $lowess
##      centerProb liProb uiProb centerOR   liOR  uiOR   zstat p_value
## [1,]     0.5621 0.4159 0.6983       NA     NA    NA      NA      NA
## [2,]     0.4644 0.3212 0.6137   0.6753 0.2918 1.563 -0.9172  0.3591
```

```r
getCIlearn(glmmSpecLearn_rIntercept)
```

```
## $n100ref
##      centerProb liProb uiProb centerOR   liOR  uiOR    zstat p_value
## [1,]     0.6837 0.5775 0.7737       NA     NA    NA       NA      NA
## [2,]     0.6778 0.5635 0.7742   0.9732 0.5126 1.848 -0.08313  0.9337
## 
## $n35
##      centerProb liProb uiProb centerOR    liOR   uiOR  zstat   p_value
## [1,]     0.8453 0.7094 0.9244       NA      NA     NA     NA        NA
## [2,]     0.4708 0.3183 0.6290   0.1628 0.05881 0.4508 -3.493 0.0004771
## 
## $n200
##      centerProb liProb uiProb centerOR   liOR  uiOR zstat p_value
## [1,]     0.3389 0.2113 0.4953       NA     NA    NA    NA      NA
## [2,]     0.5348 0.3841 0.6794    2.242 0.9347 5.377 1.809  0.0705
## 
## $bestFit
##      centerProb liProb uiProb centerOR   liOR  uiOR  zstat p_value
## [1,]     0.5124 0.3673 0.6554       NA     NA    NA     NA      NA
## [2,]     0.5924 0.4316 0.7355    1.383 0.5855 3.267 0.7394  0.4597
## 
## $axesScale
##      centerProb liProb uiProb centerOR   liOR  uiOR    zstat p_value
## [1,]     0.4868 0.3269 0.6495       NA     NA    NA       NA      NA
## [2,]     0.4805 0.3390 0.6252    0.975 0.4067 2.337 -0.05666  0.9548
## 
## $axesLabel
##      centerProb liProb uiProb centerOR   liOR  uiOR  zstat p_value
## [1,]     0.6541 0.5032 0.7792       NA     NA    NA     NA      NA
## [2,]     0.4989 0.3560 0.6419   0.5265 0.2274 1.219 -1.497  0.1343
## 
## $outlier
##      centerProb liProb uiProb centerOR   liOR  uiOR  zstat p_value
## [1,]     0.6250 0.4734 0.7555       NA     NA    NA     NA      NA
## [2,]     0.4402 0.2858 0.6072   0.4719 0.1917 1.162 -1.634  0.1023
## 
## $lowess
##      centerProb liProb uiProb centerOR   liOR uiOR  zstat p_value
## [1,]     0.4906 0.3467 0.6360       NA     NA   NA     NA      NA
## [2,]     0.5052 0.3588 0.6507     1.06 0.4626 2.43 0.1384  0.8899
```

Display results for learning.


```r
#Plotting (CIs for accuracy)
plot_LearnCImats<-function(ciMat, plotStyle='n100ref', type='sense', data=multi_try_data,
		offset=0, cex.axis=1.1, add=FALSE, ...){
	cut<-dim(ciMat)[1]
	thickness<-c(2,2)
  
  
	plotCI(x=ciMat[,'centerProb'], y=1:cut+offset, ui=ciMat[,'uiProb'], li=ciMat[,'liProb'],
		err='x', pch=19, cex=.5, xaxt='n', yaxt='n', lwd=thickness, xlim=c(-.05,1.05),
		ylim=c(.7,cut+.3), add=add, ...)
	if(add==FALSE){
	#need to reorder labels so they go down, not up
    axis(2, at=(1:cut), labels=1:cut,cex.axis=cex.axis) 
  	xLabels<-seq(0,1,length=6)
  	#need to reorder labels so they go down, not up
  	axis(1, at=xLabels, labels=xLabels*100,cex.axis=cex.axis) 
  	abline(v=seq(0,1,by=.2),lty=2,lwd=2,col='darkgray')	
    mtext(text='Attempt\nNumber',side=2,line=2.25,cex=cex.axis)
  }
}



ciMat_learn_sense <- getCIlearn(glmmSenseLearn_rIntercept)
ciMat_learn_spec  <- getCIlearn(glmmSpecLearn_rIntercept)




par(mfrow=c(3,1),mar=c(2,7,3,1),oma=c(3,0,0,0))

#REFERENCE
plot_LearnCImats(ciMat_learn_sense[['n100ref']],col='darkblue',plotStyle='n100ref',
	type='sense',xlab='',ylab='',offset=.125)
plot_LearnCImats(ciMat_learn_spec[['n100ref']],main='',col='darkred',
	plotStyle='n100ref',type='spec',xlab='',add=TRUE,offset=-.125)
mtext(text='Reference',side=3,line=.4,cex=1.2,font=2)

legend('topleft',c('Sensitivity', 'Specificity'),col=c('darkblue','darkred'),
	pch=19,cex=1.4,bg='white')

#SMALL N
plot_LearnCImats(ciMat_learn_sense[['n35']],col='darkblue',plotStyle='n35',
	type='sense',ylab='',xlab='',offset=.125)
plot_LearnCImats(ciMat_learn_spec[['n35']],main='',col='darkred',
	plotStyle='n35',type='spec',xlab='',ylab='',offset=-.125,add=TRUE)
mtext(text='Smaller n',side=3,line=.4,cex=1.2,font=2)

#BEST FIT
plot_LearnCImats(ciMat_learn_sense[['bestFit']],col='darkblue',ylab='',
	xlab='',plotStyle='bestFit',type='sense',offset=.125)
plot_LearnCImats(ciMat_learn_spec[['bestFit']],main='',col='darkred',
	plotStyle='bestFit',type='spec',xlab='',offset=-.125,add=TRUE)
mtext(text='Best Fit',side=3,line=.4,cex=1.2,font=2)

mtext(text='% Accuracy',side=1,line=3,cex=1.1)
```

![plot of chunk learning_accuracy](figure/learning_accuracy.pdf) 



