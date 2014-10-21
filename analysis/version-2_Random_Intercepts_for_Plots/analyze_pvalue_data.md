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
glmmSense <- glmer(correct ~ 1 + (1|id) + (1|qNameCoursera) + style, 
  data=attemptNum1Data[attemptNum1Data$trueSig,],family="binomial") #Sensitivity model
glmmSpec <- glmer(correct ~ 1 + (1|id) + (1|qNameCoursera) + style,
  data=attemptNum1Data[!attemptNum1Data$trueSig,],family="binomial") #Specificity model


### Show basic output
print(glmmSense,correlation=FALSE)
```

```
## Generalized linear mixed model fit by maximum likelihood (Laplace
##   Approximation) [glmerMod]
##  Family: binomial  ( logit )
## Formula: correct ~ 1 + (1 | id) + (1 | qNameCoursera) + style
##    Data: attemptNum1Data[attemptNum1Data$trueSig, ]
##      AIC      BIC   logLik deviance df.resid 
##    11896    11967    -5938    11876     9053 
## Random effects:
##  Groups        Name        Std.Dev.
##  id            (Intercept) 0.659   
##  qNameCoursera (Intercept) 0.371   
## Number of obs: 9063, groups:  id, 2036; qNameCoursera, 40
## Fixed Effects:
##    (Intercept)        stylen35       stylen200    stylebestFit  
##        -0.1165         -0.7954         -0.1085          0.5146  
## styleaxesScale  styleaxesLabel    styleoutlier     stylelowess  
##         0.3081          0.0347          1.0457          0.2589
```

```r
print(glmmSpec,correlation=FALSE)
```

```
## Generalized linear mixed model fit by maximum likelihood (Laplace
##   Approximation) [glmerMod]
##  Family: binomial  ( logit )
## Formula: correct ~ 1 + (1 | id) + (1 | qNameCoursera) + style
##    Data: attemptNum1Data[!attemptNum1Data$trueSig, ]
##      AIC      BIC   logLik deviance df.resid 
##    10921    10993    -5451    10901     9022 
## Random effects:
##  Groups        Name        Std.Dev.
##  id            (Intercept) 0.799   
##  qNameCoursera (Intercept) 0.426   
## Number of obs: 9032, groups:  id, 2032; qNameCoursera, 40
## Fixed Effects:
##    (Intercept)        stylen35       stylen200    stylebestFit  
##          1.109           0.607          -1.160          -0.507  
## styleaxesScale  styleaxesLabel    styleoutlier     stylelowess  
##         -0.433          -0.219          -0.556          -0.389
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
|Reference  |     0.890| 0.633| 1.250|
|Smaller n  |     0.451| 0.276| 0.738|
|Larger n   |     0.897| 0.551| 1.460|
|Best Fit   |     1.670| 1.030| 2.730|
|Axis Scale |     1.360| 0.834| 2.220|
|Axis Label |     1.040| 0.635| 1.690|
|Outlier    |     2.850| 1.740| 4.650|
|Lowess     |     1.300| 0.795| 2.110|

```r
getORCIs(glmmSpec)
```



|           | fitted_OR|    li|    ui|
|:----------|---------:|-----:|-----:|
|Reference  |     3.030| 2.050| 4.480|
|Smaller n  |     1.840| 1.040| 3.230|
|Larger n   |     0.314| 0.180| 0.547|
|Best Fit   |     0.602| 0.345| 1.050|
|Axis Scale |     0.649| 0.372| 1.130|
|Axis Label |     0.804| 0.460| 1.400|
|Outlier    |     0.573| 0.329| 1.000|
|Lowess     |     0.678| 0.388| 1.180|

```r
#Show variance explained by the random intercepts in each model
#Works specifically for binomial models
get_var_explained_by_rand_int<-function(model){
  G<-unlist(lapply(
      (VarCorr(model)),
      function(x)attr(x,'stddev')
    ))^2
  return( G/(sum(G)+(pi^2)/3) )
}

# get_var_explained_by_rand_int(glmmInt)
get_var_explained_by_rand_int(glmmSense)
```

           id.(Intercept) qNameCoursera.(Intercept) 
                  0.11251                   0.03571 

```r
get_var_explained_by_rand_int(glmmSpec)
```

           id.(Intercept) qNameCoursera.(Intercept) 
                  0.15543                   0.04424 


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
|Reference  | 0.4709| 0.5556| 0.3878|
|Smaller n  | 0.2866| 0.3647| 0.2195|
|Larger n   | 0.4440| 0.5319| 0.3594|
|Best Fit   | 0.5982| 0.6797| 0.5110|
|Axis Scale | 0.5477| 0.6332| 0.4594|
|Axis Label | 0.4795| 0.5673| 0.3930|
|Outlier    | 0.7169| 0.7835| 0.6393|
|Lowess     | 0.5355| 0.6215| 0.4474|

```r
mtext('% Accuracy', side=1, line=2,cex=1.1)
mtext('Accuracy of Significance Classifications', side=3, line=.4,cex=1.2,font=2)
getCIs(glmmSpec,plotInd=plotInd4CIfig,col=c("darkred"),axisLab=FALSE,main='',xlab='',
	xlim=c(0,100),lwd=2,add=TRUE,offset=-.125)
```



|           | center|     ui|     li|
|:----------|------:|------:|------:|
|Reference  | 0.7520| 0.8175| 0.6724|
|Smaller n  | 0.8477| 0.8938| 0.7863|
|Larger n   | 0.4873| 0.5866| 0.3890|
|Best Fit   | 0.6462| 0.7317| 0.5502|
|Axis Scale | 0.6629| 0.7458| 0.5686|
|Axis Label | 0.7090| 0.7848| 0.6194|
|Outlier    | 0.6348| 0.7220| 0.5379|
|Lowess     | 0.6727| 0.7544| 0.5790|

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
glmmSenseLearn_rIntercept = glmer(correct~ 1 +  (1|id)+ (1|qNameCoursera) + attemptNumFactor*style,
  data=multi_try_data_sense_leq_cut, family="binomial") # Fit model with interaction terms
```

```
## Warning: Model failed to converge with max|grad| = 0.00142253 (tol =
## 0.001, component 17)
```

```r
print(glmmSenseLearn_rIntercept,correlation=FALSE)
```

```
## Generalized linear mixed model fit by maximum likelihood (Laplace
##   Approximation) [glmerMod]
##  Family: binomial  ( logit )
## Formula: 
## correct ~ 1 + (1 | id) + (1 | qNameCoursera) + attemptNumFactor *  
##     style
##    Data: multi_try_data_sense_leq_cut
##      AIC      BIC   logLik deviance df.resid 
##   1133.0   1218.4   -548.5   1097.0      828 
## Random effects:
##  Groups        Name        Std.Dev.
##  id            (Intercept) 0.385   
##  qNameCoursera (Intercept) 0.126   
## Number of obs: 846, groups:  id, 101; qNameCoursera, 40
## Fixed Effects:
##                      (Intercept)                 attemptNumFactor2  
##                           -0.477                             1.668  
##                         stylen35                         stylen200  
##                           -1.236                             0.417  
##                     stylebestFit                    styleaxesScale  
##                            0.221                             0.399  
##                   styleaxesLabel                      styleoutlier  
##                            0.225                             0.952  
##                      stylelowess        attemptNumFactor2:stylen35  
##                            0.727                            -0.162  
##      attemptNumFactor2:stylen200    attemptNumFactor2:stylebestFit  
##                           -1.322                            -0.578  
## attemptNumFactor2:styleaxesScale  attemptNumFactor2:styleaxesLabel  
##                           -0.995                            -1.537  
##   attemptNumFactor2:styleoutlier     attemptNumFactor2:stylelowess  
##                           -1.714                            -2.063
```

```r
dim(multi_try_data_spec_leq_cut) #859 = # responses in model
```

```
## [1] 859  15
```

```r
glmmSpecLearn_rIntercept = glmer(correct ~ 1 +  (1|id) + (1|qNameCoursera)+ attemptNumFactor*style,
	data=multi_try_data_spec_leq_cut, family="binomial") #
```

```
## Warning: Model failed to converge with max|grad| = 0.00306378 (tol =
## 0.001, component 10)
```

```r
print(glmmSpecLearn_rIntercept,correlation=FALSE) 
```

```
## Generalized linear mixed model fit by maximum likelihood (Laplace
##   Approximation) [glmerMod]
##  Family: binomial  ( logit )
## Formula: 
## correct ~ 1 + (1 | id) + (1 | qNameCoursera) + attemptNumFactor *  
##     style
##    Data: multi_try_data_spec_leq_cut
##      AIC      BIC   logLik deviance df.resid 
##   1153.1   1238.7   -558.5   1117.1      841 
## Random effects:
##  Groups        Name        Std.Dev.
##  id            (Intercept) 0.669   
##  qNameCoursera (Intercept) 0.205   
## Number of obs: 859, groups:  id, 101; qNameCoursera, 40
## Fixed Effects:
##                      (Intercept)                 attemptNumFactor2  
##                          0.76381                          -0.00443  
##                         stylen35                         stylen200  
##                          0.96816                          -1.43606  
##                     stylebestFit                    styleaxesScale  
##                         -0.69900                          -0.83150  
##                   styleaxesLabel                      styleoutlier  
##                         -0.11459                          -0.25553  
##                      stylelowess        attemptNumFactor2:stylen35  
##                         -0.79142                          -1.84912  
##      attemptNumFactor2:stylen200    attemptNumFactor2:stylebestFit  
##                          0.81550                           0.30729  
## attemptNumFactor2:styleaxesScale  attemptNumFactor2:styleaxesLabel  
##                         -0.01431                          -0.64938  
##   attemptNumFactor2:styleoutlier     attemptNumFactor2:stylelowess  
##                         -0.73732                           0.04972
```

```r
#Show variance explained by random intercepts
get_var_explained_by_rand_int(glmmSenseLearn_rIntercept)
```

```
##            id.(Intercept) qNameCoursera.(Intercept) 
##                  0.042829                  0.004566
```

```r
get_var_explained_by_rand_int(glmmSpecLearn_rIntercept)
```

```
##            id.(Intercept) qNameCoursera.(Intercept) 
##                   0.11837                   0.01117
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
## [1,]     0.3831 0.2875 0.4886       NA    NA    NA    NA        NA
## [2,]     0.7671 0.6521 0.8527    5.304 2.675 10.52 4.779 1.766e-06
## 
## $n35
##      centerProb  liProb uiProb centerOR  liOR  uiOR zstat  p_value
## [1,]     0.1528 0.07892 0.2752       NA    NA    NA    NA       NA
## [2,]     0.4486 0.31018 0.5955    4.511 1.774 11.47 3.164 0.001558
## 
## $n200
##      centerProb liProb uiProb centerOR   liOR  uiOR  zstat p_value
## [1,]     0.4850 0.3473 0.6250       NA     NA    NA     NA      NA
## [2,]     0.5711 0.4078 0.7202    1.414 0.6038 3.309 0.7975  0.4252
## 
## $bestFit
##      centerProb liProb uiProb centerOR  liOR  uiOR zstat p_value
## [1,]     0.4364 0.2974 0.5861       NA    NA    NA    NA      NA
## [2,]     0.6973 0.5494 0.8132    2.975 1.264 7.003 2.497 0.01253
## 
## $axesScale
##      centerProb liProb uiProb centerOR   liOR  uiOR zstat p_value
## [1,]     0.4807 0.3491 0.6150       NA     NA    NA    NA      NA
## [2,]     0.6449 0.4889 0.7751    1.962 0.8627 4.461 1.608  0.1079
## 
## $axesLabel
##      centerProb liProb uiProb centerOR   liOR  uiOR  zstat p_value
## [1,]     0.4376 0.2989 0.5868       NA     NA    NA     NA      NA
## [2,]     0.4702 0.3099 0.6369    1.141 0.4677 2.782 0.2894  0.7723
## 
## $outlier
##      centerProb liProb uiProb centerOR   liOR  uiOR   zstat p_value
## [1,]     0.6168 0.4627 0.7506       NA     NA    NA      NA      NA
## [2,]     0.6061 0.4586 0.7365   0.9558 0.4106 2.225 -0.1049  0.9164
## 
## $lowess
##      centerProb liProb uiProb centerOR liOR  uiOR  zstat p_value
## [1,]     0.5622 0.4129 0.7010       NA   NA    NA     NA      NA
## [2,]     0.4639 0.3181 0.6162    0.674 0.29 1.566 -0.917  0.3591
```

```r
getCIlearn(glmmSpecLearn_rIntercept)
```

```
## $n100ref
##      centerProb liProb uiProb centerOR   liOR uiOR    zstat p_value
## [1,]     0.6822 0.5669 0.7787       NA     NA   NA       NA      NA
## [2,]     0.6812 0.5587 0.7830   0.9956 0.5216  1.9 -0.01344  0.9893
## 
## $n35
##      centerProb liProb uiProb centerOR    liOR   uiOR  zstat   p_value
## [1,]     0.8497 0.7101 0.9288       NA      NA     NA     NA        NA
## [2,]     0.4696 0.3108 0.6348   0.1567 0.05577 0.4402 -3.517 0.0004367
## 
## $n200
##      centerProb liProb uiProb centerOR   liOR  uiOR zstat p_value
## [1,]     0.3380 0.2060 0.5011       NA     NA    NA    NA      NA
## [2,]     0.5346 0.3766 0.6860     2.25 0.9304 5.443   1.8 0.07186
## 
## $bestFit
##      centerProb liProb uiProb centerOR   liOR  uiOR zstat p_value
## [1,]     0.5162 0.3629 0.6665       NA     NA    NA    NA      NA
## [2,]     0.5909 0.4228 0.7402    1.354 0.5655 3.241  0.68  0.4965
## 
## $axesScale
##      centerProb liProb uiProb centerOR   liOR  uiOR    zstat p_value
## [1,]     0.4831 0.3174 0.6526       NA     NA    NA       NA      NA
## [2,]     0.4784 0.3303 0.6304   0.9814 0.4073 2.365 -0.04177  0.9667
## 
## $axesLabel
##      centerProb liProb uiProb centerOR   liOR  uiOR  zstat p_value
## [1,]     0.6568 0.4987 0.7865       NA     NA    NA     NA      NA
## [2,]     0.4989 0.3491 0.6488   0.5201 0.2233 1.211 -1.516  0.1296
## 
## $outlier
##      centerProb liProb uiProb centerOR   liOR  uiOR  zstat p_value
## [1,]     0.6244 0.4649 0.7608       NA     NA    NA     NA      NA
## [2,]     0.4419 0.2814 0.6155   0.4763 0.1913 1.186 -1.594   0.111
## 
## $lowess
##      centerProb liProb uiProb centerOR   liOR  uiOR  zstat p_value
## [1,]     0.4931 0.3421 0.6454       NA     NA    NA     NA      NA
## [2,]     0.5044 0.3513 0.6567    1.046 0.4537 2.413 0.1062  0.9154
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




