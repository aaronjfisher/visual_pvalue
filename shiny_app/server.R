# A quick and dirty shiny app for illustrating different p-values.
# Uses ad hoc binary search to find the beta coefficient that results
#   in the appropriate p-value for a linear regression.
# User can specify whether a best fit line or a lowess curve should appear.

library(shiny)


trim<-function(x,e,n,percentTrim=.05){ #total trim is twice percentTrim
  num2add<-ceiling(n*percentTrim)*2
  x1<-x[1:(n+num2add)]
  e1<-e[1:(n+num2add)]
  removeInd<-order(x1)[c(1:(num2add/2), (n+1+num2add/2):(n+num2add))]
  x2<-x1[-removeInd]
  e2<-e1[-removeInd]
  return(list(x=x2,e=e2))
}

#Return a regression scenario corresponding to a p-value
mainFun<-function(pval,n,x,e,nulldist,seed4code=1){
  
  #start to get bhat by first getting close with the theory estimate

  t<-qt(pval/2,df=n-2,lower.tail=F)  
  bhat_theory<-t*sd(e)/sqrt(sum((x-mean(x))^2)  ) #sd(e)=true σ, sqrt(n)*sd(x) = Σ[(x-bar(x))^2]
  bhat<-bhat_theory
  y<-x*bhat+e
  bhat_emp<-abs(summary(lm(y~x))$coeff[2,1])
  pval_emp<-summary(lm(y~x))$coeff[2,4]
    
  getp<-function(beta){
    y<-x*beta+e
    p<-summary(lm(y~x))$coeff[2,4]
    return(p)
  }
  pval_emp<-getp(beta=bhat)
  
  #Now get a total of three betas, with at least one corresponding to a pval_emp below pval, and at least one above pval
  bhat1<-bhat_theory
  count<-0 
  while(pval_emp<pval & count<100){
    bhat<-bhat*0.1
    pval_emp<-getp(bhat)  
    count<-count+1
  }
  bhat2<-bhat
  count<-0
  while(pval_emp>pval & count<100 ){
    bhat<-bhat*10
    pval_emp<-getp(bhat)
    count<-count+1
  }
  bhat3<-bhat
  
  #then iteratively narrow down.
  #take the max & min of the three betas (possible replicates)
  #take their mean.
  #Of these three new betas (min, max & mean) keep the 
  #two with pvalues closest to the one you want
  #Set these as the new "min" & "max" and continue to narrow down
  bmin<-min(bhat1,bhat2,bhat3)
  bmax<-max(bhat1,bhat2,bhat3)
  iter<-0
  while(iter<50&!nulldist){
    iter<-iter+1
  
    bmid<-mean(c(bmin,bmax))
    
    pmax<-getp(bmin)
    pmin<-getp(bmax)
    pmid<-getp(bmid)
    
    #print(pval>pmin & pval<pmax)#test
      
    if(pval>pmid){#bmin too small
      bmin<-bmin
      bmax<-bmid
    }
    if(pval<=pmid){#bmax too big
      bmin<-bmid
      bmax<-bmax
    }
    #print(c(iter,pmid,pval))
  }
  
  bmid<-mean(c(bmin,bmax))
  bhat<-bmid
  if(nulldist)bhat<-0
  y<-x*bhat+e
  pval_emp<-summary(lm(y~x))$coeff[2,4]
  
  Rcode<-paste0( #Not including code for trimming, best fit lines etc right now
'## See for yourself!
set.seed(',seed4code,')

X<-rnorm(2000)[1:',n,']
e<-rnorm(2000)[1:',n,']
beta<-',bhat,'
Y<-X*beta+e

plot(X,Y)
summary(lm(Y~X))')
    
  return(list(t=t,e=e,x=x, bhat=bhat, y=y, pval_emp=pval_emp, Rcode=Rcode) )
}


shinyServer(function(input, output) {
  all_xe<-reactive({
    set.seed(input$XEseed)
    x_all<-rnorm(2000)
    e_all<-rnorm(2000)
    list(x=x_all,e=e_all)
  })
  
  pval<-reactive({  10^(input$logp) })
  
  #trim top & bottom 5%
  xe<-reactive({
    if(!input$trimX) xe.pre<-list(x=all_xe()$x[1:input$n],
                                  e=all_xe()$e[1:input$n])
    if(input$trimX) xe.pre<-trim(x=all_xe()$x,e=all_xe()$e,n=input$n)
    xe.pre
  })
  x<-reactive(xe()$x)
  e<-reactive(xe()$e)

  
  data22<-reactive({ mainFun(pval=pval(),n=input$n,x=x(),e=e(),nulldist=input$nulldist,seed=input$XEseed)  })
  
  coefs<-reactive({ lm(data22()$y ~ data22()$x )$coef  }) 
  lowessXY<- reactive({ lowess(data22()$x,data22()$y) })
  
  output$outplot<-renderPlot( {
      plot(data22()$x,data22()$y,xlab='X',ylab='Y',pch=19,col='darkblue') 
      if(input$bestFit) abline( coefs() ,col='darkgreen',lwd=2)
      if(input$lowess) lines(lowessXY(),col='blue',lwd=2)
  }, height=500,width=500)
   
output$detailTable<-renderTable({ 
    tabtab<- data.frame(matmat<-matrix(c(0,round(data22()$bhat,digits=4),round(coefs()[1],digits=4),round(coefs()[2],digits=4) ),byrow=TRUE,ncol=2))
    row.names(tabtab)<-c('Generating Parameters:','Fitted Parameters:')
  colnames(tabtab)<-c('α','β')
  tabtab
  },digits=4)
  
  
  #note, right now we're showing the empirical pval
  #consider changing:
  #options(scipen=...)
  output$pval<-renderText(paste('p-value =',signif(data22()$pval_emp,4)) ) 
  output$test<-renderText(x())
  output$formula<-renderText('Model: Y=α+Xβ+ε')
  output$genParams<-renderText(paste0('Generating Parameters: α=0; β =',round(data22()$bhat,digits=4)) )
  output$fitParams<-renderText(paste0('Fitted Parameters: α=',round(coefs()[1],digits=4), '; β =',round(coefs()[2],digits=4) ))
  output$Rcode<-renderText(data22()$Rcode)
  
})