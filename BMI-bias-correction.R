### Replication code for: A rigorous evaluation of a method to adjust BMI for self-report bias
### Zachary J. Ward, Steven L. Gortmaker

### This code downloads NHANES data from 1999-2018 and evaluates a method to correct BMI for
### quantile-specific self-report bias via cross-validation

# Copyright 2021 Zachary J. Ward
# 
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
# 
# http://www.apache.org/licenses/LICENSE-2.0
# 
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

### Download NHANES Datasets ############################################################################################
library(Hmisc)

###NHANES IV
#Demographics
myVars<-c("seqn","sddsrvyr","riagendr","ridageyr","ridagemn","ridageex","ridreth1","ridexprg","indhhinc",
          "wtint2yr","wtint4yr","wtmec2yr","wtmec4yr","sdmvpsu","sdmvstra")

myVars2<-c("seqn","sddsrvyr","riagendr","ridageyr","ridagemn","ridageex","ridreth1","ridexprg","indhhinc",
           "wtint2yr","wtmec2yr","sdmvpsu","sdmvstra")

myVars3<-c("seqn","sddsrvyr","riagendr","ridageyr","ridagemn","ridageex","ridreth1","ridexprg","indhhin2",
           "wtint2yr","wtmec2yr","sdmvpsu","sdmvstra")

myVars4<-c("seqn","sddsrvyr","riagendr","ridageyr","ridagemn","ridexagm","ridreth1","ridexprg","indhhin2",
           "wtint2yr","wtmec2yr","sdmvpsu","sdmvstra")

demo9900<-subset(sasxport.get('http://wwwn.cdc.gov/Nchs/Nhanes/1999-2000/DEMO.XPT'),select=myVars)
demo0102<-subset(sasxport.get('http://wwwn.cdc.gov/Nchs/Nhanes/2001-2002/DEMO_B.XPT'),select=myVars)
demo0304<-subset(sasxport.get('http://wwwn.cdc.gov/Nchs/Nhanes/2003-2004/DEMO_C.XPT'),select=myVars2)
demo0506<-subset(sasxport.get('http://wwwn.cdc.gov/Nchs/Nhanes/2005-2006/DEMO_D.XPT'),select=myVars2)
demo0708<-subset(sasxport.get('http://wwwn.cdc.gov/Nchs/Nhanes/2007-2008/DEMO_E.XPT'),select=myVars3)
demo0910<-subset(sasxport.get('http://wwwn.cdc.gov/Nchs/Nhanes/2009-2010/DEMO_F.XPT'),select=myVars3)
demo1112<-subset(sasxport.get('http://wwwn.cdc.gov/Nchs/Nhanes/2011-2012/DEMO_G.XPT'),select=myVars4)
demo1314<-subset(sasxport.get('http://wwwn.cdc.gov/Nchs/Nhanes/2013-2014/DEMO_H.XPT'),select=myVars4)
demo1516<-subset(sasxport.get('http://wwwn.cdc.gov/Nchs/Nhanes/2015-2016/DEMO_I.XPT'),select=myVars4)
demo1718<-subset(sasxport.get('http://wwwn.cdc.gov/Nchs/Nhanes/2017-2018/DEMO_J.XPT'),select=myVars4)
#Pool weights
demo9900$wtmec20yr<-demo9900$wtmec4yr*(1/5)
demo0102$wtmec20yr<-demo0102$wtmec4yr*(1/5)
demo0304$wtmec20yr<-demo0304$wtmec2yr*(1/10)
demo0506$wtmec20yr<-demo0506$wtmec2yr*(1/10)
demo0708$wtmec20yr<-demo0708$wtmec2yr*(1/10)
demo0910$wtmec20yr<-demo0910$wtmec2yr*(1/10)
demo1112$wtmec20yr<-demo1112$wtmec2yr*(1/10)
demo1314$wtmec20yr<-demo1314$wtmec2yr*(1/10)
demo1516$wtmec20yr<-demo1516$wtmec2yr*(1/10)
demo1718$wtmec20yr<-demo1718$wtmec2yr*(1/10)
#Rename age-months at exam and household income
demo9900$ageexm<-demo9900$ridageex; demo9900$income<-demo9900$indhhinc
demo0102$ageexm<-demo0102$ridageex; demo0102$income<-demo0102$indhhinc
demo0304$ageexm<-demo0304$ridageex; demo0304$income<-demo0304$indhhinc
demo0506$ageexm<-demo0506$ridageex; demo0506$income<-demo0506$indhhinc
demo0708$ageexm<-demo0708$ridageex; demo0708$income<-demo0708$indhhin2
demo0910$ageexm<-demo0910$ridageex; demo0910$income<-demo0910$indhhin2
demo1112$ageexm<-demo1112$ridexagm; demo1112$income<-demo1112$indhhin2
demo1314$ageexm<-demo1314$ridexagm; demo1314$income<-demo1314$indhhin2
demo1516$ageexm<-demo1516$ridexagm; demo1516$income<-demo1516$indhhin2
demo1718$ageexm<-demo1718$ridexagm; demo1718$income<-demo1718$indhhin2

#Merge
myVars<-c("seqn","sddsrvyr","riagendr","ridageyr","ridagemn","ageexm","ridreth1","ridexprg","income",
          "wtmec20yr","sdmvpsu","sdmvstra")
demo<-subset(demo9900,select=myVars)
demo<-rbind(demo,subset(demo0102,select=myVars))
demo<-rbind(demo,subset(demo0304,select=myVars))
demo<-rbind(demo,subset(demo0506,select=myVars))
demo<-rbind(demo,subset(demo0708,select=myVars))
demo<-rbind(demo,subset(demo0910,select=myVars))
demo<-rbind(demo,subset(demo1112,select=myVars))
demo<-rbind(demo,subset(demo1314,select=myVars))
demo<-rbind(demo,subset(demo1516,select=myVars))
demo<-rbind(demo,subset(demo1718,select=myVars))

#Body Measures
myVars<-c("seqn","bmxbmi")
bmi<-subset(sasxport.get('http://wwwn.cdc.gov/Nchs/Nhanes/1999-2000/BMX.XPT'),select=myVars) #99-00
bmi<-rbind(bmi,subset(sasxport.get('http://wwwn.cdc.gov/Nchs/Nhanes/2001-2002/BMX_B.XPT'),select=myVars)) #01-02
bmi<-rbind(bmi,subset(sasxport.get('http://wwwn.cdc.gov/Nchs/Nhanes/2003-2004/BMX_C.XPT'),select=myVars)) #03-04
bmi<-rbind(bmi,subset(sasxport.get('http://wwwn.cdc.gov/Nchs/Nhanes/2005-2006/BMX_D.XPT'),select=myVars)) #05-06
bmi<-rbind(bmi,subset(sasxport.get('http://wwwn.cdc.gov/Nchs/Nhanes/2007-2008/BMX_E.XPT'),select=myVars)) #07-08
bmi<-rbind(bmi,subset(sasxport.get('http://wwwn.cdc.gov/Nchs/Nhanes/2009-2010/BMX_F.XPT'),select=myVars)) #09-10
bmi<-rbind(bmi,subset(sasxport.get('http://wwwn.cdc.gov/Nchs/Nhanes/2011-2012/BMX_G.XPT'),select=myVars)) #11-12
bmi<-rbind(bmi,subset(sasxport.get('http://wwwn.cdc.gov/Nchs/Nhanes/2013-2014/BMX_H.XPT'),select=myVars)) #13-14
bmi<-rbind(bmi,subset(sasxport.get('http://wwwn.cdc.gov/Nchs/Nhanes/2015-2016/BMX_I.XPT'),select=myVars)) #15-16
bmi<-rbind(bmi,subset(sasxport.get('http://wwwn.cdc.gov/Nchs/Nhanes/2017-2018/BMX_J.XPT'),select=myVars)) #17-18

#Self-reported body measures
myVars<-c("seqn","whd010","whd020")
self<-subset(sasxport.get('http://wwwn.cdc.gov/Nchs/Nhanes/1999-2000/WHQ.XPT'),select=myVars) #99-00
self<-rbind(self,subset(sasxport.get('http://wwwn.cdc.gov/Nchs/Nhanes/2001-2002/WHQ_B.XPT'),select=myVars)) #01-02
self<-rbind(self,subset(sasxport.get('http://wwwn.cdc.gov/Nchs/Nhanes/2003-2004/WHQ_C.XPT'),select=myVars)) #03-04
self<-rbind(self,subset(sasxport.get('http://wwwn.cdc.gov/Nchs/Nhanes/2005-2006/WHQ_D.XPT'),select=myVars)) #05-06
self<-rbind(self,subset(sasxport.get('http://wwwn.cdc.gov/Nchs/Nhanes/2007-2008/WHQ_E.XPT'),select=myVars)) #07-08
self<-rbind(self,subset(sasxport.get('http://wwwn.cdc.gov/Nchs/Nhanes/2009-2010/WHQ_F.XPT'),select=myVars)) #09-10
self<-rbind(self,subset(sasxport.get('http://wwwn.cdc.gov/Nchs/Nhanes/2011-2012/WHQ_G.XPT'),select=myVars)) #11-12
self<-rbind(self,subset(sasxport.get('http://wwwn.cdc.gov/Nchs/Nhanes/2013-2014/WHQ_H.XPT'),select=myVars)) #13-14
self<-rbind(self,subset(sasxport.get('http://wwwn.cdc.gov/Nchs/Nhanes/2015-2016/WHQ_I.XPT'),select=myVars)) #15-16
self<-rbind(self,subset(sasxport.get('http://wwwn.cdc.gov/Nchs/Nhanes/2017-2018/WHQ_J.XPT'),select=myVars)) #17-18

#Smoking history
myVars<-c("seqn","smq020","smq040")
smoke<-subset(sasxport.get('http://wwwn.cdc.gov/Nchs/Nhanes/1999-2000/SMQ.XPT'),select=myVars) #99-00
smoke<-rbind(smoke,subset(sasxport.get('http://wwwn.cdc.gov/Nchs/Nhanes/2001-2002/SMQ_B.XPT'),select=myVars)) #01-02
smoke<-rbind(smoke,subset(sasxport.get('http://wwwn.cdc.gov/Nchs/Nhanes/2003-2004/SMQ_C.XPT'),select=myVars)) #03-04
smoke<-rbind(smoke,subset(sasxport.get('http://wwwn.cdc.gov/Nchs/Nhanes/2005-2006/SMQ_D.XPT'),select=myVars)) #05-06
smoke<-rbind(smoke,subset(sasxport.get('http://wwwn.cdc.gov/Nchs/Nhanes/2007-2008/SMQ_E.XPT'),select=myVars)) #07-08
smoke<-rbind(smoke,subset(sasxport.get('http://wwwn.cdc.gov/Nchs/Nhanes/2009-2010/SMQ_F.XPT'),select=myVars)) #09-10
smoke<-rbind(smoke,subset(sasxport.get('http://wwwn.cdc.gov/Nchs/Nhanes/2011-2012/SMQ_G.XPT'),select=myVars)) #11-12
smoke<-rbind(smoke,subset(sasxport.get('http://wwwn.cdc.gov/Nchs/Nhanes/2013-2014/SMQ_H.XPT'),select=myVars)) #13-14
smoke<-rbind(smoke,subset(sasxport.get('http://wwwn.cdc.gov/Nchs/Nhanes/2015-2016/SMQ_I.XPT'),select=myVars)) #15-16
smoke<-rbind(smoke,subset(sasxport.get('http://wwwn.cdc.gov/Nchs/Nhanes/2017-2018/SMQ_J.XPT'),select=myVars)) #17-18
#re-code smoking status
smoke$smoker<-NA
smoke$smoker[smoke$smq020==2]<-0 #never smoker (responded "no" to smoked at least 100 cigarettes in life)
smoke$smoker[smoke$smq040<3]<-1 #current smoker (currently smokes "every day" or "some days")
smoke$smoker[smoke$smq040==3]<-2 #former smoker (currently smokes "not at all")

#Merge variables
nhanes<-merge(demo,bmi,by="seqn")
nhanes<-merge(nhanes,self,by="seqn")
nhanes<-merge(nhanes,smoke,by="seqn")

#CLEAN
nhanes$ageexm[is.na(nhanes$ageexm)]<-(nhanes$ridageyr[is.na(nhanes$ageexm)]*12)+6 #Impute exam age in months if missing - use midpoint
nhanes$age<-nhanes$ageexm/12
nhanes<-subset(nhanes,age>=18) #Adults 18+
nhanes<-subset(nhanes,ridexprg>1 | is.na(ridexprg)) #Remove if pregnant
nhanes<-subset(nhanes,income<77) #Has reported household income
nhanes<-subset(nhanes,is.na(smoker)==F) #Has reported smoking status
nhanes<-subset(nhanes,bmxbmi>0) #Has measured BMI
nhanes<-subset(nhanes,whd010<1000) #Has Self-reported height
nhanes<-subset(nhanes,whd020<2000) #Has Self-reported weight
#calculate self-reported BMI
nhanes$kg<-nhanes$whd020*0.453592 #lbs to kg
nhanes$m<-nhanes$whd010*0.0254 #in to m
nhanes$bmiSelf<-nhanes$kg/(nhanes$m^2)
#calculate income groups
nhanes$incomeGroup<-NA
nhanes$incomeGroup[nhanes$income<5 | nhanes$income==13]<-0 #<20k
nhanes$incomeGroup[(nhanes$income>=5 & nhanes$income<9) | nhanes$income==12]<-1 #20k-<55k, include 20k+ in this category
nhanes$incomeGroup[(nhanes$income>=9 & nhanes$income<=11) | nhanes$income>=14]<-2 #55k+

### Evaluate method for adjusting self-reported BMI ##########################################################################

library(splines)

#adjust BMI in testing set based on distribution in training set
adjustBMI<-function(curTrain,curTest){
  #estimate quantiles in both sets
  quants<-rep(1:99)*0.01
  quantTrain<-wtd.quantile(curTrain$bmxbmi,weights=curTrain$wtmec20yr,quants,normwt=TRUE) #training: measured
  quantTest<-wtd.quantile(curTest$bmiSelf,weights=curTest$wtmec20yr,quants,normwt=TRUE) #testing: self-reported
  quantDiff<-quantTrain-quantTest #calculate difference by quantile
  
  #fit cubic splines with knots at each quintile, then predict by quantile to smooth differences
  qKnots<-c(0,0.2,0.4,0.6,0.8,1) 
  fit<-lm(quantDiff ~ bs(quants,knots = qKnots))
  pred<-predict(fit)
  
  #Adjust testing set by quantile
  curTest$bmiRank<-wtd.rank(curTest$bmiSelf,weights=curTest$wtmec20yr,normwt=TRUE) #calculate weighted rank
  curTest$bmiRank[curTest$bmiSelf==min(curTest$bmiSelf)]<-min(curTest$bmiRank,na.rm=T) #min rank
  curTest$bmiRank[curTest$bmiSelf==max(curTest$bmiSelf)]<-max(curTest$bmiRank,na.rm=T) #max rank
  curTest$bmiRank<-curTest$bmiRank/max(curTest$bmiRank) #renormalize rank
  curTest$bmiRank<-pmin(curTest$bmiRank,0.99) #ceiling of 99-percentile
  curTest$bmiAdjust<-predict(fit, newdata=data.frame(quants=curTest$bmiRank)) #calculate adjustment amount
  curTest$newBMI<-curTest$bmiSelf+curTest$bmiAdjust+rnorm(nrow(curTest),0,0.5) #smooth adjusted BMI with zero-mean noise
  
  return(curTest)
}

#calculate differences between adjusted and measured BMI
calcErr<-function(curTest){
  df_bmi_adj<-data.frame(matrix(nrow=1,ncol=16))
  colnames(df_bmi_adj)<-c("all","m","f","white","black","hisp","other","inc1","inc2","inc3","smoke_Never","smoke_Cur","smoke_Ex","age18_39","age40_64","age65")
  df_prev_adj<-df_bmi_adj; df_bmi_self<-df_bmi_adj; df_prev_self<-df_bmi_adj;
  
  #calculate differences between adjusted and measured BMI
  curTest$err<-curTest$newBMI-curTest$bmxbmi; curTest$errSelf<-curTest$bmiSelf-curTest$bmxbmi;
  #calculate mean difference by subgroup
  df_bmi_adj$all<-weighted.mean(curTest$err,curTest$wtmec20yr); df_bmi_self$all<-weighted.mean(curTest$errSelf,curTest$wtmec20yr);
  #sex
  rows<-curTest$riagendr==1; df_bmi_adj$m<-weighted.mean(curTest$err[rows],curTest$wtmec20yr[rows]); df_bmi_self$m<-weighted.mean(curTest$errSelf[rows],curTest$wtmec20yr[rows])
  rows<-curTest$riagendr==2; df_bmi_adj$f<-weighted.mean(curTest$err[rows],curTest$wtmec20yr[rows]); df_bmi_self$f<-weighted.mean(curTest$errSelf[rows],curTest$wtmec20yr[rows])
  #race/ethnicity
  rows<-curTest$ridreth1==3; df_bmi_adj$white<-weighted.mean(curTest$err[rows],curTest$wtmec20yr[rows]); df_bmi_self$white<-weighted.mean(curTest$errSelf[rows],curTest$wtmec20yr[rows])
  rows<-curTest$ridreth1==4; df_bmi_adj$black<-weighted.mean(curTest$err[rows],curTest$wtmec20yr[rows]); df_bmi_self$black<-weighted.mean(curTest$errSelf[rows],curTest$wtmec20yr[rows])
  rows<-curTest$ridreth1<3; df_bmi_adj$hisp<-weighted.mean(curTest$err[rows],curTest$wtmec20yr[rows]); df_bmi_self$hisp<-weighted.mean(curTest$errSelf[rows],curTest$wtmec20yr[rows])
  rows<-curTest$ridreth1==5; df_bmi_adj$other<-weighted.mean(curTest$err[rows],curTest$wtmec20yr[rows]); df_bmi_self$other<-weighted.mean(curTest$errSelf[rows],curTest$wtmec20yr[rows])
  #income
  rows<-curTest$incomeGroup==0; df_bmi_adj$inc1<-weighted.mean(curTest$err[rows],curTest$wtmec20yr[rows]); df_bmi_self$inc1<-weighted.mean(curTest$errSelf[rows],curTest$wtmec20yr[rows])
  rows<-curTest$incomeGroup==1; df_bmi_adj$inc2<-weighted.mean(curTest$err[rows],curTest$wtmec20yr[rows]); df_bmi_self$inc2<-weighted.mean(curTest$errSelf[rows],curTest$wtmec20yr[rows])
  rows<-curTest$incomeGroup==2; df_bmi_adj$inc3<-weighted.mean(curTest$err[rows],curTest$wtmec20yr[rows]); df_bmi_self$inc3<-weighted.mean(curTest$errSelf[rows],curTest$wtmec20yr[rows])
  #smoking
  rows<-curTest$smoker==0; df_bmi_adj$smoke_Never<-weighted.mean(curTest$err[rows],curTest$wtmec20yr[rows]); df_bmi_self$smoke_Never<-weighted.mean(curTest$errSelf[rows],curTest$wtmec20yr[rows])
  rows<-curTest$smoker==1; df_bmi_adj$smoke_Cur<-weighted.mean(curTest$err[rows],curTest$wtmec20yr[rows]); df_bmi_self$smoke_Cur<-weighted.mean(curTest$errSelf[rows],curTest$wtmec20yr[rows])
  rows<-curTest$smoker==2; df_bmi_adj$smoke_Ex<-weighted.mean(curTest$err[rows],curTest$wtmec20yr[rows]); df_bmi_self$smoke_Ex<-weighted.mean(curTest$errSelf[rows],curTest$wtmec20yr[rows])
  #age group
  rows<-(curTest$age>=18 & curTest$age<40); df_bmi_adj$age18_39<-weighted.mean(curTest$err[rows],curTest$wtmec20yr[rows]); df_bmi_self$age18_39<-weighted.mean(curTest$errSelf[rows],curTest$wtmec20yr[rows])
  rows<-(curTest$age>=40 & curTest$age<65); df_bmi_adj$age40_64<-weighted.mean(curTest$err[rows],curTest$wtmec20yr[rows]); df_bmi_self$age40_64<-weighted.mean(curTest$errSelf[rows],curTest$wtmec20yr[rows])
  rows<-(curTest$age>=65); df_bmi_adj$age65<-weighted.mean(curTest$err[rows],curTest$wtmec20yr[rows]); df_bmi_self$age65<-weighted.mean(curTest$errSelf[rows],curTest$wtmec20yr[rows])
  
  
  #calculate differences in prevalence
  curTest$prev<-ifelse(curTest$bmxbmi>=30,1,0)
  curTest$newPrev<-ifelse(curTest$newBMI>=30,1,0)
  curTest$selfPrev<-ifelse(curTest$bmiSelf>=30,1,0)
  #calculate mean difference by subgroup
  df_prev_adj$all<-weighted.mean(curTest$newPrev,curTest$wtmec20yr)-weighted.mean(curTest$prev,curTest$wtmec20yr)
  df_prev_self$all<-weighted.mean(curTest$selfPrev,curTest$wtmec20yr)-weighted.mean(curTest$prev,curTest$wtmec20yr)
  #sex
  rows<-curTest$riagendr==1; df_prev_adj$m<-weighted.mean(curTest$newPrev[rows],curTest$wtmec20yr[rows])-weighted.mean(curTest$prev[rows],curTest$wtmec20yr[rows])
  df_prev_self$m<-weighted.mean(curTest$selfPrev[rows],curTest$wtmec20yr[rows])-weighted.mean(curTest$prev[rows],curTest$wtmec20yr[rows])
  rows<-curTest$riagendr==2; df_prev_adj$f<-weighted.mean(curTest$newPrev[rows],curTest$wtmec20yr[rows])-weighted.mean(curTest$prev[rows],curTest$wtmec20yr[rows])
  df_prev_self$f<-weighted.mean(curTest$selfPrev[rows],curTest$wtmec20yr[rows])-weighted.mean(curTest$prev[rows],curTest$wtmec20yr[rows])
  #race/ethnicity
  rows<-curTest$ridreth1==3; df_prev_adj$white<-weighted.mean(curTest$newPrev[rows],curTest$wtmec20yr[rows])-weighted.mean(curTest$prev[rows],curTest$wtmec20yr[rows])
  df_prev_self$white<-weighted.mean(curTest$selfPrev[rows],curTest$wtmec20yr[rows])-weighted.mean(curTest$prev[rows],curTest$wtmec20yr[rows])
  rows<-curTest$ridreth1==4; df_prev_adj$black<-weighted.mean(curTest$newPrev[rows],curTest$wtmec20yr[rows])-weighted.mean(curTest$prev[rows],curTest$wtmec20yr[rows])
  df_prev_self$black<-weighted.mean(curTest$selfPrev[rows],curTest$wtmec20yr[rows])-weighted.mean(curTest$prev[rows],curTest$wtmec20yr[rows])
  rows<-curTest$ridreth1<3; df_prev_adj$hisp<-weighted.mean(curTest$newPrev[rows],curTest$wtmec20yr[rows])-weighted.mean(curTest$prev[rows],curTest$wtmec20yr[rows])
  df_prev_self$hisp<-weighted.mean(curTest$selfPrev[rows],curTest$wtmec20yr[rows])-weighted.mean(curTest$prev[rows],curTest$wtmec20yr[rows])
  rows<-curTest$ridreth1==5; df_prev_adj$other<-weighted.mean(curTest$newPrev[rows],curTest$wtmec20yr[rows])-weighted.mean(curTest$prev[rows],curTest$wtmec20yr[rows])
  df_prev_self$other<-weighted.mean(curTest$selfPrev[rows],curTest$wtmec20yr[rows])-weighted.mean(curTest$prev[rows],curTest$wtmec20yr[rows])
  #income
  rows<-curTest$incomeGroup==0; df_prev_adj$inc1<-weighted.mean(curTest$newPrev[rows],curTest$wtmec20yr[rows])-weighted.mean(curTest$prev[rows],curTest$wtmec20yr[rows])
  df_prev_self$inc1<-weighted.mean(curTest$selfPrev[rows],curTest$wtmec20yr[rows])-weighted.mean(curTest$prev[rows],curTest$wtmec20yr[rows])
  rows<-curTest$incomeGroup==1; df_prev_adj$inc2<-weighted.mean(curTest$newPrev[rows],curTest$wtmec20yr[rows])-weighted.mean(curTest$prev[rows],curTest$wtmec20yr[rows])
  df_prev_self$inc2<-weighted.mean(curTest$selfPrev[rows],curTest$wtmec20yr[rows])-weighted.mean(curTest$prev[rows],curTest$wtmec20yr[rows])
  rows<-curTest$incomeGroup==2; df_prev_adj$inc3<-weighted.mean(curTest$newPrev[rows],curTest$wtmec20yr[rows])-weighted.mean(curTest$prev[rows],curTest$wtmec20yr[rows])
  df_prev_self$inc3<-weighted.mean(curTest$selfPrev[rows],curTest$wtmec20yr[rows])-weighted.mean(curTest$prev[rows],curTest$wtmec20yr[rows])
  #smoking
  rows<-curTest$smoker==0; df_prev_adj$smoke_Never<-weighted.mean(curTest$newPrev[rows],curTest$wtmec20yr[rows])-weighted.mean(curTest$prev[rows],curTest$wtmec20yr[rows])
  df_prev_self$smoke_Never<-weighted.mean(curTest$selfPrev[rows],curTest$wtmec20yr[rows])-weighted.mean(curTest$prev[rows],curTest$wtmec20yr[rows])
  rows<-curTest$smoker==1; df_prev_adj$smoke_Cur<-weighted.mean(curTest$newPrev[rows],curTest$wtmec20yr[rows])-weighted.mean(curTest$prev[rows],curTest$wtmec20yr[rows])
  df_prev_self$smoke_Cur<-weighted.mean(curTest$selfPrev[rows],curTest$wtmec20yr[rows])-weighted.mean(curTest$prev[rows],curTest$wtmec20yr[rows])
  rows<-curTest$smoker==2; df_prev_adj$smoke_Ex<-weighted.mean(curTest$newPrev[rows],curTest$wtmec20yr[rows])-weighted.mean(curTest$prev[rows],curTest$wtmec20yr[rows])
  df_prev_self$smoke_Ex<-weighted.mean(curTest$selfPrev[rows],curTest$wtmec20yr[rows])-weighted.mean(curTest$prev[rows],curTest$wtmec20yr[rows])
  #age group
  rows<-(curTest$age>=18 & curTest$age<40); df_prev_adj$age18_39<-weighted.mean(curTest$newPrev[rows],curTest$wtmec20yr[rows])-weighted.mean(curTest$prev[rows],curTest$wtmec20yr[rows])
  df_prev_self$age18_39<-weighted.mean(curTest$selfPrev[rows],curTest$wtmec20yr[rows])-weighted.mean(curTest$prev[rows],curTest$wtmec20yr[rows])
  rows<-(curTest$age>=40 & curTest$age<65); df_prev_adj$age40_64<-weighted.mean(curTest$newPrev[rows],curTest$wtmec20yr[rows])-weighted.mean(curTest$prev[rows],curTest$wtmec20yr[rows])
  df_prev_self$age40_64<-weighted.mean(curTest$selfPrev[rows],curTest$wtmec20yr[rows])-weighted.mean(curTest$prev[rows],curTest$wtmec20yr[rows])
  rows<-(curTest$age>=65); df_prev_adj$age65<-weighted.mean(curTest$newPrev[rows],curTest$wtmec20yr[rows])-weighted.mean(curTest$prev[rows],curTest$wtmec20yr[rows])
  df_prev_self$age65<-weighted.mean(curTest$selfPrev[rows],curTest$wtmec20yr[rows])-weighted.mean(curTest$prev[rows],curTest$wtmec20yr[rows])
  
  
  return(list(df_bmi_adj,df_prev_adj,df_bmi_self,df_prev_self))
}


#adjust BMI by sex
simAdjustment_Sex<-function(iteration){
  set.seed(iteration)
  
  #split into training/testing sets
  N<-nrow(nhanes)
  seqnTrain<-sample(nhanes$seqn,N/2,replace=F)
  curTrain<-subset(nhanes,seqn %in% seqnTrain)
  curTest<-subset(nhanes,!(seqn %in% seqnTrain))
  
  #estimate quantiles by sex in both sets
  adjM<-adjustBMI(subset(curTrain,riagendr==1),subset(curTest,riagendr==1))
  adjF<-adjustBMI(subset(curTrain,riagendr==2),subset(curTest,riagendr==2))
  adj<-rbind(adjM,adjF) #re-combine
  
  #compare adjusted with measured BMI
  errs<-calcErr(adj)
  return(errs)
}

#adjust BMI by sex + age
simAdjustment_SexAge<-function(iteration){
  set.seed(iteration)
  
  #split into training/testing sets
  N<-nrow(nhanes)
  seqnTrain<-sample(nhanes$seqn,N/2,replace=F)
  curTrain<-subset(nhanes,seqn %in% seqnTrain)
  curTest<-subset(nhanes,!(seqn %in% seqnTrain))
  
  #estimate quantiles by sex and age in both sets
  #18-34
  adjM<-adjustBMI(subset(curTrain,riagendr==1 & age>=18 & age<35),subset(curTest,riagendr==1 & age>=18 & age<35))
  adjF<-adjustBMI(subset(curTrain,riagendr==2 & age>=18 & age<35),subset(curTest,riagendr==2 & age>=18 & age<35))
  adj<-rbind(adjM,adjF) #re-combine
  #35-49
  adj<-rbind(adj,adjustBMI(subset(curTrain,riagendr==1 & age>=35 & age<50),subset(curTest,riagendr==1 & age>=35 & age<50)))
  adj<-rbind(adj,adjustBMI(subset(curTrain,riagendr==2 & age>=35 & age<50),subset(curTest,riagendr==2 & age>=35 & age<50)))
  #50-64
  adj<-rbind(adj,adjustBMI(subset(curTrain,riagendr==1 & age>=50 & age<65),subset(curTest,riagendr==1 & age>=50 & age<65)))
  adj<-rbind(adj,adjustBMI(subset(curTrain,riagendr==2 & age>=50 & age<65),subset(curTest,riagendr==2 & age>=50 & age<65)))
  #65-79
  adj<-rbind(adj,adjustBMI(subset(curTrain,riagendr==1 & age>=65 & age<80),subset(curTest,riagendr==1 & age>=65 & age<80)))
  adj<-rbind(adj,adjustBMI(subset(curTrain,riagendr==2 & age>=65 & age<80),subset(curTest,riagendr==2 & age>=65 & age<80)))
  #80+
  adj<-rbind(adj,adjustBMI(subset(curTrain,riagendr==1 & age>=80),subset(curTest,riagendr==1 & age>=80)))
  adj<-rbind(adj,adjustBMI(subset(curTrain,riagendr==2 & age>=80),subset(curTest,riagendr==2 & age>=80)))
  
  #compare adjusted with measured BMI
  errs<-calcErr(adj)
  return(errs)
}

#initialize result dataframes
df_bmi_adj<-data.frame(matrix(nrow=0,ncol=16))
colnames(df_bmi_adj)<-c("all","m","f","white","black","hisp","other","inc1","inc2","inc3","smoke_Never","smoke_Cur","smoke_Ex","age18_39","age40_64","age65")
df_prev_adj<-df_bmi_adj; df_bmi_self<-df_bmi_adj; df_prev_self<-df_bmi_adj; df_bmi_adjAge<-df_bmi_adj; df_prev_adjAge<-df_bmi_adj

#nhanes$wtmec20yr<-1.0 #set uniform weights for unweighted analysis

#perform 1000 cross-validation iterations
for(i in 1:1000){
  print(i)
  curErrs<-simAdjustment_Sex(i)
  df_bmi_adj<-rbind(df_bmi_adj,curErrs[[1]])
  df_prev_adj<-rbind(df_prev_adj,curErrs[[2]])
  df_bmi_self<-rbind(df_bmi_self,curErrs[[3]])
  df_prev_self<-rbind(df_prev_self,curErrs[[4]])
  #adjust by age
  curErrs<-simAdjustment_SexAge(i)
  df_bmi_adjAge<-rbind(df_bmi_adjAge,curErrs[[1]])
  df_prev_adjAge<-rbind(df_prev_adjAge,curErrs[[2]])
}

#calculate mean and 95% CIs for errors
bmi_adj_mean<-c(); bmi_adj_lb<-c(); bmi_adj_ub<-c()
prev_adj_mean<-c(); prev_adj_lb<-c(); prev_adj_ub<-c()
bmi_self_mean<-c(); bmi_self_lb<-c(); bmi_self_ub<-c()
prev_self_mean<-c(); prev_self_lb<-c(); prev_self_ub<-c()
bmi_adjAge_mean<-c(); bmi_adjAge_lb<-c(); bmi_adjAge_ub<-c()
prev_adjAge_mean<-c(); prev_adjAge_lb<-c(); prev_adjAge_ub<-c()

for(c in 1:16){
  #bmi - adj
  bmi_adj_mean[c]<-mean(df_bmi_adj[,c])
  bounds<-quantile(df_bmi_adj[,c],c(0.025,0.975))
  bmi_adj_lb[c]<-bounds[[1]];  bmi_adj_ub[c]<-bounds[[2]]
  #prev - adj
  prev_adj_mean[c]<-mean(df_prev_adj[,c])*100
  bounds<-quantile(df_prev_adj[,c],c(0.025,0.975))
  prev_adj_lb[c]<-bounds[[1]]*100; prev_adj_ub[c]<-bounds[[2]]*100
  #bmi - self
  bmi_self_mean[c]<-mean(df_bmi_self[,c])
  bounds<-quantile(df_bmi_self[,c],c(0.025,0.975))
  bmi_self_lb[c]<-bounds[[1]];  bmi_self_ub[c]<-bounds[[2]]
  #prev - self
  prev_self_mean[c]<-mean(df_prev_self[,c])*100
  bounds<-quantile(df_prev_self[,c],c(0.025,0.975))
  prev_self_lb[c]<-bounds[[1]]*100; prev_self_ub[c]<-bounds[[2]]*100
  #bmi - adj age
  bmi_adjAge_mean[c]<-mean(df_bmi_adjAge[,c])
  bounds<-quantile(df_bmi_adjAge[,c],c(0.025,0.975))
  bmi_adjAge_lb[c]<-bounds[[1]];  bmi_adjAge_ub[c]<-bounds[[2]]
  #prev - adj age
  prev_adjAge_mean[c]<-mean(df_prev_adjAge[,c])*100
  bounds<-quantile(df_prev_adjAge[,c],c(0.025,0.975))
  prev_adjAge_lb[c]<-bounds[[1]]*100; prev_adjAge_ub[c]<-bounds[[2]]*100
}

#plot results and save as a PDF
setwd() #set working directory  <------------- CHANGE ME!
pdf("Figure.pdf",width=8,height=6)

lbls<-c("Overall","Males","Females","White (non-Hispanic)","Black (non-Hispanic)","Hispanic","Other (non-Hispanic)","Income: <20k","Income: 20-<55k","Income: 55k+",
        "Never Smoker","Current Smoker","Former Smoker","Age: 18-39","Age: 40-64","Age 65+")
yy<-(16:1)
par(mfrow=c(1,2),mar=c(5,7,2,1))
plot(bmi_adj_mean,yy,xlim=c(-1.5,1.5),xlab="Mean Error (BMI)",yaxt='n',ylab="",type='n',cex.axis=0.7, cex.lab=0.7)
title(main="Mean BMI Error",cex.main=0.7)
abline(v=0,lwd=0.5)
axis(2,at=16:1,labels=lbls,las=2,cex.axis=0.7)
points(bmi_self_mean,yy+0.2,col='red',pch=16,cex=0.6)
points(bmi_adj_mean,yy,col='blue',pch=16,cex=0.6)
points(bmi_adjAge_mean,yy-0.2,col='darkgreen',pch=16,cex=0.6)
for(i in 1:16){
  lines(c(bmi_self_lb[i],bmi_self_ub[i]),c(yy[i]+0.2,yy[i]+0.2),col='red')
  lines(c(bmi_adj_lb[i],bmi_adj_ub[i]),c(yy[i],yy[i]),col='blue')
  lines(c(bmi_adjAge_lb[i],bmi_adjAge_ub[i]),c(yy[i]-0.2,yy[i]-0.2),col='darkgreen')
}
legend('topright',c("Self-Reported","Adjusted: Sex","Adjusted: Sex+Age"),col=c('red','blue','darkgreen'),lty=c(1,1,1),inset=0.01,cex=0.45)

plot(bmi_adj_mean,yy,xlim=c(-8,8),xlab="Mean Error (Obesity Prevalence, %)",yaxt='n',ylab="",type='n',cex.axis=0.7, cex.lab=0.7)
title(main="Mean Obesity Prevalence Error",cex.main=0.7)
abline(v=0,lwd=0.5)
axis(2,at=16:1,labels=lbls,las=2,cex.axis=0.7)
points(prev_self_mean,yy+0.2,col='red',pch=16,cex=0.6)
points(prev_adj_mean,yy,col='blue',pch=16,cex=0.6)
points(prev_adjAge_mean,yy-0.2,col='darkgreen',pch=16,cex=0.6)
for(i in 1:16){
  lines(c(prev_self_lb[i],prev_self_ub[i]),c(yy[i]+0.2,yy[i]+0.2),col='red')
  lines(c(prev_adj_lb[i],prev_adj_ub[i]),c(yy[i],yy[i]),col='blue')
  lines(c(prev_adjAge_lb[i],prev_adjAge_ub[i]),c(yy[i]-0.2,yy[i]-0.2),col='darkgreen')
}
legend('topright',c("Self-Reported","Adjusted: Sex","Adjusted: Sex+Age"),col=c('red','blue','darkgreen'),lty=c(1,1,1),inset=0.01,cex=0.45)

dev.off()
