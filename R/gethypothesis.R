#'Plot graphs to do Exploratory Data Analysis
#'
#'Provide Dataframe along with the type of hypothesis test need to be done.
#'(dataframe,test="onetail",alpha=0.95,filenmae="fileone"
#'@param x= dataframe
#'@param vect= type of test needed to be done
#'@param dir= "provide valid file name to save test results" 
#'@return test results will be saved in separate sheets of excel file
#'@author Onkar, Gnaneeswar
#' @title Hypothesis Testing 
#'
#' @description Does hypothesis testing and prints out the results and store them in excel file in your directory
#'
#' @param (dataframe,"function name",significance level,"name of file")
#'
#' @return NULL
#'
#' @examples stats_get_git(fram,"chisq",0.95,"file4") stats_get(fram,"onetail",0.95,"file4") stats_get(fram,"twotail",0.95,"file4") stats_get(fram,"anova",0.95,"file4")
#'
#' @export callhypothesis

gethypothesis_all<-function(df,test,alpha,filename)
{
  library("dplyr")
  library("stringi")
  library("DescTools")
  library("xlsx")
  
  col<-names(df)
  
  options(digits=5)
  
  nums<-c()
  labels<-c()
  binary<-c()
  anova_self<-c()
  anova_master<-c()
  T_master<-c()
  T_master.sub<-c()
  t_count<-1
  t_count.sub<-1
  aov_count<-1
  chi_master<-c()
  chi_count<-1
  result<-c()
  pval_df<-c()
  pval_df.sub<-c()
  result.sub<-c()
  claim<-c()
  claim.sub<-c()
  statget<-c()
  statget.sub<-c()
  
  
  nums<-names(select_if(df, is.numeric))
  labels<-names(select_if(df, is.factor))
  
  for (j in labels)
    
  { 
    if(nrow(unique(df[j]))=="2")
      
    {append(binary,j)}
  }
  
  print(nums)
  print(labels)
  
  if((test=="anova"))
  { 
    print("entered")
    for(i in nums)
    {
      
      for ( j in labels)
      { 
        if(nrow(unique(df[j]))>2)
        { 
          
          #cat("\n")
          #print(c(names(df[i]),"vs",names(df[j])))
          a<-aov(unlist(df[i])~unlist(df[j]))
          pval<-round(unlist(summary(a))["Pr(>F)1"],3)
          stattemp<-unlist(summary(a))["F value1"]
          
          temp<-c(names(df[i])," vs ",names(df[j]))
          
          if(pval<(1-alpha))
            
          {
            result[aov_count]<-c(" Means are different ")}
          
          else
          { 
            result[aov_count]<-c(" Means are not different ")}
          
          pval_df[aov_count]<-pval
          statget[aov_count]<-stattemp
          
          anova_master[aov_count]<- StrTrim(StrAlign(paste(temp, collapse = ''),sep = "\\l"),method="both")
          aov_count<-(aov_count+1)
          
        }
      }}
    
    anova_master<-data.frame("Anova Test of Variables"=anova_master)
    pval_df<-data.frame("P Value"=pval_df)
    result<-data.frame("Test Result"= result)
    statget<-data.frame("F Statistic"= statget)
    
    new <- cbind(anova_master, statget)
    new <- cbind(new, pval_df)
    new <- cbind(new,result)
    
    print(new)
    
    write.xlsx(new,file=paste(append(as.character(filename),".xlsx"),collapse=""),sheetName="Anova",append=TRUE)
    print(new)
  }
  
  if((test=="twotail"))
  {
    
    for(i in nums)
    {
      for ( j in labels)
        
      { 
        if(nrow(unique(df[j]))==2)
          
        { 
          temp<-c()
          pval<-t.test(unlist(df[i])~unlist(df[j]), mu=0,alt="two.sided",conf=alpha,var.eq=F,paired=F)$p.value
          pval<-round(pval,3)
          stattemp<-unlist(t.test(unlist(df[i])~unlist(df[j]), mu=0,alt="two.sided",conf=alpha,var.eq=F,paired=F))["statistic.t"]
          stattemp<-round(as.numeric(stattemp),3)
          temp<-c(names(df[i])," grouped by ",names(df[j]))
          
          if(pval<(1-alpha))
          {
            result[t_count]<-c(" Means are different ")
          }
          
          else
          { 
            result[t_count]<-c(" Means are not different ")
          }
          pval_df[t_count]<-pval
          statget[t_count]<-stattemp
          T_master[t_count]<- StrTrim(StrAlign(paste(temp, collapse = ''),sep = "\\l"),method="both")
          t_count<-(t_count+1)
          
          
        }
        
      }}
    
    T_master<-data.frame("Two tail test on Two samples"=T_master)
    pval_df<-data.frame("P Value"=pval_df)
    result<-data.frame("Test Result "=result)
    statget<-data.frame("T Statistic"= statget)
    
    new <- cbind(T_master, statget)
    new <- cbind(new, pval_df)
    new <- cbind(new,result)
    
    write.xlsx(new,file=paste(append(as.character(filename),".xlsx"),collapse=""),sheetName="TwoTail_T_Test",append=TRUE)
    print(new)
    
  }
  
  
  if(test=="onetail")
    
  {
    
    for(i in nums)
    {
      for ( j in labels)
        
      { 
        if(nrow(unique(df[j]))==2)
          
        { 
          temp<-c()
          pval<-t.test(unlist(df[i])~unlist(df[j]), mu=0,alt="less",conf=alpha,var.eq=F,paired=F)$p.value
          pval<-round(as.double(pval),3)
          stattemp<-unlist(t.test(unlist(df[i])~unlist(df[j]), mu=0,alt="less",conf=alpha,var.eq=F,paired=F))["statistic.t"]
          stattemp<-round(as.numeric(stattemp),3)
          temp<-c(names(df[i])," grouped by ",names(df[j]))
          
          if(pval<(1-alpha))
          {
            result[t_count]<-c("Lesser than zero")
          }
          
          else
          { 
            result[t_count]<-c(" Not lesser than zero")
          }
          
          pval_df[t_count]<-pval
          statget[t_count]<-stattemp
          claim[t_count]<-"Diff between sample means lesser than Zero"
          T_master[t_count]<- StrTrim(StrAlign(paste(temp, collapse = ''),sep = "\\l"),method="both")
          t_count<-(t_count+1)
          
          
          temp.sub<-c()
          pval.sub<- t.test(unlist(df[i])~unlist(df[j]), mu=0,alt="greater",conf=alpha,var.eq=F,paired=F)$p.value
          pval.sub<-round(pval.sub,3)
          stattemp.sub<-unlist(t.test(unlist(df[i])~unlist(df[j]), mu=0,alt="greater",conf=alpha,var.eq=F,paired=F))["statistic.t"]
          stattemp.sub<-round(as.numeric(stattemp.sub,3))
          temp.sub<-c(names(df[i])," grouped by ",names(df[j]))
          
          if(pval.sub<(1-alpha))
          {
            result.sub[t_count.sub]<-c("Greater than zero")
          }
          
          else
          { 
            result.sub[t_count.sub]<-c(" Not greater than zero")
          }
          
          pval_df.sub[t_count.sub]<-pval.sub
          statget.sub[t_count.sub]<-stattemp.sub
            
          claim.sub[t_count.sub]<-" Diff between sample means Greater than Zero"
          T_master.sub[t_count.sub]<- StrTrim(StrAlign(paste(temp.sub, collapse = ''),sep = "\\l"),method="both")
          
          t_count.sub<-(t_count.sub+1)
          
          
        }}}
    
    
    T_master<-data.frame("One Sided T Test of"=T_master)
    pval_df<-data.frame("P Value"=pval_df)
    statget<-data.frame("T Statistic"= statget)
    result<-data.frame("Test Result : Difference in Mean of two samples"=result)
    claim<-data.frame("Claim"=claim)
    
    
    new <- cbind(T_master,claim)
    new <- cbind(new,statget)
    new <- cbind(new, pval_df)
    new <- cbind(new,result)
    
    T_master.sub<-data.frame("One Sided T Test of"=T_master.sub)
    pval_df.sub<-data.frame("P Value"=pval_df.sub)
    statget.sub<-data.frame("T Statistic"= statget.sub)
    result.sub<-data.frame("Test Result : Difference in Mean of two samples"=result.sub)
    claim.sub<-data.frame("Claim"=claim.sub)
    
    
    new.sub <- cbind(T_master.sub, claim.sub)
    new.sub<-cbind(new.sub,statget.sub)
    new.sub <- cbind(new.sub,pval_df.sub)
    new.sub <- cbind(new.sub,result.sub)
    
    write.xlsx(new,file=paste(append(as.character(filename),".xlsx"),collapse=""),sheetName="OneTail_T_Test_lesser",append=TRUE)
    
    write.xlsx(new.sub,file=paste(append(as.character(filename),".xlsx"),collapse=""),sheetName="OneTail_T_Test_greater",append=TRUE)
    
    
    print(new)
    cat("\n")
    print(new.sub)
    
  }
  
  
  if(test=="chisq")
  {
    for(i in labels)
    { 
      for (j in labels)
      {
        if(i!=j)
        {
          
          chi_square<-c()
          #cat("\n")
          #print(c(names(df[i]),"vs",names(df[j])))
          pval<-chisq.test(unlist(df[i]),unlist(df[j]))$p.value
          pval<-round(pval,3)
          stattemp<-unlist(chisq.test(unlist(df[i]),unlist(df[j])))["statistic.X-squared"]
          stattemp<-round(as.numeric(stattemp),3)
          temp<-c(names(df[i])," vs ",names(df[j]))
          
          if(pval<(1-alpha))
            
          { 
            result[chi_count]<-c(" Categorical Variables are Dependent ")}
          
          else
          { 
            result[chi_count]<-c(" Categorical Variables are Independent ")}
          
          pval_df[chi_count]<-pval
          statget[chi_count]<-stattemp
          chi_master[chi_count]<- StrTrim(StrAlign(paste(temp, collapse = ''),sep = "\\l"),method="both")
          chi_count<-(chi_count+1)
          
        }
      }}
    
    
    chi_master<-data.frame("CHI square Independence test on Categorical Variable"=chi_master)
    pval_df<-data.frame("P Value"=pval_df)
    statget<-data.frame("X2 Statistic"= statget)
    result<-data.frame("Test Result"=result)
    
    new <- cbind(chi_master,statget)
    new <- cbind(new,pval_df)
    new <- cbind(new,result)
    
    write.xlsx(new,file=paste(append(as.character(filename),".xlsx"),collapse=""),sheetName="Chisquare",append=TRUE)
    print(new)      
    
  }
}
