stats_get<-function(df,test,alpha,filename)
  
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
          a=aov(unlist(df[i])~unlist(df[j]))
          pval=round(unlist(summary(a))["Pr(>F)1"],3)
          temp<-c(names(df[i])," vs ",names(df[j]))
          
          if(pval<(1-alpha))
            
          {
            result[aov_count]<-c(" We accept means are different ")}
          
          else
          { 
            result[aov_count]<-c(" We accept means are not different ")}
          
          pval_df[aov_count]<-pval
          
          anova_master[aov_count]<- StrTrim(StrAlign(paste(temp, collapse = ''),sep = "\\l"),method="both")
          aov_count<-(aov_count+1)
          
        }
      }}
    
    anova_master<-data.frame("Anova Test of Variables"=anova_master)
    pval_df<-data.frame("P Value"=pval_df)
    result<-data.frame("Test Result"= result)
    new <- cbind(anova_master, pval_df)
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
          temp<-c(names(df[i])," vs ",names(df[j]))
          
          if(pval<(1-alpha))
          {
            result[t_count]<-c(" Means are different ")
          }
          
          else
          { 
            result[t_count]<-c(" Means are not different ")
          }
          pval_df[t_count]<-pval
          T_master[t_count]<- StrTrim(StrAlign(paste(temp, collapse = ''),sep = "\\l"),method="both")
          t_count<-(t_count+1)
          
        }
        
      }}
    
    
    T_master<-data.frame("Difference in mean of Variables is Zero or Not"=T_master)
    pval_df<-data.frame("P Value"=pval_df)
    result<-data.frame("Test Result"=result)
    new <- cbind(T_master, pval_df)
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
          temp<-c(names(df[i])," vs ",names(df[j]))
          
          if(pval<(1-alpha))
          {
            result[t_count]<-c(" Difference in Means lesser than zero")
          }
          
          else
          { 
            result[t_count]<-c(" Difference in Means is not Lesser than zero")
          }
          
          pval_df[t_count]<-pval
          claim[t_count]<-"Difference in mean of Variables is lesser than Zero"
          T_master[t_count]<- StrTrim(StrAlign(paste(temp, collapse = ''),sep = "\\l"),method="both")
          t_count<-(t_count+1)
          
          
          temp.sub<-c()
          pval.sub<- t.test(unlist(df[i])~unlist(df[j]), mu=0,alt="greater",conf=alpha,var.eq=F,paired=F)$p.value
          pval.sub<-round(pval.sub,3)
          temp.sub<-c(names(df[i])," vs ",names(df[j]))
          
          if(pval.sub<(1-alpha))
          {
            result.sub[t_count.sub]<-c(" Difference in Means greater than zero")
          }
          
          else
          { 
            result.sub[t_count.sub]<-c("Difference in Means not greater than zero")
          }
          
          pval_df.sub[t_count.sub]<-pval.sub
          claim.sub[t_count.sub]<-"Difference in mean of Variables is Greater than Zero"
          T_master.sub[t_count.sub]<- StrTrim(StrAlign(paste(temp.sub, collapse = ''),sep = "\\l"),method="both")
          
          t_count.sub<-(t_count.sub+1)
          
          
        }}}
    
    
    T_master<-data.frame("One Sided T Test of"=T_master)
    pval_df<-data.frame("P Value"=pval_df)
    result<-data.frame("Test Result"=result)
    claim<-data.frame("Claim"=claim)
    
    
    new <- cbind(T_master,claim)
    new <- cbind(new,pval_df)
    new <- cbind(new,result)
    
    
    T_master.sub<-data.frame("One Sided T Test of"=T_master.sub)
    pval_df.sub<-data.frame("P Value"=pval_df.sub)
    result.sub<-data.frame("Test Result"=result.sub)
    claim.sub<-data.frame("Claim"=claim.sub)
    
    
    new.sub <- cbind(T_master.sub, claim.sub)
    new.sub<-cbind(new.sub,pval_df.sub)
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
          
          temp<-c(names(df[i])," vs ",names(df[j]))
          
          if(pval<(1-alpha))
            
          { 
            result[chi_count]<-c(" Categorical Variables are Dependent ")}
          
          else
          { 
            result[chi_count]<-c(" Categorical Variables are Independent ")}
          
          pval_df[chi_count]<-pval
          chi_master[chi_count]<- StrTrim(StrAlign(paste(temp, collapse = ''),sep = "\\l"),method="both")
          chi_count<-(chi_count+1)
          
        }
      }}
    
    
    chi_master<-data.frame("CHI square Independence test on Categorical Variable"=chi_master)
    pval_df<-data.frame("P Value"=pval_df)
    result<-data.frame("Test Result"=result)
    
    new <- cbind(chi_master,pval_df)
    new <- cbind(new,result)
    
    write.xlsx(new,file=paste(append(as.character(filename),".xlsx"),collapse=""),sheetName="Chisquare",append=TRUE)
    print(new)      
    
  }
}
