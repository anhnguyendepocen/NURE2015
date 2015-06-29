####################################################################################################
#  mcas.r      creates a data frame of simulated MCAS results
#
#  caller specifies the grade, year, and subject and supplies a vector of ability values
#
#  parameters:
#              grad      grade (3,4,5,6,7,8, or 10)
#              yr        year of the IRT model  (i.e., 2011)
#              sub       subject  (i.e.  MTH)
#              abilities vector of theta values representing students
#
#  returns:    a dataframe containing one row for each scored item plus a row of scaled scores
#
#              the first column has the MCAS item number for scored items, zero for scaled scores
#
###################################################################################################
mcas<-function(grad,yr,sub,abilities){
  load("IRT_parms.Rdata")
  #dichotomous
  df<-subset(IRT_parms, grade==grad & year==yr & subj==sub & dichot & scoring)
  print(df[,1:9])
  row1=rep(0,1+length(abilities))
  r1=data.frame(row1) #initialize results data frame
  rdf=t(r1)
  s=rep(0,length(abilities))
  for (i in 1:nrow(df)){
    p = 1.0/(1+exp(-df$D[[i]]*df$a[[i]]*(abilities-df$b[[i]])))
    scores=rbinom(length(abilities),1,p)
    itemno=df$itemno[[i]]
    rdf=rbind(rdf,c(itemno,scores))
    s=s+scores
  }
  #polychotomous
  df<-subset(IRT_parms, grade==grad & year==yr & subj==sub & !dichot & scoring)
  print(df[,1:9])
  for (i in 1:nrow(df)){
    itemno=df$itemno[[i]]
    scores=rep(0,length(abilities))
    cut0=1.0/(1+exp(-df$D[[i]]*df$a[[i]]*(abilities-df$b[[i]]+df$d0[[i]])))
    cut1=1.0/(1+exp(-df$D[[i]]*df$a[[i]]*(abilities-df$b[[i]]+df$d1[[i]])))
    cut2=rep(0,length(abilities))
    cut3=rep(0,length(abilities))
    if (grad!="3"){
      cut2=1.0/(1+exp(-df$D[[i]]*df$a[[i]]*(abilities-df$b[[i]]+df$d2[[i]])))
      cut3=1.0/(1+exp(-df$D[[i]]*df$a[[i]]*(abilities-df$b[[i]]+df$d3[[i]])))
    }
    u=runif(length(abilities))
    scores=ifelse(u>1-cut0,1,0)+ifelse(u>1-cut1,1,0)+ifelse(u>1-cut2,1,0)+ifelse(u>1-cut3,1,0)
    rdf=rbind(rdf,c(itemno,scores))
  }
  #raw scores
  raw=apply(rdf,2,sum)                 #compute vector of column means for raw score
  raw[[1]]<-0                          #sum of item numbers is meaningless, set to zero
  print(table(raw[2:length(raw)]))     #print frequency table of raw scores
  #raw to scaled
  load("R2S.Rdata")
  df<-subset(R2S, grade==grad)
  r2s=df$scaled
  scaled=r2s[raw+1]
  scaled[[1]]<-0
  rdf[1,]<-scaled
 return(rdf)
}