mcas<-function(grad,yr,sub,abilities){
  load("IRT_parms.Rdata")
  #dichotomous
  df<-subset(IRT_parms, grade==grad & year==yr & subj==sub & dichot & scoring)
  print(df[,1:9])
  s=rep(0,length(abilities))
  for (i in 1:nrow(df)){
    p = df$c[[i]]+(1-df$c[[i]])*(exp(df$D[[i]]*df$a[[i]]*(abilities-df$b[[i]])))/(1+exp(df$D[[i]]*df$a[[i]]*(abilities-df$b[[i]])))
    s=s+rbinom(length(abilities),1,p)
  }
  #polychotomous
  df<-subset(IRT_parms, grade==grad & year==yr & subj==sub & !dichot & scoring)
  print(df[,1:9])
  for (i in 1:nrow(df)){
    cut0=exp(df$D*df$a*(abilities-df$b+df$d0))/(1+exp(df$D*df$a*(abilities-df$b+df$d0)))
    cut1=exp(df$D*df$a*(abilities-df$b+df$d1))/(1+exp(df$D*df$a*(abilities-df$b+df$d1)))
    cut2=rep(0,length(abilities))
    cut3=rep(0,length(abilities))
    if (grad!="3"){
      cut2=exp(df$D*df$a*(abilities-df$b+df$d2))/(1+exp(df$D*df$a*(abilities-df$b+df$d2)))
      cut3=exp(df$D*df$a*(abilities-df$b+df$d3))/(1+exp(df$D*df$a*(abilities-df$b+df$d3)))
    }
    u=runif(length(abilities))
    s=s+ifelse(u>1-cut0,1,0)+ifelse(u>1-cut1,1,0)+ifelse(u>1-cut2,1,0)+ifelse(u>1-cut3,1,0)
  }
  #raw to scaled
  load("R2S.Rdata")
  df<-subset(R2S, grade==grad)
  r2s=R2S$scaled
  scaled=r2s[s+1]
  return(scaled)
}