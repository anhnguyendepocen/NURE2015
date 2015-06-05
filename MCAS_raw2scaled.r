MCAS_raw2scaled<-function(scores,nstu,years){   
  #convert raw scores to scaled scores
  r2s=c("mcas_2011_MTH_grade3_raw2scaled.txt",  #Appendix M raw to scaled table
        "mcas_2011_MTH_grade4_raw2scaled.txt",
        "mcas_2011_MTH_grade5_raw2scaled.txt",
        "mcas_2011_MTH_grade6_raw2scaled.txt",
        "mcas_2011_MTH_grade7_raw2scaled.txt",
        "mcas_2011_MTH_grade8_raw2scaled.txt",
        "mcas_2011_MTH_grade10_raw2scaled.txt")#clear work area
  for (i in 1:years){
    rs1=read.table(r2s[i],header=FALSE)
    nitems=length(rs1$V1)/7
    sstable=array(rep(0,nitems),c(nitems,1))
    sst=array(rs1$V1,c(7,nitems))           #define a cols x items array
    sst2=array(sst[2,])
    for (j in 1:nstu){
      rs=scores[j,i+3]
      ss=sst2[rs+1]
      scores[j,i+3+7]=ss
    }
  }
return(scores)
}
