###########################################################################################################
# experiment1.r
#
# Parameters: 
#     1.  seed for random number generator
#     2.  ability parameter file name (minus the .Rdata extension)
#
# Python driver.py call:
#
#     python driver.py niter experiment1 abilities
#
#     where:    niter  is the number of iterations to run
#               abilities  is the name of the abilities paramter file to use (minus the .Rdata extension)
#
#    Example:  perform 10 iterations using sample1.Rdata as the abilities file:
#
#              python driver.py 10 experiment1 sample1
#
#    Example:  perform 200 iterations using sample70.Rdata as the abilities file:
#
#              python driver.py 200 experiment1 sample70
#
##########################################################################################################
library(SGP)
library(SGPdata)
library(foreach)
library(quantreg)
library(splines)
library(data.table)
require(plyr)
rm(list = ls())                                #clear work area
args <- commandArgs()
print('seed')
dname=substr(args[[4]],8,nchar(args[[4]])-2)   #
seed=as.numeric(args[[6]])
print(dname)
set.seed(seed)   
sample_file=args[[7]]                          #filename for ability parameters
file_name=paste(paste("Rdata/",sample_file,sep=""),".Rdata",sep="")      #abilities data file
print(file_name)
#file_name=paste(sample_file,".Rdata",sep="")   #add file name extension
load(file_name)                                #retrieve 70k cohort ability values
nstu=length(thetas$theta)                      #determine how many there are
#
theta=array(thetas$theta)                      #store abilities in array theta
years=2;                                       #how many years to generate
                                               #now read the IRT parameters
                                               #dichotomous items first
dichot=c("mcas_irt_2011_MTH_grade3_dichot.txt",  #Appendix H dichotomous tables
        "mcas_irt_2011_MTH_grade4_dichot.txt",
        "mcas_irt_2011_MTH_grade5_dichot.txt",
        "mcas_irt_2011_MTH_grade6_dichot.txt",
        "mcas_irt_2011_MTH_grade7_dichot.txt",
        "mcas_irt_2011_MTH_grade8_dichot.txt",
        "mcas_irt_2011_MTH_grade10_dichot.txt")
                                              #
quant=100*round(pnorm(theta),digits=2)        #convert abilities to percentiles
scores=array(rep(0.0,nstu),c(nstu,2*years+3)) #scores array years+3 columns, nstu rows
scores[,1]=1:nstu                             #first column is student number or ID
scores[,2]=theta                              #second is ability theta
scores[,3]=quant                              #third is percentile of ability
#                                             #simulate MC and SA scores
for (i in 1:years){                           #do this once for each grade
    parms=read.table(dichot[i],header=FALSE)  #read irt parameters for dichotomous
    nitems=length(parms$V1)/7                 #IRT parms read as a 7*items x 1 vector 
    irt=array(parms$V1,c(7,nitems))           #define a 7 x items array
    for (k in 1:nstu){                        #loop through the students to compute scores
        for (j in 1:nitems){                  #pick out common MC and SA items only
            if (((irt[3,j]>0.0) & (irt[5,j]>0.0) & (irt[1,j]!=217529))|(i==7)){
                a=irt[2,j]                    #discrimination parameter a
                b=irt[4,j]                    #difficulty parameter b
                c=irt[6,j]                    #pseudo guessing parameter c
                D=1.701                       #normalizing constant D
                                              #probability of correct answer given a,b,c,D,theta
                p=c+(1-c)*(exp(D*a*(theta[k]-b)))/(1+exp(D*a*(theta[k]-b)))  
                s=rbinom(1,1,p)               #simulated Bernoulli trial with this probability of success
                scores[k,i+3]=scores[k,i+3]+s #add outcome of Bernoulli trial to score
            }
        }
    }
  }
                                               #now polytomous items
polychot=c("mcas_irt_2011_MTH_grade3_polyt.txt",  #Appendix H polytomous tables
          "mcas_irt_2011_MTH_grade4_polyt.txt",
          "mcas_irt_2011_MTH_grade5_polyt.txt",
          "mcas_irt_2011_MTH_grade6_polyt.txt",
          "mcas_irt_2011_MTH_grade7_polyt.txt",
          "mcas_irt_2011_MTH_grade8_polyt.txt",
          "mcas_irt_2011_MTH_grade10_polyt.txt")
  for (i in 1:years){                         #do this once for each grade
    cols=17                                   #all except grade 3 have 17 columns in each row
    if (i==1) cols=13                         #grade 3 has 13 columns
    parms2=read.table(polychot[i],header=FALSE)   #read irt parameters for polytomous
    nitems=length(parms2$V1)/cols             #IRT parms read as a cols*items x 1 vector 
    irt2=array(parms2$V1,c(cols,nitems))      #define a cols x items array
    for (k in 1:nstu){                        #loop through the students to compute scores
        for (j in 1:nitems){                  #pick out common OR items only
                                              #fixing a data problem: item 281896 has no SD for a
            if (irt2[1,j]==281896) irt2[3,j]=0.2  #make up a standard deviation, we don't use it anyway 
                                              #this is just to allow 281896 to be recognized as OR
            if ((irt2[3,j]>0.0) | (i==7)){    #can tell actual OR items by positive SD for parameter a
                                              #except grade 10 is all zeros - use all grade 10 entries
                a=irt2[2,j]                   #discrimination a
                b=irt2[4,j]                   #difficulty b
                D=1.701                       #normalizing constant D
                irtparms=4                    #number of cutpoints for grades 4-10
                if (i==1) irtparms=2          #number of cutpoints for grade 3
                irtindx=8                     #starting index in row for D0
                points=0                      #start with zero points
                pval=runif(1)                 #random uniform will determine number of points scored
                prevcut=1.0                   #previous cutpoint initialize to 1
                probtot=0.0                   #sum of probabilities up to this score
                for (parmno in 1:irtparms) {  #loop through the cutpoint Dk values
                    Dk=irt2[irtindx,j]        #get Dk value
                    cut=(exp(D*a*(theta[k]-b+Dk)))/(1+exp(D*a*(theta[k]-b+Dk)))  #next cutpoint
                    prob=prevcut-cut          #probability for this value
                    probtot=probtot+prob      #cumulative probability up to this poing
                    if (probtot < pval) points=points+1 #add a point if cutoff < random uniform
                    prevcut=cut               #remember cutoff for next iteration
                    irtindx=irtindx+2         #next Dk value
                }
                scores[k,i+3]=scores[k,i+3]+points  #add OR points to total (raw) score
            }
        }
    }
  }
                                              #convert raw scores to scaled scores
r2s=c("mcas_2011_MTH_grade3_raw2scaled.txt",  #Appendix M raw to scaled table
      "mcas_2011_MTH_grade4_raw2scaled.txt",
      "mcas_2011_MTH_grade5_raw2scaled.txt",
      "mcas_2011_MTH_grade6_raw2scaled.txt",
      "mcas_2011_MTH_grade7_raw2scaled.txt",
      "mcas_2011_MTH_grade8_raw2scaled.txt",
      "mcas_2011_MTH_grade10_raw2scaled.txt")
str(scores)
for (i in 1:years){
    rs1=read.table(r2s[i],header=FALSE)
    nitems=length(rs1$V1)/7
    sstable=array(rep(0,nitems),c(nitems,1))
    sst=array(rs1$V1,c(7,nitems))           #define a cols x items array
    sst2=array(sst[2,])
    for (j in 1:nstu){
      rs=scores[j,i+3]
      ss=sst2[rs+1]
      scores[j,i+3+years]=ss
    }
}
IDcol=as.integer(scores[1:nstu,1])
GRADE_2009=rep(3,nstu)
GRADE_2010=rep(4,nstu)
SS_2009=scores[1:nstu,4+years]
SS_2010=scores[1:nstu,5+years]
MCAS_wide=data.frame(IDcol,GRADE_2009,GRADE_2010,SS_2009,SS_2010)
colnames(MCAS_wide)=c("ID","GRADE_2009","GRADE_2010","SS_2009","SS_2010")
str(MCAS_wide)
summary(MCAS_wide)
library(SGP)
#MCAS_sgp<- studentGrowthPercentiles_EQ(panel.data=MCAS_wide,
MCAS_sgp<- studentGrowthPercentiles(panel.data=MCAS_wide,
                sgp.labels=list(my.year=2010, my.subject="Mathematics"),
                grade.progression=c(3,4))
                #parallel.config=list(BACKEND="FOREACH", 
                #TYPE="doParallel", WORKERS=5))
#
MCASsgp=cbind(MCAS_wide,MCAS_sgp$SGPercentiles$MATHEMATICS.2010$SGP,thetas$theta)
#
fname=paste(dname,"/",dname,"_",sample_file,"_",format(Sys.time(),'%m%d%Y%H%M%S'),".Rdata",sep="")
save(MCAS_sgp,file=fname)
str(MCAS_sgp)
