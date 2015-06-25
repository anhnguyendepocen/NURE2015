###########################################################################################################
# experiment4.r
#
# Parameters: 
#     1.  seed for random number generator
#     2.  ability parameter file name (minus the .Rdata extension)
#
# Python driver.py call:
#
#     python driver.py niter experiment3 abilities
#
#     where:    niter  is the number of iterations to run
#               abilities  is the name of the abilities paramter file to use (minus the .Rdata extension)
#
#    Example:  perform 10 iterations using sample1.Rdata as the abilities file:
#
#              python driver.py 10 experiment3 sample1
#
#    Example:  perform 200 iterations using sample70.Rdata as the abilities file:
#
#              python driver.py 200 experiment3 sample70
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
source("mcas.r")
args <- commandArgs()
print('seed')
dname=substr(args[[4]],8,nchar(args[[4]])-2)   #
seed=as.numeric(args[[6]])
print(dname)
set.seed(seed)   
sample_file=args[[7]]                          #filename for ability parameters
file_name=paste(paste("Rdata/",sample_file,sep=""),".Rdata",sep="")      #abilities data file
print(file_name)
load(file_name)                                #retrieve 70k cohort ability values
nstu=length(thetas$theta)                      #determine how many students there are
#
years=2;                                       #how many years to generate
#
ls()
IDcol=1:nstu
GRADE_2009=rep(3,nstu)
GRADE_2010=rep(4,nstu)
SS_2009=mcas('3',"2011","MTH",thetas$theta)
table(SS_2009)
adjtheta=thetas$theta+rep(c(0.5,0,-0.5),1+nstu/3)[1:nstu]
SS_2010=mcas('4',"2011","MTH",adjtheta)
table(SS_2010)
MCAS_wide=data.frame(IDcol,GRADE_2009,GRADE_2010,SS_2009,SS_2010)
colnames(MCAS_wide)=c("ID","GRADE_2009","GRADE_2010","SS_2009","SS_2010")
str(MCAS_wide)
summary(MCAS_wide)
library(SGP)
#MCAS_sgp<- studentGrowthPercentiles_EQ(panel.data=MCAS_wide,
MCAS_sgp<- studentGrowthPercentiles(panel.data=MCAS_wide,
                                    sgp.labels=list(my.year=2010, my.subject="Mathematics"),
                                    grade.progression=c(3,4),
                                    parallel.config=list(BACKEND="FOREACH", 
                                    TYPE="doParallel", WORKERS=5))
#
fname=paste(dname,"/",dname,"_",sample_file,"_",format(Sys.time(),'%m%d%Y%H%M%S'),".Rdata",sep="")
save(MCAS_sgp,file=fname)
str(MCAS_sgp)
