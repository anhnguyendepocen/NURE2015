###############################################################
# MCASsgp.r      Program to gererate simulated MCAS data
#
# SURE2015 6/4/2015
###############################################################
source("read_abilities.r")   #function to read abilities values
source("MCAS_dichotomous.r") #function to compute dichotomous scores
source("MCAS_polytomous.r") #function to compute polytomous scores
source("MCAS_raw2scaled.r") #function to compute scaled scores
#
theta=read_abilities(1)      #read the 1,000 student file
str(theta)
nstu = length(theta)         #get the count of students
#
#Convert the normal ability parameters to percentiles
quant=100*round(qnorm(theta),digits=2)        #convert abilities to percentiles
#
#Define the number of years to simulate
years=7
#
#Define a matrix of values with nstu rows and 3+2*years columns
scores=array(rep(0.0,nstu),c(nstu,2*years+3)) #define scores array
scores[,1]=1:nstu                             #col1 is student number or ID
scores[,2]=theta                              #col2 is ability theta
scores[,3]=quant                              #col3 is percentile of ability
#
print(scores[1:15,1])
print(scores[1:15,2])
print(scores[1:15,3])
#
scores=MCAS_dichotomous(scores,nstu,years)
#
print("Raw scores for dichotomous items")
print(scores[1:15,4])
print(scores[1:15,5])
print(scores[1:15,6])
#
scores=MCAS_polytomous(scores,nstu,years)
#
print("Raw scores for dichotomous+polytomous items")
print(scores[1:15,4])
print(scores[1:15,5])
print(scores[1:15,6])
#
scores=MCAS_raw2scaled(scores,nstu,years)
#
print("Scaled scores for dichotomous+polytomous items")
print(scores[1:15,11])
print(scores[1:15,12])
print(scores[1:15,13])
#
#Now build the MCAS_wide data frame used by the SGP package
#
IDcol=as.integer(scores[1:nstu,1])
GRADE_2009=rep(3,nstu)
GRADE_2010=rep(4,nstu)
GRADE_2011=rep(5,nstu)
GRADE_2012=rep(6,nstu)
GRADE_2013=rep(7,nstu)
GRADE_2014=rep(8,nstu)
SS_2009=scores[1:nstu,11]
SS_2010=scores[1:nstu,12]
SS_2011=scores[1:nstu,13]
SS_2012=scores[1:nstu,14]
SS_2013=scores[1:nstu,15]
SS_2014=scores[1:nstu,16]
MCAS_wide=data.frame(IDcol,GRADE_2009,GRADE_2010,GRADE_2011,GRADE_2012,GRADE_2013,
                     SS_2009,SS_2010,SS_2011,SS_2012,SS_2013)
colnames(MCAS_wide)=c("ID","GRADE_2009","GRADE_2010","GRADE_2011","GRADE_2012",
                      "GRADE_2013","SS_2009","SS_2010","SS_2011",
                      "SS_2012","SS_2013")
str(MCAS_wide)
summary(MCAS_wide)
