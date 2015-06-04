###############################################################
# MCASsgp.r      Program to gererate simulated MCAS data
#
# SURE2015 6/4/2015
###############################################################
source("read_abilities.r")   #function to read abilities values
source("MCAS_dichotomous.r") #function to compute dichotomous scores
theta=read_abilities(1)      #read the 1,000 student file
str(theta)
nstu = length(theta)         #get the count of students
#
#Convert the normal ability parameters to percentiles
quant=100*round(pnorm(theta),digits=2)        #convert abilities to percentiles
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