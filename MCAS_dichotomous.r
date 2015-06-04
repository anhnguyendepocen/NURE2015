MCAS_dichotomous<-function(scores,nstu,years){                                #clear work area
#dichotomous items 
dichot=c("mcas_irt_2011_MTH_grade3_dichot.txt",  #Appendix H dichotomous tables
        "mcas_irt_2011_MTH_grade4_dichot.txt",
        "mcas_irt_2011_MTH_grade5_dichot.txt",
        "mcas_irt_2011_MTH_grade6_dichot.txt",
        "mcas_irt_2011_MTH_grade7_dichot.txt",
        "mcas_irt_2011_MTH_grade8_dichot.txt",
        "mcas_irt_2011_MTH_grade10_dichot.txt")
                                              #
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
return(scores)
}
