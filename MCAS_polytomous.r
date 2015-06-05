MCAS_polytomous<-function(scores,nstu,years){                                #clear work area
  #now polytomous items
  polychot=c("mcas_irt_2011_MTH_grade3_polyt.txt",  #Appendix H polytomous tables
             "mcas_irt_2011_MTH_grade4_polyt.txt",
             "mcas_irt_2011_MTH_grade5_polyt.txt",
             "mcas_irt_2011_MTH_grade6_polyt.txt",
             "mcas_irt_2011_MTH_grade7_polyt.txt",
             "mcas_irt_2011_MTH_grade8_polyt.txt",
             "mcas_irt_2011_MTH_grade10_polyt.txt")
                                              #
#                                             #simulate MC and SA scores
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
return(scores)
}
