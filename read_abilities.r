read_abilities<-function(kstu){    #kstu is thousands of students
  path=paste('Rdata/sample',kstu,'.Rdata',sep="")
  print(path)                      #show the file name
  load(path)                       #retrieve the ability values
  return(thetas$theta)             #return the array of theta values
}
