library(plyr)

testfunction <- function() {
  x <- rnorm(100)
  mean(x)
}

testfunction2 <-function(x) {
  x + rnorm(length(x))
}

#vector of objects
a=c(1,2,3)
a=c(T,T,F)
a=c("t","f","t")
#coercion to one class if multiple classes:
a=c(1,T,"T")
class(a)
#or explicit:
x<-0:6
class(x)
as.numberic(x)
as.logical(x)

#lists are combinations of vectors with elements of different classes
x<-list(1,2,3,"a",T)


#matrix are vectors with a dimension attribute, so one class but rows and columns
z<-matrix(1:6,nrow=2,ncol=3)
#can also create matrixes by binding
x<-1:3;y=4:6;
cbind(x,y)
rbind(x,y)
x<-matrix(1:6,nrow=3,ncol=2)
dimnames(x)<-list(c("a","b","c"),c("d","e"))

#factors are categorical, like an integer vector with labels for the integers
x<-factor(c("male", "female", "male", "male","female"),levels=c("male","female"))
table(x)
unclass(x)

#data frames are like an excel sheet, multiple classes but same lengthts
x<-data.frame(foo=1:4,bar=c(T,F,T,T))

x<-c("a", "b","c")
names(x)<-c("name","tes","bar")

x<-list(1,"a",2,T)

           
table1<-read.csv("specdata/001.csv")
cls<-sapply(table1,class)
table2<-read.csv("specdata/002.csv", colClasses=cls)

x<-data.frame(a=1,b= "a")
y="text"
dput(x,"dputtest.R")
a=dget("dputtest.R")

dump(c("x","y"),file="dumptest.R")
source("dumptest.R")

con<-url("http://www.whitelabelcoffee.nl","r")
x<-readLines(con)
head(x,10)

x<-c("a","b","c","d")
v<-x>"a"
d<-x[v]

multipone <- function(x){
  x*1
}

#time
x<-Sys.time()
p<-as.POSIXlt(x)
names(unclass(p))
p$sec

x <- matrix(1:6, 2, 3)
for(i in seq_len(nrow(x))) {
  for(j in seq_len(ncol(x))) {
    print(x[i, j])
  }
}

pollutantmean <- function(directory, pollutant, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of the pollutant for which we will calculate the
  ## mean; either "sulfate" or "nitrate".
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return the mean of the pollutant across all monitors list
  ## in the 'id' vector (ignoring NA values)
  ## NOTE: Do not round the result!
  
  #find files and readin
  d<-dir(directory) 
  names<-paste(directory, d[id], sep="/")
  fls<-lapply(names,read.csv)
  
  #convert to data.frame:
  fls2<-ldply(fls)
  
  #take mean of not na's
  naval<-is.na(fls2[[pollutant]]) 
  mean(fls2[[pollutant]][!naval])
}

complete <- function(directory, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return a data frame of the form:
  ## id nobs
  ## 1  117
  ## 2  1041
  ## ...
  ## where 'id' is the monitor ID number and 'nobs' is the
  ## number of complete cases
  
  d<-dir(directory) 
  names<-paste(directory, d[id], sep="/")
  fls<-lapply(names,read.csv)
  #t/f lists of complete cases
  fls2<-lapply(fls,complete.cases)
  #sum is count of trues because true=1
  fls3<-lapply(fls2,sum)
  #convert to data.frame
  fls4<-ldply(fls3)
  names(fls4)<-"complete"
  cbind(id,fls4)  
  
}

corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
  ## NOTE: Do not round the result!
  
  #for all files count how many completes
  cmpl<-complete(directory)
  #the ids of the files with complet above threshold
  corids<-cmpl$id[cmpl$com>threshold]
  
  d<-dir(directory) 
  names<-paste(directory, d[corids], sep="/")
  fls<-lapply(names,read.csv)
  #t/f lists of complete cases
  fls2<-lapply(fls,complete.cases)
  
  testlist<-list()

  #for all elements in the list take the 2nd and 3rd column where fls2 is true
  for(n in 1:length(fls)){
  testlist[[n]]<-fls[[n]][fls2[[n]],2:3]
  }

  #calculate correlation for all dataframes in list of dataframes
  cors<-lapply(testlist,cor)
  #in the list of matrics, take of each matrices the 1,2 element (the correlation)
  cors2=lapply(cors,'[',1,2)
  cor3=ldply(cors2)
  as.vector(cor3[,1])

}



