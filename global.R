

if(require(pcaMethods)==FALSE){
  need<-c('Rcpp', 'rJava',
          'Matrix', 'cluster', 'foreign', 'lattice', 'mgcv', 'survival')
  for(i in 1:length(need)){
    if(require(need[i], character.only = TRUE)==FALSE){
      install.packages(need[i],dependencies=TRUE);library(need[i], character.only = TRUE)
    } else { library(need[i],character.only = TRUE)
    }
  }#dependancies
  source("http://bioconductor.org/biocLite.R")
  biocLite("pcaMethods")
}

#Functions used in example
#---------------------------
#fxn to load repo from from github
source.git.hub<-function(url = "https://github.com/dgrapov/devium/tree/master/R")
{
  if(require(RCurl)==FALSE){install.packages("RCurl");library(RCurl)} else { library(RCurl)}
  #get the names of all scripts to source
  obj<-getURL("https://github.com/dgrapov/devium/tree/master/R",ssl.verifypeer=FALSE)
  tmp<-strsplit(obj,'href=\"/')
  tmp2<-unlist(strsplit(as.character(unlist(tmp)),'class'))
  scripts<-gsub("/blob","",gsub('\" ',"",tmp2[grep("dgrapov/devium/blob/master/R/",tmp2)])) # fix formatting
  #add http for git hub
  scripts<-paste("https://raw.github.com/",scripts,sep="")
  sapply(1:length(scripts),function(i)
  {
    tryCatch( eval( expr = parse( text = getURL(scripts[i],
                                                ssl.verifypeer=FALSE) ),envir=.GlobalEnv),error=function(e){print(paste("can't load:",scripts[i]))})
  })
}

#convert vector to named list
namel<-function (vec){
  tmp<-as.list(vec)
  names(tmp)<-as.character(unlist(vec))
  tmp
}

# app startup
source.git.hub() 
