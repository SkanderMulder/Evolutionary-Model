#skander evolutionary model
#02-06-2019
library(pacman); p_load(data.table,magittr); dat=data.table(iris);data(iris)
to_char=function(x)deparse(substitute(x))
to_run=function(x){f1 <-unlist(strsplit( x,"\n"))  ;    lapply(unlist(f1),function(x)eval(parse(text=x)))  }  #should update to recusive with working mem buffer, fix nopw


X=names(dat)[1:4]
Y=unique((iris$ Species))
tdasset=lapply(lapply( unique((Y)), function(x)which(x ==iris$ Species)), function(x)sample(x,1))
operator1 = c('-','+','*') #to be extended with other things
tmpdat=dat[unlist(tdasset),1:5]


##creating a gene
complexity =10 ##important var
gene1 =  paste(
		 paste('with(tmpdat,'
		,paste( sample(X,size=complexity,replace=TRUE), sample(operator1,size=complexity ,replace =TRUE)   )
		 ),'0)')
		 
phenotype=apply(setDT(to_run(gene1)),1,mean)






