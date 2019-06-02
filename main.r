#skander evolutionary model
#02-06-2019
library(pacman); p_load(data.table,magittr); dat=data.table(iris);data(iris)
to_char=function(x)deparse(substitute(x))
to_run=function(x){f1 <-unlist(strsplit( x,"\n"))  ;    lapply(unlist(f1),function(x)eval(parse(text=x)))  }  #should update to recusive with working mem buffer, fix nopw


X=names(dat)[1:4]
Y=unique((iris$ Species))
tdasset=lapply(lapply( unique((Y)), function(x)which(x ==iris$ Species)), function(x)sample(x,1))
operator1 = c('-','+','*','/');operator2 = NA; #to be extended with other things
tmpdat=dat[unlist(tdasset),1:5]


##creating a gene
complexity =10##important var
gene1 =  paste('with(dat,'		,paste( sample(X,size=complexity,replace=TRUE)),')'   )

expression1=lapply(  1:220,function(x) sample(operator1,size=complexity,replace=TRUE))
predictorsyntax=lapply(expression1,function(x) expression_f(gene1,x  ))

##creating epression profile 
expression_f=function(vectx,opsec){ 
tmp=paste(vectx, sample(operator1,replace =TRUE,size =length(vectx) ) ,collapse =' ')
return(substr(tmp,0,(nchar(tmp)-2)))	} 


##runclassifier on gene expression
m1=matrix(   unlist(lapply(predictorsyntax,to_run )) , ncol= 220)  
dim(m1)
# plot
m1=data.table(cbind(Y=iris$Species, m1))
Y=iris$Species
models=apply(  m1, 2 , function(x) glm(Y~x, family=binomial(link="probit")) )


lapply(models, summary)
glm(Y~., data=m1, family=binomial(link="logit"))



par(mfrow=c(3,5))
for(cols in ((1:15)+1))
{
boxplot(m1[[cols]]~Y , main= paste('gene:' ,cols))

}

