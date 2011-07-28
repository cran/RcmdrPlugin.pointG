Summaryvariable0<-function(x,maxsum=7,digits=max(3, getOption("digits") - 
    3)){
if(is.numeric(x)){ttt<-Summarynumeric0(x,digits=digits)}
else{

if(is.ordered(x)){ttt<-Summaryordered0(x,maxsum=maxsum)}

else{if(is.factor(x)){ttt<-Summaryfactor0(x,maxsum=maxsum)}

else{ttt<-NA}
}
}
nnaa<-round(sum(is.na(x)))
names(nnaa)<-"?"
c(ttt,nnaa)
}
