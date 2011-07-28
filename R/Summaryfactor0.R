Summaryfactor0<-function(x,maxsum=7){
k<-length(levels(x))
Table<-summary(na.omit(x),maxsum=maxsum)
if(k>maxsum){Table[1:(maxsum-1)]<-rev(sort(Table[1:(maxsum-1)]))}
else{Table<-rev(sort(Table))}
Table
}
