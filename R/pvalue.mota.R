`pvalue.mota` <-
function(object,numberOfIterations=50)
{
## hand over arguments
S<-attr(object,'S')
x<-attr(object,'x')
Hout<-attr(object,'Hout')

## preliminaries
pValue<-array(data=NaN,dim=c(length(S[,1]),1))
NOP<-length(S[1,])

for (ix in 1:length(S[,1]))
{
pCounter<-0

## find nonzero entries in S
ixOfTrue<-which(S[ix,]==TRUE)

	for(i in 1:numberOfIterations)
	{
		## permute x[,ix]
		xIxPermutated<-sample(x[,ix],length(x[,ix]),replace=FALSE)
		xPermutated<-x
		xPermutated[,ix]=xIxPermutated


		## call mota
		motaOut<-mota(xPermutated)
		Sout<-attr(motaOut,"S")
		

		## investiage if found matrix Sout corresponds with
		## input matrix S. If 'yes' count a hit
		
		compare<-(Sout[ix,]==S[ix,])
		compLeng<-which(compare==TRUE)
		compLeng<-length(compLeng)
		if(compLeng==NOP)
			{
			pCounter<-pCounter+1
			}
	}

pValue[ix]<-(pCounter/numberOfIterations)

}
# Output
K<-NULL
attributes(K)<-list(class="mota",x=x,Hout=Hout,p=pValue,S=S)
erg<-NULL
erg<-K
}

