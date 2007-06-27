`r2.mota` <-
function(object,...)   
{    
	  S<-attr(object,'S')
        x<-attr(object,'x')
	  r2 <-array(data=NaN,dim=c(length(S[,1]),1))
		
		for(i in c(1:length(S[,1])))
		{
		
			## where are the non zero entries
				ixOfOnes<-which(S[i,]==TRUE)
			if(length(ixOfOnes)!=1)
			{
				ixOfOnes<-which(ixOfOnes!=i)
			
			## call ace
				aceOut<-ace(x[,ixOfOnes],x[,i])
			## calculate r2		
				response<-aceOut$ty
				predictors<-aceOut$tx
				SSres<-0
				
				## calculate SSres
					for (j in c(1:length(predictors[,1])))
					{
						SSres<-SSres+(response[j]-sum(predictors[j,]))^2
					}
				## calculate SStot
					SStot<-sum((response-mean(response))^2)
				
				r2[i]<-1-(SSres/SStot)
			}
			else
			{
			r2[i]<-0
			}
		}
erg<-NULL
erg<-r2
}

