`mota` <-
function(x,numOfBootSamp=35,sampleSize=floor(length(x[,1])/2),threshold1=0.01,threshold2=0.07,threshold3=0.08,progress=FALSE)
{

## Preliminaries -------------------------------------------------------

## set history H to zero
	H<-0

## number of parameters
	NOP<-length(x[1,])

## strong function relations
	S<-array(data=0,dim=c(NOP,NOP))

## Hc contains all current H values for the available parameters
	Hc<-rep(0,NOP)

## Hout final value of the testfunction. If there is no functional relation the
## entry equals NaN
Hout<-array(data=0,dim=c(1,NOP))

## ----------------------------------------------------------------------

## Algorithm
for(i in c(1:NOP))
	{
	
	m<-0
	rhs<-0
	xc2=x[,i]
	## which parameters are available
		available<-rep(1,NOP)
		available[i]<-0	
		
	## number of available parameters
		numOfAvailable<-sum(available)

	## Combine the i-th unimportant parameter with ALL other
	## parameters. the maximum number of parameter which can be combined
	## is NOP-1
	for(r in c(1:(NOP-1)))
	{
	rhs<-rhs+1
	k<-0
    
  for(j in c(1:numOfAvailable))
		{
		
		k<-k+1
		check<-0
		while(check==0)
			{
			ifelse(available[k]==0,k<-k+1,check<-1)
			}
		
		xc<-data.frame(xc2,x[,k])
		newNumOfP<-length(xc[1,])
		
		## test functional relation
		
    pp<-aceOfBootstrap(xc,numOfBootSamp,sampleSize)
   	Hc[k]<-var(pp$phi[,newNumOfP])
		}
		
	## Set to zero those parameters which are not discussed at this very moment
		
		k<-1
		check<-0
		for(k in c(1:NOP))
			{
			if(available[k]==0){Hc[k]<-0}
			}
		
	
	maxHc<-max(Hc)
	ixMaxHc<-which.max(Hc)
	
	if(maxHc<threshold1 && H==0)
		{
		S[i,i]<-1
		Hout[i]<-maxHc
		break
		}
		
	## a strong functional realtion has probably been found
		xc2=data.frame(xc2,x[,ixMaxHc])
		m<-m+1
	## update list of available parameters
		available[ixMaxHc]<-0
		numOfAvailable<-sum(available)

	## now, calculate the Hbar to check for a strong functional relation
		pp<-aceOfBootstrap(xc2,numOfBootSamp,sampleSize)
    		Hcc<-rep(0,length(xc2[1,]))
		
		for(k in c(1:length(xc2[1,])))
			{
				Hcc[k]<-var(pp$phi[,k])
			}
	
		Hbar<-mean(Hcc)
		
		## Check if a strong functional relation has been detected yet
		if(H==0)
		{
			if(Hbar>threshold2)
			{    	
				if(NOP==2)
				{
				S[i,]<-1
				Hout[i]<-Hcc[1]
				break
				}
										
				HbarHist<-Hbar
				HccHist<-Hcc[1]
				availableHist<-available
				H<-1;
			}
			## Add new parameters
			else
			{					
				## If there are no parameters left to add, break!
				if(r==NOP-1)
					{
	   				S[i,i]<-1
					Hout[i]<-Hcc[1]
					break
					}
			}
		}
		else
		{				
			## proceed here if it is not the first run for parameter i
			if(Hbar>HbarHist || Hbar>threshold3)
				{	
					if(newNumOfP==NOP)
						{
						
						## create proper entry for output S
						k<-1
						for(k in c(1:NOP))
						{
							if(available[k]==0)
								{S[i,k]=1}
						}
						H<-0
						Hout[i]<-Hcc[1]
						break
						}
					
					## Keep in mind what you have just done
					HbarHist<-Hbar
					HccHist<-Hcc[1]
					availableHist<-available
					
				}
				else
				{
				      
					## create proper entry for output S
					k<-1
					for(k in c(1:NOP))
					{
						if(availableHist[k]==0)
							{S[i,k]=1}
					}
					Hout[i]<-HccHist
					H<-0
					break
				}
			}
	
		}
			if(progress==TRUE)
		  {
		  mota.progress(i,NOP)
		  }
	
		}
# Output
K<-NULL
attributes(K)<-list(class="mota",x=x,Hout=Hout,S=S)
erg<-NULL
erg<-K

	

}

