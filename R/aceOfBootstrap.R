`aceOfBootstrap` <-
function(x,numOfBootSamp=ceiling(length(x[,1])/2),sampleSize=35)
{
## number of data points
	N=length(x[,1])

## number of right hand side arguments
	rhs<-length(x[1,])-1

## preallocate outout
	phi<-array(data=0,dim=c(sampleSize,rhs+1))
	xout<-array(data=0,dim=c(sampleSize,rhs+1))


## here, bootstrap samples are drawn. Optimal transformations
## are calculated and transformed for each boostrap sample. 
## Transformed optimal transformations are averaged.

for(i in c(1:numOfBootSamp))
{
	## bootstrap array x 
		template<-sample(c(1:N),sampleSize,replace=TRUE)
		xBoot<-data.frame(x[template,])

	## call ace and calculate optimal transformation
		pp<-ace(xBoot[,2:(rhs+1)],xBoot[,1])

	## sort output

		## first, rank and sort response
			sty<-sort(pp$ty,index.return=TRUE)
			pp$ty[sty$ix]=1:sampleSize
			pp$ty=pp$ty/sampleSize
	
			sy<-sort(pp$y,index.return=TRUE)
			pp$y<-1:sampleSize
			pp$y<-pp$y/sampleSize
			
			pp$ty=pp$ty[sy$ix]
				
			phi[,1]=phi[,1]+pp$ty
	
			xout[,1]=xout[,1]+pp$y

		## second, rank and sort predictors
			for(j in c(1:rhs))
			{
				## rank and scale estimated optimal transformations
				stx<-sort(pp$tx[,j],index.return=TRUE)
				pp$tx[stx$ix,j]=1:sampleSize
				pp$tx[,j]=pp$tx[,j]/sampleSize
		
				## rank and scale corresponding x-values
				sx<-sort(pp$x[j,],index.return=TRUE)
				pp$x[j,]<-1:sampleSize
				pp$x[j,]<-(pp$x[j,])/sampleSize
			
				pp$tx[,j]=pp$tx[sx$ix,j]
				
				## average transformed optimal transformations
				phi[,j+1]=phi[,j+1]+pp$tx[,j];
				xout[,j+1]=xout[,j+1]+pp$x[j,]	

		}				
					
	}
## Output
phi<-phi/numOfBootSamp
xout<-xout/numOfBootSamp
erg<-NULL
erg$x<-xout
erg$phi<-phi
erg
}

