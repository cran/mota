`plot.mota` <-
function (x, ...) 
{
    object<-x
    S <- attr(object, "S")
    x <- slot(object, "x")
    options <- list(...)
    option <- options[[1]]

    	## internal variables
    	NOP<-length(S[1,])
	dev <- array(data = NaN, dim = c(length(S[, 1]), 1))
	
	
	############
	## ace-plots
	if(is.numeric(option))
		{
		ix<-option
    		if (ix > length(S[, 1]))
			{
        		print("Index must not be larger than number of parameters!")
        		return()
    			}
    
		ixOfTrue <- which(S[ix, ] == TRUE)
    		if(length(ixOfTrue) == 1)
			{
        		print("Can not plot aceplots with only one parameter/column")
        		return()
			}
    			
    		NumberOfPlots <- length(ixOfTrue)
    		ixOfTrueHelp <- which(ixOfTrue != ix)
    		ixOfTrue <- ixOfTrue[ixOfTrueHelp]
    
		aceOut <- ace(x[, ixOfTrue], x[, ix])
    		tx <- aceOut$tx
    		get(getOption("device"))()
    		help <- ceiling(sqrt(NumberOfPlots))
    	
		if (help^2 - help >= NumberOfPlots) 
			{
        		par(mfrow = c(help, help))
    			}
    		else 
			{
        		par(mfrow = c(help, help))
    			}
    		par(mar=c(4,3.5,2,2),mgp=c(2,0.5,0))
		plot(aceOut$y, aceOut$ty, xlab = paste("p", ix), ylab = expression(Theta),tck=0,cex.lab=1.2)
    		# nice ticks
			tckLeng=0.03
			axis(side=1,tck=tckLeng,labels=FALSE)
			axis(side=2,tck=tckLeng,labels=FALSE)
			axis(side=3,tck=tckLeng,labels=FALSE)
			axis(side=4,tck=tckLeng,labels=FALSE)
		for (i in c(1:length(ixOfTrue))) 
			{
			par(mar=c(4,3.5,2,2),mgp=c(2,0.5,0))
        		plot(x[, ixOfTrue[i]], tx[, i], xlab = paste("p", ixOfTrue[i]), ylab = expression(Phi),tck=0,cex.lab=1.2)
    			# nice ticks
			tckLeng=0.03
			axis(side=1,tck=tckLeng,labels=FALSE)
			axis(side=2,tck=tckLeng,labels=FALSE)
			axis(side=3,tck=tckLeng,labels=FALSE)
			axis(side=4,tck=tckLeng,labels=FALSE)
			}
	}

	## addtional plot options
	if(is.character(option))
	{
		#################
	  	## plot deviances
	  	if(option=="dev")
		{
		par(mfrow=c(2,1))
	
		## centre data
		xCentred<-x
		for(i in 1:NOP)
			{
			xCentred[,i]<-xCentred[,i]-mean(xCentred[,i])
			}

		## boxplots
		par(mar=c(4,4,2,2),mgp=c(2,0.5,0))
		xCentred=data.frame(xCentred)
		names(xCentred)=c(1:length(xCentred[1,]))
		boxplot(xCentred,ylab="centred values",xlab="parameters")
		
		## plot deviances in a semi-logarithmic plot
		 Var <- var(x)
    		for (i in c(1:length(S[, 1])))
			{
        		dev[i] = sqrt(Var[i, i])/mean(x[, i])
			}
		par(mar=c(4,4,1,2),mgp=c(2,0.5,0))
		plot(1:NOP,dev,lab=c(NOP,5,7),xlab="parameters",ylab="log(deviances)",log="y",tck=0)
			
			# nice ticks
			tckLeng=0.03
			axis(side=1,tck=tckLeng,labels=FALSE)
			axis(side=2,tck=tckLeng,labels=FALSE)
			axis(side=3,tck=tckLeng,labels=FALSE)
			axis(side=4,tck=tckLeng,labels=FALSE)

		## include rating in form of colored circles
		r2<-r2.mota(object)
		if(is.null(options$r2Threshold))
			r2Threshold<-0.95
		else
			r2Threshold<-options$r2Threshold
		for(i in 1:NOP)
			{
			if(r2[i]>r2Threshold)
				{
				points(i,dev[i],col="green")
				}
			}
		
		}
		################
		## pairs
	 	if(option=="pairs")
			{
			pairs(x)
			}

		################
		## scatter plots	
		if(option=="scatter" || option=="scatter.movie")
			{
			ix<-options$ix
			if(is.null(ix))
				columns<-options$col
			else			
				columns<-which(S[ix,]==1)

		
			if(is.vector(columns) && length(columns)==3)
				{
				x1<-x[,columns[1]]
				x2<-x[,columns[2]]	
				x3<-x[,columns[3]]
			
				df<-data.frame(x1,x2,x3)
				
				if(option=="scatter.movie")
					{
					for(i in seq(1,180,1))
						{
						scatterplot3d(df,angle=i)
						}
					scatterplot3d(df)
					}
				else
					{
					
					if(length(options)<3)
						angle<-40
					else
						angle<-options$angle
					
					scatterplot3d(df,angle=angle)
					}
				}
			
			}
	}



}
