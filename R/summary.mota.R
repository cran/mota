`summary.mota` <-
function (object, ...) 
{
	## hand over arguments
 	S <- attr(object, "S")
	x <- attr(object, "x")
    	p <- attr(object,"p")

    ## internal variables
    cv <- array(data = NaN, dim = c(length(S[, 1]), 1))
    scr <- array(data = NaN, dim = c(length(S[, 1]), 1))
    

	## calculate r2
	r2<-r2.mota(object)


	## calculate coefficient of variation
 	Var <- var(x)
    	for (i in c(1:length(S[, 1])))
		{
      	cv[i] = sqrt(Var[i, i])/mean(x[, i])
	    	}

	## calculate score
    for (i in c(1:length(S[, 1]))) {
        if (cv[i] >= 0.1 && r2[i] > 0.95) {
            scr[i] <- "***"
        }
        else {
            if (cv[i] >= 0.1 && r2[i] > 0.9) {
                scr[i] <- "**"
            }
            else {
                if (r2[i] > 0.9) {
                  scr[i] <- "*"
                }
            }
        }
    }
	cat('\n')
	if(is.null(p))
	{
    	niceFrame = data.frame(S, r2, cv, scr)
    	print(niceFrame, digits = 2)
	}
	else
	{
	niceFrame = data.frame(S, r2, cv,p,scr)
    	print(niceFrame, digits = 2)
	}

	## print score table
	cat('\n (*)\t r2>0.90\n (**)\t r2>0.90 cv>0.1 \n (***)\t r2>0.95 cv>0.1  \n\n')
}
