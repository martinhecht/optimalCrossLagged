## Changelog:
# MH 0.0.1 2021-11-03: copy from multi level optimal design project

## Documentation
#' @title
#' @description
#' @param
#' @param
#' @param
#' @return
#' @keywords internal

## Function definition
calc.budget. <- function( power, cost2, cost1, icc.y, icc.x, b2, b1 ){
		
		# Steffen, 7.4.21, 12:09
		b2 <- b2/sqrt(icc.x) #unstandardized b2
		b1 <- b1/sqrt(1-icc.x) #unstandardized b1
		
		# outputs = NA
		res <- list( "optclass"=NA, "optstud"=NA, "budget"=NA )
		
		  # input vector
		  i <- c(power, cost2, cost1, icc.y, icc.x, b2, b1)
		  if( any(is.na(i)) || any(i[1:5]<0) || any(i[4:5]>1) || any(i[1] >100) ) {
				#nothing 
		  } else {

			# ACHTUNG, Kopie aus Steffens Funktionen unten!!!!
			# muss hier rein kopiert werden, ansonsten geht's nicht, warum auch immer
  		    foptim2 <- function( budget,   cost2=100, cost1=10,   icc.y=0.2, icc.x=0.2, b2=0.5, b1=0.2, power, w=1, b0=0 ){
  		      
  		      k.opt <- try( suppressWarnings( optim( 2, foptim, k,   budget=budget, cost2=cost2, cost1=cost1,   icc.y=icc.y, icc.x=icc.x, b2=b2, b1=b1,   w=w, b0=b0 )$par ) )
  		      if( !inherits( k.opt, "try-error" )){
  		          n.opt <- (budget-cost2*k.opt)/(cost1*k.opt)
  		      
  		          pow <- 100*(1-pnorm( (1.96 - b2/sqrt(fvar.bayes.dir( icc.y=icc.y, icc.x=icc.x, b2=b2, b1=b1, k=k.opt, n=n.opt, w=w, b0=b0 ))), 0, 1) + pnorm( (-1.96 - b2/sqrt(fvar.bayes.dir( icc.y=icc.y, icc.x=icc.x, b2=b2, b1=b1, k=k.opt, n=n.opt, w=w, b0=b0 ))), 0, 1))

  		          diff <- ( pow - power )^2
  		      } else {
  		          diff <- NA
  		      }
  		          
  		      return( diff )
  		    }

    		  k <- NULL
    		  budget <- NULL
    		  budget.opt <- try( suppressWarnings( optim( 5e+03, foptim2, budget,   cost2, cost1,   icc.y, icc.x, b2, b1, power,   w=1, b0=0 )$par ) )
    		  k.opt <- try( suppressWarnings( optim( 2, foptim, k,   budget.opt, cost2, cost1,   icc.y, icc.x, b2, b1,   w=1, b0=0 )$par ) )
    		  
    		  if( any( c(inherits(budget.opt,"try-error"),inherits(k.opt,"try-error") ))){
    		    #nothing
    		  } else {
    		  
        		  n.opt <- (budget.opt-cost2*k.opt)/(cost1*k.opt)
        		  
        		  # results vector
        		  v <- c( k.opt, n.opt, budget.opt )
        		  
        		  if( any(is.na(v)) || any(v[1:2]<1) || v[3]<0 ){
        		    # nothing
        		  } else {
						res <- list( "optclass"=k.opt, "optstud"=n.opt, "budget"=budget.opt )
        		  }
    		  }
		  }
		  
		  res
}

### development
# Rfiles <- list.files( "c:/Users/martin/Dropbox/84_optimalclpm/04_martinhecht/R", pattern="*.R" )
# Rfiles <- Rfiles[ !Rfiles %in% "plot_density.R" ]
# for( Rfile in Rfiles ){
	# source( Rfile )
# }

### test
# require( testthat )
# test_file("../tests/testthat/XXXXX.R")
