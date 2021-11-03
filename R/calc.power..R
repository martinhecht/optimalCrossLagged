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
calc.power. <- function( budget, cost2, cost1, icc.y, icc.x, b2, b1 ){
		
		# Steffen, 7.4.21, 12:09
		b2 <- b2/sqrt(icc.x) #unstandardized b2
		b1 <- b1/sqrt(1-icc.x) #unstandardized b1
		
		# outputs = NA
		res <- list( "optclass"=NA, "optstud"=NA, "power"=NA )
		
		# input vector
		i <- c(budget, cost2, cost1, icc.y, icc.x, b2, b1)
		if( any(is.na(i)) || any(i[1:5]<0) || any(i[4:5]>1) ){
				#nothing 
		} else {
			
			# Using these values, the optimal number of classes (k.opt) and the optimal number of students per class (n.opt) are computed as:
		
			k <- NULL
			k.opt <- try( suppressWarnings( optim( 2, foptim, k,   budget, cost2, cost1,   icc.y, icc.x, b2, b1,   w=1, b0=0 )$par ) )
			
			if( any( c(inherits(k.opt,"try-error") ))){
					#nothing
			} else {  
				n.opt <- (budget-cost2*k.opt)/(cost1*k.opt)
				pow.opt <- fpow( icc.y, icc.x, b2, b1, k.opt, n.opt, w=1, b0=0 )
		
				# results vector
				v <- c( k.opt, n.opt, pow.opt )
				
				if( any(is.na(v)) || any(v[1:2]<1) || v[3]<0 || v[3]>100 ){
						#nothing
				} else {
						res <- list( "optclass"=k.opt, "optstud"=n.opt, "power"=pow.opt )
				}
			}
		}
		
		res
}

### development
# Rfiles <- list.files( "c:/Users/martin/Dropbox/68_causalSEM/04_martinhecht/R", pattern="*.R" )
# Rfiles <- Rfiles[ !Rfiles %in% "plot_density.R" ]
# for( Rfile in Rfiles ){
	# source( Rfile )
# }

### test
# require( testthat )
# test_file("../tests/testthat/XXXXX.R")
