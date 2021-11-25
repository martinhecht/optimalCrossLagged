## Changelog:
# MH 0.0.2 2021-11-25: update
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
calc.power. <- function( budget, cost2, cost1, k.start, model, target_parameter, env, verbose=TRUE ){

# browser()
		
		# Steffen, 7.4.21, 12:09
		# b2 <- b2/sqrt(icc.x) #unstandardized b2
		# b1 <- b1/sqrt(1-icc.x) #unstandardized b1
		
		# outputs = NA
		res <- list( "N.opt"=NA, "T.opt"=NA, "power"=NA )
		
		# input vector
		# i <- c(budget, cost2, cost1, icc.y, icc.x, b2, b1)
		i <- c( budget, cost2, cost1 )
		# if( any(is.na(i)) || any(i[1:5]<0) || any(i[4:5]>1) ){
		if( any(is.na(i)) || any(i[1:3]<0) ){
				#nothing 
		} else {
			
			# Using these values, the optimal number of persons (k.opt) and the optimal number of time points per class (n.opt) are computed as:
		
			if( verbose ) { cat( "starting optim","\n" ); flush.console() }
		
			k <- NULL
			# k.opt <- try( suppressWarnings( optim( 2, foptim, k,   budget, cost2, cost1,   icc.y, icc.x, b2, b1,   w=1, b0=0 )$par ) )
			k.opt <- try( suppressWarnings( optim( k.start, foptim, k, budget, cost2, cost1, model, target_parameter, env, verbose )$par ) )
			
			if( verbose ) { cat( "end of optim","\n" ); flush.console()}
			
			if( any( c(inherits(k.opt,"try-error") ))){
					#nothing
			} else {  
				k.opt <- round( k.opt )
				
				n.opt <- round( (budget-cost2*k.opt)/(cost1*k.opt) )
				# b2 true value (user Eingabe) des Target Parameters
				# pow.opt <- fpow( icc.y, icc.x, b2, b1, k.opt, n.opt, w=1, b0=0 )
				pow.opt <- fpow( k.opt, n.opt, model, target_parameter, verbose )
		
				# results vector
				v <- c( k.opt, n.opt, pow.opt )
				
				if( any(is.na(v)) || any(v[1:2]<1) || v[3]<0 || v[3]>100 ){
						#nothing
				} else {
						res <- list( "N.opt"=k.opt, "T.opt"=n.opt, "power"=pow.opt )
				}
			}
		}
		
		res
}

### development
# Rdir <- "c:/Users/martin/Dropbox/84_optimalclpm/04_martinhecht/R"
# Rfiles <- list.files( Rdir, pattern="*.R" )
# Rfiles <- Rfiles[ !Rfiles %in% c("calc.power..R","Input - Single Process with a Single Indicator.R","Input - Two Processes with Two Indicator Each.R","Make RAM matrices.R") ]
# Rfiles <- file.path( Rdir, Rfiles )
# for( Rfile in Rfiles ){
	# source( Rfile )
# }

### test
# require( testthat )
# test_file("../tests/testthat/XXXXX.R")
