## Changelog:
# MH 0.0.3 2022-01-10:
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
			# k.opt <- try( suppressWarnings( optim( k.start, foptim, k, budget, cost2, cost1, model, target_parameter, env, verbose )$par ) )
# browser()			
			# MH 0.0.3 2021-11-30
			# require( CEoptim )
			# k.opt <- CEoptim(	f=foptim,
							# f.arg=list(budget=budget, cost2=cost2, cost1=cost1, model=model, target_parameter=target_parameter, env=env, verbose=verbose),
							# maximize=FALSE,
							# continuous=NULL,
							# discrete=list(categories=300L),
							# N=100L, rho=0.1, iterThr=1e4L, noImproveThr=5, verbose=TRUE )
			
# browser()			
			# MH 0.0.3 2021-11-30
			# require( kofnGA )
			# k.opt <- kofnGA(n=200,
						# k=1,
						# OF=foptim,
						# budget=budget, cost2=cost2, cost1=cost1, model=model, target_parameter=target_parameter, env=env, verbose=1 )
						## budget=30000, cost2=100, cost1=10, model=model, target_parameter=target_parameter, env=env, verbose=1 )
						

# browser()
			# MH 0.0.3 2022-01-10
			#### GEHT NICHT DA KEINE FUNKTION in Minimize() reinkann, muesste man versuchen zu tweaken, vllt. geht's, vllt. nicht
			# https://stackoverflow.com/questions/61047653/non-linear-optimisation-programming-with-integer-variables-in-r
			# require(CVXR)

			#modified for Stackoverflow integer MIQP ####
			#Solves, but terms not normalised by y and q respectively

			# Variables minimized over
			# N <- Variable(1, integer=TRUE)
# foptim <- function( N,   budget, cost2, cost1, model, target_parameter, env, verbose=TRUE )

			# Problem definition (terms not normalised by y and q respectively)
			# objective <- Minimize( compute_se_oertzen )
			# constraints <- list(x >= 0, y >= 0, p >= 0, q >= 0, 
								# x <= 67.314, y <= 78, p <= 76.11, q <= 86)
			# prob2.1 <- Problem(objective, constraints)

			# Problem solution
			# solution2.1 <- solve(prob2.1)
			# solution2.1$status
			# solution2.1$value
			# solution2.1$getValue(x)
			# solution2.1$getValue(y)
			# solution2.1$getValue(p)
			# solution2.1$getValue(q)
			
browser()
			# MH 0.0.3 2022-01-10
			require( rgenoud )
			min.T <- 3
			min.N <- 300
			# print( max.N <- floor( budget/((cost1+cost2)*min.T) ) )
			print( max.N <- 333 )
			
			x <- genoud( foptim, nvars=1, max=FALSE, data.type.int=TRUE,
			             Domains=matrix( c( min.N, max.N ), 1, 2 ),
						 boundary.enforcement=2, # no trespassing
                         solution.tolerance=0.001,
						 starting.values=round(mean(c(min.N,max.N))),
						 budget=budget, cost2=cost2, cost1=cost1, model=model, target_parameter=target_parameter, env=env, verbose=verbose )
			k.opt <- x$par
			

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
# Rfiles <- Rfiles[ !Rfiles %in% c("calc.power..R","app.R","Input - Single Process with a Single Indicator.R","Input - Two Processes with Two Indicator Each.R","Make RAM matrices.R") ]
# Rfiles <- file.path( Rdir, Rfiles )
# for( Rfile in Rfiles ){
	# source( Rfile )
# }

### test
# require( testthat )
# test_file("../tests/testthat/XXXXX.R")
