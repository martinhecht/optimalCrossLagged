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
# Absamplen von x-Achse (between slope)
sample.x.axis <- function( budget=NULL, power=NULL, cost2, cost1, icc.y, icc.x, b2, b1, current.x, y.criterion, step=0.025 ){

		ys <- numeric(0)
		xs <- 0
		max.iter <- 1000
		continue <- TRUE
		i <- 0
		
		while (continue){
				# iter
				i <- i + 1

				# y fuer x berechnen
				ys[length(ys)+1] <- eval( parse( text=paste0( "calc.",y.criterion,"( ",
										ifelse(y.criterion %in% "power", "budget=budget,", ""), # bei power Berechnung budget uebergeben
										ifelse(y.criterion %in% "budget", "power=power,", ""),     # bei budget Berechnung power uebergeben
										" cost2=cost2, cost1=cost1, icc.y=icc.y, icc.x=icc.x, b2=xs[length(xs)], b1=b1 )$",y.criterion ) ) )
				
				# current maximal y criterion
				maxy.cur <- ifelse( all( is.na( ys ) ), NA, max( ys, na.rm=TRUE ) )
				# current minimal y criterion
				miny.cur <- ifelse( all( is.na( ys ) ), NA, min( ys, na.rm=TRUE ) )

				# stop conditions (oder )
				sc <- "length( ys[ is.na( ys ) ] ) >= 20"   # wenn zu viele NA erzeugt wurden (>=20) wird auch abgebrochen (wichtig damit das timeoutet)
				sc[length(sc)+1] <- "i >= max.iter"         # Iterationen sind letzte Notbremse
				# if ( y.criterion %in% "power" ) sc[length(sc)+1] <- "round( maxy.cur, 0 ) %in% 100"  # wenn Power von 100 erreicht
				if ( y.criterion %in% "power" ) sc[length(sc)+1] <- "ifelse( is.na(maxy.cur), FALSE, round(maxy.cur,1) >= 99.8 )"  # wenn Power von 100 erreicht
				if ( y.criterion %in% "budget" ) sc[length(sc)+1] <- "ifelse( is.na(miny.cur), FALSE, miny.cur < 5000 )"    # wenn Budget von 5000 erreicht
				sc.str <- paste( sc, collapse= " || " )
				continue <- !eval( parse( text=sc.str ) )
				
				# auf jeden Fall weitermachen (continue=TRUE) wenn das aktuelle x (b2) noch nicht erreicht ist
				# auch noch nen bisschen drueber damit in Grafik besser aussieht
				if( xs[i] < (abs(current.x)+0.05) ) continue <- TRUE
				
				if ( continue ) {
						# bei 0 rum enger samplen
						if ( xs[length(xs)] < step ) step. <- step/5 else step. <- step
						
						# x um ein step hoch
						xs[length(xs)+1] <- xs[length(xs)] + step.
				}
		}

		# x bei start nach negativ spiegeln
		xsneg <- -1*rev( xs[-1] )
		ysneg <- sapply( xsneg, function( x ) eval( parse( text=paste0( "calc.",y.criterion,"( ",
										ifelse(y.criterion %in% "power", "budget=budget,", ""), # bei power Berechnung budget uebergeben
										ifelse(y.criterion %in% "budget", "power=power,", ""),     # bei budget Berechnung power uebergeben
										" cost2=cost2, cost1=cost1, icc.y=icc.y, icc.x=icc.x, b2=x, b1=b1 )$",y.criterion ) ) )
										)
		
		# data frame
		dneu <- data.frame( "x" = c( xsneg, xs ), "y" = c( ysneg, ys ) )
		# NA raus
		dneu <- na.omit(dneu)
		attr( dneu, "na.action" ) <- NULL
		rownames( dneu ) <- seq( along=rownames( dneu ) )
		
		dneu
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
