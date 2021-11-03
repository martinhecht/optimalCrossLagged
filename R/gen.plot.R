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
gen.plot <- function(d,xvar,yvar,xlab,ylab,y.criterion,b2,power=NULL,budget=NULL){
		
		
		# x/y Achsen
		if( y.criterion %in% "power" ) y.breaks <- seq( 0, 100, 20 )
		if( y.criterion %in% "budget" ) {
				# y.breaks <- seq( 0, max( d$y, na.rm=TRUE ), length.out=5 )
# browser()				
				y.curr <- get(yvar)
				# y.achse.max <- min( c( y.curr*10, max( d$y, na.rm=TRUE ) ) )
				y.achse.max <- max( d$y, na.rm=TRUE )
				# d2 <- d[ d$y <= y.achse.max, ]
				# y.achse.max <- max( d2$y, na.rm=TRUE )
				# y.achse.max.zoom.in <- min( c( max( d2$y[d2$x < 0], na.rm=TRUE ), max( d2$y[d2$x > 0], na.rm=TRUE ) ) )
				y.achse.max.zoom.in <- min( c( y.curr*5, y.achse.max ) )
				
				round.stellen <- (nchar(round(y.curr,0))-2)
				if ( round.stellen > 0 ) round.stellen <- -1*round.stellen else round.stellen <- 0
				
				# y.breaks <- seq( 0, round( y.achse.max, round.stellen ), length.out=5 )
				by <- round( y.achse.max.zoom.in/5, round.stellen )
				y.breaks <- seq( 0, ceiling_dec( y.achse.max, round.stellen ), by )
				# noch eins hinten anhaengen
				y.breaks <- c( y.breaks, y.breaks[length(y.breaks)]+by )
				# y.achse.max.ceil <- ceiling_dec( y.achse.max, round.stellen )
				# y.breaks <- seq( 0, y.achse.max.ceil, length.out=5 )
				
				# y.achse.max.zoom.in.rounded <- round( y.achse.max.zoom.in, round.stellen )
				y.achse.max.zoom.in.thresh <- y.breaks[ y.breaks > y.achse.max.zoom.in ][1]
# browser()				
		}
		y.labs <- formatC( y.breaks, format="f", digits=0 )
		y.labs[1] <- paste0( y.labs[1], "\n" )
		
		extr <- max(d$x, abs(d$x))
		x.max.abs <- ceiling_dec( extr, 2 )
		x.breaks <- seq( -1*x.max.abs, x.max.abs , length.out=11 )
		x.labs <- formatC( x.breaks, format="f", digits=2 )
		# x.labs[1] <- ""
		
		# Farbe fuer rounds, letzte (aktuelle) Kurve schwarz, davor die letzten 5 fade out
		# cols <- c( rep( "#c5c5c5", length( levels( d$round ) ) - 1 ), "#000000" )
		n.shades <- length( levels( d$round ) ) - 1
		last.col <- "#000000"
		if ( n.shades > 0 ) {
				cols <- c( gray.colors(n=n.shades , start = 0.7, end = 0.9, gamma = 2.2, alpha=0.5, rev = TRUE), last.col )
		} else {
				cols <- last.col
		}

		# graphic
		p <- ggplot( data=d, aes( y=y, x=x, color=round ) )

		# vertikale/horizontale Linie fuer aktuellen Punkt
		p <- p + eval( parse( text=paste0( "geom_vline(xintercept = ",xvar,", linetype='solid', color = 'red', size=0.5)" ) ) )
		p <- p + eval( parse( text=paste0( "geom_hline(yintercept = ",yvar,", linetype='solid', color = 'red', size=0.5)" ) ) )
		
		if( y.criterion %in% "power" ) p <- p + geom_line(size=1.5, show.legend=FALSE)
		if( y.criterion %in% "budget" ){
				# bei 0 splitten, damit kein verbundener Strich
				p <- p + geom_line(data=d[d$x < 0, ], size=1.5, show.legend=FALSE)
				p <- p + geom_line(data=d[d$x > 0, ], size=1.5, show.legend=FALSE)
		}
		
		# p <- p + geom_line(size=1.5)
		p <- p + scale_color_manual( values = cols )

		# aktueller Punkt
		p <- p + eval( parse( text=paste0( "geom_point( aes( y=y, x=x ), data=data.frame( x=",xvar,", y=",yvar," ), color = 'red', size=3 )" ) ) )
		p <- p + xlab( paste0( "\n", xlab ) )
		p <- p + ylab( paste0( ylab, "\n" ) )

		scaleycontmulty <- 0
		if( y.criterion %in% "power" ) scaleycontmulty <- 0.008
		if( y.criterion %in% "budget" ) scaleycontmulty <- 0.00

		p <- p + scale_x_continuous( limits=c(x.breaks[1], x.breaks[length(x.breaks)]), breaks=x.breaks, labels=x.labs, expand = expand_scale(mult = c(0,0.00)) ) +
				scale_y_continuous( limits=c(y.breaks[1], y.breaks[length(y.breaks)]), breaks=y.breaks, labels=y.labs, expand = expand_scale(mult = c(0.0,scaleycontmulty)) )
		p <- p + theme

		# if( y.criterion %in% "budget" ) p <- p + coord_cartesian(ylim=c(0,y.achse.max.zoom.in))
		if( y.criterion %in% "budget" ) p <- p + coord_cartesian(ylim=c(0,y.achse.max.zoom.in.thresh))

		# https://stackoverflow.com/questions/18252827/increasing-area-around-plot-area-in-ggplot2
		# top right bottom left

		p <- p + theme( plot.margin = unit(c(5.5, 15.5, 1.5, 1.5), "points") )

		p
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
