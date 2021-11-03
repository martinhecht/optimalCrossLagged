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
theme <- theme_bw() +
         theme( axis.title=element_text(size=14, face="bold"),
                axis.text=element_text(size=14, colour="black"),
                panel.grid=element_blank(),
                panel.grid.major=element_line(colour="lightgrey", size=0.25),
                panel.grid.minor=element_blank(),
				legend.text=element_text(size=12),
                legend.title=element_text(size=14, face = "bold"),
                legend.justification=c(1,1),
                legend.position=c(0.98,0.98),
                legend.key=element_rect(linetype=0),
                legend.key.height=unit(15, "points"),
                legend.key.width=unit(40, "points"),
                legend.spacing.x=unit(0, "points"),
                strip.text.x=element_text(size=12, face="bold"),
                strip.text.y=element_text(size=12, face="bold"),
                strip.background=element_rect(colour="black", fill="white"),
                plot.background=element_rect(fill="white"),
                plot.title=element_text(face="bold", size=16)
               )


### development
# Rfiles <- list.files( "c:/Users/martin/Dropbox/68_causalSEM/04_martinhecht/R", pattern="*.R" )
# Rfiles <- Rfiles[ !Rfiles %in% "plot_density.R" ]
# for( Rfile in Rfiles ){
	# source( Rfile )
# }

### test
# require( testthat )
# test_file("../tests/testthat/XXXXX.R")
