
library( plyr )
library( psych )
library( xlsx )

folder.main <- "c:/Users/martin/Dropbox/84_optimalclpm/04_martinhecht/R/testruns/01_testrun"

Rdir <- "c:/Users/martin/Dropbox/84_optimalclpm/04_martinhecht/R"
Rfiles <- list.files( Rdir, pattern="*.R" )
Rfiles <- Rfiles[ !Rfiles %in% c("app.R","Input - Single Process with a Single Indicator.R","Input - Two Processes with Two Indicator Each.R","Make RAM matrices.R") ]
Rfiles <- file.path( Rdir, Rfiles )
for( Rfile in Rfiles ){
	source( Rfile )
}

# example 2
model <- generate_model_example2()


# k.starts <- seq( 100, 110, 10 )
k.starts <- seq( 50, 200, 10 )


loop <- function( nr, k.start ){
		
		cat( paste0( "\n\n==================== ", nr, "/", length(k.starts), " ====================\n" ) ); flush.console()
		
		r <- data.frame(
				budget=20000,
				cost2=10,
				cost1=10,
				k.start)
		
		res <- try( calc.power( budget=r$budget, cost2=r$cost2, cost1=r$cost1, k.start=r$k.start, model=model, target_parameter="arcl_eta1eta2", timeout=60, verbose=TRUE ) )
		
		if( !inherits(res,"try-error") ){
			res <- cbind( r, as.data.frame( res ) )
		} else {
			res <- r
		}
		res
}
res.l <- mapply( loop, seq( along=k.starts ), k.starts, SIMPLIFY=FALSE )
res <- do.call( "rbind.fill", res.l )

res.descr <- describe( res[,c("N.opt","T.opt","power","run.time","n_optim_runs")] )

xlsx.file <- file.path( folder.main, "01_testrun.xlsx" )
write.xlsx( res.descr,        file=xlsx.file, sheetName = "res.descr", col.names = TRUE, row.names = TRUE )
write.xlsx( res, append=TRUE, file=xlsx.file, sheetName = "res", col.names = TRUE, row.names = TRUE )

save( res, res.descr, file=file.path( folder.main, "01_testrun.Rdata" ) )


