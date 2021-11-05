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

require( shiny )
require( ggplot2 )
require( R.utils ) # withTimeout(), https://stackoverflow.com/questions/7891073/time-out-an-r-command-via-something-like-try
# source( "http://amor.cms.hu-berlin.de/~psymetho/optimalDesign/optimalDesign_functions_v3.R" )

# ui <- fluidPage( HTML( "<h2 style='text-align: center; width: 500px'><b>Calculation of Optimal Designs for<br>Classroom Climate Studies</b></h2><br>" ),
# v2
# ui <- fluidPage( HTML( "<h2 style='text-align: center; width: 100%'><b>Calculation of Optimal Designs for<br>Classroom Climate Studies</b></h2><br>" ),
ui <- fluidPage( HTML( "<h2 style='text-align: center; width: 100%'><b>Calculating Optimal Designs for<br>Classroom Climate Studies</b></h2><br>" ),

                 navbarPage(title=NULL,
					
					tabPanel("Scenario 1",

							sidebarPanel(

								tags$head(
		                          # tags$style(HTML(".navbar {width: 530px;}"))
		                          tags$style(HTML(".navbar {width: 100%;}"))
		                        ),   
		                        
		                        # h4( HTML( "<div style='width: 500px;'><b><u>Scenario 1</u>: Maximize the power to detect the relationship between classroom climate and the outcome variable <i>given a fixed study budget</i></b></div>" ) ),	
		                        # v2:
								# h4( HTML( "<div style='width: 100%;'><b><u>Scenario 1</u>: Maximize the power to detect the relationship between classroom climate and the outcome variable <i>given a fixed study budget</i></b></div>" ) ),	
		                        h4( HTML( "<div style='width: 100%;'><b><u>Scenario 1</u>: Maximizing the power to detect the relation between classroom climate and the outcome variable (i.e., the between slope) <i>given a fixed budget</i></b></div>" ) ),	
		                        
		                        h4( HTML( "&nbsp;" ) ),
		                        
		                        ## Input-Boxen rechts vom Text
		                        # https://community.rstudio.com/t/how-to-put-the-inline-textinput-label-at-the-left-of-the-column-and-the-textinput-box-at-the-right-of-the-column/24519
		                        
		                        ## Boxen-Groesse von inputNumeric aendern
		                        # https://community.rstudio.com/t/how-to-customize-selectinput-in-shiny-box-height-and-width/36378
		                        # https://stackoverflow.com/questions/49873705/css-styling-for-select-and-numeric-input
		                        
		                        fluidRow(
		                          tags$head(
		                            tags$style(type="text/css","label{ display: table-cell; text-align: left; vertical-align: middle; padding: 10px; } .form-group { display: table-row; background-color: #eaeaea;} .form-control.shiny-bound-input, .selectize-input {height: 35px;width: 100px;padding-top: 5px;text-align: center;}") 
		                          ) ),			    
		                        
		                        numericInput( inputId = "budget",
		                                      label = "Budget",
		                                      value = 20000,
		                                      min = 0,
		                                      step = 5000,
		                                      width = '140px' ),
		                        numericInput( inputId = "costsperclass",
		                                      label = "Costs per class",
		                                      value = 68.20,
		                                      min = 0,
		                                      step = 10,
		                                      width = '140px' ),
		                        numericInput( inputId = "costsperstudent",
		                                      label = "Costs per student",
		                                      value = 7.05,
		                                      min = 0,
		                                      step = 1,
		                                      width = '140px'),
		                        HTML( "<p style='margin-bottom:10px;'>" ),				
		                        numericInput( inputId = "iccoutcome",
		                                      # v2:
											  # label = HTML( "ICC of the outcome variable<sup><small>&#8224;</small></sup>" ),
		                                      label = HTML( "ICC of outcome variable<sup><small>&#8224;</small></sup>" ),
		                                      value = 0.20,
		                                      min = 0,
		                                      max = 1,
		                                      step = 0.01,
		                                      width = '200px'),
		                        numericInput( inputId = "iccclimate",
		                                      # v2:
											  # label = HTML( "ICC of the students' ratings of classroom climate&nbsp;&nbsp;&nbsp;" ),
		                                      label = HTML( "ICC of student ratings of classroom climate&nbsp;&nbsp;&nbsp;" ),
		                                      value = 0.20,
		                                      min = 0,
		                                      max = 1,
		                                      step = 0.01,
		                                      width = '340px'),
		                        # v2:
								# p( HTML( "<small><sup>&#8224;</sup> not adjusted for the students' ratings of classroom climate</small>" ) ),
		                        p( HTML( "<small><sup>&#8224;</sup> not adjusted for classroom climate</small>" ) ),
		                        numericInput( inputId = "betweenslope",
		                                      # v2:
											  # label = HTML( "Between slope<sup><small>&#8225;</small></sup>&nbsp;&nbsp;&nbsp;" ),
		                                      label = HTML( "Standardized between slope<sup><small>&#8225;</small></sup>&nbsp;&nbsp;&nbsp;" ),
		                                      value = 0.20,
		                                      step = 0.01,
		                                      width = '340px'),
		                        numericInput( inputId = "withinslope",
		                                      # v2:
											  # label = HTML( "Within slope<sup><small>&#8225;</small></sup>" ),
		                                      label = HTML( "Standardized within slope<sup><small>&#8225;</small></sup>" ),
		                                      value = 0.20,
		                                      step = 0.01,
		                                      width = '340px'),
		                        # HTML( "<div style='width: 460px;'><small><sup>&#8225;</sup> standardized with respect to the total variances of both the outcome variable and the students' ratings of classroom climate</small></div>" ),
		                        # v2:
								# HTML( "<div style='width: 100%;'><small><sup>&#8225;</sup> standardized with respect to the total variances of both the outcome variable and the students' ratings of classroom climate</small></div>" ),
		                        # HTML( "<div style='width: 100%;'><small><sup>&#8225;</sup>The between slope is standardized by standardizing the outcome variable with respect to its total SD and by standardizing the level-2 part of the classroom climate variable with respect to the estimated level-2/between classroom climate SD. The within slope is standardized by standardizing the outcome variable with respect to its total SD and by standardizing the level-1 part of the classroom climate variable with respect to the estimated level-1/within classroom climate SD (Marsh et al., 2009).</small></div>" ),
		                        HTML( "<div style='width: 100%;'><small><sup>&#8225;</sup> standardized with respect to the total variance of the outcome variable but only the between/within variance of the classroom climate variable (Marsh et al., 2009)</small></div>" ),
		                        
		                        HTML( "<p style='margin-bottom:30px;'>" ),
		                        
		                        htmlOutput("results")
								
							  , width = 6 ),
							  
							  mainPanel( plotOutput("plot1"), width = 6 )
							
							), # Klammer tabPanel
		          
                  tabPanel("Scenario 2",
                           # mainPanel(
                             
							sidebarPanel(
							 
                             tags$head(
                               # tags$style(HTML(".navbar {width: 530px;}"))
                               tags$style(HTML(".navbar {width: 100%;}"))
                             ),   
                             
                             # h4( HTML( "<div style='width: 500px;'><b><u>Scenario 2</u>: Minimize the budget required to detect the relationship between classroom climate and the outcome variable <i>given a prespecified power</i></b></div>" ) ),	
                             # v2:
							 # h4( HTML( "<div style='width: 100%;'><b><u>Scenario 2</u>: Minimize the budget required to detect the relationship between classroom climate and the outcome variable <i>given a prespecified power</i></b></div>" ) ),	
                             h4( HTML( "<div style='width: 100%;'><b><u>Scenario 2</u>: Minimizing the budget required to detect the relation between classroom climate and the outcome variable (i.e., the between slope) <i>given a prespecified level of power</i></b></div>" ) ),	
                             
                             h4( HTML( "&nbsp;" ) ),
                             
                             ## Input-Boxen rechts vom Text
                             # https://community.rstudio.com/t/how-to-put-the-inline-textinput-label-at-the-left-of-the-column-and-the-textinput-box-at-the-right-of-the-column/24519
                             
                             ## Boxen-Groesse von inputNumeric aendern
                             # https://community.rstudio.com/t/how-to-customize-selectinput-in-shiny-box-height-and-width/36378
                             # https://stackoverflow.com/questions/49873705/css-styling-for-select-and-numeric-input
                             
                             fluidRow(
                               tags$head(
                                 tags$style(type="text/css","label{ display: table-cell; text-align: left; vertical-align: middle; padding: 10px; } .form-group { display: table-row; background-color: #eaeaea;} .form-control.shiny-bound-input, .selectize-input {height: 35px;width: 100px;padding-top: 5px;text-align: center;}") 
                               ) ),			    
                             
                             numericInput( inputId = "power2",
                                           label = "Power (%)",
                                           value = 80,
                                           min = 0,
                                           max = 100,
                                           step = 1,
                                           width = '140px' ),
                             numericInput( inputId = "costsperclass2",
                                           label = "Costs per class",
                                           value = 68.20,
                                           min = 0,
                                           step = 10,
                                           width = '140px' ),
                             numericInput( inputId = "costsperstudent2",
                                           label = "Costs per student",
                                           value = 7.05,
                                           min = 0,
                                           step = 1,
                                           width = '140px'),
                             HTML( "<p style='margin-bottom:10px;'>" ),				
                             numericInput( inputId = "iccoutcome2",
                                           # v2:
										   # label = HTML( "ICC of the outcome variable<sup><small>&#8224;</small></sup>" ),
                                           label = HTML( "ICC of outcome variable<sup><small>&#8224;</small></sup>" ),
                                           value = 0.20,
                                           min = 0,
                                           max = 1,
                                           step = 0.01,
                                           width = '200px'),
                             numericInput( inputId = "iccclimate2",
                                           # v2:
										   # label = HTML( "ICC of the students' ratings of classroom climate&nbsp;&nbsp;&nbsp;" ),
		                                   label = HTML( "ICC of student ratings of classroom climate&nbsp;&nbsp;&nbsp;" ),
                                           value = 0.20,
                                           min = 0,
                                           max = 1,
                                           step = 0.01,
                                           width = '340px'),
                             # v2:
							 # p( HTML( "<small><sup>&#8224;</sup> not adjusted for the students' ratings of classroom climate</small>" ) ),
                             p( HTML( "<small><sup>&#8224;</sup> not adjusted for classroom climate</small>" ) ),
                             numericInput( inputId = "betweenslope2",
                                           # v2:
										   # label = HTML( "Between slope<sup><small>&#8225;</small></sup>&nbsp;&nbsp;&nbsp;" ),
                                           label = HTML( "Standardized between slope<sup><small>&#8225;</small></sup>&nbsp;&nbsp;&nbsp;" ),
                                           value = 0.20,
                                           step = 0.01,
                                           width = '340px'),
                             numericInput( inputId = "withinslope2",
                                           # v2:
										   # label = HTML( "Within slope<sup><small>&#8225;</small></sup>" ),
                                           label = HTML( "Standardized within slope<sup><small>&#8225;</small></sup>" ),
                                           value = 0.20,
                                           step = 0.01,
                                           width = '340px'),
                             # HTML( "<div style='width: 460px;'><small><sup>&#8225;</sup> standardized with respect to the total variances of both the outcome variable and the students' ratings of classroom climate</small></div>" ),
                             # v2:
							 # HTML( "<div style='width: 100%;'><small><sup>&#8225;</sup> standardized with respect to the total variances of both the outcome variable and the students' ratings of classroom climate</small></div>" ),
 	                         # HTML( "<div style='width: 100%;'><small><sup>&#8225;</sup>The between slope is standardized by standardizing the outcome variable with respect to its total SD and by standardizing the level-2 part of the classroom climate variable with respect to the estimated level-2/between classroom climate SD. The within slope is standardized by standardizing the outcome variable with respect to its total SD and by standardizing the level-1 part of the classroom climate variable with respect to the estimated level-1/within classroom climate SD (Marsh et al., 2009).</small></div>" ),
		                     HTML( "<div style='width: 100%;'><small><sup>&#8225;</sup> standardized with respect to the total variance of the outcome variable but only the between/within variance of the classroom climate variable (Marsh et al., 2009)</small></div>" ),

                             
                             HTML( "<p style='margin-bottom:30px;'>" ),
                             
                             htmlOutput("results2"),
							 
							, width = 6 ),
							  
							mainPanel( plotOutput("plot2"), width = 6 )
                           
                           ), # Ende tabPanel
                  
                  tabPanel("How to cite",
                           # v2:
						   # HTML( "<p style='width:500px;'>This shiny app is based on the article:<br><br>AUTHORS (YEAR). Should we sample more students within fewer classrooms or more classrooms with fewer students when assessing the role of classroom climate via stutents' ratings and budget is limited? - An optimal design perspective. JOURNAL.</p>" )
                          # HTML( "<p style='width:500px;'>This shiny app is based on the article:<br><br>AUTHORS (YEAR). How many classes and students should ideally be sampled when assessing the role of classroom climate via student ratings on a limited budget? An optimal design perspective. JOURNAL.</p>" )
						              HTML( "<p style='width:500px;'>This shiny app is based on the article:<br><br>Zitzmann, S., Wagner, W., Hecht, M., Helm, C., Fischer, C., Bardach, L., &amp; G&ouml;llner, R. (2021). How many classes and students should ideally be sampled when assessing the role of classroom climate via student ratings on a limited budget? An optimal design perspective. <i>Educational Psychology Review</i>. Advance online publication. <a href='https://doi.org/10.1007/s10648-021-09635-4'>https://doi.org/10.1007/s10648-021-09635-4</a></p>" )
						   
                          ) # Ende tabPanel
                  
) )		


server <- function(input, output) {
		
		output$results <- renderText( { 
        
		    # ------------------------------------------------------
		    # ----------------------- Demo 1 -----------------------
		    # ------------------------------------------------------
		  
		    # In the first demo, it is shown how optimal sample sizes can be derived given a fixed budget
		    # Here, the power is maximized!
		    # User must specify following quantities first:		  
		  
		    budget <- input$budget # Budget
		    cost2 <- input$costsperclass # Costs per class
		    cost1 <- input$costsperstudent # Costs per student
		  
		    icc.y <- input$iccoutcome # ICC of outcome variable
		    icc.x <- input$iccclimate # ICC of students' ratings of classroom climate
		  
		    b2 <- input$betweenslope
		    b1 <- input$withinslope
        

			### Modus ###

			## y criterion
			y.criterion="power"
			# y.criterion="budget"


			### Berechnung der aktuellen Ergebnisse

			# fuer b2 e [-0.005,0.005] keine Berechnung
			if( b2 <= -0.005 || b2 >= 0.005 ){
					res <- eval( parse( text=paste0( "calc.",y.criterion,"( ",
															ifelse(y.criterion %in% "power", "budget=budget,", ""), # bei power Berechnung budget uebergeben
															ifelse(y.criterion %in% "budget", "power=power,", ""),     # bei budget Berechnung power uebergeben
															" cost2=cost2, cost1=cost1, icc.y=icc.y, icc.x=icc.x, b2=b2, b1=b1 )" ) ) )
			} else {
				res <- list( "optclass"=NA, "optstud"=NA, "power"=NA, "budget"=NA )
			}
			optclass <- res$optclass
			optstud <- res$optstud
			if( !is.null( res$power ) ) power <- res$power
			if( !is.null( res$budget ) ) budget <- res$budget

		    ## html table background
		    # https://itwebtutorials.mga.edu/html/chp8/table-colors-and-backgrounds.aspx
		    ## shades of orange
		    # https://www.color-meanings.com/shades-of-orange-color-names-html-hex-rgb-codes/
		    
				HTML( paste0( "<style type='text/css'>
				                table          {border:ridge 0px red;}
				                table td       {border:inset 0px #000; padding: 10px;}
				                table td#TDCENTER       {border:inset 0px #000; padding: 10px; text-align: center;}				                
				                table tr       {background-color:#ed820e; color:black;}
				                </style>",
				              # "<div style='margin-left: 60px; width: 480px;'>",
				              "<div style='margin-left: 60px; width: 100%;'>",
				              "<table style='width:70%'><tr><td>", "<b>Optimal no. of classes</b></td><td id='TDCENTER'>", formatC( round( optclass, 0 ), format="f", digits=0 ), "</td></tr>",
				              "<tr><td><b>Optimal no. of students per class</b></td><td id='TDCENTER'>", formatC( round( optstud, 0 ), format="f", digits=0 ), "</td></tr>",
				              "<tr><td><b>Maximum power</b></td><td id='TDCENTER'>", formatC( round( power, 0 ), format="f", digits=0) , ifelse(is.na(power),"","%"), "</td></tr>",
				              "</table>","</div>","<br><br>"
				              ) )
		    
		} )

#####################################################################################################################################################################################
#####################################################################################################################################################################################
#####################################################################################################################################################################################
		
		output$results2 <- renderText( { 

		  # ------------------------------------------------------
		  # ----------------------- Demo 2 -----------------------
		  # ------------------------------------------------------
		  
		  # In the second demo, it is shown how optimal sample sizes can be derived given a prespecified budget
		  # Here, the budget is minimized!
		  # User must specify following quantities first:  
		  
		  power <- input$power2 # targeted power; e.g., 80%
		  cost2 <- input$costsperclass2 # Costs per class
		  cost1 <- input$costsperstudent2 # Costs per student
		  
		  icc.y <- input$iccoutcome2 # ICC of outcome variable
		  icc.x <- input$iccclimate2 # ICC of students' ratings of classroom climate
		  
		  b2 <- input$betweenslope2
		  b1 <- input$withinslope2  


		  ### Modus ###

		  ## y criterion
		  # y.criterion="power"
		  y.criterion="budget"

		  
		  ### Berechnung der aktuellen Ergebnisse
		  
		  # fuer b2 e [-0.005,0.005] keine Berechnung
		  if( b2 <= -0.005 || b2 >= 0.005 ){
		  		res <- eval( parse( text=paste0( "calc.",y.criterion,"( ",
		  												ifelse(y.criterion %in% "power", "budget=budget,", ""), # bei power Berechnung budget uebergeben
		  												ifelse(y.criterion %in% "budget", "power=power,", ""),     # bei budget Berechnung power uebergeben
		  												" cost2=cost2, cost1=cost1, icc.y=icc.y, icc.x=icc.x, b2=b2, b1=b1 )" ) ) )
		  } else {
		  	res <- list( "optclass"=NA, "optstud"=NA, "power"=NA, "budget"=NA )
		  }
		  optclass <- res$optclass
		  optstud <- res$optstud
		  if( !is.null( res$power ) ) power <- res$power
		  if( !is.null( res$budget ) ) budget <- res$budget
    		  
		  ## html table background
		  # https://itwebtutorials.mga.edu/html/chp8/table-colors-and-backgrounds.aspx
		  ## shades of orange
		  # https://www.color-meanings.com/shades-of-orange-color-names-html-hex-rgb-codes/
		  
		  HTML( paste0( "<style type='text/css'>
				                table          {border:ridge 0px red;}
				                table td       {border:inset 0px #000; padding: 10px;}
				                table td#TDCENTER       {border:inset 0px #000; padding: 10px; text-align: center;}				                
				                table tr       {background-color:#ed820e; color:black;}
				                </style>",
		                # "<div style='margin-left: 60px; width: 480px;'>",
		                "<div style='margin-left: 60px; width: 100%;'>",
		                "<table style='width:70%'><tr><td>", "<b>Optimal no. of classes</b></td><td id='TDCENTER'>", formatC( round( optclass, 0 ), format="f", digits=0 ), "</td></tr>",
		                "<tr><td><b>Optimal no. of students per class</b></td><td id='TDCENTER'>", formatC( round( optstud, 0 ), format="f", digits=0 ), "</td></tr>",
		                "<tr><td><b>Minimum required budget</b></td><td id='TDCENTER'>", formatC( round( budget, 0 ), format="f", digits=0) , "</td></tr>",
		                "</table>",
		                "</div>","<br><br>"
		  ) )
		  
		} )		

#####################################################################################################################################################################################
#####################################################################################################################################################################################
#####################################################################################################################################################################################

		output$plot1 <- renderPlot({
				
				# ------------------------------------------------------
				# ----------------------- Demo 1 -----------------------
				# ------------------------------------------------------
				  
				budget <- input$budget # Budget
				# budget <- 20000 # Budget
				cost2 <- input$costsperclass # Costs per class
				# cost2 <- 68.2 # Costs per class
				cost1 <- input$costsperstudent # Costs per student
				# cost1 <- 7.05 # Costs per student
				  
				icc.y <- input$iccoutcome # ICC of outcome variable
				# icc.y <- 0.2 # ICC of outcome variable
				icc.x <- input$iccclimate # ICC of students' ratings of classroom climate
				# icc.x <- 0.2 # ICC of students' ratings of classroom climate
				  
				b2 <- input$betweenslope
				# b2 <- 0.2
				b1 <- input$withinslope  
				# b1 <- 0.2		  
		  
				
				### Modus ###
				## y criterion
				y.criterion="power"
				# y.criterion="budget"				
				
				### Einstellungen ###
				einst <- get.einstellungen( y.criterion=y.criterion )
				xvar <- einst$xvar 
				yvar <- einst$yvar 
				xlab <- einst$xlab 
				ylab <- einst$ylab 


				### Berechnung der aktuellen Ergebnisse

				# fuer b2 e [-0.005,0.005] keine Berechnung
				if( b2 <= -0.005 || b2 >= 0.005 ){
						res <- eval( parse( text=paste0( "calc.",y.criterion,"( ",
																ifelse(y.criterion %in% "power", "budget=budget,", ""), # bei power Berechnung budget uebergeben
																ifelse(y.criterion %in% "budget", "power=power,", ""),     # bei budget Berechnung power uebergeben
																" cost2=cost2, cost1=cost1, icc.y=icc.y, icc.x=icc.x, b2=b2, b1=b1 )" ) ) )
				} else {
					res <- list( "optclass"=NA, "optstud"=NA, "power"=NA, "budget"=NA )
				}
				optclass <- res$optclass
				optstud <- res$optstud
				if( !is.null( res$power ) ) power <- res$power
				if( !is.null( res$budget ) ) budget <- res$budget


				# nur zum updaten von not.available.plot fuer die y Positionierung das alte Budget speichern
				# if( exists("budget") ) old.budget <- budget else old.budget <- res$budget
				if( exists("d") ) median.budget <- median( d$y, na.rm=TRUE ) else median.budget <- NULL

				# Plots waehrend Erstellung
				if (!exists("p",mode="list")) {
						# leerer Plot
						p <- gen.empty.plot()
				} else {
						# aktuellen Plot mit "generating plot" ueberlagern
						p <- annotate.plot( p, y.criterion=y.criterion, budget=median.budget )
				}
				print( p )

				# Plot not available, Standard Ergebnis-Plot falls Erstellung fails
				p <- plot.not.avail()

				# wenn aktuelle Ergebnisse berechenbar, dann versuchen Grafik zu erstellen (kann aber trotzdem Probleme geben, da ja die x-Achse abgesampled wird)
				if ( all( !is.na( res ) ) ){

						# x-Achse absamplen
						dneu <-  eval( parse( text=paste0( "sample.x.axis( ", 
																ifelse(y.criterion %in% "power", "budget=budget,", ""), # bei power Berechnung budget uebergeben
																ifelse(y.criterion %in% "budget", "power=power,", ""),     # bei budget Berechnung power uebergeben
																"cost2=cost2, cost1=cost1, icc.y=icc.y, icc.x=icc.x, b2=b2, b1=b1,  current.x=b2, y.criterion=y.criterion )" ) ) )
						
						if( !is.null(dneu) && !inherits( dneu, "try-error" ) && is.data.frame(dneu) && nrow(dneu>0) ){
						
								if( !exists( "d" ) ){
										dneu$round <- factor( 1 )
										d <- dneu
								} else {
										# letzte round
										lastround <- d[d$round %in% (current.max <- max( as.integer( as.character( d$round ) ) )),c("x","y")]
										rownames( lastround ) <- seq( along=rownames( lastround ) )
								
										# nur wenn letzte Kurve nicht identisch ist mit neuer, dann neue dranhaengen
										if (!identical(dneu,lastround)){
												newlevel <- current.max + 1
												dneu$round <- factor( newlevel, levels=c( levels(d), newlevel ) )
												d <- rbind( d, dneu )
										}
								}
								
								## fade-out, nur die letzen nlast Kurven behalten
								#### ACHTUNG, fade-out funktioniert in ShinyApp nicht
								# nlast <- 5
								nlast <- NULL
								current.max <- max( as.integer( as.character( d$round ) ) )
								if( !is.null( nlast ) && identical(nlast,floor(nlast)) && nlast >= 0 ){
										keep <- seq( current.max, current.max - nlast, -1 )
										keep <- keep[ keep > 0 ]
								} else {
										keep <- levels( d$round )
								}
								d <- d[ d$round %in% as.character( keep ), ]
								d$round <- factor( as.character( d$round ) )
								
								# generate plot
								p <- gen.plot(d=d,xvar=xvar,yvar=yvar,xlab=xlab,ylab=ylab,y.criterion=y.criterion,b2=b2,power=power,budget=budget)
						} 
				}

				print( p )
		})

#####################################################################################################################################################################################
#####################################################################################################################################################################################
#####################################################################################################################################################################################

		output$plot2 <- renderPlot({

				# ------------------------------------------------------
				# ----------------------- Demo 2 -----------------------
				# ------------------------------------------------------
				
				# In the second demo, it is shown how optimal sample sizes can be derived given a prespecified budget
				# Here, the budget is minimized!
				# User must specify following quantities first:  
				
				power <- input$power2 # targeted power; e.g., 80%
				cost2 <- input$costsperclass2 # Costs per class
				cost1 <- input$costsperstudent2 # Costs per student
				
				icc.y <- input$iccoutcome2 # ICC of outcome variable
				icc.x <- input$iccclimate2 # ICC of students' ratings of classroom climate
				
				b2 <- input$betweenslope2
				b1 <- input$withinslope2
		  
				### Modus ###
				## y criterion
				# y.criterion="power"
				y.criterion="budget"
					
				### Einstellungen ###
				einst <- get.einstellungen( y.criterion=y.criterion )
				xvar <- einst$xvar 
				yvar <- einst$yvar 
				xlab <- einst$xlab 
				ylab <- einst$ylab 
				
				
				### Berechnung der aktuellen Ergebnisse

				# fuer b2 e [-0.005,0.005] keine Berechnung
				if( b2 <= -0.005 || b2 >= 0.005 ){
						res <- eval( parse( text=paste0( "calc.",y.criterion,"( ",
																ifelse(y.criterion %in% "power", "budget=budget,", ""), # bei power Berechnung budget uebergeben
																ifelse(y.criterion %in% "budget", "power=power,", ""),     # bei budget Berechnung power uebergeben
																" cost2=cost2, cost1=cost1, icc.y=icc.y, icc.x=icc.x, b2=b2, b1=b1 )" ) ) )
				} else {
					res <- list( "optclass"=NA, "optstud"=NA, "power"=NA, "budget"=NA )
				}
				optclass <- res$optclass
				optstud <- res$optstud
				if( !is.null( res$power ) ) power <- res$power
				if( !is.null( res$budget ) ) budget <- res$budget
				
				
				# nur zum updaten von not.available.plot fuer die y Positionierung das alte Budget speichern
				# if( exists("budget") ) old.budget <- budget else old.budget <- res$budget
				if( exists("d") ) median.budget <- median( d$y, na.rm=TRUE ) else median.budget <- NULL
				
				# Plots waehrend Erstellung
				if (!exists("p",mode="list")) {
						# leerer Plot
						p <- gen.empty.plot()
				} else {
						# aktuellen Plot mit "generating plot" ueberlagern
						p <- annotate.plot( p, y.criterion=y.criterion, budget=median.budget )
				}
				print( p )
				
				# Plot not available, Standard Ergebnis-Plot falls Erstellung fails
				p <- plot.not.avail()
				
				# wenn aktuelle Ergebnisse berechenbar, dann versuchen Grafik zu erstellen (kann aber trotzdem Probleme geben, da ja die x-Achse abgesampled wird)
				if ( all( !is.na( res ) ) ){
				
						# x-Achse absamplen
						dneu <-  eval( parse( text=paste0( "sample.x.axis( ", 
																ifelse(y.criterion %in% "power", "budget=budget,", ""), # bei power Berechnung budget uebergeben
																ifelse(y.criterion %in% "budget", "power=power,", ""),     # bei budget Berechnung power uebergeben
																"cost2=cost2, cost1=cost1, icc.y=icc.y, icc.x=icc.x, b2=b2, b1=b1,  current.x=b2, y.criterion=y.criterion )" ) ) )
						
						if( !is.null(dneu) && !inherits( dneu, "try-error" ) && is.data.frame(dneu) && nrow(dneu>0) ){
						
								if( !exists( "d" ) ){
										dneu$round <- factor( 1 )
										d <- dneu
								} else {
										# letzte round
										lastround <- d[d$round %in% (current.max <- max( as.integer( as.character( d$round ) ) )),c("x","y")]
										rownames( lastround ) <- seq( along=rownames( lastround ) )
								
										# nur wenn letzte Kurve nicht identisch ist mit neuer, dann neue dranhaengen
										if (!identical(dneu,lastround)){
												newlevel <- current.max + 1
												dneu$round <- factor( newlevel, levels=c( levels(d), newlevel ) )
												d <- rbind( d, dneu )
										}
								}
								
								## fade-out, nur die letzen nlast Kurven behalten
								#### ACHTUNG, fade-out funktioniert in ShinyApp nicht
								# nlast <- 5
								nlast <- NULL
								current.max <- max( as.integer( as.character( d$round ) ) )
								if( !is.null( nlast ) && identical(nlast,floor(nlast)) && nlast >= 0 ){
										keep <- seq( current.max, current.max - nlast, -1 )
										keep <- keep[ keep > 0 ]
								} else {
										keep <- levels( d$round )
								}
								d <- d[ d$round %in% as.character( keep ), ]
								d$round <- factor( as.character( d$round ) )
								
								# generate plot
								p <- gen.plot(d=d,xvar=xvar,yvar=yvar,xlab=xlab,ylab=ylab,y.criterion=y.criterion,b2=b2,power=power,budget=budget)
						} 
				}
				
				print( p )
		})

#####################################################################################################################################################################################
#####################################################################################################################################################################################
#####################################################################################################################################################################################



		
}

### development
Rfiles <- list.files( "c:/Users/martin/Dropbox/84_optimalclpm/04_martinhecht/R", pattern="*.R" )
Rfiles <- Rfiles[ !Rfiles %in% "app.R" ]
for( Rfile in Rfiles ){
	source( Rfile )
}


shinyApp( ui = ui, server = server )



