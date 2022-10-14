## Changelog:
# MH 0.0.39 2022-10-14: initial programming

source("R/createTables.R")

ok <- try.createTables()

ui <- tabPanel(
  title = HTML("Calculate <b>Power</b>"),
  icon = icon("magnifying-glass-chart"),
  fluidPage(
    fluidRow(
      column(
        id = "one",
        width = 10,
        div(class = "unit", paste0("created db tables (overwritten if existed!!): ", ok))
      ),
      column(
        id = "two",
        width = 3,
        div(class = "unit", "")
      ),
      column(
        id = "three",
        width = 3,
        div(class = "unit", "" )
      )
    )
  )
)

server <- shinyServer(function(input, output, session){

})

shinyApp(ui = ui, server = server)
