require(shiny)

shinyUI(navbarPage("Neighbour Resource Dashboard",
  tabPanel("Key:Value pairs",
  br(),
  fluidRow(
   column(3,
    fileInput('data', 'Choose .osm File',
              accept=c('text', 'text/comma-separated-values,text/plain', '.osm')),
    fileInput('dataTwo', 'Choose .osm File',
              accept=c('text', 'text/comma-separated-values,text/plain', '.osm')),
   
    h4('Select filter:'),
    selectInput("osm1",label = "Filter1", choices = "",multiple = TRUE,selectize=FALSE),

    downloadButton('downloadTest','Dowload Output When Analysis Complete'),
    helpText(a("Download example demo test data here", href="https://raw.github.com/fozy81/darleq/master/testdata.csv", target="_blank")),
    helpText(a("Report issues or view the code for this site on Github", href="https://github.com/fozy81/darleq/issues", target="_blank"))
  ),
      column(4,
              hr(),
              h4(textOutput("text1")),
             dataTableOutput("table2")),
 
   
      column(4,
             hr(),
             h4(textOutput("text2")),
             dataTableOutput("table3"))
  )),
  tabPanel("Landuse",
  fluidRow(
    column(3,
           h4(textOutput("text3")),
           tableOutput("table4")),
    
    column(3,    
           h4(textOutput("text4")),
    tableOutput("table5"))
    )   
      ))
  )
