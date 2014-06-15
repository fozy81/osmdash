require(shiny)

shinyUI(navbarPage("Neighbour Resource Dashboard",
  tabPanel("Key:Value pairs",
  br(),
  fluidRow(
   column(3,
    fileInput('data', h4('Choose .osm File'),
              accept=c('text', 'text/comma-separated-values,text/plain', '.osm')),
    fileInput('dataTwo', h4('Choose .osm File'),
              accept=c('text', 'text/comma-separated-values,text/plain', '.osm')),
   
    h4('Select filter:'),
    selectInput("osm1",label = "Key=Value", choices = "osm1",multiple = TRUE,selectize=FALSE),

    downloadButton('downloadTest','Dowload Output When Analysis Complete'),
    hr(),
    helpText(a("Report issues or view the code for this site on Github", href="https://github.com/fozy81/osmdash/issues", target="_blank"))
  ),
      column(4,
             h5(textOutput("text1")),
             dataTableOutput("table2")),
 
   
      column(4,
             h5(textOutput("text2")),
             dataTableOutput("table3"))
  )),
  tabPanel("Landuse",
  fluidRow(
    column(4,
           h5(textOutput("text3")),
           plotOutput("plotArea1"),
           hr(),
           tableOutput("table4")),
    
    column(4,    
           h5(textOutput("text4")),
           plotOutput("plotArea2"),
           hr(),
    tableOutput("table5"))
    )   
      ))
  )
