require(shiny)

dataset <- 

shinyUI(pageWithSidebar(
  
  headerPanel(''),
  sidebarPanel(
    fileInput('data', 'Choose osm File',
              accept=c('text', 'text/comma-separated-values,text/plain', '.osm')),
    tags$hr(),
    h4('Select filter:'),
    selectInput("osm1",label = "Filter1", choices = ""),

   # checkboxInput('lake', 'Lake', TRUE),
  #  checkboxInput('river', 'River', TRUE),  
    downloadButton('downloadTest','Dowload Output When Analysis Complete'),
    helpText(a("Download example demo test data here", href="https://raw.github.com/fozy81/darleq/master/testdata.csv", target="_blank")),
    helpText(a("Report issues or view the code for this site on Github", href="https://github.com/fozy81/darleq/issues", target="_blank"))
  ),
  mainPanel(
  #  tableOutput("table"),
    tableOutput("table2")
  )
))
