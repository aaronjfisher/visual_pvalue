library(shiny)


shinyUI(pageWithSidebar(
  headerPanel("What do p-values look like?"),

  sidebarPanel(
    
      sliderInput("n", "Sample Size (n):", 
                  min=20, max=500, value=100,step=1,animate=FALSE),
      sliderInput("logp", "p-value=10^", 
                  min=-20, max=-.02, value=-2,step=.2,animate=FALSE),

    checkboxInput("nulldist", "Generate from an actual null", FALSE),
        
    checkboxInput("bestFit", "Best fit line", FALSE),
    checkboxInput("lowess", "LOWESS", FALSE),
    checkboxInput("trimX", "Exclude X outliers (keep n constant)", FALSE),
    numericInput("XEseed", "Change seed for X & e", 312,min=0,max=100000,step=1),
    checkboxInput("showCode", "Show R Code", FALSE)
  ),


 mainPanel(
    h3(textOutput("pval")),
    plotOutput("outplot",height='auto'),
    h3(textOutput("formula")),
    tableOutput('detailTable'),
    conditionalPanel(condition = "input.showCode",
                     verbatimTextOutput('Rcode'))
  )
))