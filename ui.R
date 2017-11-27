
library(shiny)

shinyUI(fluidPage(
  titlePanel("Google Scholar Network Visualization"),
  fluidRow(
    column(12,
           helpText("Please Copy & Paste the link to your Google Scholar profile to see your scholar network. 
                    For example, the default is Dr. Alan Ker's google scholar page"),
           helpText("https://scholar.google.ca/citations?user=z-FBV_MAAAAJ&hl=en"),
           helpText("You can check any scholar's newtwork as long as they are on Google Scholar. If you have any comments or suggestions, feel free to email me at zshang@uoguelph.ca"))),
  fluidRow(column(12,textInput("scholarID", "Google Scholar Profile Link:", 
                     value = "https://scholar.google.ca/citations?user=z-FBV_MAAAAJ&hl=en", width = NULL, placeholder = NULL))),
fluidRow(column(12,
      plotOutput("distPlot",width="100%", height="1024px"))
    )
  )
)

