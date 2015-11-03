library(shiny)

# Load data file pre-processed with "voorbewerking MWTL.R"
# This is a selection of data from MWTL
# Only in the vicinity of Eems estuary
# Only surface samples (0 - 4 m), extinction and Secchi depth
print(getwd())
load(file = "DATA/MWTL_Noordzee_bewerkt.Rdata")

# Define the overall UI

shinyUI(
  fluidPage(
    titlePanel("Metingen waterkwaliteit Noordzee"),
    sidebarLayout(
      # Define the sidebar with one input
      sidebarPanel(width = 3,
                   img(src = "logo.png", width = "175px"),
                   selectInput("location", "Location:", 
                               choices = levels(as.factor(rws_dat$locatie)), selected = "Noordwijk 10 km uit de kust"), 
                   #                    helpText("select location"),
                   selectInput("substance", "Substance:", 
                               choices = levels(as.factor(rws_dat$variable)), selected = "opgelost fosfaat"),
                   #                    helpText("select substance"),
                   sliderInput("interval", "Interval", min = 1970, max = 2014, c(1970, 2014), step = 1, sep = ""),
                   #                    helpText("select time interval"),
                   radioButtons("analysis", "Analysis",
                                c("Trend" = "trend",
                                  "90 percentile" = "per90",
                                  "Loess" = "loess"), selected = "loess"),
                   sliderInput("lspan", "Loess span",
                               min = 0, max = 1, 0.7, step = 0.05, sep = ""),
                   plotOutput("map", width = "300px", height = "250px")
      ),
      # Create a spot for the plots
      mainPanel(
        fluidRow(
          ggvisOutput("tp"),   #, width = "450px", height = "350px"
          uiOutput("tp_ui")
          #           column(width = 4,
          #           ),
          #           column(width = 4, offset = 2,
          #                  plotOutput("boxPlot", width = "450px", height = "350px")#,
          #                  #                  plotOutput("map", width = "400px", height = "250px")
          #           )
        )      )
    )
  )
)
