## Run these examples only in interactive R sessions
if (interactive()) {
  
  library(shiny)
  library(ggvis)
  library(ggplot2)
  library(mapproj)
  library(plyr)
  library(scales)
  library(rworldmap)
  
  setwd("d:/Tools_Scripts/R/ShinyMeetNoordzee_1.1/R")
  
  load(file = "DATA/MWTL_Noordzee_bewerkt.Rdata")
  load(file = "DATA/map.rdata")
  
  alllocs <- as.character(unique(rws_dat$locatie))
  alllocmap <- subset(locmap, locmap$LOCOMSCH %in% alllocs)
  
  
  runApp(list(
    ui = fluidPage(
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
            print("Onderstaande grafieken zijn samengesteld uit metingen gedaan door Rijkswaterstaat binnen het MWTL programma. De site is bedoeld als demonstratie, en conclusies aan de hand van de gepresenteerde data en analyse zijn voor rekening van de gebruiker. "),
            print("Ruwe data zijn te downloaden van http://live.waterbase.nl")
          ),
          fluidRow(
            column(width = 4,
                   plotOutput("timePlot", width = "450px", height = "350px")
                   #figuurtje invoegen modelled SPM vs fPP of Chla??
            ),
            column(width = 4, offset = 2,
                   plotOutput("boxPlot", width = "450px", height = "350px")#,
                   #                  plotOutput("map", width = "400px", height = "250px")
            )
          ),
          fluidRow(
            column(width = 4,
                   print("Trend: Tijdserie met jaarlijkse periodiciteit\n"),
                   print("y = a + b*x + c*cos(2*pi*x)+d*sin(2*pi*x), waarbij the trend door -b- wordt gerepresenteerd\n"),
                   print("   - 90-percentile: 90 percentile of observations per summer/winter per jaar en voor de geselecteerde periode"),
                   print("   - Loess: curve met in te stellen stijfheid voor patroonherkenning")
            ),
            column(width = 4, offset = 2,
                   print("Boxplot of violinplot van de jaarlijkse periodiciteit voor de geselecteerde periode")
            )
          )
        )
      )
    ),
    server = function(input, output, session) {
      ## Select data that meet the criteria defined in the UI
      output$timePlot <- renderPlot({
        subdf = subset(rws_dat, rws_dat$variable == input$substance &  #"phosphate" &
                         rws_dat$locatie == input$location &   #  "Groote Gat noord" &
                         rws_dat$year >= input$interval[1] &
                         rws_dat$year <= input$interval[2])
        unit = unique(subdf$eenheid)
        
        ## Create statistics information needed in the plot annotation
        if(input$analysis == "trend") {
          x = as.numeric(subdf$datetime); y = subdf$waarde    
          regression <- summary(glm(y ~ x + I(cos(2 * pi * x / 31556995)) + I(sin(2 * pi * x / 31556995))))
          slope <- regression$coefficients[2,1]  #slope of regression including season effect
          pvalue <- format(regression$coefficients[2,4], digits = 2)
          intercept <- regression$coefficients[[1]]
        }
        
        ## make parameters for positioning of plot annotation
        minscale <- min(subdf$datetime)
        maxscale <- max(subdf$datetime)
        yposition <- quantile(subdf$waarde, 0.99, na.rm = T)
        #     q90 <- function(x) {quantile(x,probs=0.9)}
        
        subdf_year <- ddply(subdf, ~ year + season, summarize, perc = quantile(waarde, probs = 0.9, na.rm = T))
        subdf_year$year <- as.POSIXct(paste(as.character(subdf_year$year), "-07-01 00:00:00", sep = ""))
        
        summer90 <- quantile(subset(subdf, subdf$season == "summer", na.rm = T)$waarde,  probs = 0.9, na.rm = T)
        winter90 <- quantile(subset(subdf, subdf$season == "winter", na.rm = T)$waarde,  probs = 0.9, na.rm = T)
        
        ## create plot 
        pl <- ggplot(subdf, aes(datetime, waarde))
        pl <- pl + facet_grid(variable ~ locatie, scales = "free")
        #            pl <- pl + geom_smooth(aes(color = season) , method="lm", size = 2)
        if(input$analysis == "trend") {   # in case trend is choice of analysis
          pl <- pl + geom_point(aes(), color = "seagreen4", alpha = 0.5)
          pl <- pl + geom_smooth(aes(), method='lm', 
                                 formula = y ~ x+I(cos(2*pi*x/31622400))+I(sin(2*pi*x/31622400)), 
                                 size = 0.7, alpha = 0.3, n = 1000)
          pl <- pl + geom_abline(intercept = intercept, slope = slope , color = "darkblue", size = 1)
          pl <- pl + annotate("text", label = paste("slope =", format(slope*31622400, digits = 2), unit, "per year | ", "p=", pvalue),
                              x = maxscale - 0.5 * (maxscale - minscale), y = yposition)
        }
        if(input$analysis == "per90") {  # in case 90 percentile is choice of analysis
          pl <- pl + geom_point(aes(color = season), size = 2, alpha = 0.4)
          pl <- pl + geom_crossbar(data = subdf_year, aes(year, perc, color = season), size = 0.75, ymin =F, ymax = F)
          #       pl <- pl + geom_hline(yintercept = summer90, color = "orange", size = 1, alpha = 0.5)
          #       pl <- pl + geom_hline(yintercept = winter90, color = "turquoise4", size = 1, alpha = 0.5)
        }
        if(input$analysis == "loess") {   # in case trend is choice of analysis
          pl <- pl + geom_point(aes(), color = "seagreen4", alpha = 0.5)
          pl <- pl + geom_smooth(aes(), method='loess', span = input$lspan, size = 1)
        }
        
        pl <- pl + theme(text = element_text(size = 16), legend.position = "bottom")
        pl <- pl + scale_x_datetime(minor_breaks = date_breaks("1 year"))
        pl <- pl + xlab("date") + ylab(unit)
        if(input$substance == "chlorofyl-a"){
          pl <- pl + scale_y_log10()
        }
        print(pl)
        
      })
      output$boxPlot <- renderPlot({
        
        ## Select data that meet the criteria defined in the UI
        subdf = subset(rws_dat, rws_dat$variable == input$substance &
                         rws_dat$locatie == input$location &
                         rws_dat$waarde < 500 &
                         rws_dat$year >= input$interval[1] &
                         rws_dat$year <= input$interval[2])
        unit = unique(subdf$eenheid)
        
        minscale <- min(subdf$datetime)
        maxscale <- max(subdf$datetime)
        
        summer90 <- quantile(subset(subdf, subdf$season == "summer")$waarde,  probs = 0.9)
        winter90 <- quantile(subset(subdf, subdf$season == "winter")$waarde,  probs = 0.9)
        subdf_season <- ddply(subdf, ~ season, summarize, perc = quantile(waarde, probs = 0.9))
        
        ## Create plot
        q <- ggplot(subdf,aes(month,waarde))
        if(input$analysis == "trend") {   # in case trend is choice of analysis
          q <- q + geom_jitter(position = position_jitter(width = .5), colour = "seagreen4", alpha = 0.3)
          q <- q + geom_boxplot(outlier.colour = "orange", alpha = 0.5, color = "orange", notch = F, notchwidth = 0.5, outlier.size = F)
          q <- q + geom_smooth(aes(group = 1), method='lm', formula = y ~ I(cos(2*pi*x/12))+I(sin(2*pi*x/12)), size = 0.7, alpha = 0.2)
        }
        if(input$analysis == "per90") {  # in case 90 percentile is choice of analysis
          #       q <- q + geom_violin(outlier.colour = "orange", alpha = 0.5, color = "orange", notch = F, notchwidth = 0.5)
          q <- q + geom_jitter(aes(color = season), position = position_jitter(width = .5), alpha = 0.6)
          q <- q + geom_hline(data = subdf_season, aes(yintercept = perc, color = season), size = 0.75, ymin =F, ymax = F)
          #       q <- q + geom_hline(yintercept = summer90, color = "orange", size = 1, alpha = 0.5)
          #       q <- q + geom_hline(yintercept = winter90, color = "turquoise4", size = 1, alpha = 0.5)
        }
        if(input$analysis == "loess") {   # in case trend is choice of analysis
          q <- q + geom_jitter(position = position_jitter(width = .5), colour = "seagreen4", alpha = 0.3)
          q <- q + geom_violin(outlier.colour = "orange", alpha = 0.5, color = "orange", notch = F, notchwidth = 0.3)
          q <- q + facet_grid(variable ~ locatie, scales = "free") +
            theme(text = element_text(size = 16), legend.position = "bottom") +
            xlab("month") + ylab(unit)
        }
        if(input$substance == "chlorofyl-a"){
          q <- q + scale_y_log10()
        }
        print(q)  
      })
      
      output$map <- renderPlot({
        ## Select data that meet the criteria defined in the UI
        maploc   <-   input$location
        locmapsub  <- subset(locmap, locmap$LOCOMSCH == maploc)
        
        ## Create map
        data(countriesLow)
        world <- fortify(countriesLow)
        
        map <- ggplot() + 
          geom_polygon(data = world, 
                       aes(x=long, y=lat, group=group), 
                       color = "lightgrey", fill = "darkgrey")
        
        xxlim <- c(-2,8)
        yylim <- c(51.2,55.5)
        
        map +
          coord_map("orthographic", xlim = xxlim, ylim = yylim, orientation=c(55, 10, 0)) +
          geom_point(aes(X_WGS, Y_WGS), data = alllocmap, alpha = 0.4, size = 3, color = "blue") +
          geom_point(aes(X_WGS, Y_WGS), data = locmapsub, size = 8, color = "red") +
          geom_text(aes(X_WGS, Y_WGS, label = LOCOMSCH),hjust = 1, vjust = 0, data = locmapsub, size = 4) +
          theme(axis.text = element_blank(),
                axis.title = element_blank(),
                axis.line = element_blank(),
                axis.ticks = element_blank())
        
      })
      
    }
  ))
  
}

