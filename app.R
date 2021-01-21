## R Shiny app for visualization of CMIP6 global climate model simulations for BC and subregions
## author: Colin Mahony colin.mahony@gov.bc.ca


library(shiny)
library(RColorBrewer)
library(DT)
library(scales)
library(shinydashboard)
library(markdown)


# ----------------------------------------------
# Load the input data
# ----------------------------------------------

modelMetadata <- read.csv("data/ModelList.csv")

dem.pts <- read.csv("data/dem_cmip6eval.csv")

## time series for observations
obs.ts <- read.csv("data/obs.ts.csv") #gridded station observations
era5.ts <- read.csv("data/era5.ts.csv") #ERA5 reanalysis

# list of variables
variables <- names(obs.ts)[-1]

# Define ecoprovinces (subregions of BC) and climate elements
ecoprovs <- c("BC", sort(as.character(unique(dem.pts$id2))))
ecoprov.names <- c("British Columbia", "Boreal Plains", "Central Interior", "Coast and Mountains", "Georgia Depression", "Northern Boreal Mountains", "Sub-Boreal Interior", "Southern Interior Mountains", "Southern Interior", "Taiga Plains")
elements <- c("Tave", "Tmax", "Tmin", "PPT")
element.names <- c("Mean temperature" , "Mean daily maximum temperature (Tmax)", "Mean daily minimum temperature (Tmin)", "Precipitation")
element.names.units <- c(bquote(Mean~temperature~"("*degree*C*")"),bquote(Mean~daily~bold(maximum)~temperature~"("*degree*C*")"),bquote(Mean~daily~bold(minimum)~temperature~"("*degree*C*")"), "Precipitation (mm)")

# extract the global climate models and scenarios from an arbitrary file. 
files <- list.files("data/", pattern="^ensmin.BC")
template <- read.csv(paste("data/", files[1], sep=""), stringsAsFactors = F)
gcms <- names(template)[-c(1:2, length(names(template)))]
scenarios <- unique(template[,1])
scenario.names <- c("Historical simulations", "SSP1-2.6", "SSP2-4.5", "SSP3-7.0", "SSP5-8.5")
gcm.names <- as.character(modelMetadata[,1])

# Other definitions
monthdays <- c(31, 28.25, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
monthcodes <- c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12")
seasonmonth.mat <- matrix(monthcodes[c(12, 1:11)],4, byrow=T)

seasons <- c("wt", "sp", "sm", "at")
season.names <- c("Winter", "Spring", "Summer", "Autumn")

yeartimes <- c(seasons, monthcodes)
yeartime.names <- c(season.names, month.name)

ensstats <- c("ensmin", "ensmax", "ensmean")

# Define UI ----
ui <- fluidPage(
  navbarPage(title = "CMIP6 viewer for British Columbia", theme = "bcgov.css", 
             tabPanel("App",
                      sidebarLayout(
                        sidebarPanel(
                          helpText("Compare CMIP6 climate model simulations to each other and to observations. Compile custom ensembles with and without bias correction. See projections for individual regions of BC."),
                          
                          tags$head(tags$script('$(document).on("shiny:connected", function(e) {
                            Shiny.onInputChange("innerWidth", window.innerWidth);
                            });
                            $(window).resize(function(e) {
                            Shiny.onInputChange("innerWidth", window.innerWidth);
                            });
                            ')),
                          
                          checkboxGroupInput("gcms1", "Choose global climate models:",
                                             choiceNames = gcm.names,
                                             choiceValues = gcms,
                                             selected = gcms[4],
                                             inline = T
                          ),
                          
                          checkboxInput("compile", label = "Compile into custom ensemble", value = FALSE),
                          
                          checkboxInput("biascorrect", label = "Bias correction (match 1951-80 model climate to observations)", value = FALSE),
                          
                          checkboxGroupInput("scenarios1", "Choose emissions scenarios",
                                             choiceNames = scenario.names[-1],
                                             choiceValues = scenarios[-1],
                                             selected = scenarios[c(2,3,4)],
                                             inline = T
                          ),
                          
                          checkboxInput("showmean", label = "Show mean of projections", value = FALSE),
                          
                          checkboxInput("refline", label = "Show 1951-1980 baseline for models", value = T),
                          
                          checkboxInput("era5", label = "Show ERA5 reanalysis", value = F),
                          
                          # ELEMENT NAMES. THIS IS WHERE THE DERIVED VARIABLES WILL BE ADDED TO THE LIST
                          selectInput("element1",
                                      label = "Choose the climate element",
                                      choices = as.list(element.names),
                                      selected = element.names[1]),
                          
                          selectInput("yeartime1",
                                      label = "Choose the month/season",
                                      choices = as.list(yeartime.names),
                                      selected = yeartime.names[3]),
                          
                          checkboxInput("compare", label = "Compare two variables", value = T),
                          
                          conditionalPanel(
                            condition = "input.compare == true",
                            
                            # ELEMENT NAMES. THIS IS WHERE THE DERIVED VARIABLES WILL BE ADDED TO THE LIST
                            selectInput("element2",
                                        label = "Choose a climate element for comparison",
                                        choices = as.list(element.names),
                                        selected = element.names[1]),
                            
                            selectInput("yeartime2",
                                        label = "Choose a month/season for comparison",
                                        choices = as.list(yeartime.names),
                                        selected = yeartime.names[1]),
                          ),
                          
                          selectInput("ecoprov.name",
                                      label = "Choose an ecoprovince",
                                      choices = as.list(ecoprov.names),
                                      selected = ecoprov.names[1]),
                          
                          img(src = "Ecoprovinces_Title.png", height = round(1861*1/5), width = round(1993*1/5))
                        ),
                        
                        mainPanel(
                          
                          plotOutput(outputId = "timeSeries")
                          
                        )
                      ),
                      column(width = 12,
                             style = "background-color:#003366; border-top:2px solid #fcba19;",
                             
                             tags$footer(class="footer",
                                         tags$div(class="container", style="display:flex; justify-content:center; flex-direction:column; text-align:center; height:46px;",
                                                  tags$ul(style="display:flex; flex-direction:row; flex-wrap:wrap; margin:0; list-style:none; align-items:center; height:100%;",
                                                          tags$li(a(href="https://www2.gov.bc.ca/gov/content/home", "Home", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")),
                                                          tags$li(a(href="https://www2.gov.bc.ca/gov/content/home/disclaimer", "Disclaimer", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")),
                                                          tags$li(a(href="https://www2.gov.bc.ca/gov/content/home/privacy", "Privacy", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")),
                                                          tags$li(a(href="https://www2.gov.bc.ca/gov/content/home/accessibility", "Accessibility", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")),
                                                          tags$li(a(href="https://www2.gov.bc.ca/gov/content/home/copyright", "Copyright", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")),
                                                          tags$li(a(href="https://www2.gov.bc.ca/StaticWebResources/static/gov3/html/contact-us.html", "Contact", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;"))
                                                  )
                                         )
                             )
                      )
             ),
             
             tabPanel("About",
                      
                      includeMarkdown("about.Rmd"),
                      
                      img(src = "ModelRes.png", height = 2000/3, width = 1000),
                      
                      column(width = 12,
                             style = "background-color:#003366; border-top:2px solid #fcba19;",
                             
                             tags$footer(class="footer",
                                         tags$div(class="container", style="display:flex; justify-content:center; flex-direction:column; text-align:center; height:46px;",
                                                  tags$ul(style="display:flex; flex-direction:row; flex-wrap:wrap; margin:0; list-style:none; align-items:center; height:100%;",
                                                          tags$li(a(href="https://www2.gov.bc.ca/gov/content/home", "Home", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")),
                                                          tags$li(a(href="https://www2.gov.bc.ca/gov/content/home/disclaimer", "Disclaimer", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")),
                                                          tags$li(a(href="https://www2.gov.bc.ca/gov/content/home/privacy", "Privacy", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")),
                                                          tags$li(a(href="https://www2.gov.bc.ca/gov/content/home/accessibility", "Accessibility", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")),
                                                          tags$li(a(href="https://www2.gov.bc.ca/gov/content/home/copyright", "Copyright", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")),
                                                          tags$li(a(href="https://www2.gov.bc.ca/StaticWebResources/static/gov3/html/contact-us.html", "Contact", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;"))
                                                  )
                                         )
                             )
                      )
             ),

                          tabPanel("Model Info",
                      DT::dataTableOutput("table"),
                      column(width = 12,
                             style = "background-color:#003366; border-top:2px solid #fcba19;",
                             
                             tags$footer(class="footer",
                                         tags$div(class="container", style="display:flex; justify-content:center; flex-direction:column; text-align:center; height:46px;",
                                                  tags$ul(style="display:flex; flex-direction:row; flex-wrap:wrap; margin:0; list-style:none; align-items:center; height:100%;",
                                                          tags$li(a(href="https://www2.gov.bc.ca/gov/content/home", "Home", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")),
                                                          tags$li(a(href="https://www2.gov.bc.ca/gov/content/home/disclaimer", "Disclaimer", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")),
                                                          tags$li(a(href="https://www2.gov.bc.ca/gov/content/home/privacy", "Privacy", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")),
                                                          tags$li(a(href="https://www2.gov.bc.ca/gov/content/home/accessibility", "Accessibility", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")),
                                                          tags$li(a(href="https://www2.gov.bc.ca/gov/content/home/copyright", "Copyright", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")),
                                                          tags$li(a(href="https://www2.gov.bc.ca/StaticWebResources/static/gov3/html/contact-us.html", "Contact", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;"))
                                                  )
                                         )
                             )
                      )
                      )
             )
)

# Define server logic ----
server <- function(input, output) {
  
  output$timeSeries <- renderPlot({
    
    # user specificationS
    ecoprov <- ecoprovs[which(ecoprov.names==input$ecoprov.name)]
    yeartime1 <- yeartimes[which(yeartime.names==input$yeartime1)]
    yeartime2 <- if(input$compare==T) yeartimes[which(yeartime.names==input$yeartime2)] else yeartimes[which(yeartime.names==input$yeartime1)]
    element1 <- elements[which(element.names==input$element1)]
    element2 <- if(input$compare==T) elements[which(element.names==input$element2)] else elements[which(element.names==input$element1)]
    variable1 <- paste(element1, yeartime1, sep= if(yeartime1%in%seasons) "_" else "")
    variable2 <- paste(element2, yeartime2, sep= if(yeartime2%in%seasons) "_" else "")
    gcms1 <- input$gcms1
    scenarios1 <- c("historical", input$scenarios1)
    nums <- if(input$compare==T) c(1,2) else c(1)
    
    # BC/ecoprovince mean observed climate
    if(ecoprov=="BC") s <- 1:dim(obs.ts)[1] else {
      for(i in 1:length(unique(obs.ts$Year))){ if(i==1) s <- which(dem.pts$id2==ecoprov) else s <- c(s,which(dem.pts$id2==ecoprov)+length(dem.pts$id2)*(i-1))  }
    }
    obs.ts.mean <- aggregate(obs.ts[s,], by=list(obs.ts$Year[s]), FUN = mean, na.rm=T)[,-1]
    obs.ts.mean <- cbind(obs.ts.mean, (obs.ts.mean[,2:13]+obs.ts.mean[,14:25])/2)
    names(obs.ts.mean) <- c(names(obs.ts.mean)[1:37], paste("Tave", monthcodes, sep=""))
    
    # BC/ecoprovince mean ERA5 climate
    if(input$era5==T){
      if(ecoprov=="BC") s <- 1:dim(era5.ts)[1] else {
        for(i in 1:length(unique(era5.ts$Year))){ if(i==1) s <- which(dem.pts$id2==ecoprov) else s <- c(s,which(dem.pts$id2==ecoprov)+length(dem.pts$id2)*(i-1))  }
      }
      era5.ts.mean <- aggregate(era5.ts[s,], by=list(era5.ts$Year[s]), FUN = mean, na.rm=T)[,-1]
      era5.ts.mean <- cbind(era5.ts.mean, (era5.ts.mean[,2:13]+era5.ts.mean[,14:25])/2)
      names(era5.ts.mean) <- c(names(era5.ts.mean)[1:37], paste("Tave", monthcodes, sep=""))
    }
    
    ## Assemble the data that will be used in the plot
    alldata <- vector() # a vector of all data on the plot for setting the ylim (y axis range)
    num <- 1
    for(num in nums){
      
      # data for observations
      yeartime <- get(paste("yeartime",num,sep=""))
      element <- get(paste("element",num,sep=""))
      variable <- get(paste("variable",num,sep=""))
      x1 <- unique(obs.ts.mean[,1])
      if(yeartime%in%seasons){
        seasonmonths <- seasonmonth.mat[which(seasons==yeartime),]
        y1 <- obs.ts.mean[,which(names(obs.ts.mean)%in%paste(element,seasonmonths, sep=""))]
        if(yeartime=="wt") y1[2:dim(y1)[1],grep(12, names(y1))] <- y1[1:(dim(y1)[1]-1),grep(12, names(y1))] #advance december by one year (doesn't account for first year in series, but not a big deal)
        y1 <- apply(y1, 1, if(element=="PPT") sum else mean)
      } else {
        y1 <- obs.ts.mean[,which(names(obs.ts.mean)==variable)]
      }
      baseline.obs <- mean(y1[which(x1%in%1951:1980)])
      recent.obs <- mean(y1[(length(y1)-10):(length(y1))])
      
      #data for GCMs
      # ensstat <- ensstats[1]
      for(ensstat in ensstats[c(3,1,2)]){ #need to reorder the enstats so that mean comes first, for bias correction
        # scenario <- scenarios[1]
        ## Note: rather than reading in a single large data file, this app stores data in many (~2000) small files, and reads them in on demand by user input
        data <- read.csv(paste("data/", paste(ensstat, ecoprov, get(paste("variable",num, sep="")), "csv", sep="."), sep=""))
        temp.historical <- data[which(data[,1]=="historical"),-1]
        for(scenario in scenarios1){
          temp <- data[which(data[,1]==scenario),-1]
          if(scenario != "historical"){
            temp <- rbind(temp.historical[dim(temp.historical)[1],match(names(temp), names(temp.historical))], temp) # add last year of historical runs
          }
          if(scenario == "historical") if(ensstat=="ensmean") baseline.mod <- apply(temp[which(temp[,1]%in%1951:1980),-1], 2, mean)
          
          alldata <- c(alldata, as.vector(unlist(temp[-1]))) #store values in a big vector for maintaining a constant ylim
          # optional bias correction
          if(input$biascorrect==T){
            if(element=="PPT"){
              delta <- baseline.obs/baseline.mod
              delta <- delta[which(names(delta)!="compile")]
              temp[,-1] <- sweep(temp[,-1], 2, delta, '*')
            } else {
              delta <- baseline.obs-baseline.mod
              delta <- delta[which(names(delta)!="compile")]
              temp[,-1] <- sweep(temp[,-1], 2, delta[match(names(temp[-1]), names(delta))], '+')
            }
          }
          temp$compile <- if(length(gcms1)==0) rep(NA, dim(temp)[1]) else if(length(gcms1)==1) temp[,which(names(temp)==gcms1)] else apply(temp[,which(names(temp)%in%gcms1)], 1, substr(ensstat, 4, nchar(ensstat)), na.rm=T)
          assign(paste(ensstat, scenario, num, sep="."), temp)
        }
      }
    }
    
    # PLOT
    par(mfrow=c(1,1), mar=c(3,3,0.1,3), mgp=c(1.75, 0.25, 0), cex=1.4)
    if(element1==element2){
      ylab <- element.names.units[[which(elements==element1)]]
    } else {
      ylab <- if("PPT"%in%c(element1, element2)) bquote(Precipitation~"("*mm*")"~or~Mean ~ temperature ~ "(" * degree * C * ")") else element.names.units[[1]]
    }
    plot(0, col="white", xlim=c(1900, 2100), ylim=range(alldata, na.rm = T), xaxs="i", tck=0, xlab="", ylab=ylab)
    
    num <- 1
    for(num in nums){
      yeartime <- get(paste("yeartime",num,sep=""))
      element <- get(paste("element",num,sep=""))
      variable <- get(paste("variable",num,sep=""))
      
      # data for observations
      x1 <- unique(obs.ts.mean[,1])
      x2 <- unique(era5.ts.mean[,1])
      if(yeartime%in%seasons){
        seasonmonths <- seasonmonth.mat[which(seasons==yeartime),]
        y1 <- obs.ts.mean[,which(names(obs.ts.mean)%in%paste(element,seasonmonths, sep=""))]
        if(yeartime=="wt") y1[2:dim(y1)[1],grep(12, names(y1))] <- y1[1:(dim(y1)[1]-1),grep(12, names(y1))] #advance december by one year (doesn't account for first year in series, but not a big deal)
        y1 <- apply(y1, 1, if(element=="PPT") sum else mean)
      } else {
        y1 <- obs.ts.mean[,which(names(obs.ts.mean)==variable)]
      }
      baseline.obs <- mean(y1[which(x1%in%1951:1980)])
      recent.obs <- mean(y1[(length(y1)-10):(length(y1))])
      
      # data for era5
      if(input$era5==T){
        x2 <- unique(era5.ts.mean[,1])
        if(yeartime%in%seasons){
          seasonmonths <- seasonmonth.mat[which(seasons==yeartime),]
          y2 <- era5.ts.mean[,which(names(era5.ts.mean)%in%paste(element,seasonmonths, sep=""))]
          if(yeartime=="wt") y2[2:dim(y2)[1],grep(12, names(y2))] <- y2[1:(dim(y2)[1]-1),grep(12, names(y2))] #advance december by one year (doesn't account for first year in series, but not a big deal)
          y2 <- apply(y2, 1, if(element=="PPT") sum else mean)
        } else {
          y2 <- era5.ts.mean[,which(names(era5.ts.mean)==variable)]
        }
      }
      
      if(input$compile==T) gcms1 <- "compile" #this prevents the plotting of individual GCM projections and plots a single envelope for the ensemble as a whole. 
      for(gcm in gcms1){
        # scenario <- scenarios1[1]
        for(scenario in scenarios1[order(c(1,4,5,3,2)[which(scenarios%in%scenarios1)])]){
          
          for(ensstat in ensstats){
            temp <- get(paste(ensstat, scenario, num, sep="."))
            x <- temp[,1]
            temp <- temp[,which(names(temp)==gcm)]
            assign(ensstat, temp)
            if(scenario == "historical"){
              assign(paste(ensstat, scenario, sep="."), temp)
            }
          }
          
          # colScheme <- c("gray60", "seagreen", "goldenrod4", "darkorange3", "darkred")
          colScheme <- c("gray60", "dodgerblue4", "seagreen", "darkorange3", "darkred")
          # colScheme <- c("gray80", "#1d3354", "#e9dc3d", "#f11111", "#830b22")
          polygon(c(x, rev(x)), c(ensmin, rev(ensmax)), col=alpha(colScheme[which(scenarios==scenario)], if(gcm=="ensemble") 0.5 else 0.5), border=colScheme[which(scenarios==scenario)])
          if(input$showmean==T) lines(x, ensmean, col=colScheme[which(scenarios==scenario)], lwd=2)
          
          if(input$refline==T){
            ref.temp <- mean(ensmean.historical[101:130])
            lines(1951:1980, rep(ref.temp, 30), lwd=2)
            lines(c(1980,2100), rep(ref.temp, 2), lty=2)
          }
          
          if(scenario != "historical"){
            par(xpd=T)
            baseline <- mean(ensmean.historical[1:50])
            projected <- mean(ensmean[(length(x)-10):(length(x))])
            if(element=="PPT"){
              change <- round(projected/baseline-1,2)
              if(is.na(change)==F) text(2098,projected, if(change>0) paste("+",change*100,"%", sep="") else paste(change*100,"%", sep=""), col=colScheme[which(scenarios==scenario)], pos=4, font=2, cex=1)
            } else {
              change <- round(projected-baseline,1)
              if(is.na(change)==F) text(2098,projected, if(change>0) paste("+",change,"C", sep="") else paste(change,"C", sep=""), col=colScheme[which(scenarios==scenario)], pos=4, font=2, cex=1)
            }
            par(xpd=F)
          }
          
          print(scenario)
        }
        print(gcm)
      }
      
      # Text to identify the time of year
      # if(input$compare==T){
      if(element1==element2){
        label <- yeartime.names[which(yeartimes==yeartime)]
      } else {
        label <- paste(yeartime.names[which(yeartimes==yeartime)], get(paste("element", num, sep="")))
      }
      temp <- get(paste("ensmax.historical", num, sep="."))
      text(1915,mean(temp$compile[60:80]), label, col="black", pos=3, font=2, cex=1)
      # }
      
      # add in observations
      obs.color <- "blue"
      lines(x1[which(x1<1951)], y1[which(x1<1951)], lwd=3, lty=3, col=obs.color)
      lines(x1[which(x1>1949)], y1[which(x1>1949)], lwd=3, col=obs.color)
      if(element=="PPT"){
        change <- round(recent.obs/baseline.obs-1,2)
        text(2018,recent.obs, if(change>0) paste("+",change*100,"%", sep="") else paste(change*100,"%", sep=""), col=obs.color, pos=4, font=2, cex=1)
      } else {
        change <- round(recent.obs-baseline.obs,1)
        text(2018,recent.obs, if(change>0) paste("+",change,"C", sep="") else paste(change,"C", sep=""), col=obs.color, pos=4, font=2, cex=1)
      }
      lines(1951:1980, rep(baseline.obs, 30), lwd=1, col=obs.color)
      lines(c(1980,2019), rep(baseline.obs, 2), lty=2, col=obs.color)
      
      era5.color <- "darkorange"
      if(input$era5==T){
        lines(x2, y2, col=era5.color, lwd=2)
      }
      
      #legend
      a <- 1
      b <- if(input$era5==T) 2 else NA
      c <- if(length(gcms1>0)) 3 else NA
      s <- !is.na(c(a,b,c))
      legend("topleft", title = "Historical Period", legend=c("Station observations", "ERA5 reanalysis", "GCM simulations (min & max)")[s], bty="n",
             lty=c(1,1,NA)[s], col=c(obs.color, era5.color, NA)[s], lwd=c(3,2,NA)[s], pch=c(NA,NA, 22)[s], pt.bg = c(NA, NA, colScheme[1])[s], pt.cex=c(NA,NA,2)[s])
      
      s <- which(scenarios[-1]%in%input$scenarios1)
      legend("top", title = "Future Scenarios", legend=scenario.names[-1][s], bty="n",
             lty=c(NA,NA,NA,NA)[s], col=c(NA,NA,NA,NA)[s], lwd=c(NA,NA,NA,NA)[s], pch=c(22, 22, 22, 22)[s], pt.bg = colScheme[-1][s], pt.cex=c(2,2,2,2)[s])
      
      
      mtext(ecoprov.names[which(ecoprovs==ecoprov)], side=1, line=-1.5, adj=0.95, font=2, cex=1.4)
      
      print(num)
    }
    box()
  },
  height=reactive(ifelse(!is.null(input$innerWidth),input$innerWidth*0.4,0))
  )
  
  output$table <- DT::renderDataTable({
    DT::datatable(modelMetadata, 
                  options = list(pageLength = dim(modelMetadata)[1]), 
                  rownames= FALSE, 
                  caption = 'Model Metadata. Number of runs for each scenario are what we have downloaded, not necessarily what are currently available.'
    )
  })
  
}

# Run the app ----
shinyApp(ui = ui, server = server)


