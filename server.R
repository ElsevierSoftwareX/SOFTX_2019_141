#libraries
library(arules)
library(arulesViz)
library(shinythemes)
library(leaflet)
library(RColorBrewer)
library(scales)
library(lattice)
library(dplyr)
library(plotly)
library(visNetwork)
library(networkD3)
library(shinyjs)
library(shinyWidgets)

draw <- vc
# include electre_tri.R file for association rules selection
if(!exists("foo", mode="function")) source("Electre_tri.R")

# Rules to data frame
rules2df <- function(rules, list=F){  
  df <- as(rules, 'data.frame')
  df[,1] <- as.character(df[,1])
  df$lhs <- sapply(df[,1], function(x) strsplit(x, split=' => ')[[1]][1])
  df$rhs <- sapply(df[,1], function(x) strsplit(x, split=' => ')[[1]][2])
  df$lhs <- gsub(pattern='\\{', replacement='', x=df$lhs)
  df$lhs <- gsub(pattern='}', replacement='', x=df$lhs)
  df$rhs <- gsub(pattern='\\{', replacement='', x=df$rhs)
  df$rhs <- gsub(pattern='}', replacement='', x=df$rhs)
  
  if(list==T){
    p <- rules@lhs@data@p
    i <- rules@lhs@data@i+1
    lhsItems <- unlist(rules@lhs@itemInfo@.Data)
    lhsL <- list()
    for(j in 2:length(p)) lhsL[[j-1]] <- lhsItems[i[(p[j-1]+1):(p[j])]]
    df$lhs <- lhsL
    
    p <- rules@rhs@data@p
    i <- rules@rhs@data@i+1
    rhsItems <- unlist(rules@rhs@itemInfo@.Data)
    rhsL <- list()
    for(j in 2:length(p)) rhsL[[j-1]] <- rhsItems[i[(p[j-1]+1):(p[j])]]
    df$rhs <- rhsL
  }
  return(df)
}

depthbin <- function(ser, nbins=10, qtype=7, digits=10, labelRange=T, labelPct=F, labelOrder=F) {
  cutpts <- quantile(ser, probs=seq(0, 1, 1/nbins), na.rm=T, type=qtype)
  if(length(unique(cutpts))==nbins+1) {
    returnser <- cut(ser, breaks=cutpts, right=T, include.lowest=T)  
  } else {
    alldup <- vector()
    while(length(unique(cutpts))+length(alldup) < nbins+1) {
      dup <- cutpts[duplicated(cutpts)]
      dups <- unique(dup)
      alldup <- c(alldup, dups)
      dupL <- length(alldup) + length(dups)
      ser2 <- ser[which(!ser %in% alldup)]
      cutpts <- quantile(ser2, probs=seq(0, 1, 1/(nbins-length(dups))), na.rm=T, type=qtype)
    }
    cutpts <- c(unique(cutpts), alldup)
    returnser <- cut(ser, breaks=cutpts, include.lowest=T, dig.lab=digits, right=F)
  }
  if(sum(labelRange, labelPct, labelOrder)==0) {
    labelRange <- T
    warning('arguments labelRange, labelOrder, labelPct should not all be set to FALSE. Setting labelRange to TRUE.')
  }
  rawlev <- levels(returnser)
  if (labelRange==T) levels(returnser) <- paste0(levels(returnser), rawlev)
  if (labelOrder==T) levels(returnser) <- paste0(levels(returnser), ' ', 1:length(rawlev), '/', length(rawlev))
  if (labelPct==T) levels(returnser) <- paste0(levels(returnser), ' ', paste0('(', as.character(round(table(returnser)/length(returnser)*100, 1)), '%)'))
  for(i in 1:length(levels(returnser))) levels(returnser)[i] <- substr(levels(returnser)[i], nchar(rawlev[i])+1, nchar(levels(returnser)[i]))
  return(returnser)
}


roundCut <- function(x, r=1){
  x <- as.character(x)
  b <- substr(x,0,1)
  e <- substr(x, nchar(x), nchar(x))
  xx <- substr(x, 2, nchar(x)-1)
  xx1 <- round(as.numeric(sapply(xx, function(z) strsplit(z, ',')[[1]][1])), r)
  xx2 <- round(as.numeric(sapply(xx, function(z) strsplit(z, ',')[[1]][2])), r)
  return(paste(b, xx1, ', ', xx2, e, sep=''))
}

binCat <- function(x, ncat=NULL, maxp=NULL, results=F, setNA=NA, keepNA=F) {
  if(is.null(maxp)==F & is.null(ncat)==F) warning("Parameters 'ncat' and 'maxp' are both specified.  It is advisable to only specify one of these criteria.  Algorithm will stop at the first criteria met.")
  if(is.na(setNA)==F) x[is.na(x)] <- setNA
  
  ncat <- min(ncat, length(unique(x)))
  x <- as.character(x)
  n <- length(x)
  if(is.null(maxp)) maxp <- 1
  
  for(i in 1:length(unique(x))){
    xc <- x
    x1 <- sort(table(xc, exclude=NULL), decreasing=T)[1:i]
    catp <- sum(x1)/n
    if(i==ncat | catp>maxp)  {
      x2 <- sort(table(xc, exclude=NULL), decreasing=T)[1:(i+1)]
      if(keepNA==T) {xc[which(!xc %in% c(names(x2), setNA))] <- 'other'
      } else {xc[which(!xc %in% names(x2))] <- 'other'}
      returnser <- xc
      break
    }
  }
  if(results==T) print(sort(table(returnser)/n, decreasing=T))
  return(returnser)  
}

# shiny server with functions
shinyServer(function(input, output, session) {
    # mini left sidebar
    runjs('
        var el2 = document.querySelector(".skin-blue");
        el2.className = "skin-blue sidebar-mini";
        ')
  
    rules <- reactive({
      if (is.null(input$file))
        # if no input data file (csv) display default data
        dataset <-  read.csv("dataset.csv")
      else {
        dataset <- read.csv(input$file$datapath)
      }
      
      # changing data  type to factor
      for(i in 1:10){
        dataset[,i]<-factor(dataset[,i])

      }
      dataset$LATITUDE <- discretize(dataset$LATITUDE)
      dataset$LONGITUDE <- discretize(dataset$LONGITUDE)

      output$choose_columns <- renderUI({
        checkboxGroupInput("cols", "Choose variables:", 
                           choices  = colnames(dataset),
                           selected = colnames(dataset))
      })
    
    # data structure: dataset
    output$dataset_ds <- DT::renderDataTable({
        ds <- read.csv("dataset.csv")
        DT::datatable(ds, options = list(scrollX = T))
      })
      
    # data structure: decision matrix
    output$descion_matrix_ds <- DT::renderDataTable({
        ds <- read.csv("decision_matrix_model.csv")
        ds
      })
      
    # Item Frequency Plot
    output$itemFreqPlot <- renderPlot({
        trans <- as(dataset[,input$cols], 'transactions')
        itemFrequencyPlot(trans)
      }, height=800, width=800)
    
    
    output$choose_lhs <- renderUI({
      checkboxGroupInput("colsLHS", "Choose LHS variables:", 
                         choices  = input$cols,
                         selected = input$cols[1])
    })
    
    output$choose_rhs <- renderUI({
      checkboxGroupInput("colsRHS", "Choose RHS variables:", 
                         choices  = input$cols,
                         selected = input$cols[1])
    })
    
    # generating association rules
    rules <-apriori(dataset,parameter=list(support=input$supp,confidence=input$conf,minlen=input$minL,maxlen=input$maxL))
    return(sort(rules))
    if(input$rhsv=='Subset' & input$lhsv!='Subset'){
      varsR <- character()
      for(i in 1:length(input$colsRHS)){
        tmp <- with(dataset, paste(input$colsRHS[i], '=', levels(as.factor(get(input$colsRHS[i]))), sep=''))
        varsR <- c(varsR, tmp)
      }
      ar <- subset(arAll, subset=rhs %in% varsR)
      
    } else if(input$lhsv=='Subset' & input$rhsv!='Subset') {
      varsL <- character()
      for(i in 1:length(input$colsLHS)){
        tmp <- with(dataset, paste(input$colsLHS[i], '=', levels(as.factor(get(input$colsLHS[i]))), sep=''))
        varsL <- c(varsL, tmp)
      }
      ar <- subset(arAll, subset=lhs %in% varsL)
      
    } else if(input$lhsv=='Subset' & input$rhsv=='Subset') {
      varsL <- character()
      for(i in 1:length(input$colsLHS)){
        tmp <- with(dataset, paste(input$colsLHS[i], '=', levels(as.factor(get(input$colsLHS[i]))), sep=''))
        varsL <- c(varsL, tmp)
      }
      varsR <- character()
      for(i in 1:length(input$colsRHS)){
        tmp <- with(dataset, paste(input$colsRHS[i], '=', levels(as.factor(get(input$colsRHS[i]))), sep=''))
        varsR <- c(varsR, tmp)
      }
      ar <- subset(arAll, subset=lhs %in% varsL & rhs %in% varsR)
      
    } else {
      ar <- arAll
    }
    quality(ar)$conviction <- interestMeasure(ar, method='conviction', transactions=tr)
    quality(ar)$hyperConfidence <- interestMeasure(ar, method='hyperConfidence', transactions=tr)
    quality(ar)$cosine <- interestMeasure(ar, method='cosine', transactions=tr)
    quality(ar)$chiSquare <- interestMeasure(ar, method='chiSquare', transactions=tr)
    quality(ar)$coverage <- interestMeasure(ar, method='coverage', transactions=tr)
    quality(ar)$doc <- interestMeasure(ar, method='doc', transactions=tr)
    quality(ar)$gini <- interestMeasure(ar, method='gini', transactions=tr)
    quality(ar)$hyperLift <- interestMeasure(ar, method='hyperLift', transactions=tr)
    ar
  })
 
  # statistics about rules (apriori algorithm)
  output$statistics <- renderPrint({
    capture.output(rules())
    if (is.null(rules()))
      return(invisible())
    summary(rules())
  })
  
  # Rule length
  nR <- reactive({
    nRule <- ifelse(input$samp == 'All Rules', length(rules()), input$nrule)
  })
  
  # Grouped Plot visualization
  output$groupedPlot <- renderPlot({
    ar <- rules()
    plot(sort(ar, by=input$sort)[1:nR()], method='grouped', control=list(k=input$k))
  }, height=800, width=800)
  
  # Graph Plot visualization
  output$graphPlot <- renderVisNetwork({
    ar <- rules()
    plot(sort(ar, by=input$sort)[1:nR()], method='graph', engine='htmlwidget', control=list(type=input$graphType))
  })
  
  # Scatter Plot visualization
  output$scatterPlot <- renderPlotly({
    ar <- rules()
    plotly_arules(sort(ar, by=input$sort)[1:nR()], method='scatterplot')
  })
  
  # Parallel Coordinates Plot visualization
  output$paracoordPlot <- renderPlot({
    ar <- rules()
    plot(sort(ar, by=input$sort)[1:nR()], method='paracoord')
  }, height=800, width=800)
  

  
  # Rules Data Table
  output$rulesDataTable <- DT::renderDataTable({
    ar <- rules()
    rulesdt <- rules2df(ar)
    DT::datatable(rulesdt, options = list(scrollX = T))
  })
  
  
  # Download data to csv
  output$downloadData <- downloadHandler(
    filename = 'arules_data.csv',
    content = function(file) {
      write.csv(rules2df(rules()), file)
    }
  )
  
  #Evaluation of extracted rules: MCDA
  #This function is repsonsible for loading in the selected file
  filedata_DM <- reactive({
    if (is.null(input$datafile)){
      # if no decsion matrix load dafault decision matrix
      read.csv("decision_matrix_model.csv")
    }
    else {
      read.csv(infile$datapath,header=TRUE)
    }
  })

  # alternatives (actions) definition
  alternatives <- c("R1","R2","R3","R4","R5","R6","R7","R8","R9","R10","R11","R12","R13","R14","R15","R16","R17","R18","R19","R20")
  criteria <- c( "g1","g2","g3","g4","g5")
  criteriaWeights <- c(0.25,0.45,0.10,0.12,0.08)
  minmaxcriteria <- c("max","max","max","max","max")
  profiles <- cbind(c(-100,-50),c(-1000,-500),c(4,7),c(4,7),c(15,20))
  profiles_names <-c("b1","b2")
  
  # thresholds vector
  IndifferenceThresholds <- c(15,80,1,0.5,1)
  PreferenceThresholds <- c(40,350,3,3.5,5)
  VetoThresholds <- c(100,850,5,4.5,8)
 
  # This previews the CSV data file
  output$filetable_DM1 <- renderDataTable({
    filedata_DM()
  })
  # computation steps
  # partialConcordance_al_pr_gj
  output$partialConcordance_al_pr_gj <- renderDataTable({
    data2=filedata_DM()
    performanceMatrix <- cbind(
      c(data2$C1),
      c(data2$C2),				
      c(data2$C3),					
      c(data2$C4),		
      c(data2$C5)
    )
    result<- Electre_tri(performanceMatrix,
                        alternatives,
                        profiles,
                        profiles_names,
                        criteria,
                        minmaxcriteria,
                        criteriaWeights,
                        IndifferenceThresholds,
                        PreferenceThresholds,
                        VetoThresholds,
                        lambda=NULL)
    result$PartialConcordance_al_pr_gj
  })
  
  # partialConcordance_pr_al_gj
  output$partialConcordance_pr_al_gj <- renderDataTable({
    data2=filedata_DM()
    performanceMatrix <- cbind(
      c(data2$C1),
      c(data2$C2),				
      c(data2$C3),					
      c(data2$C4),		
      c(data2$C5)
    )
    result_partialConcordance_pr_al_gj<- Electre_tri(performanceMatrix,
                         alternatives,
                         profiles,
                         profiles_names,
                         criteria,
                         minmaxcriteria,
                         criteriaWeights,
                         IndifferenceThresholds,
                         PreferenceThresholds,
                         VetoThresholds,
                         lambda=NULL)
    result_partialConcordance_pr_al_gj$partialConcordance_pr_al_gj
  })
  
  # partialDiscordance_al_pr_gj
  output$partialDiscordance_al_pr_gj <- renderDataTable({
    data2=filedata_DM()
    performanceMatrix <- cbind(
      c(data2$C1),
      c(data2$C2),				
      c(data2$C3),					
      c(data2$C4),		
      c(data2$C5)
    )
    partialDiscordance_al_pr_gj<- Electre_tri(performanceMatrix,
                                                     alternatives,
                                                     profiles,
                                                     profiles_names,
                                                     criteria,
                                                     minmaxcriteria,
                                                     criteriaWeights,
                                                     IndifferenceThresholds,
                                                     PreferenceThresholds,
                                                     VetoThresholds,
                                                     lambda=NULL)
    partialDiscordance_al_pr_gj$partialDiscordance_al_pr_gj
  })
  
  # partialDiscordance_pr_al_gj
  output$partialDiscordance_pr_al_gj <- renderDataTable({
    data2=filedata_DM()
    performanceMatrix <- cbind(
      c(data2$C1),
      c(data2$C2),				
      c(data2$C3),					
      c(data2$C4),		
      c(data2$C5)
    )
    partialDiscordance_pr_al_gj<- Electre_tri(performanceMatrix,
                                              alternatives,
                                              profiles,
                                              profiles_names,
                                              criteria,
                                              minmaxcriteria,
                                              criteriaWeights,
                                              IndifferenceThresholds,
                                              PreferenceThresholds,
                                              VetoThresholds,
                                              lambda=NULL)
    partialDiscordance_pr_al_gj$partialDiscordance_pr_al_gj
  })
  
  # globalconcordance
  output$globalconcordance <- renderDataTable({
    data2=filedata_DM()
    performanceMatrix <- cbind(
      c(data2$C1),
      c(data2$C2),				
      c(data2$C3),					
      c(data2$C4),		
      c(data2$C5)
    )
    globalconcordance<- Electre_tri(performanceMatrix,
                                              alternatives,
                                              profiles,
                                              profiles_names,
                                              criteria,
                                              minmaxcriteria,
                                              criteriaWeights,
                                              IndifferenceThresholds,
                                              PreferenceThresholds,
                                              VetoThresholds,
                                              lambda=NULL)
    globalconcordance$globalconcordance
  })
  
  # credibility indexes
  output$credibility <- renderDataTable({
    data2=filedata_DM()
    performanceMatrix <- cbind(
      c(data2$C1),
      c(data2$C2),				
      c(data2$C3),					
      c(data2$C4),		
      c(data2$C5)
    )
    credibility<- Electre_tri(performanceMatrix,
                                    alternatives,
                                    profiles,
                                    profiles_names,
                                    criteria,
                                    minmaxcriteria,
                                    criteriaWeights,
                                    IndifferenceThresholds,
                                    PreferenceThresholds,
                                    VetoThresholds,
                                    lambda=NULL)
    credibility$credibility
  })
  
  # relations
  output$relations <- renderDataTable({
    data2=filedata_DM()
    performanceMatrix <- cbind(
      c(data2$C1),
      c(data2$C2),				
      c(data2$C3),					
      c(data2$C4),		
      c(data2$C5)
    )
    relations<- Electre_tri(performanceMatrix,
                              alternatives,
                              profiles,
                              profiles_names,
                              criteria,
                              minmaxcriteria,
                              criteriaWeights,
                              IndifferenceThresholds,
                              PreferenceThresholds,
                              VetoThresholds,
                              lambda=NULL)
    relations$relations
  })
  
  # Assignments of results (rules)
  output$assignment <- renderDataTable({
    data2=filedata_DM()
    performanceMatrix <- cbind(
      c(data2$C1),
      c(data2$C2),				
      c(data2$C3),					
      c(data2$C4),		
      c(data2$C5)
    )
    assignment<- Electre_tri(performanceMatrix,
                              alternatives,
                              profiles,
                              profiles_names,
                              criteria,
                              minmaxcriteria,
                              criteriaWeights,
                              IndifferenceThresholds,
                              PreferenceThresholds,
                              VetoThresholds,
                              lambda=NULL)
    #assignment[8:9]
    #assignment$Pessimistic
    lis_assig <- list(Pessimistic=assignment$Pessimistic,Optimistic=assignment$Optimistic)
    df <- as.data.frame(lis_assig)
    df
  })
  

  # Interactive Map
  # Create the map
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles(
        urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
        attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
      ) %>%
      setView(lng = -7.973328, lat = 31.669746, zoom = 10)
  })
  
  # Choose just one vehicle
  drawvalue <- reactive({if (input$vehicle == ''){return(vc)}else{
    t <- filter(vc, VEHICLE.TYPE.CODE.1 == input$vehicle | VEHICLE.TYPE.CODE.2 == input$vehicle)
    return(t)
  }})
  
  # This observer is responsible for maintaining the circles and legend,
  observe({
    
    colorBy <- input$color
    sizeBy <- input$size
    draw <- drawvalue()
    
    colorData <- draw[[colorBy]]
    if (colorBy == "NUMBER.OF.PERSONS.INJURED"|colorBy == "NUMBER.OF.PERSONS.KILLED") {
      pal <- colorBin(heat.colors(7), colorData, 7)} else{
        pal <- colorFactor("Set1", colorData)
      }    
    
    radius <- draw[[sizeBy]] / 9 * 250 + 30
    
    if (input$cluster == TRUE){
      leafletProxy("map", data = draw) %>%
        clearShapes() %>%
        showGroup('Cluster') %>%
        addCircles(~LONGITUDE, ~LATITUDE, radius=radius, group = "Circle",
                   stroke=FALSE, fillOpacity=0.8, fillColor=pal(colorData)) %>%
        addCircleMarkers(~LONGITUDE, ~LATITUDE, radius = 0, group = "Cluster",
                         clusterOptions = markerClusterOptions())%>%
        addLegend("bottomleft", pal=pal, values=colorData, title=colorBy,
                  layerId="colorLegend")
    }else{
      leafletProxy("map", data = draw) %>%
        clearShapes() %>%
        hideGroup('Cluster') %>%
        addCircles(~LONGITUDE, ~LATITUDE, radius=radius, group = "Circle",
                   stroke=FALSE, fillOpacity=0.8, fillColor=pal(colorData)) %>%
        addLegend("bottomleft", pal=pal, values=colorData, title=colorBy,
                  layerId="colorLegend")
    }
  })
  
  # Show a popup at the given location
  showvcPopup <- function(eventid, lat, lng) {
    draw <- drawvalue()
    selectedvc <- filter(draw, LATITUDE == lat, LONGITUDE == lng)
    entry <- function(row){
      result <- as.character(tagList(
        tags$h6(row[2], row[3]), 
        tags$strong(HTML(sprintf("%s & %s", row[9], row[10]))), tags$br(),
        sprintf("Vehicles: %s & %s", row[26], row[27]), tags$br(),
        sprintf("Factors: %s & %s", row[20], row[21]), tags$br(),
        sprintf("%s Injuries & %s Deaths", row[12], row[13]), tags$br()))
      return(result)
    }
    content <- apply(selectedvc, 1, entry)
    content <- paste0(content, collapse = "\n")
    leafletProxy("map") %>% addPopups(lng, lat, content, layerId = eventid)
  }
  
  # When map is clicked, show a popup with city info
  observe({
    leafletProxy("map") %>% clearPopups()
    event <- input$map_shape_click
    if (is.null(event))
      return()
    isolate({
      showvcPopup(event$id, event$lat, event$lng)
    })
  })
  
  # See Your Neighbourhood
  observe({
    zipcodes <- if (is.null(input$boroughs)) character(0) else {
      filter(cleantable, Borough %in% input$boroughs) %>%
        `$`('Zipcode') %>%
        unique() %>%
        sort()
    }
    stillSelected <- isolate(input$zipcodes[input$zipcodes %in% zipcodes])
    updateSelectInput(session, "zipcodes", choices = zipcodes,
                      selected = stillSelected)
  })
  
  # When actions is clicked, call popup function for the corresponding latitude and longitude
  observe({
    if (is.null(input$goto))
      return()
    isolate({
      map <- leafletProxy("map")
      map %>% clearPopups()
      dist <- 0.02
      zip <- input$goto$zip
      lat <- input$goto$lat
      lng <- input$goto$lng
      showvcPopup(zip, lat, lng)
      map %>% fitBounds(lng - dist, lat - dist, lng + dist, lat + dist)
    })
  })
  
  output$vctable <- DT::renderDataTable({
    df <- cleantable %>%
      filter(
        is.null(input$boroughs) | Borough %in% input$boroughs,
        is.null(input$zipcodes) | Zipcode %in% input$zipcodes
      ) %>%
      mutate(Action = paste('<a class="go-map" href="" data-lat="', Lat, '" data-long="', Long, '" data-zip="', Borough, '"><i class="fa fa-crosshairs"></i></a>', sep=""))
    action <- DT::dataTableAjax(session, df)
    
    DT::datatable(df, options = list(ajax = list(url = action)), escape = FALSE)
  })
  
  
  data_ie <- reactive({
    
    if(input$choose == "all"){
      df <- Time_of_day_fatalities %>% group_by(BOROUGH, DATE) %>% summarise(Morning_fatalities = sum(Morning_fatalities), Afternoon_fatalities = sum(Afternoon_fatalities), Evening_fatalities = sum(Evening_fatalities), Night_fatalities = sum(Night_fatalities))
      df <- subset(df, select = c("BOROUGH", "DATE", input$select))
      return (df)
    }
    else{
      
      Tim <- Time_of_fatalities[Time_of_fatalities$VEHICLE.TYPE.CODE.1 == input$choose,]
      Time_of_fatalities1 <- Tim %>% group_by(BOROUGH, DATE) %>% summarise(Morning_fatalities = sum(Morning_fatalities), Afternoon_fatalities = sum(Afternoon_fatalities), Evening_fatalities = sum(Evening_fatalities), Night_fatalities = sum(Night_fatalities))
      Time_of_fatalities1 <- subset(Time_of_fatalities1, select = c("BOROUGH", "DATE", input$select))
      return(Time_of_fatalities1) 
    }
    
    
  })
  
  output$plot_ie <- renderPlot({
    
    g = ggplot(data = data_ie(), aes(x = DATE))
    g + geom_line(aes(group=1, y = data_ie()[,3])) + facet_grid(~BOROUGH) + xlab('Year') + ylab('Number of Fatalities') + ggtitle('Total Fatalities based on time of the day') + theme_economist() + theme(legend.text=element_text(size=8)) + theme(axis.text.x = element_text(size = 8 , angle = 90, hjust = 0)) + theme(axis.text.y = element_text(size = 8 , angle = 0, hjust = 0)) + theme(plot.title = element_text(hjust = 0.5))
    
    
    
    
  })
  
  
  # Most Dangerous Intersections
  output$toptable <- DT::renderDataTable({
    # load prepared data
    df <- read.csv('Most_Dangerous_Intersections.csv') %>%
      mutate(Action = paste('<a class="go-map" href="" data-lat="', Lat, '" data-long="', Long, '" data-zip="', Borough, '"><i class="fa fa-crosshairs"></i></a>', sep=""))
    action <- DT::dataTableAjax(session, df)
    
    DT::datatable(df, options = list(ajax = list(url = action)), escape = FALSE)
  })
  
  predicted1 <- reactive({
    hw <- HoltWinters(myts)
    predict(hw, n.ahead = input$months, 
            prediction.interval = TRUE,
            level = as.numeric(input$interval))
  })
  
  output$dygraph1 <- renderDygraph({
    dygraph(predicted1(), main = "Predicted Injuries/Month") %>%
      dySeries(c("lwr", "fit", "upr"), label = "Deaths") %>%
      dyOptions(drawGrid = input$showgrid)
  })
  
  predicted2 <- reactive({
    hw1 <- HoltWinters(myts1)
    predict(hw1, n.ahead = input$months, 
            prediction.interval = TRUE,
            level = as.numeric(input$interval))
  })
  
  
  output$dygraph2 <- renderDygraph({
    dygraph(predicted2(), main = "Predicted Deaths/Month") %>%
      dySeries(c("lwr", "fit", "upr"), label = "Deaths") %>%
      dyOptions(drawGrid = input$showgrid)
  })
  
  # download dataset model
  output$dataset_model <- downloadHandler(
    filename <- function() {
      paste("dataset_model", "csv", sep=".")
    },
    content <- function(file) {
      file.copy("dataset_model.csv", file)
    },
    contentType = "csv"
  )
  
  # download decision matrix model
  output$decision_matrix_model <- downloadHandler(
    filename <- function() {
      paste("decision_matrix_model", "csv", sep=".")
    },
    content <- function(file) {
      file.copy("decision_matrix_model", file)
    },
    contentType = "csv"
  )
  
})