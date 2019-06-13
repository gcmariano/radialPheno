# ------------------ LIBS ---------
library(shiny)
library(RColorBrewer)
library(tidyquant)
library(plyr)


#----------- DRAW VISUALIZATION - FUNCTIONS TO DRAW AND COLOR -------------------------
addalpha <- function(colors, alpha=1.0) {
  r <- col2rgb(colors, alpha=T)
  r[4,] <- alpha*255
  r <- r/255.0
  return(rgb(r[1,], r[2,], r[3,], r[4,]))
}

colorRampPaletteAlpha <- function(colors, n=32, interpolate='linear') {
  # Create the color ramp normally
  cr <- colorRampPalette(colors, interpolate=interpolate)(n)
  # Find the alpha channel
  a <- col2rgb(colors, alpha=T)[4,]
  # Interpolate
  if (interpolate=='linear') {
    l <- approx(a, n=n)
  } else {
    l <- spline(a, n=n)
  }
  l$y[l$y > 255] <- 255 # Clamp if spline is > 255
  cr <- addalpha(cr, l$y/255.0)
  return(cr)
}

circle <- function(x, y, rad = 1, nvert = 500, ...){
  rads <- seq(0,2*pi,length.out = nvert)
  xcoords <- cos(rads) * rad + x
  ycoords <- sin(rads) * rad + y
  polygon(xcoords, ycoords, ...)
}

fillSegment<-function(rmenor, rmaior, thetamenor, thetamaior, segment.color){
  x<-0
  y<-0
  rads <- seq(thetamaior,thetamenor,length.out = 500)
  seg1.xcoords <- cos(rads) * rmenor + x
  seg1.ycoords <- sin(rads) * rmenor + y
  
  rads <- seq(thetamenor,thetamaior,length.out = 500)
  seg2.xcoords <- cos(rads) * rmaior + x
  seg2.ycoords <- sin(rads) * rmaior + y
  
  
  x0<- rmenor*cos(thetamenor)
  x1<- rmaior*cos(thetamenor)
  y0<- rmenor*sin(thetamenor)
  y1<- rmaior*sin(thetamenor)
  
  seg3.xcoords<- seq(x0,x1,length.out = 500)
  seg3.ycoords<-y0+ ((y1-y0)/(x1-x0))*(seg3.xcoords-x0)
  
  x0<- rmenor*cos(thetamaior)
  x1<- rmaior*cos(thetamaior)
  y0<- rmenor*sin(thetamaior)
  y1<- rmaior*sin(thetamaior)
  
  seg4.xcoords<- seq(x1,x0,length.out = 500)
  seg4.ycoords<-y0+ ((y1-y0)/(x1-x0))*(seg4.xcoords-x0)
  
  cord.x <- c(seg3.xcoords,seg2.xcoords,seg4.xcoords,seg1.xcoords) 
  cord.y <- c(seg3.ycoords,seg2.ycoords,seg4.ycoords,seg1.ycoords) 
  
  polygon(cord.x,cord.y,col=segment.color)
}

#main function 
drawVisualStructureImages<- function(ncircles, nmacrosegments, nmicrosegments, circle.names, macrosegment.names, microsegment.names, segment.colors, vr, species, minValue, maxValue){
  plot.size <- ncircles + 4.5
  plot(-plot.size:plot.size, type="n", xlim = c(-plot.size,plot.size), ylim = c(-plot.size,plot.size), xlab="",ylab="",asp = 1, axes = FALSE, main = substitute(paste("Species:", italic(species), sep=""), list(species=species)), cex.main=1.5)
  
  circle(0,0,3)
  for (i in 1:ncircles){
    circle(0,0,2+i+1)
  }
  
  rads <- seq(0,-2*pi, by = -2*pi/(nmicrosegments*nmacrosegments))
  segments(3*cos(rads),3*sin(rads),(2+ncircles+1)*cos(rads),(2+ncircles+1)*sin(rads))
  
  
  rads <- seq(0,-2*pi, by = -2*pi/(nmacrosegments))
  segments(3*cos(rads),3*sin(rads),(2+ncircles+3)*cos(rads),(2+ncircles+3)*sin(rads))
  rads <- seq(pi/2, -1.5*pi, by = -2*pi/nmacrosegments)
  x <- (2+ncircles+2.5) * cos(rads+(-pi/nmacrosegments))
  y <- (2+ncircles+2.5) * sin(rads+(-pi/nmacrosegments))
  text(x,y, labels = macrosegment.names, col = 1, cex = 1.5)
  
  radsMicroSegments <- seq(pi/2, 1.5*-pi, by = 2*-pi/(nmicrosegments*nmacrosegments))
  
  x <- (2+ncircles+1.5) * cos(radsMicroSegments+(-pi/(nmicrosegments*nmacrosegments)))
  y <- (2+ncircles+1.5) * sin(radsMicroSegments+(-pi/(nmicrosegments*nmacrosegments)))
  text(x,y, labels = microsegment.names, col = 1, cex = 1.5) 
  
  pal1 <- colorRampPalette(c("tan4", "tan"))
  pal2 <- colorRampPalette(c("springgreen4", "springgreen"))
  pal3 <- colorRampPalette(c("royalblue4", "royalblue"))
  pal4 <- colorRampPalette(c("purple4", "purple"))
  
  
  palette <- c(pal1(5), pal2(5),pal3(5),pal4(5))
  
  rads <- seq(pi/2,-1.5*pi, by = -2*pi/(nmicrosegments*nmacrosegments)) 
  
  temp <-  vr
  
  cuts <- round(seq(minValue, maxValue, length = 20), digits=3)
  
  for (i in 1:ncircles){
    count <- 1
    Col <- palette[as.numeric(cut(temp[,i], cuts))] 
    for(j in 1:(nmicrosegments * nmacrosegments)){
      fillSegment(2+i, 2+i+1, rads[j], rads[j+1], Col[j])
    }
  }  
  
  x <- seq(0,0,length.out=ncircles)
  y <- seq(3.5, by = 1.0, length.out = ncircles)
  text(x,y, labels = circle.names, col = 1, font = 2, cex = 1.5)
  
  legend(x=c((plot.size/2)+(ncircles+3)), y=5, legend=c("No data"), fill=c("white"), cex=1.5, bty="n")
  legend(x=c((plot.size/2)+(ncircles+3)), y=4, # << THIS IS THE HACKISH PART,
         title=c(c("Color Palette")),
         legend=c(cuts),
         fill =c(palette),
         ncol=1, cex =1.5, bty = "n")
  
}


#----------------------- SERVER FUNCTION ---------------------------------
shinyServer(function(input, output, session) {
  
  dataIndexSelected <- 1
  
  values <- reactiveValues()
  
  
  #--------------------- UI CONTROLLERS (FILE UPLOAD) --------------------------
  output$controllers <- renderUI({
    req(input$file1)
    tryCatch(
      {
        df <- read.csv(input$file1$datapath,
                       header = input$header,
                       sep = input$sep,
                       quote = input$quote)
        
        df <- as.data.frame(df)
        values$plot_graph <- 0
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
    
    years <- unique(df$year)
    months <- unique(df$month)
    days <- unique(df$day)
    values$doys <- unique(df$doy)
    
    print(paste("Size of months: ", length(months), "months: ", months))
    print(paste("Size of days: ", length(days), "days: ", days))
    print(paste("Size of doys: ", length(values$doys)))
    
    variables<-colnames(df)
    
    output$years <- renderText({
      years
    })
    
    output$doys <- renderText({
      doys
    })
    
    variablesAux <- subset(df, select=-c(year, month, doy))
    colVariablesAux <- colnames(variablesAux)
    tagList(
      titlePanel("Visualization options"),
      sliderInput("sliderYearsID",
                  label = "Years",
                  min = min(years),
                  max = max(years),
                  value = range(years),
                  width="100%",
                  dragRange = TRUE, step = 1),
      
      checkboxGroupInput(inputId = "checkBoxVariablesID", label = h3("Checkbox variables"), 
                         choices = colVariablesAux),
      
      radioButtons("radioMeanPeriodID", label = h3("Periodicity:"),
                   choices = list("Daily" = 1, "Weekly" = 3, "Monthly" = 2), 
                   selected = 1),
      textInput("speciesID", "Species: ", " "),
      textInput("roiID", "ROI Name: ", "ROI 1"),
      actionButton("doID", "View Data")
    )
  })
  
  
  #--------------------- READ UPLOADED FILE --------------------------
  observeEvent(input$doID, {
    tryCatch(
      {
        df <- read.csv(input$file1$datapath,
                       header = input$header,
                       sep = input$sep,
                       quote = input$quote)
        
        df <- as.data.frame(df)
        df[is.na(df)] <- 0
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
    
    #variables that change according to the filters
    values$yearsSelected <- format(seq(as.Date(as.character(c(input$sliderYearsID[1])), '%Y'), as.Date(as.character(c(input$sliderYearsID[2])), '%Y'), by="year"), '%Y')
    values$variablesSelected <- input$checkBoxVariablesID
    
    values$nmacrosegments <- 12
    values$macrosegment.names<-c("Jan","Feb","Mar","Apr","May","Jun","Jul","Ago","Sep","Oct","Nov","Dec")
    values$circle.names <- values$variablesSelected
    values$microsegment.names <- c("")
    values$macrosegment.colors <- c("")
    values$periodicitySelected <- input$radioMeanPeriodID
    values$ncircles <- length(values$variablesSelected)
    
    minMask <- 0
    maxMask <- 0
    
    for(y in 1:length(values$yearsSelected)){
      dataYear <- subset(df, year==values$yearsSelected[y])
      print(dataYear)
      assign(paste("data", values$yearsSelected[y], sep=""), dataYear)
      #new 
      days <- seq(as.Date(paste(unique(dataYear$year), "/01/01", sep="")), as.Date(paste(unique(dataYear$year), "/12/31", sep="")), by="day")
      months <- data.frame(interval = seq(as.Date(paste(unique(dataYear$year), "/01/01", sep="")), as.Date(paste(unique(dataYear$year), "/12/31", sep="")), by="month"))
      weeks <- data.frame(interval = seq(as.Date(paste(unique(dataYear$year), "/01/01", sep="")), as.Date(paste(unique(dataYear$year), "/12/31", sep="")), by="week")) 
      ###end new
      
      
      for(v in 1:length(values$variablesSelected)){
        var <-  round(subset(dataYear, select = values$variablesSelected[v]), digits = 3)
        print(var)
        
        #check if is neecessary complement the series with the another doys
        if(values$periodicitySelected == 1){
          
          if(length(values$doys) < 365){ 
            
            start <- paste(unique(dataYear$year), "-01-01", sep="")
            end <- paste(unique(dataYear$year), "-12-31", sep="")
            dateComplement <- data.frame(date=seq(as.Date(start), as.Date(end), by="days"))
            
            #original data with original dates
            varAux <- data.frame(interval = as.Date(dataYear$doy-1, origin=paste(dataYear$year, "-01-01", sep="")), value= var)
            print("varAux")
            print(dateComplement)
            
            #final dataframe
            var <- merge(varAux, dateComplement, by.x='interval', by.y='date', all.x=T, all.y=T)
            
            var[is.na(var)] <- 0
            var <- as.data.frame(var[,2])
          }
          
          values$nmicrosegments <- 30
          
          if(nrow(var) == 366){
            var <-  var[-c(366),]
          }
        }
        else if(values$periodicitySelected == 2){ #mean by monthly
          values$nmicrosegments <- 1
          
          varAux <- data.frame(interval = as.Date(dataYear$doy-1, origin=paste(dataYear$year, "-01-01", sep="")), value= var)
          
          namecol <- colnames(var)
          ##new
          varAux <- varAux[varAux[,2]>0,]
          ###end new
          
          xts.ts <- xts(varAux[,2], varAux$interval)
          (m <- apply.monthly(xts.ts, mean))
          index(m) <-  floor_date(index(m),"month")
          
          #new
          dfMonthAux <- cbind(data.frame(interval = index(m)), value = data.frame(m))
          dfJoinMonths <- join(months, dfMonthAux, by = 'interval', type = "left")
          var <- as.data.frame(dfJoinMonths$m)
          rownames(var) <- dfJoinMonths$interval
          ###end new
          
          
          #var <- as.data.frame(m)
          colnames(var) <- values$variablesSelected[v] #arrumar aqui para mostrar média deos meses para imagens por espécie e ano
          
          #new
          #put Na as 0
          var[is.na(var)] <- 0
          ##end new
        }  else if(values$periodicitySelected == 3){ #mean by week
          values$nmicrosegments <- 4
          varAux <- data.frame(interval = as.Date(dataYear$doy-1, origin=paste(dataYear$year, "-01-01", sep="")), value= var)
          
          ##new
          varAux <- varAux[varAux[,2]>0,]
          ###end new
          
          namecol <- colnames(var)
          xts.ts <- xts(varAux[,2], varAux$interval)
          (w <- apply.weekly(xts.ts, mean))
          #index(w) <- floor_date(index(w),"week") 
          
          
          dfWeekAux <- cbind(data.frame(interval = as.Date(rownames(data.frame(w)))), value = data.frame(w))
          dfJoinWeeks <- join(weeks, dfWeekAux, by = 'interval', type = "left")
          var <- as.data.frame(dfJoinWeeks$w)
          rownames(var) <- dfJoinWeeks$interval
          ###end new
          
          #var <- as.data.frame(m)
          colnames(var) <- values$variablesSelected[v] #arrumar aqui para mostrar média deos meses para imagens por espécie e ano
          
          #new
          #put Na as 0
          var[is.na(var)] <- 0
          
          
          ### old uncomment
          
          # if(length(values$doys) < 365){ 
          #   
          #   start <- paste(unique(dataYear$year), "-01-01", sep="")
          #   end <- paste(unique(dataYear$year), "-12-30", sep="")
          #   dateComplement <- data.frame(date=seq(as.Date(start), as.Date(end), by="days"))
          #   
          #   #final dataframe
          #   varAux <- merge(varAux, dateComplement, by.x='interval', by.y='date', all.x=T, all.y=T)
          #   
          #   varAux[is.na(varAux)] <- 0
          # }
          # 
          # 
          # namecol <- colnames(var)
          # xts.ts <- xts(varAux[,2], varAux$interval)
          # var2 <- apply.weekly(xts.ts, mean)
          # var <- var2[,1]
          # colnames(var) <- namecol
          # print(var)
        } 
        
        assign(paste("var", v, sep=""), var)
        
        #compute the min and max values for each data set
        assign(paste("minVar", v, sep=""), min(var[var>0]))
        assign(paste("maxVar", v, sep=""), max(var[var>0]))
        
      }
      
      #compute the min and max values using all variables assigned (among all years)
      if(y == 1) {
        minD <- max(minMask, get(paste("minVar", 1, sep="")))
        maxD <- max(maxMask, get(paste("maxVar", 1, sep="")))
      }
      if(length(values$variablesSelected) > 1){
        for(v in 1:length(values$variablesSelected)){
          minD <- min(minD, get(paste("minVar", v, sep="")))
          maxD <- max(maxD, get(paste("maxVar", v, sep="")))
        }
      }
      
      values$minD <- minD
      values$maxD <- maxD
      
      vr <- get(paste("var", 1, sep=""))
      
      if(length(values$variablesSelected) > 1){
        for(v in 2:length(values$variablesSelected)){
          var <- get(paste("var", v, sep=""))
          vr <- cbind(vr, var)
        }
      }
      
      assign(paste("vr", y, sep=""), vr, envir = globalenv())
    }
    
    #create one variable for each year
    if(length(values$variablesSelected) == 1 && length(values$yearsSelected) > 1){
      vrVars <- get(paste("vr", 1, sep=""))
      for(y in 2:length(values$yearsSelected)){
        varAux <- get(paste("vr", y, sep=""))
        vrVars <- cbind(vrVars, varAux)
      }
      
      assign(paste("vrVars", 1, sep=""), vrVars, envir = globalenv())
    }
    
    
    values$dataIndexSelected <- 1
    shinyjs::show("previousDrawID")
    shinyjs::show("nextDrawID")
    shinyjs::show("savePdfID")
    shinyjs::show("saveImgID")
    shinyjs::show("boxDataTable")
    shinyjs::show("convertYearsID")
    values$plot_graph <-  1
    values$showVarsData <- FALSE
  })
  
  
  
  values <- reactiveValues(
    plot_graph = 0
  )
  
  #--------------------- UI CONTROLLERS VISUALIZATION (NAVIGATION BUTTONS) --------------------------
  #next year
  observeEvent(input$nextDrawID, {
    values$plot_graph <-  1
    
    if(values$dataIndexSelected < length(values$yearsSelected)){
      values$dataIndexSelected <- values$dataIndexSelected + 1  
    }
  })  
  
  #previous year
  observeEvent(input$previousDrawID, {
    # Reset data_next and data_all to 0
    values$plot_graph <-  1
    
    if(values$dataIndexSelected > 1){
      values$dataIndexSelected <- values$dataIndexSelected - 1
    }
  })  
  
  #save pdf
  observeEvent(input$savePdfID, {
    values$plot_graph <-  1
    
    species <- paste(input$speciesID, "(", input$roiID, ")", "-", values$yearsSelected[values$dataIndexSelected])
    pdf(file=paste(species, ".pdf", sep=""))
    if(!values$showVarsData){
      values$ncircles <- length(values$variablesSelected)
      values$circle.names <- values$variablesSelected
      drawVisualStructureImages(values$ncircles, values$nmacrosegments, values$nmicrosegments, values$circle.names, values$macrosegment.names, values$microsegment.names, values$macrosegment.colors, get(paste("vr", values$dataIndexSelected, sep="")), species, values$minD, values$maxD)
    }else{
      values$ncircles <- length(values$yearsSelected)
      values$circle.names <- values$yearsSelected
      drawVisualStructureImages(values$ncircles, values$nmacrosegments, values$nmicrosegments, values$circle.names, values$macrosegment.names, values$microsegment.names, values$macrosegment.colors, get(paste("vrVars", values$dataIndexSelected, sep="")), species, values$minD, values$maxD)
      
    }
    dev.off()
  })  
  
  #save img
  observeEvent(input$saveImgID, {
    values$plot_graph <-  1
    
    species <- paste(input$speciesID, "(", values$variablesSelected, " ", input$roiID, ")", "-", values$yearsSelected[values$dataIndexSelected])
    png(file=paste(species, ".png", sep=""), width = 1300, height = 800)
    
    if(!values$showVarsData){
      values$ncircles <- length(values$variablesSelected)
      values$circle.names <- values$variablesSelected
      
      drawVisualStructureImages(values$ncircles, values$nmacrosegments, values$nmicrosegments, values$circle.names, values$macrosegment.names, values$microsegment.names, values$macrosegment.colors, get(paste("vr", values$dataIndexSelected, sep="")), species, values$minD, values$maxD)
    }else{
      values$ncircles <- length(values$yearsSelected)
      values$circle.names <- values$yearsSelected
      drawVisualStructureImages(values$ncircles, values$nmacrosegments, values$nmicrosegments, values$circle.names, values$macrosegment.names, values$microsegment.names, values$macrosegment.colors, get(paste("vrVars", values$dataIndexSelected, sep="")), species, values$minD, values$maxD)
      
    }
    dev.off()
  })
  
  #change years
  observeEvent(input$convertYearsID, {
    values$plot_graph <-  1
    
    if(values$showVarsData){
      values$showVarsData <- FALSE
    } else {
      values$showVarsData <- TRUE
    }
    
  })
  
  #--------------------- VISUALIZATION (PLOT) --------------------------
  output$contentsPlot <- renderPlot({
    if(values$plot_graph == 1){
      
      species <- paste(input$speciesID, "(", input$roiID, ")", "-", values$yearsSelected[values$dataIndexSelected])
      if(!values$showVarsData){
        values$ncircles <- length(values$variablesSelected)
        values$circle.names <- values$variablesSelected
        drawVisualStructureImages(values$ncircles, values$nmacrosegments, values$nmicrosegments, values$circle.names, values$macrosegment.names, values$microsegment.names, values$macrosegment.colors, get(paste("vr", values$dataIndexSelected, sep="")), species, values$minD, values$maxD)
        output$tableData <- renderDataTable({
          get(paste("vr", values$dataIndexSelected, sep=""))
        })
      } else {
        values$ncircles <- length(values$yearsSelected)
        values$circle.names <- values$yearsSelected
        species <- paste(input$speciesID, "(", values$variablesSelected, " ", input$roiID, ")", "-", values$yearsSelected[1], "-", values$yearsSelected[length(values$yearsSelected)])
        drawVisualStructureImages(values$ncircles, values$nmacrosegments, values$nmicrosegments, values$circle.names, values$macrosegment.names, values$microsegment.names, values$macrosegment.colors, get(paste("vrVars", values$dataIndexSelected, sep="")), species, values$minD, values$maxD)
        vrVars <- get(paste("vrVars", values$dataIndexSelected, sep=""))
        colnames(vrVars) <-  values$yearsSelected
        output$tableData <- renderDataTable({
          vrVars
        })
        
      }
    }
    
    
    if(input$disp == "head") {
      return(head(df))
    }
    else {
      return(df)
    }
    
  })
})