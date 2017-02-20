#To Do: 

#Problem with ordering by datetime is that not every player has entries for each datetime
#so need to add entries for those datetimes that players don't have values for

#FIX MEAN VARIABLE, NOT CORRECT FOR WHAT IS CURRENTLY BEING DISPLAYED
#FIX BARPLOT ORDERINGS NOT CORRECT WHEN AVERAGE BY DURATION
#FIX BARPLOT LABEL, CAN'T VIEW IT PROPERLY
#FIX BARPLOT VARIABLES, WANT THE SUM OF MOST OF THEM, GIVE OPTION FOR AVERAGE AND OPTION TO SCALE BY DURATION
#BARPLOT AVERAGE BY DURATION DOESN'T WORK
#Use mday from lubridate to fix weeks

#CALENDAR WEEKS ARE SLIGHTLY OFF, MIGHT TAKE A LOT OF TIME TO FIX AND DOESN'T REALLY MATTER

#(a) Fix dates in barplot   y
#(b) add ranking tab    y
#(c) if user wants to change file, add an option  (This occurs automatically) y
#(d) make greps more specific to avoid and bad matches
#(e) give the user an opportunity to upload multiple data sets and move between them  n - this would be too memory intensive. Instead
# give reccomendation to use bigger data frames
#(f) fileInputDown(), dropboxDown() are both called in all output when this really isn't necessary  y(taken care of using an observer)
#(g) refactor code so that unnecessary code in reactive expressions moved to their own functions  y
#(h) optimise report generating code (if possible), it's currently quite slow
#(i) add a 'player' tab which describes the current player being analysed   y
#(j) rename everything to give more meaning and make code more readable
#(k) Document how the csv file needs to be formatted in order for the program to work
#(l) Consider adding a tooltip for each session/drill  y
#(m) Add checkbox for adding tooltip foradding names to training drills  y
#(n) Barplot change date to session  
#(o) Add selectize input with multiple inputs possible which add tooltips for certain sessions  y
#(p) Add a report preview pane  y
#(q) For each type of drill, offer a player comparison
#(r) Offer filers by position (e.g. wingers, etc.)  y (for ranking)
#(s) Change player summary to table for nicer output  y
#(t) Refactor code so that dataframe provided can be very general. Ideally would like the format:
#Drill name (including matches)
#date (or datetime)
#Player name
#(u) Add option for have a multivariate graph if desired, might want to look at the interaction between collisions and something else (for example)
#This would require a double axis which could make things awkward
# (v) Add Player Drill calendar to track who has trained when (and for how long would be nice)  y
#(w) Move all functions not in server to helper.R
#(x) Redesign mean so that it gives the mean value for the day for whatever exercise was being performed
#(y) Change barplot to use monthly data rather than daily - or try and reuse other code to give option of weekly/monthly  y

#Program workflow:
#1)First, read in the data based on it's format 
#2)Validate that the data is in the correct format, if it isn't want to give the user some details
#3)Do some preprocessing, this will be almost certainly necessary
#4)Once data is in correct format, enable download report, render plots etc.


#ProcessedData has 1012 observations of 6 variables
#DateModified data has 8157 observations of 72 variables

print('loading libraries Server')
#might need to install some of these packages, check if there is an if not installed, then install
library(dygraphs)
library(datasets)
library(xts)
library(plyr)
library(tools)
library(rdrop2)
library(fmsb)
library(lubridate)
library(RColorBrewer)
#library(DT)
print('libraries loaded SERVER')

########################################################################################################
########################################## Auxiliary Functions########################################## 
########################################################################################################
print('loading functions SERVER')

#gets the start times of a certain type of drill
getDrillDates=function(drill){
  unique(ProcessedData[ProcessedData$Drill==drill,'Start.Time'],na.rm=TRUE)
}
#This adds a tool tip to give which team was played against
add_toolTips <- function(dygraph,periods,series){
  print(periods)
  #loop through the desired periods to add tooltips to (matchdays in this case)
  for(period in periods){
    #series correpond to the various lines that have already been plotted, tooltips are the team names in match column in specified period
    #print('adding tooltip to: ')
    #print(period)
    #print(as.POSIXct(period))
    #tooltip=unique(ProcessedData[ProcessedData$Date==period,'Match'])
    dygraph=dyAnnotation(dygraph, as.POSIXct(period),width=20,height=25,text = "M", tooltip = 'Friendly Match',series=series,cssClass = 'Annotation')
  }
  #return the dygraph
  dygraph
}
#adds a tootip which denotes all drills of a certain type in dygraph
add_Drill_toolTips <- function(dygraph,periods,series,drill){
  #series correpond to the various lines that have already been plotted, tooltips are the team names in match column in specified period
  for(period in periods){
    #series correpond to the various lines that have already been plotted, tooltips are the team names in match column in specified period
    print('adding tooltip to: ')
    print(period)
    dygraph=dyAnnotation(dygraph, as.POSIXct(period,origin = "1960-01-01"),width=15,height=20,text = "D", tooltip = drill,series=series,cssClass = 'MatchAnnotation')
  #return the dygraph
  }
  dygraph
}
#Returns week in year of a date
wk <- function(x){
  as.numeric(format(x, "%U"))
}
#returns week in month of a date
getMonthWk <- function(x){
  wk(x) - wk(as.Date(cut(x, "month")))+1
}

minutesToHoursAndMinutes <-function(x){
  hours=x%/%60
  print(hours)
  if(x>60){
    minutes=x-(60*hours)
  }
  else{
    minutes=x
  }
  paste(as.character(hours),' hours ',as.character(minutes),' minutes')
}
print('functions loaded SERVER')


########################################################################################################
########################################## Auxiliary Functions########################################## 
########################################################################################################

#server code
shinyServer(function(input, output,session) {
  print('SERVER started')
  #start with a clean workspace to avoid name clashes
  #This caused a huge bug; initially removes input, output and session from env, be wary of this in future
  #rm(list=ls())
  #get rid of anything named simultaedData if it already exists
  if(exists('ProcessedData')){
    rm(ProcessedData)
    print('Removed ProcessedData from workspace')
  }
  # dean attali has milked his package for upvotes on stack exchange! But his answers are useful
  #http://stackoverflow.com/questions/25247852/shiny-app-disable-downloadbutton
  #disable the downdload button on initial page load (no data inputted yet)
  shinyjs::disable("report")
  shinyjs::disable("refreshReport")
  
  ##########################################################################################
  ############################### Deal with local data input/dropbox #############################
  ##########################################################################################
  alreadyVisited=FALSE
  #observes deal with input/ouput
  
  #This function deals with data inputted via dropbox
  dropboxDown=reactive({
    print('in dropboxdown SERVER')
    print(alreadyVisited)
    if(input$dropboxDownload){
      #only run this authorisation once (not sure how to do this without the use of global variable)
      if(!alreadyVisited){
        #consider adding new_user=TRUE
        dropbox_credentials <- drop_auth(new_user=TRUE,cache=FALSE)
        updateCheckboxInput(session, "dropboxDownload",label='Choose a file:')
        updateSelectizeInput(session,'dropboxFileSelection',choices=basename(drop_search('.csv')[order(drop_search('.csv')$modified),]$path))
        alreadyVisited<<-TRUE
      }
      inFile1 <- input$dropboxFileSelection
      #check file exists
      if (is.null(inFile1)){
        print('No file provided')
        return(NULL)
      }
      else{
        if(length(grep('.csv',inFile1))!=0){
          #read csv by finding the path in dropbox
          ProcessedData<<-drop_read_csv(drop_search('.csv')[match(inFile1,basename(drop_search('.csv')$path)),]$path,stringsAsFactors=FALSE)
          #be careful as write.csv adds an x column if column.names=FALSE
          ProcessedData$Date<<-as.Date(ProcessedData$Date)
          #ProcessedData$Day<<-as.character(ProcessedData$Day)
        }
        else if(length(grep('.RData',inFile1))!=0){
          load(inFile1$datapath)
          ProcessedData<<-ProcessedData
        }
      }
      if(exists('ProcessedData')){
        Preprocess()
        #now allow the user to download a report
        shinyjs::enable("report")
        shinyjs::enable("refreshReport")
        #call data preprocessing function, look into passing Preprocess to this function
      }
      
    }
  })
  
  #This function deals with data inputted via local upload
  fileInputDown=reactive({
    print('in fileInputsdown')
    #check file exists
    if (!is.null(input$file1)){
      inFile=input$file1
      #Read in dataframe here 
      #don't like this feature of grep; intead of returning false it returns
      #integer(0) (an integer vector of length 0) which makes it necessary to check 
      #the length of the vector
      if(length(grep('.csv',inFile$name))!=0){
        print('reading data')
        ProcessedData<<-read.csv(inFile$datapath, stringsAsFactors=FALSE)
        #be careful as write.csv adds an x column if column.names=FALSE
        #ProcessedData$Dates<<-as.Date(ProcessedData$Dates)
        #check ProcessedData has a date column
        ProcessedData$Date<<-as.Date(ProcessedData$Date)
        #ProcessedData$Day<<-as.character(ProcessedData$Day)
        print('data read')
      }
      else if(length(grep('.RData',inFile$name))!=0){
        load(inFile$datapath)
        ProcessedData<<-ProcessedData
        #print(str(ProcessedData))
      }
      if(exists('ProcessedData')){
        #now allow the user to download a report
        Preprocess()
        shinyjs::enable("report")
        shinyjs::enable("refreshReport")
        #call data preprocessing function, look into passing Preprocess to this function
      }
      
    }
  })
  checkInput = observe({
    fileInputDown()
    dropboxDown()
  })
  
  ##########################################################################################
  ############################### Deal with data input/dropbox #############################
  ##########################################################################################
  
  ##########################################################################################
  ################### Preprocess data & make necessary varaibles global ####################
  ##########################################################################################
  
  #Leave function reactive because chances are that ProcessedData has not changed and so will be cached
  Preprocess=reactive({
    #do all preprocessing, validation and error checking here
    #then once validation and error checking is done the following code should
    #really go into it's own function because it's not 'reactive'
    #Create new 'mean' player, maybe not very efficient
    #Apparently <<- makes left operand a global variable
    print('In Preprocess')
    print(names(ProcessedData))
    if(!'Name'%in%names(ProcessedData)|!'Date'%in%names(ProcessedData)){
      print('Data frame does not have a name column or a date column')
      stopApp()
    }
    ProcessedData$X=NULL
    ProcessedData$Name=as.character(ProcessedData$Name)
    relevantCols<<-names(ProcessedData)[sapply(ProcessedData, is.numeric)]
    nonNumericCols<<- names(ProcessedData)[!names(ProcessedData) %in% relevantCols]
    #append on a mean column. First start with name data and a numeric column and then add further columns. Maybe mean shouldn't be per date but per week?
    dfappend<-setNames(cbind('Mean',aggregate(ProcessedData[,relevantCols[1]],by=list(ProcessedData$Date),FUN = mean)),c('Name','Date',relevantCols[1]))
    #make sure the number of numeric columns is greater than 2
    print('2')
    if(length(relevantCols)>1){
      for(attribute in relevantCols[2:length(relevantCols)]){
        dfappend[,attribute]=setNames(aggregate(ProcessedData[,attribute],by=list(ProcessedData$Date),FUN = mean),c('Date1',attribute))[,2]
        #aggregate(ProcessedData$Attribute2,by=list(ProcessedData$Date),FUN = mean)),c('Name','Date','Attribute1','Date1','Attribute2'))
      }
    }
    dfappend$Start.Time=as.POSIXct(dfappend$Date)
    print('3')
    ProcessedData<<-rbind.fill(ProcessedData,dfappend)
    matchDays<<-unique(ProcessedData[ProcessedData$Day.Code=='Game','Start.Time'],na.rm=TRUE)
    #Once RData (or csv in future) is read in, update the select input dropdowns
    #Plot tab updates
    updateSelectInput(session, "getname", choices = unique(ProcessedData$Name),
                      selected = unique(ProcessedData$Name)[1])
    updateSelectInput(session, "getnameDownload", choices = unique(ProcessedData$Name),
                      selected = unique(ProcessedData$Name)[1])
    updateSelectInput(session, "getComparisonName",choices =append(c('Mean','No Comparison'),unique(ProcessedData$Name)),
                      selected = "No Comparison")
    updateSelectInput(session, "getVariable", label = NULL, choices =names(ProcessedData)[sapply(ProcessedData, is.numeric)],
                      selected = names(ProcessedData)[sapply(ProcessedData, is.numeric)][1])
    updateSelectInput(session, "getComparisonName", label = NULL, choices =append(c('Mean','No Comparison'),unique(ProcessedData$Name)),
                      selected = "No Comparison")
    updateSelectInput(session, "getDate", label = NULL, choices =sort(unique(ProcessedData$Date)),
                      selected = max(unique(ProcessedData$Date)))
    updateSelectInput(session, "getVariableBarplotTab", label = NULL, choices =names(ProcessedData)[sapply(ProcessedData, is.numeric)],
                      selected = names(ProcessedData)[sapply(ProcessedData, is.numeric)][1])
    updateSelectInput(session, "PlayerSelection", choices = unique(ProcessedData$Name),
                      selected = unique(ProcessedData$Name)[1])
    updateSelectInput(session, "annotateDrills", choices=unique(ProcessedData[ProcessedData$Name==unique(ProcessedData$Name)[1],'Drill']))
    
    #calendar tab updates
    updateSelectInput(session, "getnameCalendar", choices = unique(ProcessedData$Name),
                      selected = unique(ProcessedData$Name)[1])
    updateSelectInput(session, "calendarYear", choices = unique(format(as.Date(ProcessedData[ProcessedData$Name==unique(ProcessedData$Name)[1],'Date'], format="%Y/%m/%d"),"%Y"))
                      )
    updateSelectInput(session, "calendarMonth", choices = unique(format(as.Date(ProcessedData[ProcessedData$Name==unique(ProcessedData$Name)[1],'Date'], format="%Y/%m/%d"),"%m"))
                      )
    updateSelectInput(session, "calendarWeek", choices = unique(c('1','2','3','4','5'))
                      )
    #barplot tab updates
    updateSelectInput(session, 'barplotYear',choices=unique(format(as.Date(ProcessedData[,'Date'], format="%Y/%m/%d"),"%Y")))
    updateSelectInput(session, 'barplotMonth',choices=sort(unique(format(as.Date(ProcessedData[,'Date'], format="%Y/%m/%d"),"%m"))))
    updateSelectInput(session, 'getColourVariableBarplotTab',choices=relevantCols)
    
    #update ranking tab
    updateSelectInput(session, 'rankYear',choices=unique(format(as.Date(ProcessedData[,'Date'], format="%Y/%m/%d"),"%Y")))
    updateSelectInput(session, 'rankMonth',choices=sort(unique(format(as.Date(ProcessedData[,'Date'], format="%Y/%m/%d"),"%m"))))
    updateSelectInput(session, 'rankFilterPosition',choices=append(c('All positions'),unique(ProcessedData$Position)))
    ProcessedData
  })
  ##########################################################################################
  ################### Preprocess data & make necessary varaibles global ####################
  ##########################################################################################
  
  ##########################################################################################
  ############################### Download Report button action ############################
  ##########################################################################################
  #taken from http://shiny.rstudio.com/articles/generating-reports.html
  #this needed some modification; there might be a better way to do this
  #Initialise the final RMarkdown file with output=file. Then keep appending on all players individual markdown files
  #Everything must be kept in temp directories due to write permissions
  output$report<-downloadHandler(
    filename='ShinyAppExampleDownload.html',
    content=function(file){
      tempRmdFile <- file.path(tempdir(), "FinalMarkdown.Rmd")
      file.copy("FinalMarkdown.Rmd", tempRmdFile, overwrite = TRUE)
      
      #generate the parent markdown file
      rmarkdown::render(tempRmdFile, output_file =  file,
                        envir = new.env(parent = globalenv())
      )
      
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport <- file.path(tempdir(), "PracticeMarkdown.Rmd")
      file.copy("PracticeMarkdown.Rmd", tempReport, overwrite = TRUE)
      
      
      #generate a sequence of reports and then concatenate them all together
      #for some reason lapply seems to be slower than the for loop! Will have to figure out why this is
      #generateFiles=function(name,file){
      #  rmarkdown::render(file.path(tempdir(), "PracticeMarkdown.Rmd"), output_file = file.path(tempdir(), paste("PracticeMarkdown",name,".html",sep="")),
      #                    params = list(name=name),
      #                    envir = new.env(parent = globalenv())
      #  )
      #  file.append(file,file.path(tempdir(), paste("PracticeMarkdown",name,".html",sep="")))
      #}
      #lapply(unique(ProcessedData$Name),FUN=generateFiles,file=file)
      
      print('additional notes')
      print(input$additionalNotes)
      for(name in input$getnameDownload){
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app)..
      #params here should only be name, the markdown file should already know about the data (since it's already in global env)
      rmarkdown::render(tempReport, output_file = file.path(tempdir(), paste("PracticeMarkdown",name,".html",sep="")),
                        params = list(name=name,notes=input$additionalNotes),
                        envir = new.env(parent = globalenv())
      )
      #append on all of the player html files to the parent html file
      
      file.append(file,file.path(tempdir(), paste("PracticeMarkdown",name,".html",sep="")))
      }
      return(file)
      }
  )
  #function to create preview for the actual download file.
  CreateContent=function(file){
    ##tempRmdFile <- file.path(tempdir(), "FinalMarkdown.Rmd")
    
    ##file.copy("FinalMarkdown.Rmd", tempRmdFile, overwrite = TRUE)
    
    #generate the parent markdown file
    ##rmarkdown::render(tempRmdFile, output_file =  file,
    ##                  envir = new.env(parent = globalenv())
    ##)
    #rmarkdown::render('FinalMarkdown.Rmd', output_file =  file,
    #                  envir = new.env()
    #)
    
    # Copy the report file to a temporary directory before processing it, in
    # case we don't have write permissions to the current working dir (which
    # can happen when deployed).
    
    ##tempReport <- file.path(tempdir(), "PracticeMarkdown.Rmd")
    ##file.copy("PracticeMarkdown.Rmd", tempReport, overwrite = TRUE)
    
    
    #generate a sequence of reports and then concatenate them all together
    #for some reason lapply seems to be slower than the for loop! Will have to figure out why this is
      print('additional notes')
      print(input$additionalNotes)
      rmarkdown::render("PreviewMarkdown.Rmd", output_file = paste("PreviewMarkdown",input$getnameDownload,".html",sep=""),
                        params = list(name=input$getnameDownload),
                        envir = new.env(parent = globalenv())
      )
      #file.append(file,paste("PreviewMarkdown",name,".html",sep=""))
    #lapply(unique(input$getnameDownload),FUN=generateFiles,file=file)
    
    
    ##for(name in input$getnameDownload){
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app)..
      #params here should only be name, the markdown file should already know about the data (since it's already in global env)
      ##rmarkdown::render(tempReport, output_file = file.path(tempdir(), paste("PracticeMarkdown",name,".html",sep="")),
      ##                  params = list(name=name),
      ##                  envir = new.env(parent = globalenv())
      ##)
      #append on all of the player html files to the parent html file
      
      ##file.append(file,file.path(tempdir(), paste("PracticeMarkdown",name,".html",sep="")))
    ##}
  }
  
  output$dataFrameSummary <-renderDataTable ({
    print('rendering')
    validate(need(exists("ProcessedData"),""))
    validate(need(exists("completed"),''))
    if(input$reportPreview){
      dataFrameNames=c('Drill','Start.Time','Duration','Distance','Max.Speed','Avg.Speed','Max.HR',"Avg.HR" ,"Sprint.Count","New.Bodyload" ,"Collisions","Sprint.Total.Distance" ,"Metabolic.Power.Average")
      ProcessedData[ProcessedData$Name=='1',dataFrameNames]
    }
  },options=list(dom = 't',
                   scrollX = TRUE,
                   deferRender = TRUE,
                   scrollY = 500,
                   scroller = TRUE,
                   rownames=FALSE))
  
  output$frame <- renderUI({
    validate(need(exists("ProcessedData"),""))
    if(input$reportPreview){
      input$refreshReport
      #generate the report in a temporary directory and then add the html to the main panel of the download page
      #would be nice to somehow contain this in an iframe, would take too much time
      my_test <-includeHTML(CreateContent('ShinyAppExampleDownload.html'))
      gc()
      completed<<-TRUE
      my_test}
  })
  
  ##########################################################################################
  ############################### Download report button action ############################
  ##########################################################################################
  
  ##########################################################################################
  ############################### Tables  ############################
  ##########################################################################################  
  #Table outputs summary statistics
  output$tableView <- renderPrint({
    validate(need(exists("ProcessedData"),"Summary will be here when you enter a RData File"))
    summary(ProcessedData[,relevantCols])
  })
  #Summary Tab contains a short summary of the player that is currently selected
  output$tableView1 <- renderPrint({
    validate(need(exists('ProcessedData'),"Summary will be here when you enter a RData File"))
    summary(ProcessedData[,relevantCols])
  })
  
  playerSummary <-function(name){
    validate(need(exists('ProcessedData'),"Summary will be here when you enter a csv File"))
    Attribute=c('Name: ','Position: ', 'Squad: ','Number of entries in DataFrame: ','Performs Best in: ', 'Performs worst in: ')
    Value=c(name,
            paste(unique(ProcessedData[ProcessedData$Name==name,'Position'])[unlist(lapply(unique(ProcessedData[ProcessedData$Name==name,'Position']),function (x) x!=''))],collapse=' & '),    
            paste(unique(ProcessedData[ProcessedData$Name==name,'Squad'])[unlist(lapply(unique(ProcessedData[ProcessedData$Name==name,'Squad']),function (x) x!=''))],collapse=' & '),
            unique(nrow(ProcessedData[ProcessedData$Name==name,])),'','')
    print(Value)
    as.data.frame(cbind(Attribute,Value))
  }
  
  #Outputs a brief summary of the player that is currenlty focussed in the app.
  
  output$PlayerSummaryDownloadTab <- renderTable({
    playerSummary(input$getnameDownload)
  },width = '400',align= 'l',colnames = FALSE,bordered = FALSE,caption = "Summary",
  caption.placement = getOption("xtable.caption.placement", "top"), 
  caption.width = getOption("xtable.caption.width", NULL))
  
  output$PlayerSummaryPlot <- renderTable({
    input$getname
    playerSummary(input$getname)
  })
  
  drillFrequency <-function(name){
    validate(need(exists('ProcessedData'),""))
    validate(need(input$getname!='',''))
    drillTable=as.data.frame(table(ProcessedData[ProcessedData$Name==name,'Drill']))
    drillTable=drillTable[order(drillTable$Freq, decreasing=TRUE),]#[c(1:7),]
    colnames(drillTable)=c('Drills','Frequency')
    drillTable
  }
  #Frequency table denoting how often each drill was performed
  output$FrequencyTable <- renderTable({
    drillFrequency(input$getname)[c(1:5),]
  },width = '300',align= 'l',colnames = FALSE,bordered = FALSE,caption = "Drill Frequency",
  caption.placement = getOption("xtable.caption.placement", "top"), 
  caption.width = getOption("xtable.caption.width", NULL))
  
  output$FrequencyTableDownloadTab <- renderDataTable({
    drillFrequency(input$getnameDownload)
  },options=list(
    #lengthMenu = list(c(5, 15, -1), c('5', '15', 'All')),
    pageLength = 5
  ))#,width = '300',align= 'l',colnames = FALSE,bordered = FALSE,caption = "Drill Frequency",
  #caption.placement = getOption("xtable.caption.placement", "top"), 
  #caption.width = getOption("xtable.caption.width", NULL))
  
  #barplot for barplot tab
  output$Barplot <- renderPlot({
    validate(need(exists("ProcessedData"),"Barplot will be here when you enter a csv File"))
    validate(need(!is.null(input$getDate),"Barplot will be here when you enter a csv File"))
    playerPositions=unique(ProcessedData$Position)
    mapPlayerToPosition<-function(playerName){
      sort(table(ProcessedData[ProcessedData$Name==playerName,'Position']),decreasing=TRUE)
    }
    
    if(input$dailyOrNot){
      barPlotData<<-aggregate(ProcessedData[ProcessedData$Date==input$getDate,relevantCols],by=list(ProcessedData[ProcessedData$Date==input$getDate,'Name']),FUN=sum)
      print('names')
      #print(barPlotData[o,'Name'])
      print('yvals: ')
      t=paste('Barplot for Mean ',input$getVariableBarplotTab,' for ',input$getDate,sep='')
      print('t')
      print(t)
    }
    else{
      dateRangeStart=as.Date(paste('1/',input$barplotMonth,'/',input$barplotYear,sep=''),format='%d/%m/%Y')
      dateRangeEnd=as.Date(paste('1/',as.character(as.integer(input$barplotMonth)+1),'/',input$barplotYear,sep=''),format='%d/%m/%Y')
      print('dateRangeStart')
      print(dateRangeStart)
      print(dateRangeEnd)
      barPlotData<<-aggregate(ProcessedData[ProcessedData$Date>=dateRangeStart & ProcessedData$Date<=dateRangeEnd,relevantCols],by=list(ProcessedData[ProcessedData$Date>=dateRangeStart & ProcessedData$Date<=dateRangeEnd,'Name']),FUN=mean)
      #barplot title
      t=paste('Barplot for ',input$getVariableBarplotTab,' from ',dateRangeStart,' to ',dateRangeEnd,sep='')
      print('t')
      print(t)
    }
    if(input$divideByDuration){
      o=order(barPlotData[,input$getVariableBarplotTab]/barPlotData$Duration,decreasing=TRUE)
      var=barPlotData[o,input$getVariableBarplotTab]/barPlotData[o,'Duration']
      colours=unlist(lapply(barPlotData[o,'Group.1'],mapPlayerToPosition))
    }
    else{
      o=order(barPlotData[,input$getVariableBarplotTab],decreasing=TRUE)
      var=barPlotData[o,input$getVariableBarplotTab]
      colours=unlist(lapply(barPlotData[o,'Group.1'],mapPlayerToPosition))
    }
    print('non-averaged: ')
    print(barPlotData[o,input$getVariableBarplotTab])
    print('averaged: ')
    print(barPlotData[o,input$getVariableBarplotTab]/barPlotData[o,'Duration'])
    colourIndices=match(names(colours),playerPositions)
    colours=brewer.pal(length(unique(colourIndices)),'Set3')[colourIndices]
    
    barPlotData=rename(barPlotData,replace = c("Group.1" = "Name"))
    barplot(var,
            main=t,
            names.arg=barPlotData[o,'Name'],
            ylim=c(0, max(var)*1.25),
            col = colours,
            xlab='Player',
            ylab=paste(input$getVariableBarplotTab,' values')
    )
    legend('topright', playerPositions , 
           lty=1, col=colours, bty='n', cex=1.2)
  },height = 600, width = 1000)
  #Give a player ranking
  #output$Ranking <-
  output$PlayerSummaryCalendar <- renderTable({
    playerSummary(input$getnameCalendar)
  },width = '400',align= 'l',colnames = FALSE,bordered = FALSE,caption = "Summary",
  caption.placement = getOption("xtable.caption.placement", "top"), 
  caption.width = getOption("xtable.caption.width", NULL))
  
  output$titleDrillCalendar <- renderPrint({
    cat(paste('Drills for player ',input$getnameCalendar,' ',
    c('January','February','March','April','May','June','July','August','September','October','November','December')[as.numeric(input$calendarMonth)],input$calendarYear))
  })
  
  upDateBarplotMonths <- observe({
    #Create dependency on barplotyear - when this changes, dates need to update
    input$barplotYear
    validate(need(exists('ProcessedData'),""))
    updateSelectInput(session, "barplotMonth", 
          choices = sort(unique(format(as.Date(ProcessedData[format(as.Date(ProcessedData$Date,format="%Y/%m/%d"),'%Y')==input$barplotYear,'Date'], format="%Y/%m/%d"),"%m")))
    )
  })
  
  upDateCalendarYears <- observe({
    #Create dependency on getnameCalendar - when this changes, dates need to update
    input$getnameCalendar
    validate(need(exists('ProcessedData'),""))
    validate(need(input$getnameCalendar!='',''))
    updateSelectInput(session, "calendarYear", choices = sort(unique(format(as.Date(ProcessedData[ProcessedData$Name==input$getnameCalendar,'Date'], format="%Y/%m/%d"),"%Y")))
    )
  })
  #Create dependency on calendarYear- when this changes, dates need to update
  upDateCalendarMonths <- observe({
    input$calendarYear
    validate(need(exists('ProcessedData'),""))
    validate(need(input$getnameCalendar!='',''))
    #print('match year')
    #print(format(as.Date(ProcessedData[ProcessedData$Name==input$getnameCalendar,'Date'], format="%Y/%m/%d"),"%Y")==input$calendarYear)
    #print(ProcessedData$Name==input$getnameCalendar)
    #print(sort(unique(format(as.Date(ProcessedData[ProcessedData$Name==input$getnameCalendar & 
    #                                                 format(as.Date(ProcessedData$Date, format="%Y/%m/%d"),"%Y")==input$calendarYear,'Date'], format="%Y/%m/%d"),"%m"))))
    #Year=format(as.Date(ProcessedData[ProcessedData$Name==input$getnameCalendar,'Date'], format="%Y/%m/%d"),"%Y")
    updateSelectInput(session, "calendarMonth", choices = sort(unique(format(as.Date(ProcessedData[ProcessedData$Name==input$getnameCalendar & format(as.Date(ProcessedData$Date, format="%Y/%m/%d"),"%Y")==input$calendarYear,'Date'], format="%Y/%m/%d"),"%m")))
    )
  })
  #############################################################################################################################################
  ############################################################# Ranking tab ###################################################################
  #############################################################################################################################################

  #Create dependency on calendarYear- when this changes, dates need to update
  updateRankingMonths <- observe({
    input$rankYear
    validate(need(exists('ProcessedData'),""))
    print('rankYear changed, updating rank month now')
    #print('match year')
    #print(format(as.Date(ProcessedData[ProcessedData$Name==input$getnameCalendar,'Date'], format="%Y/%m/%d"),"%Y")==input$calendarYear)
    #print(ProcessedData$Name==input$getnameCalendar)
    #print(sort(unique(format(as.Date(ProcessedData[ProcessedData$Name==input$getnameCalendar & 
    #                                                 format(as.Date(ProcessedData$Date, format="%Y/%m/%d"),"%Y")==input$calendarYear,'Date'], format="%Y/%m/%d"),"%m"))))
    #Year=format(as.Date(ProcessedData[ProcessedData$Name==input$getnameCalendar,'Date'], format="%Y/%m/%d"),"%Y")
    print('updating ranking months')
    print(sort(unique(format(as.Date(ProcessedData[format(as.Date(ProcessedData$Date, format="%Y/%m/%d"),"%Y")==input$rankYear,'Date'], format="%Y/%m/%d"),"%m"))))
    updateSelectInput(session, "rankMonth", choices = sort(unique(format(as.Date(ProcessedData[format(as.Date(ProcessedData$Date, format="%Y/%m/%d"),"%Y")==input$rankYear,'Date'], format="%Y/%m/%d"),"%m")))
    )
  })
  
  #Ranking tab variables
  output$rankingSummary=DT::renderDataTable({
    validate(need(exists('ProcessedData'),"Summary will be here when you enter a csv File"))
    #test
    #input=list()
    #input$rankFilterPosition='Forward'
    #name='Distance'
    #input$rankYear='2015'
    #input$rankMonth='4'
    dateRangeStart=as.Date(paste('1/',input$rankMonth,'/',input$rankYear,sep=''),format='%d/%m/%Y')
    dateRangeEnd=as.Date(paste('1/',as.character(as.integer(input$rankMonth)+1),'/',input$rankYear,sep=''),format='%d/%m/%Y')
    print(dateRangeStart)
    print(dateRangeStart)
    #need to find out how many entries current position has for month
    if(input$rankFilterPosition!='All positions'){
      noEntriesTotal=length(unique(ProcessedData[ProcessedData$Date>=dateRangeStart & ProcessedData$Date<=dateRangeEnd&ProcessedData$Position==input$rankFilterPosition,'Name']))
    } else{ #Having else on the next line here is incorrect, just a syntatic sugar it seems. It would treat it as a new statement on a new line
      noEntriesTotal=length(unique(ProcessedData[ProcessedData$Date>=dateRangeStart & ProcessedData$Date<=dateRangeEnd,'Name']))
    }
    print('noEntries')
    print(noEntriesTotal)
    validate(need(noEntriesTotal>=2,"Too few values to display!"))
    RankingBest=c('Best')
    RankingWorst=c('Worst')
    noEntries=ifelse(noEntriesTotal>8,4,floor(noEntriesTotal/2))
    for(i in c(1:noEntries)){
      RankingBest=append(RankingBest,as.character(i))
      RankingWorst=append(RankingWorst,as.character(noEntriesTotal-noEntries+i))
    }
    Ranking=append(RankingBest,RankingWorst)
    returndf=data.frame(Ranking)
    #input=list()
    #input$rankFilterPosition='All positions'
    #positionCondition=ifelse(input$rankFilterPosition=='All Positions',TRUE,ProcessedData$Position==input$rankFilterPosition)
    #ProcessedData$Metabolic.Power.Average
    cols=c('Distance','Duration','Avg.Speed','Metabolic.Power.Average')
    for(name in cols){
      if(name%in%c('Distance','Duration')){
        funct=sum
      }
      else{
        funct=mean
      }
      #here the function should be mean for some of the entries and sum for other (e.g. distance)
      if(input$rankFilterPosition!='All positions'){
        columnToBind=aggregate(ProcessedData[ProcessedData$Date>=dateRangeStart & ProcessedData$Date<=dateRangeEnd&ProcessedData$Position==input$rankFilterPosition,name],
        by=list(ProcessedData[ProcessedData$Date>=dateRangeStart & ProcessedData$Date<=dateRangeEnd&ProcessedData$Position==input$rankFilterPosition,'Name']),funct)
      }
      else{
        columnToBind=aggregate(ProcessedData[ProcessedData$Date>=dateRangeStart & ProcessedData$Date<=dateRangeEnd,name],
        by=list(ProcessedData[ProcessedData$Date>=dateRangeStart & ProcessedData$Date<=dateRangeEnd,'Name']),funct)
      }
      
      columnToBind=columnToBind[order(columnToBind$x,decreasing = TRUE),]
      #if(name%in%c('Distance','Duration')){
      #  pasteName=paste('Total ',as.character(name))
      #}
      #else{
      #  pasteName=paste('Avg. ',as.character(name))
      #}
      
      returndf=cbind(returndf,c('',head(paste(columnToBind$Group.1, signif(columnToBind$x,4), sep="  |  "),noEntries),'',tail(paste(columnToBind$Group.1, signif(columnToBind$x,4), sep="  |  "),noEntries)))
      #if(name%in%c('Distance','Duration')){
      #  v=c(paste('Total',as.character(name)))
      #}
      #else{
      #  v=c(paste('Avg.',as.character(name)))
      #}
      #names(v)=as.character(tail(names(returndf),1))
      #print('v')
      #print(v)
      #returndf=rename(returndf,replace=v)
    }
    colours=c(0,rep(1,noEntries),0,rep(2,noEntries))
    returndf=cbind(returndf,colours)
    colnames(returndf)=c('Ranking','Total Distance','Total Duration','Avg. Avg.Speed','Avg. Metabolic.Power.Average','Colours')
    #rename(returndf)
    print(returndf)
    returndatatable=DT::datatable(returndf,rownames = FALSE,
      options=list(columnDefs = list(list(visible=FALSE, targets=c(ncol(returndf)-1))),  searching = FALSE,
      paging = FALSE,info=FALSE))%>%
      formatStyle('Colours',target = 'row',backgroundColor = styleEqual(c(0,1,2), c('lightorange','lightgreen', 'pink')))
    print('returnDataTable')
    print(returndatatable)
    returndatatable
    })
  
  
  output$rankingTitle=renderText({
    dateRangeStart=as.Date(paste('1/',input$rankMonth,'/',input$rankYear,sep=''),format='%d/%m/%Y')
    dateRangeEnd=as.Date(paste('1/',as.character(as.integer(input$rankMonth)+1),'/',input$rankYear,sep=''),format='%d/%m/%Y')
    paste('Rankings for ',input$rankFilterPosition,' starting ',dateRangeStart,' and ending on ',dateRangeEnd,'\n')
  })
  
  #############################################################################################################################################
  ############################################################# Ranking tab ###################################################################
  #############################################################################################################################################
  
  output$DrillCalendarTable <- renderTable({
    #input=list()
    #input$getnameCalendar='1'
    #input$calendarYear='2015'
    #input$calendarMonth='03'
    #input$calendarWeek='1'
    validate(need(exists('ProcessedData'),""))
    validate(need(input$getnameCalendar!='',''))
    

    DatesMatchingInput=lapply(c(1,2,3,4,5),function(y) lapply(ProcessedData[ProcessedData$Name==input$getnameCalendar,'Date'],function(x) getMonthWk(x)==y & substr(x,6,7)==input$calendarMonth & substr(x,1,4)==input$calendarYear))
    #My dates holds the dates which are used for the player which matches the specified input
    mydates=lapply(DatesMatchingInput,function(x) unique(ProcessedData[ProcessedData$Name==input$getnameCalendar,'Date'][which(unlist(x))]))
    #Initialise empty return data frame
    daysOfWeek=c('Monday','Tuesday','Wednesday','Thursday','Friday','Saturday','Sunday')
    returndataframe=data.frame(stringsAsFactors=FALSE)
    outputWeek=function(somedates){
      emptyDF=data.frame(stringsAsFactors=FALSE)
      #print(emptyDF)
      for(d in somedates){
        #startOfWeek is either the first day of the month or the most recent monday
        startOfWeek=ifelse(month(as.Date(d)-days(7))==month(as.Date(d)),as.Date(as.Date(d)-days(which(weekdays(as.Date(d))==daysOfWeek))+1),as.Date(paste(year(as.Date(d)),month(as.Date(d)),'1',sep='-')))
        endOfWeek=ifelse(month(as.Date(d)-days(7))==month(as.Date(d)),as.Date(startOfWeek)+days(6),as.Date(as.Date(startOfWeek)+days(7-which(weekdays(as.Date(startOfWeek))==daysOfWeek))))
        #Get week start times and end times
        emptyDF=rbind(emptyDF,c(paste('Week',getMonthWk(as.Date(d))),paste('Starting',as.Date(startOfWeek)),paste('Ending',as.Date(endOfWeek)),'         ','         ','         ','         ','         '),stringsAsFactors = FALSE)
        #print('emptyDF')
        #print(emptyDF)
        accDur=0
        for(drill in ProcessedData[ProcessedData$Name==input$getnameCalendar&ProcessedData$Date==d,'Start.Time']){
          #print(as.character(drill))
          data=c(weekdays(ProcessedData[ProcessedData$Name== input$getnameCalendar& ProcessedData$Date==d & ProcessedData$Start.Time==drill,'Date'])[1],
                 as.character(ymd(ProcessedData[ProcessedData$Name== input$getnameCalendar& ProcessedData$Date==d & ProcessedData$Start.Time==drill,'Date'])[1]),
                 ProcessedData[ProcessedData$Name== input$getnameCalendar& ProcessedData$Date==d& ProcessedData$Start.Time==drill,'Drill'][1],
                 substr(as.character(ymd_hms(drill)[1]),12,16),
                 substr(as.character(ymd_hms(drill)+minutes(round(unique(ProcessedData[ProcessedData$Name==input$getnameCalendar& ProcessedData$Date==d & ProcessedData$Start.Time==drill,'Duration'])))),12,16),
                 ProcessedData[ProcessedData$Name== input$getnameCalendar& ProcessedData$Date==d & ProcessedData$Start.Time==drill,'Duration'][1],
                 ProcessedData[ProcessedData$Name== input$getnameCalendar& ProcessedData$Date==d & ProcessedData$Start.Time==drill,'Distance'],
                 ProcessedData[ProcessedData$Name== input$getnameCalendar& ProcessedData$Date==d & ProcessedData$Start.Time==drill,'New.Bodyload']
          )
          accDur=accDur+ProcessedData[ProcessedData$Name== input$getnameCalendar& ProcessedData$Date==d & ProcessedData$Start.Time==drill,'Duration'][1]
          print('data')
          print(data)
          emptyDF=rbind(emptyDF,data,stringsAsFactors=FALSE)
        }
        emptyDF=rbind(emptyDF,c('           ','           ','           ','           ','           ','           ','Time spent training',minutesToHoursAndMinutes(as.numeric(accDur))),stringsAsFactors = FALSE)
        emptyDF=rbind(emptyDF,c('-----------','-----------','-----------','-----------','-----------','-----------','-----------','-----------'),stringsAsFactors = FALSE)
      }
      #validate(need(nrow(emptyDF)!=0,''))
      if(nrow(emptyDF)!=0){
        colnames(emptyDF)=c('Day','Date','Drill name','Drill start time','Drill end time','Drill duration','Distance Covered','New.Bodyload Value')
      }
      print('emptyDF')
      return(emptyDF)
    }
    mapReducedData=Reduce(rbind,lapply(mydates,outputWeek),init=returndataframe)
  })
  
  ##########################################################################################
  ######################################### Help Tab #######################################
  ##########################################################################################
  output$helpTab=renderPrint({cat('This application was designed as a proof-of-concept. You can upload either csv or Rdata files using the local file uploader or the dropbox uploader. Currently, there must be columns in the uploaded data with variable names:

Date, Start.Time, Name, Duration, Drill, Distance, Max.Speed, Avg.Speed, Max.HR, Avg.HR, Sprint.Count,
New.Bodyload, Collisions, Sprint.Total.Distance, Metabolic.Power.Average, Position, Squad.

However, with minor modifications the app can be revised to work with any longitudinal athlete data with the assumption that the data is of a longitudinal nature. There are known bugs, these have been documented in  the server file at ______. A github account of known issues will be set up to record these. Any errors, bugs or requested functionality should be communicated through github at _______. 
                                                ')})
  ##########################################################################################
  ######################################### Help Tab #######################################
  ##########################################################################################
  
  
  
  ##########################################################################################
  ################################## Dygraph Constructor code ##############################
  ##########################################################################################
  output$dygraph <- renderDygraph({
    print('In renderDygraph')
    print(input$file1$name)
    #Not sure if there's a way around calling this, not too inefficient at the moment
    validate(need(exists("ProcessedData"),"Plot will be here when you enter a RData File"))
    #start with basic case and then build up the args to pass to the dygraph
    args=c(as.double(ProcessedData[ProcessedData$Name==input$getname,input$getVariable]))
    datesCorrsepondingToArgs<<-ProcessedData[ProcessedData$Name==input$getname,'Start.Time']
    commonDate=datesCorrsepondingToArgs
    #seriesArgs represents the label names
    seriesArgs<<-c(input$getVariable)
    if(input$addSmootherPlayer1){
      smoothingSpline = smooth.spline(rep(1:nrow(ProcessedData[ProcessedData$Name==input$getname,])), as.double(ProcessedData[ProcessedData$Name==input$getname,input$getVariable]), spar=input$getsmoothness)
      args=cbind(args,c(as.double(predict(smoothingSpline,rep(1:nrow(ProcessedData[ProcessedData$Name==input$getname,])))$y)))
      seriesArgs=append(seriesArgs,paste('Smoothed',input$getVariable))
    }
    if(input$getComparisonName!='No Comparison'){
      commonDate<<-unique(c(datesCorrsepondingToArgs, ProcessedData[ProcessedData$Name==input$getComparisonName,'Start.Time']))
      #print('commonDate')
      #print(commonDate)
      argument1=ifelse(commonDate[order(commonDate)]%in%ProcessedData[ProcessedData$Name==input$getname,'Start.Time'][order(ProcessedData[ProcessedData$Name==input$getname,'Start.Time'])] ,ProcessedData[ProcessedData$Name==input$getname,input$getVariable],0)
      argument2=ifelse(commonDate[order(commonDate)]%in%ProcessedData[ProcessedData$Name==input$getComparisonName,'Start.Time'][order(ProcessedData[ProcessedData$Name==input$getComparisonName,'Start.Time'])] ,ProcessedData[ProcessedData$Name==input$getComparisonName,input$getVariable],0)
      #argument1=ifelse(commonDate in ProcessedData[ProcessedData$Name==input$getname,'Start.Time'])
      #args=cbind(args,as.double(ProcessedData[ProcessedData$Name==input$getComparisonName,input$getVariable]))
      #reset args
      args[1]=argument1
      args=cbind(args,argument2)
      seriesArgs=append(seriesArgs,input$getComparisonName)
    }
    if(input$addSmootherPlayer2 && input$getComparisonName!='No Comparison'){
      smoothingSpline1 = smooth.spline(rep(1:nrow(ProcessedData[ProcessedData$Name==input$getComparisonName,])), as.double(ProcessedData[ProcessedData$Name==input$getComparisonName,input$getVariable]), spar=input$getsmoothness)
      args=cbind(args,as.double(predict(smoothingSpline1,rep(1:nrow(ProcessedData[ProcessedData$Name==input$getComparisonName,])))$y))
      seriesArgs=append(seriesArgs,paste('Smoothed',input$getComparisonName))
    }
    #convert to dataframe for convenient naming, maybe inefficient
    args=data.frame(args)
    colnames(args)<-seriesArgs
    #validator prevents premature rendering of graph
    validate(need(nrow(args)>0,"Processing graph"))
    #print('commonDate')
    #print(as.POSIXct(commonDate))
    #print('xts')
    #print(xts(args,order.by = as.POSIXct(commonDate)))
    #print(xts(args,order.by = ProcessedData[ProcessedData$Name==input$getname,'Start.Time']), main = paste(input$getVariable," graph for",input$getname))
    #Create graph and add some nice features
    #order.by=ProcessedData[ProcessedData$Name==input$getname,'Start.Time']
    graph=dygraph(xts(args,order.by = as.POSIXct(commonDate)), main = paste(input$getVariable," graph for",input$getname)) %>%
      dyOptions(drawGrid = input$showgrid, strokeWidth=0.2, drawPoints=TRUE,pointSize=3) %>%
      dyRangeSelector()
    graph=add_toolTips(graph,matchDays,seriesArgs[1])
    #graph=add_Drill_toolTips(graph,getDrillDates(drill),seriesArgs[1])
    print('input$annotateDrills value: ')
    print(input$annotateDrills)
    if(input$annotateAllDrills){
      #print('Annotating all drills')
      updateSelectInput(session,'annotateDrills',selected = unique(ProcessedData[ProcessedData$Name==input$getname,'Drill']))
    }
    for(i in input$annotateDrills){
      graph=add_Drill_toolTips(graph,getDrillDates(i),seriesArgs[1],i)
    }
    graph
    
    #Add drill tooltips, come back to this
    #for(i in which(ProcessedData$Name=='1' & ProcessedData$Drill!="")){
    #  print(ProcessedData[i,'Start.Time'])
    #  print(ProcessedData[i,'Drill'])
    #  graph=add_Drill_toolTips(graph,ProcessedData[i,'Start.Time'],seriesArgs[1],ProcessedData[i,'Drill'])
    #}
    #graph
    #,add_Drill_toolTips)
  })
  ##########################################################################################
  ################################## Dygraph Constructor code ##############################
  ##########################################################################################
  print('END of SERVER')
})
print('SERVER finished running')
#options(shiny.trace=TRUE)
options(shiny.reactlog=TRUE) 

#Code which transformed the original dataframe
#ProcessedData$Date=as.Date(testDframe1$Date, format='%d/%m/%Y')
#ProcessedData$Start.Time=as.POSIXct(strptime(ProcessedData$Start.Time, "%d/%m/%Y %H:%M"))
#write.csv(ProcessedData,'C:/Users/Marion/Downloads/GPSDataDateModified.csv')
##ProcessedData$Date=ProcessedData$Start.Time
#setwd("C:/Users/Marion/RCode")
#
#xts(args,order.by = as.POSIXct(ProcessedData[ProcessedData$Name=='1','Start.Time']))
#args=c(as.double(ProcessedData[ProcessedData$Name=='1','Duration']))
#args=cbind(args,as.double(ProcessedData[ProcessedData$Name=='Mean','Duration']))
#which(datesCorrsepondingToArgs==ProcessedData[ProcessedData$Name=='2','Start.Time'])
#which(ProcessedData[ProcessedData$Name=='2','Start.Time']==datesCorrsepondingToArgs)
#ifelse(commonDate[order(commonDate)]%in%ProcessedData[ProcessedData$Name=='1','Start.Time'][order(ProcessedData[ProcessedData$Name=='1','Start.Time'])] ,ProcessedData[ProcessedData$Name=='1','Duration'],0)

#sort(table(ProcessedData[ProcessedData$Name=='1','Drill']), decreasing=TRUE)[1:5]




#Player=1
#==============================================================================
# pos=ProcessedData[ProcessedData$Name=='1',]$Position[1]
# players=unique(ProcessedData[ProcessedData$Position==pos,'Name'],na.rm=TRUE)
# sum(ProcessedData$Name%in%'2')
# freqs=lapply(players,function (x) sum(ProcessedData$Name%in%x))
# mostSimilar=which(unlist(freqs)==max(unlist(freqs)))
# secondMost=which(unlist(freqs)==max(unlist(freqs)[-mostSimilar]))
# relevantVariables=c('Duration','Distance','New.Bodyload','Collisions')
# mydata=ProcessedData[ProcessedData$Name==players[mostSimilar],relevantVariables]
# mydata1=ProcessedData[ProcessedData$Name=='1',relevantVariables]
# mydata2=ProcessedData[ProcessedData$Name==players[secondMost],relevantVariables]
# colMeans(mydata)
# colMeans(mydata1)
# renderData=as.data.frame(rbind(colMeans(mydata),colMeans(mydata1),colMeans(mydata2)))
# rownames(renderData)=c('1',as.character(players[mostSimilar]),as.character(players[secondMost]))
# renderData=as.data.frame(rbind(c(35,2500,80,15),c(0,0,0,0),renderData))
# colors_border=c( rgb(0.2,0.5,0.5,0.9), rgb(0.8,0.2,0.5,0.9) , rgb(0.7,0.5,0.1,0.9) )
# colors_in=c( rgb(0.2,0.5,0.5,0.4), rgb(0.8,0.2,0.5,0.4) , rgb(0.7,0.5,0.1,0.4) )
# plot=radarchart( renderData[-c(1,2),]  , axistype=1 , maxmin=F,
#             #custom polygon
#             pcol=colors_border , pfcol=colors_in , plwd=4 , plty=1,
#             #custom the grid
#             cglcol="grey", cglty=1, axislabcol="black", cglwd=0.8, 
#             #custom labels
#             vlcex=0.8,centerzero = F,seg=5
#             
# )
# 
# legend(x=0.7, y=1, legend =rownames(renderData[-c(1,2),]), bty = "n", pch=20 , col=colors_in , text.col = "grey", cex=1.2, pt.cex=3)
# 
# 
# 
# colnames(renderData)
# t(renderData)
# df<-data.frame(id=c("a","b","c","d","e"),val1=runif(5,0,1),val2=runif(5,0,1))
# #muliply number by 100 to get percentage
# df[,-1]<-df[,-1]*100
# 
# plot <- Highcharts$new()
# plot$chart(polar = TRUE, type = "line",height=500)
# plot$xAxis(categories=c('a','b','c','d'), tickmarkPlacement= 'on', lineWidth= 0)
# plot$yAxis(gridLineInterpolation= 'circle', lineWidth= 0, min= 0,max=2000,endOnTick=T,tickInterval=10)
# plot$series(data = t(renderData)[,'11'],name = "Series 1", pointPlacement="on")
# plot$series(data = t(renderData)[,'19'],name = "Series 2", pointPlacement="on")
# #df[,"val1"]
# plot
# 
# 
# # Create data: note in High school for several students
# set.seed(99)
# data=as.data.frame(matrix( sample( 0:20 , 15 , replace=F) , ncol=5))
# colnames(data)=c("math" , "english" , "biology" , "music" , "R-coding" )
# rownames(data)=paste("mister" , letters[1:3] , sep="-")
# 
# # To use the fmsb package, I have to add 2 lines to the dataframe: the max and min of each topic to show on the plot!
# data=rbind(rep(20,5) , rep(0,5) , data)
# 
# 
# 
# 
# 
# #==================
# # Plot 1: Default radar chart proposed by the library:
# radarchart(data)
# 
# 
# #==================
# # Plot 2: Same plot with custom features
# colors_border=c( rgb(0.2,0.5,0.5,0.9), rgb(0.8,0.2,0.5,0.9) , rgb(0.7,0.5,0.1,0.9) )
# colors_in=c( rgb(0.2,0.5,0.5,0.4), rgb(0.8,0.2,0.5,0.4) , rgb(0.7,0.5,0.1,0.4) )
# radarchart( data  , axistype=1 , 
#             #custom polygon
#             pcol=colors_border , pfcol=colors_in , plwd=4 , plty=1,
#             #custom the grid
#             cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,20,5), cglwd=0.8,
#             #custom labels
#             vlcex=0.8 
# )
# legend(x=0.7, y=1, legend = rownames(data[-c(1,2),]), bty = "n", pch=20 , col=colors_in , text.col = "grey", cex=1.2, pt.cex=3)
# 
# 
# 
# #=================
# # Plot3: If you remove the 2 first lines, the function compute the max and min of each variable with the available data:
# colors_border=c( rgb(0.2,0.5,0.5,0.9), rgb(0.8,0.2,0.5,0.9) , rgb(0.7,0.5,0.1,0.9) )
# colors_in=c( rgb(0.2,0.5,0.5,0.4), rgb(0.8,0.2,0.5,0.4) , rgb(0.7,0.5,0.1,0.4) )
# radarchart( data[-c(1,2),]  , axistype=0 , maxmin=F,
#             #custom polygon
#             pcol=colors_border , pfcol=colors_in , plwd=4 , plty=1,
#             #custom the grid
#             cglcol="grey", cglty=1, axislabcol="black", cglwd=0.8, 
#             #custom labels
#==============================================================================
#vlcex=0.8 
#)
#legend(x=0.7, y=1, legend = rownames(data[-c(1,2),]), bty = "n", pch=20 , col=colors_in , text.col = "grey", cex=1.2, pt.cex=3)
