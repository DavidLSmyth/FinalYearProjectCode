#drop keys:
#App key: x7ocg03f558ya44
#App secret: az2ayuyt3w6gu8v

#To Do:
#1).Modularize this better - make it clear which sections correspond to tabs in app
#2).Make it clear which parts of the ui are connected to corresponding actions in server
#3).consider changing all selectInputs to selectizeInputs for easy searching
#4).Move player tab to plot tab


library(dygraphs)
library(shiny)
library(shinyjs)
library(DT)
#library(shinythemes)

#Some of the code is commented out because it will be useful to uncomment in future
shinyUI(fluidPage(theme = "bootstrap.css",useShinyjs(),
        #shinythemes::shinytheme("slate"),
        #"bootstrap.css",
  #banner, see www bootstrap.css for style classes/ids                
  div(class='title','Sports Analysis Dashboard',img(src='player.jpeg',align='right',height=75,width=75)),
  div(align='center', style='background-color:white; color:black',
  div(style="display:inline-block; color:black", checkboxInput('dropboxDownload',label='Would you like to choose a file from your dropbox? (Authorization can take a few seconds to initiate)',value=FALSE)),
  div(style="display:inline-block;color:black", selectizeInput(
    'dropboxFileSelection', label = 'Choose a file to upload', choices = list(),options = list(
      placeholder = 'Please select an option below',
      maxItems = 1
    )
  )),
  fileInput('file1', 'Choose local file to upload', 
        accept = c(
        'text/csv',
         #'text/comma-separated-values',
         #'text/tab-separated-values',
         #'text/plain',
         '.csv',
         #'.tsv',
         '.RData'
  )
  )),
  #titlePanel("Variable tracker")),
  
  #navbarPage provides the tabs that the user can navigate across.
  navbarPage("Menu: ",
        tabPanel("Plot",
            sidebarLayout(
              sidebarPanel(style='background:#F3F3FF;',
                selectInput("getname", label = h3("Choose a player"), 
                            choices = list()
                            #unique(SimulatedData$Name), 
                            #selected = unique(SimulatedData$Name)[1]),
                ),
                sliderInput("getsmoothness",label=h4("Interpolating curve smoothness:"),min=0,max=1,step=0.05,value=0.5),
                selectInput("getVariable", label = h3("Choose a variable"), 
                            choices = list()
                            #names(SimulatedData)[sapply(SimulatedData, is.numeric)], 
                            #selected = names(SimulatedData)[sapply(SimulatedData, is.numeric)][1]),
                ),
                #htmlOutput("selectUI"),
                
                checkboxInput("addSmootherPlayer1", label = "Add Smoother", value = FALSE),
                
                selectInput("getComparisonName", label = h3("Choose a comparison player"), 
                            choices =list()
                            #append(c('Mean','No Comparison'),unique(SimulatedData$Name)), 
                            #selected = "No Comparison"),
                ),
                checkboxInput("addSmootherPlayer2", label = "Add Smoother", value = FALSE),
                selectInput(
                  'annotateDrills', label = 'Annotate selected drills:', choices = list(),
                    multiple = TRUE
                  ),
                checkboxInput('annotateAllDrills',label='Annotate all drills',value=FALSE),
                checkboxInput("showgrid", label = "Show Grid", value = TRUE)#,
                
              ),
              mainPanel(
                #Change this to Drill calendar
                    fluidRow(
                    #column(1,tableOutput('PlayerSummaryPlot'))),
                      div(align='center',tableOutput('PlayerSummaryPlot'))),
                br(),
                dygraphOutput("dygraph"),
                br()
              )
            )
          ),
        # Summary tab - Provide a high level summary of the variables in the data
        tabPanel("Training Calendar",
                 sidebarLayout(
                   sidebarPanel(style='background:#F3F3FF;',
                      selectInput("getnameCalendar", label = h3("Choose a player"),choices = list()),
                      selectInput("calendarYear", label='Year',choices=list()),
                      selectInput("calendarMonth", label='Month',choices=list())#,
                      #selectInput("calendarWeek",label = h3("Choose a week"),choices = list())
                 ),         
                 mainPanel(
                   fluidRow(
                     tableOutput('PlayerSummaryCalendar'),
                     htmlOutput('titleDrillCalendar'),
                     tableOutput('DrillCalendarTable'),
                     #column(1,tableOutput('PlayerSummaryDownloadTab')),
                     #div(style='margin-left: 500px',
                    #     column(2,dataTableOutput('FrequencyTableDownloadTab'),width=11))),
                   br()#,
                   #htmlOutput('downloadPreview')))
                 ))
                 )
        ),
        #Barplot tab - plots a barplot high to low player attribute data
        tabPanel("Barplot",
          sidebarPanel(
            #perhaps change this to a daterange
            selectInput("getVariableBarplotTab", label = h3("Choose a variable"), 
                        choices = list()),
            selectInput('barplotYear',label='Choose year',choices = list()),
            selectInput('barplotMonth',label='Choose month',choices = list()),
            checkboxInput('divideByDuration',label='Average by Duration'),
            checkboxInput('dailyOrNot',label=('Daily barplot')),
            selectInput("getDate", label = h3("Choose a Date"), 
                  choices = list())
            ),

            mainPanel(plotOutput("Barplot"))
        ),
        #Rankings tab, think about how best to display this
        #would be nice to have an overall ranking option for this
        tabPanel("Rankings",
          sidebarLayout(sidebarPanel(
              #selectInput("rankBy",label='Rank by',choices=list()),     
              selectInput("rankYear", label='Year',choices=list()),
              selectInput("rankMonth", label='Month',choices=list()),
              selectInput("rankFilterPosition", label='Filter by position:',choices=list())
              ),
            mainPanel(
              div(align='center',
                  h3(tags$b(textOutput('rankingTitle')))),
              DT::dataTableOutput('rankingSummary')
            )
          )
        ),
        #Download report tab, should eventually provide user some customizable options and a give a snippet of example report
        #on the page
        tabPanel("Download Report",
           sidebarLayout(sidebarPanel(style='background:#F3F3FF;',
                selectInput("getnameDownload", label = h3("Choose a player"),choices = list()),
                downloadButton("report","Generate Report"),
                checkboxInput("reportPreview", label = "Preview Report", value = FALSE),
                actionButton("refreshReport", "Refresh Report"),
                textInput("additionalNotes", label = h3("Enter report notes"), value = "")
                
            ),         
            mainPanel(
              fluidRow(
                #column(width=2),
                #column(width=2,selectInput("getnameDownload", label = h3("Choose a player"),choices = list())),
                #column(width=1),
                #column(width=2,downloadButton("report","Generate Report")),
                #column(1,checkboxInput("reportPreview", label = "Preview Report", value = FALSE)),
                #column(2,actionButton("refreshReport", "Refresh Report"))
              ),
              br(),
              fluidRow(
              column(width=1),
              column(width=11,htmlOutput("frame")),
              htmlOutput('downloadPreview'))
            )
          )
        ),
        tabPanel("Help",
              column(width=8,offset=2,wellPanel(verbatimTextOutput("helpTab"))))
      )
  )
)

