
source('test.R')

ui <- fluidPage(
  navbarPage("Educational Institution Evaluation",
             
      tabPanel("Home",
        h2("Project introduction"),
        p("Authors: CaiZenan, Tom, Johnny"),
        h4("Purpose of this Application"),
        p("The purpose of this web application is to help the parents, students, and the government evaluate the colleges in each US state given their generalized intentions. 
          For example, parents and students may use this web app to evaluate potential schools. Some generalized factors that they may consider are the tuition,
          the cost of living, the financial support, the earning after graduate. The US government may have a different use case, as explainedd later. By going through this app,
          clients will gain a basic idea about university education from a quantitative perspective."),
        p("Specifically, the goal of this app is to answer the following questions:"),
        tags$ul(
          tags$li("What is the cost effectiveness of the university within each state (for the students)?"), 
          tags$li("What is the satisfaction of the degree amongst the students of the universities?"), 
          tags$li("What is the cost effectiveness of the university within each state (for government)?"),
          tags$li("Should there be an evenness of the education resources when distribution within each state?")
        ),
        h4("General Data Description"),
        p("Data used in this web application comes from the the US government's public, open datasets. It contains a multitude of variables describing various statistics on the vast
          demographic of the collgiate population. The data categories ranged from \"Number of students not working and not enrolled 7 years after entry\" to \"Religous affiliation of the institution\". We only used what varibles that we believed were significant and necessary to
          answering our target questions. Therefore, we only used a select few of the variables from the dataset."),
        h4("Sources of the Data"),
        p("Dataset: https://catalog.data.gov/dataset/college-scorecard/resource/2a7f670e-0799-436a-9394-df0a9b3ba7c5"),
        p("Dictionary for the Dataset: https://collegescorecard.ed.gov/assets/CollegeScorecardDataDictionary.xlsx"),
        h3("Tab Navigation Descriptions"),
        h4("Find College"),
        p("Lets you pick a college and it will be located on its state map!"),
        h4("EarningAnalysisPlot"),
        p("This tab is about the detailed analysis of the earning and earning tuition ratio within each states"),
        h4("MapAboutEarning"),
        p("This tab is about earning condition throughout universities in US. The client is should interpret the general condition about earning of students after graduate"),
        h4("Median Debt vs. Cost of Attendance"),
        p("This tab gives some information regarding the Median Debt for student of each college. The median debt is the calculated by taking the median of debt for each college
          student. The graph is then created by plotting the number of colleges which has this median debt. Debt is important to consider when evaluating which state you want to 
          go to school in. Also under the first graph is the cost of attendance. The cost of attendance is calculated for each college by totaling the cost of tuition, living expenses,
          fees, books, and more. This is also important to consider when deciding for a college as a higher cost of attendance means you need more money to go to a certain school.
          So by having both these histograms we can see patterns of high debt or high cost of attendance for each state."),
        h4("Government Educational Funding Comparison"),
        p("In this tab, you will easily be able to compare two states on their merit for government funding at a state level perspective, based on a few selected factors. The targetted user
          group of this tab is the US government. They may use this area of the application to dictate their distribution of government welfare for educational institutions to the states.")
      ),

      tabPanel("Find College",
               sidebarLayout(
                 sidebarPanel(
                   selectInput("state4", "Choose your State:", choices = states, selected = "WA"),
                   conditionalPanel(condition = "input$state4 == input$state4", 
                                    uiOutput("colleges"))
                 ),
                 mainPanel(
                   tabsetPanel(type = "tabs",
                               tabPanel("State Map", plotOutput("plot4")),
                               tabPanel("Table", dataTableOutput("table"))
                   )
                 )
                 
               )
      ),
       tabPanel("Income After Grad Related",
                sidebarLayout(
                  ## there are two part ofthe control widge
                  ## one is for the map one is for the plot table
                  sidebarPanel(
                    conditionalPanel(
                      condition="input.tabselected==1",
                      selectInput("State", "Choose a state", state.name),
                      radioButtons("filterData", "Which type of earning you care the most", 
                                   c("The long term earning" = 1, "ShortTerm earning" = 2, "I do not care" = 3)),
                      radioButtons("mySummaryTable", "Do you want to see my conclusion(Of course yes)", 
                                   c("No" = 1, "Yes(after you click this you will be smart)" = 2))
                    ),
                    conditionalPanel(
                      condition="input.tabselected==2", 
                      radioButtons("mySummary", "Do you want to see my conclusion(Of course yes)", 
                                   c("No" = 1, "Yes(after you click this you will be smart)" = 2))
                    )
                  ),
                  
                  mainPanel(
                    ## there are two panel one is about the map and one is about plot
                    ## summary
                    tabsetPanel(type= "tabs", 
                                tabPanel("EarningAnalysisPlot", value = 1,
                                         textOutput("tableHead"),
                                         tags$head(tags$style("#tableHead{color:black;
                                                              font-size:25px;
                                                              font-weight:bold;
                                                              }")
                           ),
                           textOutput("tableTitle"),
                           textOutput("tableInstruction"),
                           plotOutput("cityInStateEarningRatio"),
                           textOutput("top10head"),
                           tags$head(tags$style("#top10head{
                                                color:black;
                                                font-size:25px;
                                                font-weight:bold;
                                                }")
                           ),
                           tableOutput("Top10EarningUniveristy"),
                           textOutput("conclusionAboutTableTitle"),
                           tags$head(tags$style("#conclusionAboutTableTitle{
                                                color:black;
                                                font-size:25px;
                                                font-weight:bold;
                                                }")
                           ),
                           textOutput("conclusionAboutTable"),
                           tags$head(tags$style("#conclusionAboutTable{
                                                color:black;
                                                font-size:15px;
                                                }")
                           )
                           ),
                           tabPanel("MapAboutEarning", value = 2,
                                    textOutput("maptitle"),
                                    tags$head(tags$style("#maptitle{color:black;
                                                         font-size:25px;
                                                         font-weight:bold;
                                                         }")
                           ),
                           textOutput("mapIntro"),
                           plotOutput("usmap", click = "plot_click"),
                           verbatimTextOutput("info"),
                           plotOutput("boxWdiagram"),
                           textOutput("explainboxandw"),
                           textOutput("myconclusiontitle"),
                           tags$head(tags$style("#myconclusiontitle{
                                                color:black;
                                                font-size:25px;
                                                font-weight:bold;
                                                }")
                           ),
                           textOutput("myconclusion"),
                           tags$head(tags$style("#myconclusion{
                                                color:black;
                                                font-size:15px;
                                                
                                                }")
                           ),
                           tableOutput("top10university")
                           
                           ),
                           id = "tabselected"
                           )
                           )
                  )      
      ),      
             

    tabPanel("Median Debt Histogram vs. Cost of Attendance Histogram",
             sidebarLayout(
               sidebarPanel(
                 selectInput("state3", "Choose your State:", choices = states, selected = "WA")
               ),
               mainPanel(
                 h2("Introduction"),
                 p("The goal of this tab is to compare the Median Debt that each university has to the Cost of Attendance for each university. The graphs are created by taking the median student debt
                  of each college and plotting on the histogram. This way we can see which state has the high median debts or low median debts. By also adding the Cost of Attendance for the State as well, 
                  we can determine if going to school in that state is worth the Debt and the Cost.
                   "),
                 plotOutput("plot5"),
                 plotOutput("plot6")
               )
             )

    ),
    tabPanel("Government Funding Comparison",
      titlePanel("Government Funding Comparison"),
      h3("Data Selection"),
      sidebarLayout(
        sidebarPanel(
          selectInput('state1.key', label = "Select the first state", state.name),
          selectInput('state2.key', label = "Select the second state", state.name, selected = "Alaska"),
          radioButtons('filt.key', label = "Select a filter",
                       choices = c("Percentage of undergraduates who receive a Pell Grant",
                                   "Average family income of dependent students in real 2015 dollars",
                                   "Seven-year repayment rate for completers"))
        ),
        mainPanel(
          tabsetPanel(type = "tabs",
                      tabPanel("Comparison Chart", plotlyOutput("graph"),
                               p("This donut pie chart helps to visualize the comparison between two states by metrics
                                 defined by the selected filter. The chart compares strictly the average value of each respective
                                 state for the given filter. Each of the filters were chosen in regard
                                 to possible factors concerning the distribution of government educational
                                 funding at a state level. Based on the comparisons from these factors, the US government
                                 can more effectively and fairly determine how to distribute funding and aid."),
                               p("Note: The data comes from the 2015-2016 dataset. For reference, the data aggregated to create this
                                 data visualization can be viewed in the \"Raw Data\" tab.")),
                      tabPanel("Raw Data", dataTableOutput('data.table')
                      )
                      )
        )
      ))
  )
)


