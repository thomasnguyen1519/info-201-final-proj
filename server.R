
library("plotly")
library("tidyr")
library("dplyr")
library("ggplot2")
library("maps")
library("Hmisc")
library("sp")
library("maptools")
library("reshape2")

source('test.R')


server <- function(input, output) {

  # #reactive variable 
  # data1 <- reactive({
  #   sb <- read.csv('data/MERGED2015_16_PP.csv', stringsAsFactors = FALSE)
  #   sb <- filter(sb, STABBR == input$state1)
  #   sb <- select(sb, STABBR, INSTNM, TUITIONFEE_IN, TUITIONFEE_OUT)
  #   sb <- filter(sb, TUITIONFEE_IN != 'NULL' & TUITIONFEE_OUT != 'NULL')
  #   return(sb)
  # })
  # 
  # data2 <- reactive ({
  #   sb <- read.csv('data/MERGED2015_16_PP.csv', stringsAsFactors = FALSE)
  #   sb <- filter(sb, STABBR == input$state2)
  #   sb <- select(sb, STABBR, INSTNM, TUITIONFEE_IN, TUITIONFEE_OUT)
  #   sb <- filter(sb, TUITIONFEE_IN != 'NULL' & TUITIONFEE_OUT != 'NULL')
  #   return(sb)
  # })
  
  data3 <- reactive({
    sb <- read.csv('data/MERGED2015_16_PP.csv', stringsAsFactors = FALSE)
    sb <- filter(sb, STABBR == input$state3)
     sb <- select(sb, STABBR, INSTNM, DEBT_MDN, COSTT4_A)
    return(sb)
  })
  
  data4 <- reactive({
    sb <- sb.data
    sb <- filter(sb, STABBR == input$state3)
    sb <- select(sb, STABBR, INSTNM)
    return(sb)
  })
  
  data5 <- reactive ({
    state <- input$state4
    region <- combine2$long[combine2$states == state]
    return(region)
  })
  
  data6 <- reactive ({
    sb <- read.csv('data/MERGED2015_16_PP.csv', stringsAsFactors = FALSE)
    sb <- filter(sb <- filter(sb, STABBR == input$state4))
    sb <- select(sb, STABBR, INSTNM, LONGITUDE, LATITUDE)
    
  })
  
  output$graph <- renderPlotly({
    selection <- paste0("", input$filt.key)
    col <- ""
    if (selection == "Percentage of undergraduates who receive a Pell Grant") {
      col <- "PCTPELL"
    } else if (selection == "Average family income of dependent students in real 2015 dollars") {
      col <- "DEP_INC_AVG"
    } else {
      col <- "COMPL_RPY_7YR_RT"
    }
    col.sym <- rlang::sym(col)
    data <- read.csv("data/MERGED2015_16_PP.csv", stringsAsFactors=FALSE)
    state1 <- state.abb[grep(input$state1.key, state.name)]
    state2 <- state.abb[grep(input$state2.key, state.name)]
    table <- filter(data, STABBR == state1 | STABBR == state2) %>%
      group_by(STABBR) %>%
      rename(State = STABBR) %>%
      rename(Institution = INSTNM) %>%
      select(Institution, State, col) %>%
      na.omit() %>%
      filter((!!col.sym) != "NULL" & (!!col.sym) != "PrivacySuppressed")
    table <- transform(table, converted = as.numeric(table[[3]]))
    summarise(group_by(table, State), average = mean(converted)) %>%
      plot_ly(labels = ~State, values = ~average) %>%
      add_pie(hole = 0.4) %>%
      layout(title = selection, showlegend = T,
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  })
  
  output$data.table <- renderDataTable({
    selection <- paste0("", input$filt.key)
    col <- ""
    if (selection == "Percentage of undergraduates who receive a Pell Grant") {
      col <- "PCTPELL"
    } else if (selection == "Average family income of dependent students in real 2015 dollars") {
      col <- "DEP_INC_AVG"
    } else {
      col <- "COMPL_RPY_7YR_RT"
    }
    col.sym <- rlang::sym(col)
    data <- read.csv("data/MERGED2015_16_PP.csv", stringsAsFactors=FALSE)
    state1 <- state.abb[grep(input$state1.key, state.name)]
    state2 <- state.abb[grep(input$state2.key, state.name)]
    table <- filter(data, STABBR == state1 | STABBR == state2) %>%
      group_by(STABBR) %>%
      rename(State = STABBR) %>%
      rename(Institution = INSTNM) %>%
      select(Institution, State, col) %>%
      na.omit() %>%
      filter((!!col.sym) != "NULL" & (!!col.sym) != "PrivacySuppressed")
    return(table)
  })
  
  
output$plot4 <- renderPlot({
  tom <- map_data('county')
  state123 <- subset(tom, region == data5())
  base123 <- ggplot() + geom_polygon(data = state123, mapping = aes(x = long, y = lat, group = group), color = 'white',  fill = "gray") +
    coord_fixed(1.3)
  point <- filter(data6(), INSTNM == input$college)
  base <- base123 + geom_point(data = point, aes(x = as.numeric(LONGITUDE), y = as.numeric(LATITUDE)), color="red", size=10)
  return(base)
})


output$plot5 <- renderPlot({
  p <- ggplot(data3()) +
    geom_histogram(aes(as.numeric(DEBT_MDN)), fill = "#465362") + 
    labs(x = "MEDIAN STUDENT DEBT" , y = "NUMBER OF COLLEGES")
    
  return(p)
})

output$colleges <- renderUI({
  selectInput("college", "Choose your College:", choices = data6()$INSTNM, selected = "The Art Institute of Seattle")
})

output$plot6 <- renderPlot ({
  p <- ggplot(data3()) + 
    geom_histogram(aes(as.numeric(COSTT4_A)),fill = "#011936")+ 
  labs(x = "COST OF ATTENDACE" , y = "NUMBER OF COLLEGES")
  return(p)
  })

  #renders data table
  output$table <- renderDataTable({
    filter(data6())
  })
  
  #mario
  output$tableHead <- renderText({
    paste("Yo,yo,check out the table for your state")
  })
  
  ## this is the title of table
  output$tableTitle <- renderText({
    paste("This is a table shows the 10 cities with the highest ratio of income and cost
          may be less(because they don have 10 cities) 
          citys with in" , input$State)
  })
  
  ## this is the introducation of the table to help reader
  ## to interpret the table and plot
  output$tableInstruction <- renderText({
    paste("You can interpret this table by just simply read how long is the bar, the bar 
          indicate the combination of cost of the university education, and the earning
          result from this university education, the higher the bar the more return of 
          earning with each dollar spend on education. And the table below shows university
          with city states, and the earning of students in this university
          after 10yrs,8yrs, 6yrs graduate" )
  })
  
  ## this is the plot which is about the earning and tuition ratio
  output$cityInStateEarningRatio <- renderPlot({
    name.of.state <- input$State
    abb.of.state <- state.abb[match(toupper(name.of.state),toupper(state.name))]
    get.state.city.to.plot <- get.one.state.all.city.earning.ratio(combine.earning.tuition, abb.of.state, input$filterData)
    
    get.state.city.to.plot <- head(get.state.city.to.plot[order(get.state.city.to.plot$avg_earning_tuition_ration, decreasing = TRUE),],10)
    ggplot(data = get.state.city.to.plot) +
      geom_col(mapping = aes(x = CITY, y=avg_earning_tuition_ration, fill = CITY) ) + coord_flip()
  })
  
  output$top10head <- renderText({
    paste("These are the university which have the highest income student in avg")
  })
  
  ## this is the table which is about the top 10 university which have the richest student
  output$Top10EarningUniveristy <- renderTable({
    name.of.state <- input$State
    abb.of.state <- state.abb[match(toupper(name.of.state),toupper(state.name))]
    result.data <- select(get.the.top10.highest.earning.in.one.state(processed.university.only.earning, abb.of.state), university.name.state, only.mean.earning)
    result.data
  })
  
  ## this is the conclusion made by me to address the question
  output$conclusionAboutTable <- renderText({
    if(input$mySummaryTable == "2") {
      output$conclusionAboutTableTitle <- renderText({
        paste("My conclusion about table")
      })
      paste("It will be very easy for the client to make the decision, when they know which
            state they want to send their children to, when on state is clicked the earning tuition
            ratio is shown, this is get by earning divide by the tuition, the earning is higher
            the ration larger, the tuition lower the ratio larger, so the city with the highest
            ratio, is the city with the most lucrative college education.And the univeristy list
            below will be helpful when they consider, because the problem of using the ratio is that,
            it might be the low tuition make the ratio high no the high earning, which I think the 
            parent would also want to choose the one with high mean income, and the user can also choose
            their focus to get the data, they like, whether they are short term people or long term people
            will have different aglorism to deal with the data")
    }
    })
  
  ## this is the title of the map
  output$maptitle <- renderText({
    paste("This is a smoking ass map about in which states has the richest students")
  })
  
  # this is the introducation of the map to help client to interpret the data
  output$mapIntro <- renderText({
    paste("This is a map which show the data about avg salary after students graduated, the darker
          the color is the more salary students made after they graduate. The data decribe the
          condition after students graduate are evaluate in three terms, the short term 6 years, 
          the med term 8 years, the the long term 10 years. I use the average of these three to 
          interpret the avg earning of student graduated from the university in this states.By 
          clicking each states you will get the box and whisker plot of the earning of students in
          that states, and the top 10 university with the richest student after graduate.BTW(this
          is from 2014 data, the reason we only use one year data is that, the csv file is too large
          and the data before 2014 is not filled properly and most of them are half blank, and also
          by reviewing the past data, I see that there is no striking in increase in income of student
          with the consideration of the influnence of inflation)
          ")
  })
  
  ## this is the map
  output$usmap <- renderPlot({
    ggplot() + 
      geom_polygon(data = all.country.summary.map, aes(x = long, y = lat, group = group, fill = avg.of.three.time), colour = "grey60") + 
      coord_fixed(1.3) + 
      ggtitle("In which states has the richest students") +
      theme_bw() +
      scale_fill_brewer(palette = "Blues")
  })
  
  # this is the respond for your click
  output$info <- renderText({
    point.passed.in <- data.frame(x= c(input$plot_click$x), y=c(input$plot_click$y))
    if(ncol(point.passed.in)==0) {
      paste("click the state to get some data")
    } else if(is.na(nchar(latlong2state(point.passed.in)))) {
      paste("there is no data about the coordinate you click")
    } else {
      paste("You click", latlong2state(point.passed.in))
    }
  })
  
  ## this is the box and w diagram of the state
  output$boxWdiagram <- renderPlot({
    point.passed.in <- data.frame(x= c(input$plot_click$x), y=c(input$plot_click$y))
    if(ncol(point.passed.in)==0) {
      paste("click the state to get some data")
    } else if(is.na(nchar(latlong2state(point.passed.in)))) {
      paste("there is no data about the coordinate you click")
    } else {
      name.of.state <- latlong2state(point.passed.in)
      abb.of.state.click <- state.abb[match(toupper(name.of.state),toupper(state.name))]
      bwd.data.result <- get.data.for.box.whisker(processed.university.only.earning, abb.of.state.click)
      boxplot(value~variable, data = bwd.data.result)
      output$explainboxandw <- renderText({
        paste("this is a box and whisker diagram of the state that you click, 1 is for 10 year after graduate
              2 is for 8 year after graduate, 3 is for 6 years after graduate, you can see always see a 
              increase in median of along there three, and more widespread, this reason is that as
              people work longer they go better experience and higher wage, and the reason for 
              more widespread of data is that, poor people stuck here and get poorer, and rich people
              fight for theri life and promotion, and become rich")
      })
      }
      })
  
  # this is the top10 univerisity of state that is clicked      
  output$top10university <- renderTable({
    point.passed.in <- data.frame(x= c(input$plot_click$x), y=c(input$plot_click$y))
    if(ncol(point.passed.in)!=0 && !is.na(nchar(latlong2state(point.passed.in)))){
      name.of.state <- latlong2state(point.passed.in)
      abb.of.state.click <- state.abb[match(toupper(name.of.state),toupper(state.name))]
      return(get.the.top10.highest.earning.in.one.state(processed.university.only.earning, abb.of.state.click))
    }
  }) 
  
  
  ## this is the conclusion I made about from the map observation
  output$myconclusion <- renderText({
    if(input$mySummary == 2) {
      output$myconclusiontitle <- renderText({
        paste("My conclusion")
      })
      paste("Generally speaking, from the map you can see that, the west coast and the east cost 
            produce students with heighst income. And the highest state is massachusetts, which 
            which is not very surprising, because this state have the reputation that this state 
            is a giant education center. Considering the common sense that the state on these
            two coast are always have very high cost of living which might be the reason that 
            these states have the higher income. If I have a child, I would send him to the mid
            part of US such as North Dakota, Nebraska. The reason is that those states are in the 
            same rank of income as Washington, but in common sense those states have much lower 
            cost of living, which means that the real value not the monetary value that the
            student produced after they graduate is much higher")
    } else {
      paste("Please Click yes, otherwise you will loose 100 dollar in you paypal account")
    }
  })
  #mario
  
  
  
  
}