library(shiny)
library(plotly)
library(DT)

ui <- fluidPage(

    titlePanel("COVID Dicer"),

    sidebarLayout(
        sidebarPanel(
            selectInput("variable", "Variable", choices = list("Cases", "Deaths"), selected = 1),
            selectInput("region", "Region", c(), multiple = TRUE),
            selectInput("country", "Country", c(), multiple = TRUE),
            selectInput("province", "Province/State", c(), multiple = TRUE),
            checkboxInput("geoAggregate", "Geographic Aggregation", value = FALSE),
            dateRangeInput("dates", "Date Range", start = '2020-01-22', min = '2020-01-22', max = Sys.Date()-1, end = Sys.Date()-1),
            actionButton("action", "Update Data"),
        ),

        mainPanel(
            tabsetPanel(
                tabPanel("Plot",
                    plotlyOutput('plot'),
                    checkboxInput("logScale", "Log Scale?", FALSE)
                ),
                tabPanel("Table", 
                    dataTableOutput("table"),
                    downloadLink('downloadData', 'Download Data'))
            )
        )
    )
)

server <- function(input, output, session) {
    
    regions <- list(
        'Africa' = c(),
        'Asia' = c(),
        #https://www.metaculus.com/questions/3720/how-many-covid-2019-cases-in-europe-will-be-confirmed-on-april-the-27th/
        'Europe' = c("Albania", "Andorra", "Armenia", "Austria", "Azerbaijan", "Belarus", "Belgium", "Bosnia and Herzegovina", "Bulgaria", "Croatia", "Cyprus", "Czech Republic", "Denmark", "Estonia", "Finland", "France", "Georgia", "Germany", "Greece", "Hungary", "Iceland", "Ireland", "Italy", "Kazakhstan", "Latvia", "Liechtenstein", "Lithuania", "Luxembourg", "Malta", "Moldova", "Monaco", "Montenegro", "Netherlands", "North Macedonia", "Norway", "Poland", "Portugal", "Romania", "Russia", "San Marino", "Serbia", "Slovakia", "Slovenia", "Spain", "Sweden", "Switzerland", "Turkey", "Ukraine", "United Kingdom", "Vatican City"),
        'Middle East' = c(),
        'North America' = c(),
        'Oceania' = c(),
        'South America' = c()
    )
    cases <- read_csv('time_series_19-covid-Confirmed.csv')
    updateSelectInput(session, "region", choices = names(regions))
    updateSelectInput(session, "country", choices = unique(cases$`Country/Region`))
    updateSelectInput(session, "province", choices = unique(cases$`Province/State`))

    getData <- reactive({
        earliest <- paste(
            as.integer(format(input$dates[1], format="%m")), 
            as.integer(format(input$dates[1], format="%d")), 
            as.integer(format(input$dates[1], format="%y")), 
            sep="/"
        )
        
        latest <- paste(
            as.integer(format(input$dates[2], format="%m")), 
            as.integer(format(input$dates[2], format="%d"))-1, 
            as.integer(format(input$dates[2], format="%y")), 
            sep="/"
        )
        
        cases <- (input$variable == 'Cases') %>%
            ifelse('time_series_19-covid-Confirmed.csv', 'time_series_19-covid-Deaths.csv') %>%
            read_csv() %>%
            select(`Country/Region`, `Province/State`, earliest:latest)
        
        if(!is.null(input$region)){
            cases <- cases %>% filter(`Country/Region` %in% regions[[input$region]])
        }
        
        if(!is.null(input$country)){
            cases <- cases %>% filter(`Country/Region` %in% input$country)
        }
        
        if(!is.null(input$province)){
            cases <- cases %>% filter(`Province/State` %in% input$province)
        }
        
        cases <- cases %>%
            pivot_longer(cols = earliest:latest, names_to = 'Date', values_to = input$variable) %>%
            mutate(Date = mdy(Date))

        if(input$geoAggregate){
            cases <- cases %>% group_by(Date)
            if(input$variable == 'Cases'){
                cases <- cases %>% summarise(Cases = sum(Cases))
            } else {
                cases <- cases %>% summarise(Deaths = sum(Deaths))
            }
        }
        
        cases
    })
    
    output$plot <- renderPlotly({
        data <- getData()
        colorBy <- data$`Province/State`
        if(is.null(input$province)){
            colorBy = data$`Country/Region`
        }
        plot <- plot_ly(type = 'scatter', mode = 'lines', x = data$Date, y = data[[input$variable]], color = colorBy)
        if(input$logScale){
            plot <- layout(plot, yaxis = list(type = "log"))
        }
        plot
    })
    
    output$table <- renderDT({ getData() })
    
    output$downloadData <- downloadHandler(
        filename = function() {
            paste('data-', Sys.Date(), '.csv', sep='')
        },
        content = function(con) {
            write_csv(getData(), con)
        }
    )

    observeEvent(input$action, {
        read_csv('https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv') %>%
            write_csv('time_series_19-covid-Confirmed.csv')
        read_csv('https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Deaths.csv') %>%
            write_csv('time_series_19-covid-Deaths.csv')
    })

    observeEvent(input$region, {
        if(is.null(input$region)) {
            updateSelectInput(session, "country", choices = cases$`Country/Region`)
        } else {
            updateSelectInput(session, "country", choices = intersect(cases$`Country/Region`, regions[[input$region]]))
        }
    })
        
    observeEvent(input$country, {
        updateSelectInput(session, "province", choices = filter(cases, `Country/Region` == input$country)$`Province/State`)
    })
}

shinyApp(ui = ui, server = server)
