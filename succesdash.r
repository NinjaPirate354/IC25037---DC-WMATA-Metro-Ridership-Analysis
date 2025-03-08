library(shiny)
library(shinydashboard)
library(tidyverse)
library(plotly)
library(viridis)
library(lubridate)
library(DT)
library(broom)

# Data Loading and Cleaning
data <- read_csv("Updated stats.csv", show_col_types = FALSE) %>%
  rename(
    Station_Name = `Station Name`,
    Time_Period = `Time Period`,
    Avg_Daily_Entries = `Avg Daily Entries`
  ) %>%
  mutate(
    Date = mdy(gsub("_", " ", `December_21_2024`)),
    Avg_Daily_Entries = as.numeric(str_remove(Avg_Daily_Entries, "K")) * 1000,
    Time_Period = factor(Time_Period, levels = c(
      "AM Peak (Open-9:30am)", "Midday (9:30am-3pm)", 
      "PM Peak (3pm-7pm)", "Evening (7pm-12am)", 
      "Late Night (12am-Close)"
    )),
    Season = factor(Season, levels = c("Spring", "Summer", "Fall", "Winter")),
    Month = month(Date, label = TRUE, abbr = FALSE),
    Week = week(Date),
    Weekday = wday(Date, label = TRUE)
  ) %>%
  select(-`December_21_2024`) %>%
  filter(!is.na(Date))

date_range <- range(data$Date)

# UI Definition
ui <- dashboardPage(
  dashboardHeader(
    title = "Station Analytics Dashboard",
    titleWidth = 650,
    tags$li(class = "dropdown",
            tags$a(href = "https://www.linkedin.com/in/defang-ako-eyong-809168294/",
                   icon("linkedin"), "Eyong", target = "_blank")),
    tags$li(class = "dropdown",
            tags$a(href = "https://discord.com/channels/@tony05310",
                   icon("discord"), "Eyong", target = "_blank")),
    tags$li(class = "dropdown",
            tags$a(href = "https://github.com/edefang",
                   icon("github"), "Eyong", target = "_blank"))
  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Main Dashboard", tabName = "main", icon = icon("dashboard")),
      menuItem("Pattern Analysis", tabName = "patterns", icon = icon("chart-line"),
               menuSubItem("Hierarchy Distribution", tabName = "hierarchy"),
               menuSubItem("Monthly Patterns", tabName = "monthly"),
               menuSubItem("Distribution Analysis", tabName = "stats")
      ),
      menuItem("Data Explorer", tabName = "data", icon = icon("database"))
    ),
    dateRangeInput("date_range", "Date Range",
                   start = min(date_range),
                   end = max(date_range)),
    selectInput("season", "Season", 
                choices = c("All", "Spring", "Summer", "Fall", "Winter")),
    checkboxGroupInput("time_filter", "Time Period", 
                       choices = levels(data$Time_Period),
                       selected = levels(data$Time_Period))
  ),
  dashboardBody(
    tags$style(HTML("
      .box { 
        border-radius: 8px; 
        box-shadow: 0 2px 4px rgba(0,0,0,0.1);
        margin-bottom: 15px;
      }
      .plot-description {
        padding: 15px;
        background: #f8f9fa;
        border-top: 1px solid #dee2e6;
        font-size: 13px;
        color: #495057;
      }
    ")),
    
    tabItems(
      tabItem(tabName = "main",
              fluidRow(
                valueBoxOutput("total_entries", width = 3),
                valueBoxOutput("peak_station", width = 3),
                valueBoxOutput("busiest_month", width = 3),
                valueBoxOutput("trend_indicator", width = 3)
              ),
              fluidRow(
                box(width = 12, plotlyOutput("station_ranking"),
                    tags$div(class = "plot-description",
                             "Top 20 stations ranked by total entries")
                )
              )
      ),
      
      tabItem(tabName = "hierarchy",
              box(width = 12, plotlyOutput("hierarchy_tree"),
                  tags$div(class = "plot-description",
                           "Hierarchical breakdown of entries")
              )
      ),
      
      tabItem(tabName = "monthly",
              box(width = 12, plotlyOutput("monthly_heatmap"),
                  tags$div(class = "plot-description",
                           "Monthly entry patterns visualization")
              )
      ),
      
      tabItem(tabName = "stats",
              box(width = 12, plotlyOutput("distribution_plot"),
                  tags$div(class = "plot-description",
                           "Statistical distribution analysis")),
              box(width = 12, DTOutput("stats_table"),
                  tags$div(class = "plot-description",
                           "Summary statistics by time period"))
      ),
      
      tabItem(tabName = "data",
              box(width = 12, DTOutput("data_table"), 
                  style = "overflow-x: scroll;")
      )
    )
  )
)

# Server Logic
server <- function(input, output, session) {
  
  filtered_data <- reactive({
    req(input$date_range)
    df <- data %>%
      filter(
        Date >= input$date_range[1],
        Date <= input$date_range[2],
        Time_Period %in% input$time_filter
      )
    
    if(input$season != "All") {
      df <- df %>% filter(Season == input$season)
    }
    
    df
  })
  
  output$total_entries <- renderValueBox({
    entries <- sum(filtered_data()$Avg_Daily_Entries, na.rm = TRUE)
    valueBox(
      format(entries, big.mark = ","), 
      "Total Entries", 
      color = "blue", 
      icon = icon("users")
    )
  })
  
  output$peak_station <- renderValueBox({
    station <- filtered_data() %>% 
      count(Station_Name, wt = Avg_Daily_Entries, sort = TRUE) %>% 
      slice(1) %>% 
      pull(Station_Name)
    valueBox(station, "Busiest Station", color = "green", icon = icon("train"))
  })
  
  output$busiest_month <- renderValueBox({
    month <- filtered_data() %>% 
      count(Month, wt = Avg_Daily_Entries, sort = TRUE) %>% 
      slice(1) %>% 
      pull(Month)
    valueBox(month, "Busiest Month", color = "purple", icon = icon("calendar"))
  })
  
  output$trend_indicator <- renderValueBox({
    trend_data <- filtered_data() %>%
      group_by(Date) %>% 
      summarise(Daily_Entries = sum(Avg_Daily_Entries))
    
    if(nrow(trend_data) < 2) {
      return(valueBox(
        "0.00/day",
        "Stable",
        color = "yellow", 
        icon = icon("minus")
      ))
    }
    
    model <- lm(Daily_Entries ~ Date, data = trend_data)
    trend <- tidy(model) %>% filter(term == "Date") %>% pull(estimate)
    
    valueBox(
      paste0(round(ifelse(is.na(trend), 0, trend), 2), "/day"),
      ifelse(trend > 0, "Increasing", "Decreasing"),
      color = ifelse(trend > 0, "green", "red"), 
      icon = icon(ifelse(trend > 0, "arrow-up", "arrow-down"))
    )
  })
  
  output$station_ranking <- renderPlotly({
    plot_data <- filtered_data() %>%
      group_by(Station_Name, Time_Period) %>%
      summarise(Total_Entries = sum(Avg_Daily_Entries), .groups = "drop") %>%
      arrange(desc(Total_Entries)) %>%
      head(20)
    
    ggplot(plot_data, aes(x = Total_Entries, y = reorder(Station_Name, Total_Entries), fill = Time_Period)) +
      geom_col() +
      scale_fill_viridis(discrete = TRUE) +
      labs(x = "Total Entries", y = "Station") +
      theme_minimal()
  })
  
  output$hierarchy_tree <- renderPlotly({
    plot_data <- filtered_data() %>%
      group_by(Station_Name, Time_Period) %>%
      summarise(Total_Entries = sum(Avg_Daily_Entries), .groups = "drop")
    
    plot_ly(
      data = plot_data,
      type = "treemap",
      labels = ~paste(Station_Name, Time_Period),
      parents = ~"All Stations",
      values = ~Total_Entries,
      textinfo = "label+value+percent parent",
      hoverinfo = "label+value+percent",
      marker = list(colorscale = "Viridis")
    )
  })
  
  output$monthly_heatmap <- renderPlotly({
    plot_data <- filtered_data() %>%
      group_by(Month, Time_Period) %>%
      summarise(Entries = sum(Avg_Daily_Entries), .groups = "drop")
    
    ggplot(plot_data, aes(x = Month, y = Time_Period, fill = Entries)) +
      geom_tile() +
      scale_fill_viridis(option = "inferno") +
      labs(x = "Month", y = "Time Period") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  output$distribution_plot <- renderPlotly({
    plot_data <- filtered_data()
    
    ggplot(plot_data, aes(x = Time_Period, y = Avg_Daily_Entries, fill = Time_Period)) +
      geom_boxplot(outlier.shape = NA) +
      stat_summary(fun = mean, geom = "point", shape = 18, size = 3, color = "red") +
      scale_fill_viridis(discrete = TRUE) +
      scale_y_log10(
        labels = scales::comma,
        breaks = 10^(0:10)
      ) +
      labs(
        x = "Time Period", 
        y = "Daily Entries (log scale)",
        title = "Distribution Analysis with Mean (red diamond)"
      ) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none"
      )
  })
  
  output$stats_table <- renderDT({
    stats_data <- filtered_data() %>%
      group_by(Time_Period) %>%
      summarise(
        Mean = mean(Avg_Daily_Entries, na.rm = TRUE),
        Median = median(Avg_Daily_Entries, na.rm = TRUE),
        SD = sd(Avg_Daily_Entries, na.rm = TRUE),
        IQR = IQR(Avg_Daily_Entries, na.rm = TRUE),
        Min = min(Avg_Daily_Entries, na.rm = TRUE),
        Max = max(Avg_Daily_Entries, na.rm = TRUE),
        .groups = "drop"
      )
    
    datatable(stats_data,
              options = list(pageLength = 5, scrollX = TRUE),
              rownames = FALSE) %>%
      formatRound(columns = c("Mean", "Median", "SD", "IQR", "Min", "Max"), digits = 0)
  })
  
  output$data_table <- renderDT({
    req(nrow(filtered_data()) > 0)
    datatable(
      filtered_data(),
      extensions = c('Buttons', 'Scroller'),
      options = list(
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel'),
        pageLength = 10,
        scrollX = TRUE,
        scrollY = "400px",
        scroller = TRUE
      )
    ) %>%
      formatRound("Avg_Daily_Entries", digits = 0)
  })
}

shinyApp(ui, server)