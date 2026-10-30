library(shiny)
library(shinydashboard)
library(readxl)
library(ggplot2)

blinkit <- read_excel(
  "C:/Users/UTPAL KUMAR/OneDrive/Desktop/POWER BI PROJECT/BlinkIT Grocery Data.xlsx"
)
fat_content <- tolower(trimws(as.character(blinkit[["Item Fat Content"]])))

fat_content[fat_content %in% c("low fat", "lf")] <- "Low Fat"
fat_content[fat_content %in% c("regular", "reg")] <- "Regular"

blinkit[["Item Fat Content"]] <- fat_content
ui <- dashboardPage(
  skin = "yellow",
  dashboardHeader(
    title = HTML('blink<span style="color: green;">it</span>')
  ),
  
  dashboardSidebar(
    h3("FILTER PANEL", style = "padding-left: 15px;"),
    
    selectInput(
      "location",
      "Outlet Location Type",
      choices = c("All", sort(unique(blinkit$`Outlet Location Type`)))
    ),
    
    selectInput(
      "size",
      "Outlet Size",
      choices = c("All", sort(unique(blinkit$`Outlet Size`)))
    ),

    selectInput(
      "item_type",
      "Item Type",
      choices = c("All", sort(unique(blinkit$`Item Type`)))
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$style(HTML("
      .content { padding: 6px; }
.box { margin-bottom: 4px !important; }
.box-header { padding: 6px !important; }
.box-body { padding: 4px !important; }
.small-box { min-height: 95px !important; margin-bottom: 4px !important; }
.small-box .inner { padding: 7px !important; }
.small-box h3 { font-size: 21px !important; white-space: nowrap; margin: 0 0 4px !important; }
.small-box p { font-size: 11px !important; margin: 0; }
.dataTable tbody td { padding: 4px 6px !important; }

.skin-yellow .main-sidebar { background-color: #ffd400 !important; }
.skin-yellow .sidebar h3,
.skin-yellow .sidebar .control-label { color: #222 !important; font-weight: bold; }
.skin-yellow .main-header .logo { background-color: #f5c400 !important; color: #111 !important; font-weight: bold; }
.skin-yellow .main-header .navbar { background-color: #ffc900 !important; }
#sales_year { height: 190px !important; }
#fat_content { height: 195px !important; }
#item_type_sales { height: 205px !important; }
#fat_by_outlet { height: 110px !important; }
#outlet_size,
#location_sales { height: 135px !important; }
    "))
    ),
    
    fluidRow(
      column(
        width = 6,
        fluidRow(
          valueBoxOutput("total_sales", width = 6),
          valueBoxOutput("avg_sales", width = 6)
        ),
        fluidRow(
          valueBoxOutput("item_count", width = 6),
          valueBoxOutput("avg_rating", width = 6)
        )
      ),
      column(
        width = 6,
        box(
          title = "OUTLET ESTABLISHMENT",
          width = 12,
          plotOutput("sales_year", height = "220px")
        )
      )
    ),
    
    fluidRow(
      column(
        width = 6,
        fluidRow(
          column(
            width = 6,
            box(
              title = "FAT CONTENT",
              width = 12,
              plotOutput("fat_content", height = "180px")
            )
          ),
          column(
            width = 6,
            box(
              title = "TOTAL TYPE",
              width = 12,
              plotOutput("item_type_sales", height = "260px")
            )
          )
        ),
        box(
          title = "FAT BY OUTLET",
          width = 12,
          plotOutput("fat_by_outlet", height = "150px")
        )
      ),
      
      column(
        width = 6,
        fluidRow(
          column(
            width = 6,
            box(
              title = "OUTLET SIZE",
              width = 12,
              plotOutput("outlet_size", height = "170px")
            )
          ),
          column(
            width = 6,
            box(
              title = "OUTLET LOCATION",
              width = 12,
              plotOutput("location_sales", height = "170px")
            )
          )
        ),
        box(
          title = "OUTLET TYPE",
          width = 12,
          DT::DTOutput("outlet_summary")
        )
      )
    )
  )
)
server <- function(input, output, session) {
  
  filtered_data <- reactive({
    data <- blinkit
    
    if (input$location != "All") {
      data <- data[data$`Outlet Location Type` == input$location, ]
    }
    
    if (input$size != "All") {
      data <- data[data$`Outlet Size` == input$size, ]
    }
    
    if (input$item_type != "All") {
      data <- data[data$`Item Type` == input$item_type, ]
    }
    
    data
  })
  
  output$total_sales <- renderValueBox({
    sales_value <- base::sum(
      as.numeric(filtered_data()[["Sales"]]),
      na.rm = TRUE
    ) / 1000
    
    valueBox(
      value = base::sprintf("$%.2fK", sales_value),
      subtitle = "TOTAL SALES",
      icon = icon("dollar-sign"),
      color = "yellow"
    )
  })
  
  output$avg_sales <- renderValueBox({
    valueBox(
      paste0("$", round(mean(filtered_data()$Sales), 2)),
      "AVG SALES",
      icon = icon("chart-line"),
      color = "green"
    )
  })
  
  output$item_count <- renderValueBox({
    valueBox(
      nrow(filtered_data()),
      "NO OF ITEMS",
      icon = icon("shopping-basket"),
      color = "yellow"
    )
  })
  
  output$avg_rating <- renderValueBox({
    valueBox(
      round(mean(filtered_data()$Rating, na.rm = TRUE), 2),
      "AVG RATING",
      icon = icon("star"),
      color = "green"
    )
  })
  output$outlet_size <- renderPlot({
    current_data <- filtered_data()
    
    size_data <- dplyr::summarise(
      dplyr::group_by(current_data, `Outlet Size`),
      TotalSales = base::sum(Sales, na.rm = TRUE),
      .groups = "drop"
    )
    
    size_data <- dplyr::filter(size_data, !is.na(`Outlet Size`))
    
    ggplot(size_data, aes(x = "", y = TotalSales, fill = `Outlet Size`)) +
      geom_col(width = 1) +
      coord_polar(theta = "y") +
      theme_void() +
      labs(fill = "Outlet Size")
  })
  
  output$location_sales <- renderPlot({
    current_data <- filtered_data()
    
    location_data <- dplyr::summarise(
      dplyr::group_by(current_data, `Outlet Location Type`),
      TotalSales = base::sum(Sales, na.rm = TRUE),
      .groups = "drop"
    )
    
    ggplot(
      location_data,
      aes(x = reorder(`Outlet Location Type`, TotalSales), y = TotalSales)
    ) +
      geom_col(fill = "#d4b000") +
      coord_flip() +
      labs(x = "Outlet Location", y = "Total Sales") +
      theme_minimal()
  })
  output$sales_year <- renderPlot({
    current_data <- filtered_data()
    
    year_data <- data.frame(
      Year = as.numeric(current_data[["Outlet Establishment Year"]]),
      Sales = as.numeric(current_data[["Sales"]])
    )
    
    year_data <- year_data[complete.cases(year_data), ]
    
    year_data <- dplyr::summarise(
      dplyr::group_by(year_data, Year),
      TotalSales = base::sum(Sales),
      .groups = "drop"
    )
    
    ggplot(year_data, aes(x = Year, y = TotalSales)) +
      geom_line(color = "#d4b000", linewidth = 1.2) +
      geom_point(color = "#d4b000", size = 2) +
      labs(
        x = "Outlet Establishment Year",
        y = "Total Sales"
      ) +
      theme_minimal()
  })
  output$item_type_sales <- renderPlot({
    current_data <- filtered_data()
    
    item_data <- dplyr::summarise(
      dplyr::group_by(current_data, `Item Type`),
      TotalSales = base::sum(Sales, na.rm = TRUE),
      .groups = "drop"
    )
    
    item_data <- dplyr::filter(item_data, !is.na(`Item Type`))
    
    ggplot(
      item_data,
      aes(x = reorder(`Item Type`, TotalSales), y = TotalSales)
    ) +
      geom_col(fill = "#d4b000") +
      coord_flip() +
      labs(x = NULL, y = "Total Sales") +
      theme_minimal()
  })
  output$fat_content <- renderPlot({
    current_data <- filtered_data()
    
    fat_data <- dplyr::summarise(
      dplyr::group_by(current_data, `Item Fat Content`),
      TotalSales = base::sum(Sales, na.rm = TRUE),
      .groups = "drop"
    )
    
    fat_data <- dplyr::filter(fat_data, !is.na(`Item Fat Content`))
    
    ggplot(fat_data, aes(x = 2, y = TotalSales, fill = `Item Fat Content`)) +
      geom_col(width = 1) +
      coord_polar(theta = "y") +
      xlim(0.5, 2.5) +
      scale_fill_manual(values = c("Low Fat" = "#b29a00", "Regular" = "#a8c77b")) +
      theme_void() +
      labs(fill = "Fat Content")
  })
  
  output$fat_by_outlet <- renderPlot({
    current_data <- filtered_data()
    
    fat_outlet_data <- dplyr::summarise(
      dplyr::group_by(current_data, `Outlet Location Type`, `Item Fat Content`),
      TotalSales = base::sum(Sales, na.rm = TRUE),
      .groups = "drop"
    )
    
    ggplot(
      fat_outlet_data,
      aes(x = `Outlet Location Type`, y = TotalSales, fill = `Item Fat Content`)
    ) +
      geom_col() +
      coord_flip() +
      scale_fill_manual(values = c("Low Fat" = "#b29a00", "Regular" = "#a8c77b")) +
      labs(x = "Outlet Location", y = "Total Sales", fill = "Fat Content") +
      theme_minimal()
  })
  output$outlet_summary <- DT::renderDT({
    current_data <- filtered_data()
    
    outlet_summary <- dplyr::summarise(
      dplyr::group_by(current_data, `Outlet Type`),
      `Total Sales` = paste0(
        "$",
        round(base::sum(Sales, na.rm = TRUE) / 1000, 2),
        "K"
      ),
      `No of Items` = dplyr::n(),
      `Avg Sales` = paste0("$", round(mean(Sales, na.rm = TRUE), 0)),
      `Item Visibility` = round(mean(`Item Visibility`, na.rm = TRUE), 2),
      .groups = "drop"
    )
    
    table <- DT::datatable(
      outlet_summary,
      rownames = FALSE,
      options = list(
        dom = "t",
        paging = FALSE,
        searching = FALSE,
        ordering = FALSE
      )
    )
    
    table <- DT::formatStyle(
      table, "Total Sales",
      backgroundColor = "#ffd400",
      fontWeight = "bold"
    )
    table <- DT::formatStyle(
      table, "No of Items",
      backgroundColor = "#27b300",
      fontWeight = "bold"
    )
    table <- DT::formatStyle(
      table, "Avg Sales",
      backgroundColor = "#e89595",
      fontWeight = "bold"
    )
    DT::formatStyle(
      table, "Item Visibility",
      backgroundColor = "#6fa5d8",
      fontWeight = "bold"
    )
  })
}

shinyApp(ui, server)