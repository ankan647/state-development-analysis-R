# =========================================================
# STATE DEVELOPMENT ANALYTICAL SYSTEM
# =========================================================

# ---------------------------------------------------------
# LOAD LIBRARIES
# ---------------------------------------------------------
# =========================================================
# STATE DEVELOPMENT ANALYTICAL SYSTEM
# =========================================================

# =========================================================
# LOAD LIBRARIES
# =========================================================
setwd("C:/Users/ankan/Downloads")
library(shiny)
library(DT)
library(DBI)
library(RSQLite)
library(ggplot2)
library(dplyr)

# =========================================================
# CONNECT TO DATABASE
# =========================================================

con <- dbConnect(
  SQLite(),
  "development_analysis.db"
)

# =========================================================
# LOAD DATA FROM DATABASE
# =========================================================

df <- dbReadTable(
  con,
  "state_development_data"
)

# =========================================================
# UI
# =========================================================

ui <- fluidPage(
  
  titlePanel("State-wise Development Analysis System"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      selectInput(
        "selected_state",
        "Select State:",
        choices = sort(df$State)
      )
      
    ),
    
    mainPanel(
      
      tabsetPanel(
        
        # =================================================
        # OVERVIEW TAB
        # =================================================
        
        tabPanel(
          
          "Overview",
          
          h2("Project Overview"),
          
          p("This analytical system evaluates development disparities across Indian states using composite scoring, clustering analysis, regression modeling, database connectivity, and visualization techniques in R."),
          
          br(),
          
          h4("Key Features"),
          tags$ul(
            tags$li("Composite Development Score"),
            tags$li("K-Means Clustering"),
            tags$li("Regression Analysis"),
            tags$li("Interactive State Explorer"),
            tags$li("Visualization Dashboard"),
            tags$li("SQLite Database Integration")
          )
        ),
        
        # =================================================
        # STATE RANKINGS TABLE
        # =================================================
        
        tabPanel(
          
          "State Rankings",
          
          DTOutput("ranking_table")
        ),
        
        # =================================================
        # HISTOGRAM
        # =================================================
        
        tabPanel(
          
          "Distribution Analysis",
          
          plotOutput("histogram_plot")
        ),
        
        # =================================================
        # BARPLOT
        # =================================================
        
        tabPanel(
          
          "State Rankings Plot",
          
          plotOutput("ranking_plot")
        ),
        
        # =================================================
        # PIE CHART
        # =================================================
        
        tabPanel(
          
          "Category Distribution",
          
          plotOutput("pie_chart")
        ),
        
        # =================================================
        # BOXPLOT
        # =================================================
        
        tabPanel(
          
          "Category Comparison",
          
          plotOutput("boxplot_chart")
        ),
        
        # =================================================
        # SCATTER PLOT
        # =================================================
        
        tabPanel(
          
          "Correlation Analysis",
          
          plotOutput("scatter_plot")
        ),
        
        # =================================================
        # CLUSTER ANALYSIS
        # =================================================
        
        tabPanel(
          
          "Cluster Analysis",
          
          plotOutput("cluster_plot")
        ),
        
        # =================================================
        # DEVELOPMENT MAP
        # =================================================
        
        tabPanel(
          
          "Development Map",
          
          h3("State-wise Classification of Development Levels in India"),
          
          br(),
          
          img(
            src = "india_map.png",
            width = "85%"
          )
        ),
        
        # =================================================
        # REGRESSION INSIGHTS
        # =================================================
        
        tabPanel(
          
          "Regression Insights",
          
          verbatimTextOutput("regression_summary")
        ),
        
        # =================================================
        # STATE EXPLORER
        # =================================================
        
        tabPanel(
          
          "State Explorer",
          
          h2(textOutput("state_name")),
          
          br(),
          
          tableOutput("state_details")
        )
      )
    )
  )
)

# =========================================================
# SERVER
# =========================================================

server <- function(input, output) {
  
  # =======================================================
  # STATE RANKING TABLE
  # =======================================================
  
  output$ranking_table <- renderDT({
    
    datatable(
      
      df %>%
        select(
          State,
          Development_Score,
          Rank,
          Category,
          Cluster
        ),
      
      options = list(pageLength = 10)
    )
  })
  
  # =======================================================
  # HISTOGRAM
  # =======================================================
  
  output$histogram_plot <- renderPlot({
    
    ggplot(df, aes(x = Development_Score)) +
      
      geom_histogram(
        bins = 8,
        fill = "skyblue",
        color = "black"
      ) +
      
      theme_minimal() +
      
      labs(
        title = "Distribution of Development Scores",
        x = "Development Score",
        y = "Number of States"
      )
  })
  
  # =======================================================
  # STATE RANKING BARPLOT
  # =======================================================
  
  output$ranking_plot <- renderPlot({
    
    ggplot(
      df %>% arrange(Development_Score),
      
      aes(
        x = reorder(State, Development_Score),
        y = Development_Score,
        fill = Category
      )
    ) +
      
      geom_bar(stat = "identity") +
      
      coord_flip() +
      
      theme_minimal() +
      
      labs(
        title = "State-wise Development Ranking",
        x = "State",
        y = "Development Score"
      )
  })
  
  # =======================================================
  # PIE CHART
  # =======================================================
  
  output$pie_chart <- renderPlot({
    
    category_count <- df %>%
      count(Category)
    
    ggplot(
      category_count,
      aes(
        x = "",
        y = n,
        fill = Category
      )
    ) +
      
      geom_bar(
        stat = "identity",
        width = 1
      ) +
      
      coord_polar(theta = "y") +
      
      geom_text(
        aes(label = n),
        position = position_stack(vjust = 0.5)
      ) +
      
      theme_void() +
      
      labs(
        title = "Distribution of States by Category"
      )
  })
  
  # =======================================================
  # BOXPLOT
  # =======================================================
  
  output$boxplot_chart <- renderPlot({
    
    ggplot(
      df,
      aes(
        x = Category,
        y = Development_Score,
        fill = Category
      )
    ) +
      
      geom_boxplot() +
      
      theme_minimal() +
      
      labs(
        title = "Development Score Distribution by Category"
      )
  })
  
  # =======================================================
  # SCATTER PLOT
  # =======================================================
  
  output$scatter_plot <- renderPlot({
    
    ggplot(
      df,
      aes(
        x = Internet,
        y = Development_Score,
        color = Category
      )
    ) +
      
      geom_point(size = 4) +
      
      theme_minimal() +
      
      labs(
        title = "Internet Access vs Development Score",
        x = "Internet Access",
        y = "Development Score"
      )
  })
  
  # =======================================================
  # CLUSTER PLOT
  # =======================================================
  
  output$cluster_plot <- renderPlot({
    
    ggplot(
      df,
      aes(
        x = internet_norm,
        y = Development_Score,
        color = Cluster,
        label = State
      )
    ) +
      
      geom_point(size = 4) +
      
      geom_text(
        hjust = 0,
        nudge_x = 0.01,
        size = 3
      ) +
      
      theme_minimal() +
      
      labs(
        title = "K-Means Clustering of Indian States",
        x = "Normalized Internet Access",
        y = "Development Score"
      )
  })
  
  # =======================================================
  # REGRESSION SUMMARY
  # =======================================================
  
  output$regression_summary <- renderPrint({
    
    regression_model <- lm(
      Development_Score ~
        Internet +
        EUS +
        Hospitals +
        accessibility_raw,
      
      data = df
    )
    
    summary(regression_model)
  })
  
  # =======================================================
  # STATE EXPLORER
  # =======================================================
  
  selected_data <- reactive({
    
    df %>%
      filter(State == input$selected_state)
  })
  
  output$state_name <- renderText({
    
    input$selected_state
  })
  
  output$state_details <- renderTable({
    
    selected_data() %>%
      select(
        Development_Score,
        Rank,
        Category,
        Cluster,
        Internet,
        EUS,
        Hospitals
      )
  })
}

# =========================================================
# RUN APP
# =========================================================

shinyApp(ui = ui, server = server)