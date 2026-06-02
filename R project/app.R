# Load libraries
library(shiny)
library(DT)
library(DBI)
library(RSQLite)
library(ggplot2)
library(dplyr)

# Connect to database and load data
con <- dbConnect(SQLite(), "development_analysis.db")
df <- dbReadTable(con, "state_development_data")

# UI
ui <- fluidPage(
  titlePanel("State-wise Development Analysis System"),
  sidebarLayout(
    sidebarPanel(
      selectInput("selected_state", "Select State:", choices = sort(df$State))
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Overview",
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

        tabPanel("State Rankings",
          DTOutput("ranking_table")
        ),

        tabPanel("Distribution Analysis",
          plotOutput("histogram_plot")
        ),

        tabPanel("State Rankings Plot",
          plotOutput("ranking_plot")
        ),

        tabPanel("Category Distribution",
          plotOutput("pie_chart")
        ),

        tabPanel("Category Comparison",
          plotOutput("boxplot_chart")
        ),

        tabPanel("Correlation Analysis",
          plotOutput("scatter_plot")
        ),

        tabPanel("Cluster Analysis",
          plotOutput("cluster_plot")
        ),

        tabPanel("Elbow Method",
          plotOutput("elbow_plot")
        ),

        tabPanel("Development Map",
          h3("State-wise Classification of Development Levels in India"),
          br(),
          img(src = "india_map.png", width = "85%")
        ),

        tabPanel("Regression Insights",
          verbatimTextOutput("regression_summary")
        ),

        tabPanel("State Explorer",
          h2(textOutput("state_name")),
          br(),
          tableOutput("state_details")
        )
      )
    )
  )
)

# Server
server <- function(input, output) {

  # State ranking table
  output$ranking_table <- renderDT({
    datatable(
      df %>% select(State, Development_Score, Rank, Category, Cluster),
      options = list(pageLength = 10)
    )
  })

  # Histogram
  output$histogram_plot <- renderPlot({
    ggplot(df, aes(x = Development_Score)) +
      geom_histogram(bins = 8, fill = "skyblue", color = "black") +
      theme_minimal() +
      labs(title = "Distribution of Development Scores",
           x = "Development Score", y = "Number of States")
  })

  # State ranking barplot
  output$ranking_plot <- renderPlot({
    ggplot(df %>% arrange(Development_Score),
           aes(x = reorder(State, Development_Score), y = Development_Score, fill = Category)) +
      geom_bar(stat = "identity") +
      coord_flip() +
      theme_minimal() +
      labs(title = "State-wise Development Ranking", x = "State", y = "Development Score")
  })

  # Pie chart
  output$pie_chart <- renderPlot({
    category_count <- df %>% count(Category)
    ggplot(category_count, aes(x = "", y = n, fill = Category)) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar(theta = "y") +
      geom_text(aes(label = n), position = position_stack(vjust = 0.5)) +
      theme_void() +
      labs(title = "Distribution of States by Category")
  })

  # Boxplot
  output$boxplot_chart <- renderPlot({
    ggplot(df, aes(x = Category, y = Development_Score, fill = Category)) +
      geom_boxplot() +
      theme_minimal() +
      labs(title = "Development Score Distribution by Category")
  })

  # Scatter plot
  output$scatter_plot <- renderPlot({
    ggplot(df, aes(x = Internet, y = Development_Score, color = Category)) +
      geom_point(size = 4) +
      theme_minimal() +
      labs(title = "Internet Access vs Development Score",
           x = "Internet Access", y = "Development Score")
  })

  # Cluster plot
  output$cluster_plot <- renderPlot({
    ggplot(df, aes(x = internet_norm, y = Development_Score, color = Cluster, label = State)) +
      geom_point(size = 4) +
      geom_text(hjust = 0, nudge_x = 0.01, size = 3) +
      theme_minimal() +
      labs(title = "K-Means Clustering of Indian States",
           x = "Normalized Internet Access", y = "Development Score")
  })

  # Elbow method plot
  output$elbow_plot <- renderPlot({
    cluster_data <- df %>% select(internet_norm, eus_norm, hospital_norm, accessibility_norm)
    cluster_data <- na.omit(cluster_data)

    set.seed(123)
    wcss <- sapply(1:10, function(k) {
      kmeans(cluster_data, centers = k, nstart = 10)$tot.withinss
    })

    elbow_df <- data.frame(k = 1:10, WCSS = wcss)
    ggplot(elbow_df, aes(x = k, y = WCSS)) +
      geom_line(color = "steelblue", size = 1) +
      geom_point(color = "steelblue", size = 3) +
      scale_x_continuous(breaks = 1:10) +
      theme_minimal() +
      labs(title = "Elbow Method for Optimal Number of Clusters",
           x = "Number of Clusters (K)", y = "Total Within Sum of Squares")
  })

  # Regression summary
  output$regression_summary <- renderPrint({
    model <- lm(Development_Score ~ Internet + EUS + Hospitals + accessibility_raw, data = df)
    summary(model)
  })

  # State explorer
  selected_data <- reactive({
    df %>% filter(State == input$selected_state)
  })

  output$state_name <- renderText({ input$selected_state })

  output$state_details <- renderTable({
    selected_data() %>%
      select(Development_Score, Rank, Category, Cluster, Internet, EUS, Hospitals)
  })
}

shinyApp(ui = ui, server = server)