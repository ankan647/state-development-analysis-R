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

        tabPanel("Correlation Heatmap",
          plotOutput("heatmap_plot", height = "550px")
        ),

        tabPanel("Bubble Chart",
          plotOutput("bubble_plot", height = "550px")
        ),

        tabPanel("Score Decomposition",
          plotOutput("decomposition_plot", height = "650px")
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

  # Correlation heatmap
  output$heatmap_plot <- renderPlot({
    numeric_data <- df %>% select(Internet, EUS, Rural, Urban, Hospitals, Development_Score)
    cor_matrix <- cor(numeric_data, use = "complete.obs")
    cor_df <- as.data.frame(as.table(cor_matrix))
    names(cor_df) <- c("Var1", "Var2", "Correlation")

    ggplot(cor_df, aes(x = Var1, y = Var2, fill = Correlation)) +
      geom_tile(color = "white", linewidth = 1) +
      geom_text(aes(label = round(Correlation, 2)), size = 4.5, color = "black") +
      scale_fill_gradient2(low = "#d73027", mid = "white", high = "#1a9850", midpoint = 0, limits = c(-1, 1)) +
      theme_minimal() +
      labs(title = "Correlation Matrix of Development Indicators", x = "", y = "") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 11),
            axis.text.y = element_text(size = 11))
  })

  # Bubble chart
  output$bubble_plot <- renderPlot({
    ggplot(df, aes(x = Internet, y = EUS, size = Hospitals, color = Category, label = State)) +
      geom_point(alpha = 0.7) +
      geom_text(size = 2.5, vjust = -1.5, show.legend = FALSE) +
      scale_size_continuous(range = c(3, 15), name = "Hospitals") +
      theme_minimal() +
      labs(title = "Multi-dimensional State Comparison",
           subtitle = "Bubble size represents number of hospitals",
           x = "Internet Access (%)", y = "Unemployment Rate (EUS)",
           color = "Category")
  })

  # Score decomposition (dumbbell chart)
  output$decomposition_plot <- renderPlot({
    plot_df <- df %>%
      arrange(Development_Score) %>%
      mutate(State = factor(State, levels = State))

    ggplot(plot_df) +
      geom_segment(aes(x = State, xend = State, y = base_score, yend = Development_Score),
                   color = "grey60", linewidth = 0.8) +
      geom_point(aes(x = State, y = base_score, color = "Base Score"), size = 3) +
      geom_point(aes(x = State, y = Development_Score, color = "Final Score"), size = 3) +
      coord_flip() +
      theme_minimal() +
      scale_color_manual(values = c("Base Score" = "#2196F3", "Final Score" = "#E91E63")) +
      labs(title = "Score Decomposition: Impact of Standard Deviation Penalty",
           subtitle = "Gap between dots shows the penalty applied for uneven development",
           x = "", y = "Score", color = "")
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