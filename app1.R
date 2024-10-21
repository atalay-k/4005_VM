
library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)

# Function to generate normal distribution
generate_normal <- function(mean, sd, n) {
  scores <- rnorm(n, mean, sd)
  scores <- pmin(pmax(round(scores), 0), 100)  # Clamp between 0 and 100
  return(scores)
}

# Function to generate skewed distribution
generate_skewed <- function(mean, sd, skewness, n) {
  delta <- skewness / sqrt(1 + skewness^2)
  scale <- sd / sqrt(1 - 2 * delta^2 / pi)
  location <- mean - scale * delta * sqrt(2 / pi)
  
  scores <- rnorm(n)
  scores <- location + scale * (scores + delta * abs(scores))
  scores <- pmin(pmax(round(scores), 0), 100)  # Clamp between 0 and 100
  return(scores)
}

# UI
ui <- fluidPage(
  titlePanel("Class Score Distribution Simulator"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("mean", "Mean:", min = 0, max = 100, value = 70),
      sliderInput("sd", "Standard Deviation:", min = 1, max = 30, value = 10),
      sliderInput("skewness", "Skewness:", min = -1, max = 1, value = 0, step = 0.1),
      sliderInput("sample_size", "Sample Size:", min = 100, max = 10000, value = 1000, step = 100),
      actionButton("generate", "Generate Distributions")
    ),
    
    mainPanel(
      plotOutput("distributionPlot"),
      verbatimTextOutput("summary")
    )
  )
)

# Server
server <- function(input, output) {
  
  distributions <- reactiveVal(NULL)
  
  observeEvent(input$generate, {
    normal_scores <- generate_normal(input$mean, input$sd, input$sample_size)
    skewed_scores <- generate_skewed(input$mean, input$sd, input$skewness, input$sample_size)
    
    distributions(
      data.frame(
        Score = c(normal_scores, skewed_scores),
        Distribution = c(rep("Normal", input$sample_size), rep("Skewed", input$sample_size))
      )
    )
  })
  
  output$distributionPlot <- renderPlot({
    req(distributions())
    
    ggplot(distributions(), aes(x = Score, fill = Distribution)) +
      geom_histogram(position = "identity", alpha = 0.5, bins = 30) +
      scale_fill_manual(values = c("Normal" = "#4BC0C0", "Skewed" = "#FF6384")) +
      labs(title = "Score Distributions",
           x = "Score",
           y = "Frequency") +
      theme_minimal() +
      theme(legend.position = "top")
  })
  
  output$summary <- renderPrint({
    req(distributions())
    
    distributions() %>%
      group_by(Distribution) %>%
      summarise(
        Mean = mean(Score),
        SD = sd(Score),
        Median = median(Score),
        Min = min(Score),
        Max = max(Score)
      ) %>%
      knitr::kable(digits = 2)
  })
}

# Run the app
shinyApp(ui = ui, server = server)
