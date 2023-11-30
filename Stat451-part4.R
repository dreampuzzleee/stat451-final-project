library(shiny)
library(shinythemes)
library(tidyverse)

# Read the data
df_line <- read_csv("/Users/rita/Downloads/Drug_overdose_death_rates__by_drug_type__sex__age__race__and_Hispanic_origin__United_States.csv")
df_line2 <- read_csv("/Users/rita/Downloads/Drug_overdose_death_rates__by_drug_type__sex__age__race__and_Hispanic_origin__United_States.csv")
df_hist <- read_csv("/Users/rita/Downloads/Drug_overdose_death_rate_by_race.csv")

df2 <- df_line2 %>%
  filter(PANEL %in% c("All drug overdose deaths")) %>%
  filter(STUB_NAME %in% c("Age")) %>%
  filter(UNIT_NUM == 2)

df2 <- df2 %>%
  mutate(STUB_LABEL = gsub("85 years and over", "85 and over", STUB_LABEL),
         STUB_LABEL = gsub("Under 15 years", "0-15", STUB_LABEL),
         STUB_LABEL = gsub("15-24 years", "15-24", STUB_LABEL),
         STUB_LABEL = gsub("25-34 years", "25-34", STUB_LABEL),
         STUB_LABEL = gsub("35-44 years", "35-44", STUB_LABEL),
         STUB_LABEL = gsub("45-54 years", "45-54", STUB_LABEL),
         STUB_LABEL = gsub("55-64 years", "55-64", STUB_LABEL),
         STUB_LABEL = gsub("65-74 years", "65-74", STUB_LABEL),
         STUB_LABEL = gsub("75-84 years", "75-84", STUB_LABEL))

df1_line <- df_line %>%
  filter(PANEL %in% c("All drug overdose deaths")) %>%
  filter(STUB_NAME %in% c("Sex")) %>%
  filter(UNIT_NUM == 2)


df1_hist <- df_hist %>%
  filter(PANEL %in% c("All drug overdose deaths")) %>%
  filter(STUB_NAME %in% c("Race"))

#  UI
ui <- fluidPage(
  theme = shinytheme("flatly"),
  
  titlePanel("Drug Overdose Death Rates"),
  
  sidebarLayout(
    sidebarPanel(
      div(
        sliderInput("year_range_slider_line", "Select Year Range for Line Plot:",
                    min = min(df1_line$YEAR),
                    max = max(df1_line$YEAR),
                    value = c(min(df1_line$YEAR), max(df1_line$YEAR)),
                    sep = "",
                    step = 1
        ),
        style = "border-bottom: 2px solid black; padding-bottom: 10px;"  # Add a black line and padding at the bottom
      ),
      
      div(
        checkboxGroupInput("selected_races", "Select Race(s) for Histogram:",
                           choices = unique(df1_hist$STUB_LABEL),
                           selected = unique(df1_hist$STUB_LABEL),
                           
        ),
        style = "padding-top: 10px;"  # Add padding at the top
      )
    ),
    
    mainPanel(
      plotOutput("line_plot"),
      plotOutput("histogram_plot"),
      plotOutput("line_plot2")
      
    )
  )
)

# Server 
server <- function(input, output) {
  
  filtered_data_line <- reactive({
    df1_line %>%
      filter(YEAR >= input$year_range_slider_line[1] & YEAR <= input$year_range_slider_line[2])
  })
  
  output$line_plot <- renderPlot({
    ggplot(filtered_data_line(), aes(x = YEAR, y = ESTIMATE, color = STUB_LABEL, group = STUB_LABEL)) +
      geom_line() +
      geom_point() +
      labs(title = "Drug Overdose Death Rates",
           x = "Year",
           y = "Death rate per 100000 population",
           color = "Sex") +
      ylim(0, 35) +
      guides(color = guide_legend(reverse = TRUE)) +
      scale_x_continuous(breaks = c(1999, 2004, 2009, 2014, 2018)) +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, size = 15, face = "bold"),
        axis.title = element_text(size = 15),
        axis.text = element_text(size = 15),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 15)
      )
  })
  
  filtered_data_hist <- reactive({
    df1_hist %>%
      filter(YEAR >= input$year_range_slider_line[1] & YEAR <= input$year_range_slider_line[2] &
               STUB_LABEL %in% input$selected_races)
  })
  
  output$histogram_plot <- renderPlot({
    df_filtered <- filtered_data_hist()
    
    df_avg <- df_filtered %>%
      group_by(STUB_LABEL) %>%
      summarise(avg_death_rate = mean(ESTIMATE))
    
    df_avg$STUB_LABEL <- gsub("^([[:alnum:]]+ [[:alnum:]]+) ", "\\1\n", df_avg$STUB_LABEL)
    
    ggplot(df_avg, aes(x = avg_death_rate, y = STUB_LABEL, fill = STUB_LABEL)) +
      geom_bar(stat = "identity") +
      labs(
        title = paste("Average Drug Overdose Death Rates by Race in", input$year_range_slider_line[1],'to',input$year_range_slider_line[2]),
        x = "Average Deaths per 100,000 resident population",
        y = "Race"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, size = 15, face = "bold"),
        axis.title = element_text(size = 15),
        axis.text = element_text(size = 15, color = "black") 
      ) + 
      guides(fill = FALSE) +
      coord_flip()
  })
  
  filtered_data_line <- reactive({
    df1_line %>%
      filter(YEAR >= input$year_range_slider_line[1] & YEAR <= input$year_range_slider_line[2])
  })
  
  
  filtered_data_3 <- reactive({
    df2 %>%
      filter(YEAR >= input$year_range_slider_line[1] & YEAR <= input$year_range_slider_line[2])
  })
  
  output$line_plot2 <- renderPlot({
    df_filtered3 = filtered_data_3()
    
    
    ggplot(df_filtered3, aes(x = YEAR, y = ESTIMATE, color = STUB_LABEL, group = STUB_LABEL)) +
        geom_line() +
        labs(
          title = "What are the trends in drug overdose death rates
          in the United States among different age?",
          x = "Year",
          y = "Death rate per 100000 population",
          color = "Age"
        ) +
        ylim(0, 40) +
        guides(color = guide_legend(reverse = FALSE)) + scale_x_continuous(breaks = c(1999, 2004, 2009, 2014, 2018)) +
        theme_minimal() +
        theme(
          plot.title = element_text(hjust = 0.5, size = 15, face = "bold"),
          axis.title = element_text(size = 13.5),
          axis.text = element_text(size = 12),
          legend.title = element_text(size = 13),
          legend.text = element_text(size = 12)
        )
    })
}

# Run the app
shinyApp(ui, server)
