library(shiny)
library(shinythemes)
library(tidyverse)
library(readxl)


# Change to your path name 
file_path  = "/Users/rita/Downloads/Drug_overdose_death_rates__by_drug_type__sex__age__race__and_Hispanic_origin__United_States.xls"


df_line = read_excel(file_path, sheet = "Sheet1")
df_line2 = read_excel(file_path, sheet = "Sheet1")
df_hist <- read_excel(file_path, sheet = "Sheet2")

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
  
  titlePanel("Drug Overdose Death Rates in U.S"),
  
  sidebarLayout(
    sidebarPanel(
      div(
        sliderInput("year_range_slider_line", "Select Year Range for Plots:",
                    
                    min = min(df1_line$YEAR),
                    max = max(df1_line$YEAR),
                    value = c(min(df1_line$YEAR), max(df1_line$YEAR)),
                    sep = "",
                    step = 1
        ),
        
        style = "border-bottom: 2px solid black; padding-bottom: 10px;"  # Add a black line and padding at the bottom
      ),
      
      div(
        checkboxGroupInput("selected_races", "Select Race(s) for Histogram (Average Drug Overdose Death Rate by Race) :",
                           choices = unique(df1_hist$STUB_LABEL),
                           selected = unique(df1_hist$STUB_LABEL),
                           
        ),
        style = "border-bottom: 2px solid black; padding-bottom: 10px;" # Add padding at the top
      ),
      div(
        checkboxGroupInput("selected_age", "Select Age(s) for Line plot(Drug Overdose Death Rate by Age) ",
                           choices = unique(df2$STUB_LABEL),
                           selected = unique(df2$STUB_LABEL),
      
        ),
        style = "padding-top: 10px;"  # Add padding at the top
      )
      
    ),
    
    mainPanel(tabsetPanel(
      tabPanel("Plot for different race and gender ", plotOutput("line_plot"),plotOutput("histogram_plot")),
      tabPanel("Plot for different age", plotOutput("line_plot2"),)
      )
                
                              
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
      labs(title = paste("Drug Overdose Death Rates by gender", input$year_range_slider_line[1],'to',input$year_range_slider_line[2]),
           x = "Year",
           y = "Death rate per 100000 population",
           color = "Sex") +
      ylim(0, 35) +
      guides(color = guide_legend(reverse = TRUE)) +
      scale_x_continuous(breaks = c(1999, 2000,2001,2002,2003,2004,2005,2006,2007,2008, 2009,2010,2011,2012,2013, 2014, 2015,2016,2017,2018)) +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, size = 15, face = "bold"),
        axis.title = element_text(size = 15),
        axis.text.x = element_text(angle = 45, hjust = 1),
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

  filtered_data_3 <- reactive({
    df2 %>%
      filter(YEAR >= input$year_range_slider_line[1] & YEAR <= input$year_range_slider_line[2] &
               STUB_LABEL %in% input$selected_age)
     
  })
  
  output$line_plot2 <- renderPlot({
    df_filtered3 = filtered_data_3()
    age_group_colors <- c(
      "0-15" = "blue",
      "15-24" = "red",
      "25-34" = "green",
      "35-44" = "purple",
      "45-54" = "orange",
      "55-64" = "brown",
      "65-74" = "pink",
      "75-84" = "cyan",
      "85 and over" = "gray"
    )
    
    
    ggplot(df_filtered3, aes(x = YEAR, y = ESTIMATE, color = reorder(STUB_LABEL,desc(ESTIMATE)), group = STUB_LABEL)) +
        geom_line() +
        labs(title = paste("Drug Overdose Death Rates by Age Group", input$year_range_slider_line[1],'to',input$year_range_slider_line[2]),
          x = "Year",
          y = "Death rate per 100000 population",
          color = "Age"
        ) +
        ylim(0, 40) +
        guides(color = guide_legend(reverse = FALSE)) + 
        scale_x_continuous(breaks = c(1999, 2000,2001,2002,2003,2004,2005,2006,2007,2008, 2009,2010,2011,2012,2013, 2014, 2015,2016,2017,2018)) +
      scale_color_manual(values = age_group_colors) + 
        theme_minimal() +
        theme(
          plot.title = element_text(hjust = 0.5, size = 15, face = "bold"),
          axis.title = element_text(size = 13.5),
          axis.text = element_text(size = 12),
          axis.text.x = element_text(angle = 45, hjust = 1),
          legend.title = element_text(size = 13),
          legend.text = element_text(size = 12)
        )
    })
}



# Run the app
shinyApp(ui, server)
