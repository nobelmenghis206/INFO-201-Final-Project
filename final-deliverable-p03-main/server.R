library(shiny)
library(dplyr)
library(billboarder)
library(ggplot2)
library(plotly)
library(tidyr)
library(readr)

data <- read.csv("Agrofood_co2_emission.csv")

server <- function(input, output) {
  
  #Page 2 - Bar Chart code
  exclude_columns <- c(26, 27, 28, 29, 30, 31)
  filtered_data <- data %>%
    select(-exclude_columns)
  
  ui <- fluidPage(
    titlePanel("CO2 Emissions Analysis"),
    
    tabsetPanel(
      tabPanel("Top Emission Areas by Facet", 
               sidebarLayout(
                 sidebarPanel(
                   selectInput("facet", "Select Facet:",
                               choices = colnames(filtered_data)[-c(1, 2)],
                               selected = "Savanna Fires"),
                   
                   selectInput("year", "Select Year:",
                               choices = unique(filtered_data$Year),
                               selected = 2020)
                 ),
                 mainPanel(
                   plotOutput("top_areas_chart")
                 )
               )),
      tabPanel("Top 10 Emitters by Year",
               sidebarLayout(
                 sidebarPanel(
                   selectInput("chart_year", "Select Year:",
                               choices = unique(filtered_data$Year),
                               selected = 2020)
                 ),
                 mainPanel(
                   plotlyOutput("top_emitters_chart")
                 )
               ))
    )
  )
  
  output$top_areas_chart <- renderPlotly({
    selected_facet <- input$facet
    selected_year <- input$year1
    
    chart_data <- filtered_data %>%
      filter(Year == selected_year) %>%
      select(Area, !!sym(selected_facet)) %>%
      arrange(desc(!!sym(selected_facet))) %>% 
      head(10)
    
    top_areas <- ggplot(chart_data, aes(x = reorder(Area, !!sym(selected_facet)),
                                        y = !!sym(selected_facet))) +
      geom_bar(stat = "identity", fill = "dodgerblue") +
      labs(
        x = "Area",
        y = "CO2 Emissions",
        title = paste("CO2 Emissions by", selected_facet, "in", selected_year),
        caption = "This widget helps create an understanding of the top emissions producers for each facet by year"
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    top_areas_plotly <- ggplotly(top_areas)
    return(top_areas_plotly)
  })
  
  output$top_emitters_chart <- renderPlotly({
    selected_chart_year <- input$chart_year
    
    top_emitters_data <- filtered_data %>%
      filter(Year == selected_chart_year) %>%
      pivot_longer(cols = colnames(filtered_data)[-(1:2)],
                   names_to = "Facet",
                   values_to = "Emissions") %>%
      group_by(Facet, Year) %>%
      summarise(total_emissions = sum(Emissions)) %>%
      arrange(desc(total_emissions)) %>%
      group_by(Year) %>%
      top_n(10) %>%
      arrange(desc(total_emissions))
    
    colors <- c("blue", "green", "red", "orange", "purple", "pink", "brown",
                "cyan", "magenta", "yellow")
    
    emitters_plot <- plot_ly(data = top_emitters_data, x = ~Facet, y = ~total_emissions,
                             type = "bar", text = ~paste("Facet: ", Facet, "<br>Total Emissions: ", total_emissions),
                             marker = list(color = colors)) %>%
      layout(
        xaxis = list(title = "Facet"),
        yaxis = list(title = "Total Emissions"),
        title = "Top 10 Emission Producers by Facet for Each Year",
        showlegend = FALSE
      )
    
    top_emitters_plotly <- ggplotly(emitters_plot)
    return(top_emitters_plotly)
  })
  
  
  #Page 3 - Pie Chart code 
   output$pieChart <- renderUI({
    selected_country <- input$country
    
    accurate_values <- data %>%
      filter(Area == selected_country &
               (`Forest.fires` > 0 |`Food.Household.Consumption` > 0 |`Food.Transport` > 0 |`Food.Retail` > 0 | `Food.Packaging` > 0 | `Food.Processing` > 0)) %>%
      summarise(
        `Forest fires` = sum(`Forest.fires`),
        `Food Retail` = sum(`Food.Retail`),
        `Food Packaging` = sum(`Food.Packaging`),
        `Food Processing` = sum(`Food.Processing`),
        `Food Transport` = sum(`Food.Transport`),
        `Food Household Consumption` = sum(`Food.Household.Consumption`)
      )
    
    categories <- colnames(accurate_values)[-1]
    values <- unlist(accurate_values[-1])
    
    if (length(categories) > 0 && length(values) > 0) {
      chart_width <- 900
      chart_height <- 900
      
      chart <- billboarder() %>% 
        bb_piechart(data.frame(categories, values), width = chart_width, height = chart_height) %>% 
        bb_legend(position = 'right')
      
      country_heading <- h3(paste("CO2 Emissions in", selected_country))
      
      return(list(country_heading, chart))
    } else {
      return(NULL)
    }
  })
  
   # Chart 1 total emissions
   output$emissions_plotly <- renderPlotly ({
     min_year <- input$year_select[1]
     max_year <- input$year_select[2]
     
     selected_df <- data %>% select(Area, Year, total_emission) %>%
       filter(Area %in% input$area_select,
              Year >= min_year, Year <= max_year)
     
     emissions_plot <- ggplot(selected_df) +
       geom_line(aes(x = Year, y = total_emission, color = Area)) +
       ggtitle(paste("Total Emissions by Year \n Between", min_year, "and",
              max_year)) + labs(x = "Year",
                                y = "Total Emissions in Kilotons") +
       theme(plot.margin = margin(20, 20, 20, 20, unit = "pt"))
             
     emissions_plotly <- ggplotly(emissions_plot)
     return(emissions_plotly)
   })
   
 
   #Summary and Conclusion code
   data_2019 <- filter(data, Year == '2019')
  
  year_area <- mutate(data_2019, area_year = paste(Area, ", ", Year, sep = ""))
  highest_emissions <- pull(arrange(year_area, desc(total_emission)), area_year)[1]
  second_emissions <- pull(arrange(year_area, desc(total_emission)), area_year)[3]
  third_emissions <- pull(arrange(year_area, desc(total_emission)), area_year)[4]


  sums_2019 <- summarise(data_2019,
                         across(c(`Crop.Residues`, `Food.Retail`,
                                  `Food.Processing`, `Rice.Cultivation`,
                                  `Food.Packaging`, `Fertilizers.Manufacturing`,
                                  `Agrifood.Systems.Waste.Disposal`),
                                sum, na.rm = TRUE))
  highest_sector <- names(sums_2019)[which.max(sums_2019)]
  
  by_year <- group_by(data, Year)
  sum_by_year <- summarise(by_year, 
                           total_emissions_sum = sum(total_emission, 
                                                     na.rm = TRUE))
  highest_emission_year <- filter(sum_by_year, 
                                  total_emissions_sum == 
                                    max(total_emissions_sum))$Year
  
  summary_stats <- data.frame(
    question = c("Highest Emissions (Area, Year)",
                 "Second Highest Emissions",
                 "Third Highest Emissions",
                 "Highest Emissions Sector Worldwide (2019)",
                 "Highest Emissions Year"),
    values = c(highest_emissions, second_emissions,
               third_emissions, highest_sector,
               highest_emission_year)
  )
  
  output$summary_table <- renderTable({
    summary_stats
  })
}

