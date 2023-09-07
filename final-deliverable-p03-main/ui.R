library(shiny)
library(dplyr)
library(shinythemes)
library(billboarder)
library(readr)

primary_color <- "#000000"
secondary_color <- "#000000"  
background_color <- "#222222"
text_color <- "#FFFFFF"   
introduction <- tabPanel("Introduction",
                         h1("Introduction"),
                         p("As the population of our planet continues to grow 
                          year by year, the importance of food production within 
                          the agri-food sector becomes more vital. The increased 
                          scale of food production coupled with a greater 
                          reliance on technology to increase efficiency has led 
                          to the agri-food sector to make up a sizeable portion 
                          of total emissions worldwide. The carbon footprint 
                          left by human production is leading to global warming, 
                          which stands as one of the greatest threats to our 
                          planet and living animals, including ourselves. By 
                          analyzing the emissions created by our food 
                          manufacturing, we can better understand where a large 
                          portion of the pollution is coming from and will allow 
                          us to address the issues better. Reducing emissions in 
                          every sector could greatly help with global warming 
                          and identifying the highest pollution sectors of the 
                          agri-food industry as well as countries that 
                         contribute the most is the first step in this 
                            goal."),
                         img(src = "farm_emissions_sources.png", 
                             width = "600px", height = "400px"),
                         h2("Questions"),
                         p("Which countries produce the most overall pollution 
                           in the agri-food sector?"),
                         p("Which facite of the industry produces the most 
                           pollution?"),
                         p("How does the total pollution from each sector 
                           change by country?"),
                         h2("Dataset"),
                         p("In order to answer our research questions, we used 
                           data that included data from around the world that 
                           includes a wide range of measurements regarding 
                           emissions throughout the agri-food sector. This 
                           dataset came from kaggle and is comprised of data 
                           collected by the Food and Agriculture Organization 
                           (FAO) and data from IPCC. The data contains 
                           information on nearly every country in the world and 
                           covers a wide variety of factors that lead to the 
                           overall carbon footprint of the country's 
                           agriculture. Information that is contained within 
                           the data set includes emissions from food processing,
                           on-farm energy use, fertilizer manufacturing, 
                           food transport, rice cultivation, and several more. 
                           Some of the data is based on estimation as tracking 
                           all of the emissions in every country would not be 
                           possible."),
                         h2("Ethical Questions and Limitations"),
                         p("The data we found on Kaggle may have some issues 
                         with inconsistencies, which could make our findings 
                         less reliable such as missing information, and 
                         esitmation errors. The dataset could be biased towards 
                         specific countries with better emission tracking, 
                         which could affect the accuracy of our analysis. 
                         Understanding CO2 emissions across countries is 
                         complicated due to various factors like agricultural 
                         practices and economic development and some countries 
                         might have better resources and equiment to track 
                         these emissions then other which can cause problems. 
                         We also need to consider ethical concerns about data 
                         privacy and usage. Many farms are privatly owned but 
                         all contribute to the emissions. As such, it is 
                         possible that not all farms have agreed to be within 
                         the data set."))


page_1 <- tabPanel("Line Graph - Total Emissions",
                   h1("Total Emissions by Area"),
                   p("We chose a line graph to observe the countries that
                     produce the most overall polution in the agri-food
                     sector by measure of Kilotons (1 kt represents 1000 kg of 
                     CO2). Plotting kiloton levels of CO2 over time helps
                     us see clear trends and patterns in pollution production.
                     Through this visual we can easily compare countries
                     total emissions and identify which countries consistently
                     produce the most CO2 throughout the years. This graph
                     also highlights potential outliers or fluctuation in
                     pollution production which can show countries to observe,
                     and countries to recognize efforts in reducing CO2
                     emissions. This chart can be interacted through selecting
                     different countries to observe and choosing time frames in 
                     years too see how CO2 emissions have changed."),
                   sidebarLayout(
                     sidebarPanel(
                    
                       selectInput(inputId = "area_select",
                                   label = "Select Areas",
                                   choices = data$Area,
                                   selected = "United States of America",
                                   multiple = TRUE),
                      
                       sliderInput("year_select",
                                   label = "Select Years",
                                   min = 1990,
                                   max = 2020,
                                   step = 1,
                                   value = c(1990, 2020),
                                   sep = "")
                     ),
                     mainPanel(
                       plotlyOutput(outputId = "emissions_plotly")
                     )
                   )
)

page_2 <- tabPanel("Bar Chart - Facets",
                   h1("CO2 Emissions by Industry Facet"),
                   p("We chose to create a bar chart to aid in illustrating the 
                   distribution of CO2 emmissions among facets of the 
                   Agro-industry. This visualization provides a comprehensive 
                     view of how various facets contribute to CO2 emissions over
                     time by each year and allows for easy comparison of 
                     emission patterns between different years and facets. 
                     It helps in identifying trends, anomalies, and potential 
                     areas for mitigation strategies to reduce emissions in the 
                     agro-food sector. A key insight from this chart is the 
                     fact that the top five facets remain a heavy constant for 
                     the 30 years of data provided."),
                   
                     ui <- fluidPage(
                     titlePanel("CO2 Emissions Analysis"),
                     
                     tabsetPanel(
                       tabPanel("Top Emission Areas by Facet", 
                                sidebarLayout(
                                  sidebarPanel(
                                    selectInput("facet", "Select Facet:",
                                                choices = colnames(data)[-c(1, 2)]),
                                    
                                    selectInput("year1", "Select Year:",
                                                choices = unique(data$Year)),
                                  ),
                                  mainPanel(
                                    plotlyOutput(outputId = "top_areas_chart")
                                  ),
                                )),
                       tabPanel("Top 10 Emitters by Year",
                                sidebarLayout(
                                  sidebarPanel(
                                    selectInput("chart_year", "Select Year:",
                                                choices = unique(data$Year))
                                  ),
                                  mainPanel(
                                    plotlyOutput(outputId = "top_emitters_chart")
                                  )
                                )
                       )
                   ))
)

                   

                   


page_3 <- tabPanel("Pie Chart - CO2 Emissions",
                   h1("CO2 Emissions by Country"),
                   p("We chose a pie chart as a visual tool to effectively 
                   illustrate the distribution of CO2 emissions attributed 
                   to food-related issues across various countries.
                      The insights from the visual pie chart gives us vaulable 
                      understanding of the CO2 emissions landscape. It has 
                      become evident that the distribution of total CO2 
                      emissions among different food sectors varies 
                      significantly from one country to another.Through this 
                      crafted pie chart, we wanted to provide a visually 
                      informative representation of diverse countries and their 
                      respective contributions to CO2 emissions, To explore 
                      further, you can simply select a country of interest and 
                      observe how the pie chart delineates the intricate 
                     relationship between CO2 emissions and food-related 
                     sectors, complete with precise percentage breakdowns."),
                   
                   selectInput("country", "Select a Country",
                               choices = unique(data$Area)),
                   uiOutput("pieChart") 
)
conclusion <- tabPanel("Conclusion",
                       fluidRow(
                         column(8,
                                h1("Conclusion"),
                                p("After analyzing the data for this project, we 
                                found some key takeaways. Firstly, a large 
                                amount of the total emissions come from a 
                                relatively small amount of countries. Many small 
                                countries or countries with low populations 
                                hardly contribute to total emissions with many 
                                being considered to produce negative amounts of 
                                pollution because the vast amounts of forested 
                                area takes the carbon dioxide out of the air. 
                                While these large countries have many more 
                                people to feed, a reduction in overall emissions 
                                could greatly lower the overall carbon footprint 
                                of the agriculture of the world. Secondly, we 
                                see that waste disposal was the leading factor 
                                in emissions worldwide. Similarly to the first 
                                takeaway, large countries such as China and the 
                                US have a much higher percentage of their total 
                                emissions coming from waste disposal when 
                                compared to smaller countries. These issues are 
                                a large area for improvement for these countries
                                as cutting down on waste is an easier focus than
                                some other sectors and would greatly contribute 
                                  to the overall emissions. The final takeaway 
                                  is that emissions were at an all-time high in 
                                  2019. The most important insight from our data 
                                  is that serious steps need to be taken in 
                                  order to reduce emissions and the first step 
                                  must be stopping the increase year over year. 
                                  The agri-food sector compared with other 
                                  major factors in global warming is not 
                                  increasing as much, taking up a smaller 
                                  percentage of overall emissions every year 
                                  while still increasing year over year. 
                                  More drastic changes must be made in the 
                                  coming decade in the agricultural and other 
                                  fields or greater irreversible damage will be 
                                  done to the planet."),
                                column(8,
                                       h2("Summary Statistics"),
                                       tableOutput("summary_table")
                                )
                         )
                         
                       )
)





ui <- navbarPage("Agri-food Impacts on Climate Change",
                 introduction,
                 page_1,
                 page_2,
                 page_3,
                 conclusion,
                 theme = shinytheme("flatly"),
                 
                 tags$head(
                   tags$style(HTML(
                     sprintf(
                       "
        /* Custom color scheme */
        .navbar { background-color: %s; }
        .navbar-default .navbar-nav > li > a { color: %s; }
        /* Add more custom styles here for various elements */
        ",
                       primary_color, text_color
                     )
                   ))
                 ))

