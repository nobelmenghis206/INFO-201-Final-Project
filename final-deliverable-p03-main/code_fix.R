library(ggplot2)
library(plotly)
library(dplyr)

min_year <- 1990
max_year <- 2020
selected_area <- "United States of America"

selected_df <- data %>%
  select(Area, Year, total_emission) %>%
  filter(Area == selected_area,
         Year >= min_year, Year <= max_year)

emissions_plot <- ggplot(selected_df) +
  geom_line(aes(x = Year, y = total_emission, color = Area)) +
  ggtitle(paste("Total Emissions by Year \n Between", min_year, "and", max_year)) +
  labs(x = "Year", y = "Total Emissions in Kilotons") +
  theme(plot.margin = margin(20, 20, 20, 20, unit = "pt"))

emissions_plotly <- ggplotly(emissions_plot)
print(emissions_plotly)

