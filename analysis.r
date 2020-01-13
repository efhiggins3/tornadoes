library(ggplot2)
library(dplyr)
library(tidyverse)
library(ggmap)
library(RMySQL)

# Start Dremel:
conn <- dbConnect(MySQL()) 

# This weather_tornadoes data is pulled from the NOAA storm events dataset (https://www.ncdc.noaa.gov/stormevents/faq.jsp)

mysql_query <- """
    SELECT
      t.national_id AS national_id,
      MIN(date + " " + time) AS start_date_time,
    # Start
      MAX(IF(segment_num = 1, state, "")) AS origin_state,
      MAX(IF(segment_num = 1, latitude_start, -1000.0)) AS latitude_start,
      MAX(IF(segment_num = 1, longitude_start, -1000.0)) AS longitude_start,
    # End
      MAX(IF(segment_num = ms.last_segment_num, latitude_end, -1000.0)) AS latitude_end,
      MAX(IF(segment_num = ms.last_segment_num, longitude_end, -1000.0)) AS longitude_end,
    # Other data
      MAX(num_states_impacted) AS num_of_states_impacted,
      SUM(damage_cost) AS damage_cost,
      SUM(fatalities) AS fatalities,
      SUM(injuries) AS injuries,
      MAX(magnitude) AS max_magnitude,
      SUM(miles_traveled) AS miles_traveled
    FROM
      weather_tornadoes t
      LEFT JOIN
        (SELECT
           national_id, MAX(segment_num) AS last_segment_num
         FROM weather_tornadoes
         GROUP BY 1
         ) ms ON
      (t.national_id = ms.national_id)
    GROUP BY 1
"""

tornado_data2 <- dbSendQuery(conn, mysql_query)

# National ID 60 is incorrect (tornado from California to Georgia)
twisters <- tornado_data2 %>%
            filter(latitude_start > 20.0 & longitude_start <= -50.0 & longitude_start >= -130.0 &
                   latitude_end > 20.0 & longitude_end <= -50.0 & longitude_start >= -130.0 &
                   national_id != 60) %>%
            mutate(south = latitude_start >= latitude_end,
                   east = longitude_start <= longitude_end)

# Longitudes are negative, so this tornado is moving NORTH and EAST
# twisters %>% filter(national_id == 15886)
#  national_id    start_date_time origin_state latitude_start longitude_start latitude_end longitude_end
#1       15886 2014-06-17 9:57:00           MI        44.3722        -83.8247      44.3787      -83.7941


# Stage start & finish data for ggmap grouping
sta <- twisters %>%
       select(national_id, latitude_start, longitude_start, south, east) %>%
       rename(latitude = latitude_start, longitude = longitude_start)

fin <- twisters %>%
       select(national_id, latitude_end, longitude_end, south, east) %>%
       rename(latitude = latitude_end, longitude = longitude_end)

# Build final dataset
tornado_df <- union(sta, fin) %>%
              add_count(national_id) %>%
              filter(n == 2) %>%
              arrange(national_id)

# Build subset directional data
nw_tor <- tornado_df %>% filter(south == FALSE & east == FALSE)
ne_tor <- tornado_df %>% filter(south == FALSE & east == TRUE)
se_tor <- tornado_df %>% filter(south == TRUE & east == TRUE)
sw_tor <- tornado_df %>% filter(south == TRUE & east == FALSE)

# Plot data
states <- map_data("state")
# Function to generate US map with tornado lines plotted
plot_map <- function(data_set, line_color, chart_title) {
               ggplot(data = states) +
               geom_polygon(
                 aes(x = long, y = lat, fill = region, group = group),
                 color = "white",
                 fill = "grey"
               ) +
               coord_fixed(1.3) +
               guides(fill=FALSE) +
               geom_path(
                 aes(x = longitude,
                     y = latitude,
                     group = national_id),
                 data = data_set,
                 color = line_color
               ) +
               labs(title = chart_title)
            }

# Generate all plots:
all_plot <- plot_map(tornado_df, "red", "US Tornadoes, 2000-present")
nw_plot  <- plot_map(nw_tor, "green", "Moving Northwest")
ne_plot  <- plot_map(ne_tor, "red", "Moving Northeast")
se_plot  <- plot_map(se_tor, "blue", "Moving Southeast")
sw_plot  <- plot_map(sw_tor, "yellow", "Moving Southwest")

# Render plots
all_plot
nw_plot
ne_plot
sw_plot
se_plot
