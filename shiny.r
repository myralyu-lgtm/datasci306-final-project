 
#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(maps)


usa_map <- map_data("state")

unemployment <- read_csv(
  "unemployment_yearly_2011_2022.csv",
  show_col_types = FALSE
)

unemp_long <- unemployment |>
  pivot_longer(
    cols = -year,
    names_to  = "code",
    values_to = "unemployment_rate"
  ) |>
  mutate(
    state_abbr = substr(code, 1, 2)
  )

lookup <- tibble(
  state_abbr = state.abb,
  region     = tolower(state.name)
)

unemp_long <- unemp_long |>
  inner_join(lookup, by = "state_abbr")

states_map <- map_data("state")
state_choices <- sort(unique(unemp_long$state_abbr))
all_years     <- sort(unique(unemp_long$year))

migration_raw <- read_csv(
  "migration_flows_combined_2011-2022.csv",
  show_col_types = FALSE
) %>%
  mutate(
    n_returns    = as.numeric(n_returns),
    n_exemptions = as.numeric(n_exemptions),
    total_agi = as.numeric(total_agi)
  ) %>%
  # use only outflow rows
  filter(flow_type == "outflow")

fips_lut <- tibble(
  fips = c(
    "01","02","04","05","06","08","09","10","11","12",
    "13","15","16","17","18","19","20","21","22","23",
    "24","25","26","27","28","29","30","31","32","33",
    "34","35","36","37","38","39","40","41","42","44",
    "45","46","47","48","49","50","51","53","54","55","56"
  ),
  state_abb = c(
    "AL","AK","AZ","AR","CA","CO","CT","DE","DC","FL",
    "GA","HI","ID","IL","IN","IA","KS","KY","LA","ME",
    "MD","MA","MI","MN","MS","MO","MT","NE","NV","NH",
    "NJ","NM","NY","NC","ND","OH","OK","OR","PA","RI",
    "SC","SD","TN","TX","UT","VT","VA","WA","WV","WI","WY"
  )
)

migration_flows <- migration_raw %>%
  left_join(fips_lut, by = c("origin_fips" = "fips")) %>%
  rename(origin_abb = state_abb) %>%
  left_join(fips_lut, by = c("dest_fips" = "fips")) %>%
  rename(dest_abb = state_abb) %>%
  filter(origin_abb %in% state.abb,
         dest_abb   %in% state.abb)

state_centers <- tibble(
  state_abb  = state.abb,
  state_name = state.name,
  lon        = state.center$x,
  lat        = state.center$y
)

migration_geo <- migration_flows %>%
  left_join(state_centers, by = c("origin_abb" = "state_abb")) %>%
  rename(orig_lon = lon, orig_lat = lat) %>%
  left_join(state_centers, by = c("dest_abb" = "state_abb")) %>%
  rename(dest_lon = lon, dest_lat = lat) %>%
  mutate(
    flow_period = factor(flow_period, levels = sort(unique(flow_period)))
  ) %>%
  filter(origin_abb != dest_abb)

max_agi <- as.numeric(quantile(migration_geo$total_agi, 0.99, na.rm = TRUE))

year_start <- 2011
year_end   <- 2022

year_pairs <- paste(
  year_start:(year_end - 1),
  (year_start + 1):year_end,
  sep = "–"
)

year_keys <- paste(
  year_start:(year_end - 1),
  (year_start + 1):year_end,
  sep = "_"
)

year_choices <- setNames(year_keys, year_pairs)

ui <- navbarPage(
  "US Migration & Unemployment",
  
  tabPanel(
    "Migration flows",
    fluidPage(
      titlePanel("US State-to-State Migration of Tax Returns (Households)"),
      
      sidebarLayout(
        sidebarPanel(
          width = 4,
          
          selectInput(
            "flow_period", "Migration year:",
            choices  = levels(migration_geo$flow_period),
            selected = levels(migration_geo$flow_period)[1]
          ),
          
          sliderInput(
            "agi_range",
            "Adjusted Gross Income (AGI) range:",
            min   = 0,
            max   = max_agi,
            value = c(round(max_agi * 0.1), round(max_agi * 0.9)),  # Default shows middle 80%
            step  = max(1, round(max_agi / 1000)),
            pre = "$",  # Adds dollar sign
            sep = ",",   # Adds comma thousands separator
            ticks = FALSE
          ),
          
          selectInput(
            "origin_state", "Origin state (from):",
            choices  = c("All", sort(unique(migration_geo$origin_abb))),
            selected = "All"
          ),
          selectInput(
            "dest_state", "Destination state (to):",
            choices  = c("All", sort(unique(migration_geo$dest_abb))),
            selected = "All"
          ),
          
          br(),
          h4("Top destinations by AGI (within selected range)"),
          tableOutput("top_dest"),
          br(),
          h4("Top origins by AGI"),
          tableOutput("top_origin")
        ),
        
        mainPanel(
          width = 8,
          plotOutput("flow_map", height = "650px")
        )
      )
    )
  ),
  
  tabPanel(
    "Unemployment",
    fluidPage(
      titlePanel("US Unemployment Rate by State (2011–2022)"),
      
      sidebarLayout(
        sidebarPanel(
          selectInput(
            "year_range",
            "Select year pair:",
            choices  = year_choices,
            selected = year_keys[1]
          ),
          selectInput(
            "state",
            "Select state (abbreviation):",
            choices  = state_choices,
            selected = "CA"
          )
        ),
        mainPanel(
          plotOutput("unemp_map", height = "550px"),
          tags$hr(),
          plotOutput("state_ts", height = "300px")
        )
      )
    )
  )
)


server <- function(input, output, session) {
  filtered_flows <- reactive({
    out <- migration_geo %>%
      filter(
        flow_period == input$flow_period,
        total_agi  >= input$agi_range[1] & total_agi <= input$agi_range[2]  
      )
    
    if (input$origin_state != "All") {
      out <- out %>% filter(origin_abb == input$origin_state)
    }
    if (input$dest_state != "All") {
      out <- out %>% filter(dest_abb == input$dest_state)
    }
    
    out
  })
  
  unemp_for_period <- reactive({
    yrs <- strsplit(as.character(input$flow_period), "-")[[1]] |> as.numeric()
    
    unemp_long %>%
      filter(year %in% yrs) %>%
      group_by(region) %>%
      summarise(
        unemployment_rate = mean(unemployment_rate, na.rm = TRUE),
        .groups = "drop"
      )
  })
  
  output$flow_map <- renderPlot({
    flows <- filtered_flows()
    unemp <- unemp_for_period()
    
    # background polygons with unemployment shading
    bg_states <- usa_map %>%
      left_join(unemp, by = "region")
    
    ggplot() +
      geom_polygon(
        data = bg_states,
        aes(x = long, y = lat, group = group, fill = unemployment_rate),
        colour = "white"
      ) +
      geom_curve(
        data = flows,
        aes(
          x = orig_lon,  y = orig_lat,
          xend = dest_lon, yend = dest_lat,
          size = n_returns,
          colour = total_agi
        ),
        curvature = 0.25,
        alpha = 0.9,
        arrow = arrow(length = unit(0.12, "inches"))
      ) +
      scale_size_continuous(range = c(0.2, 4), guide = "none") +
      scale_colour_gradient(
        low  = "blue",
        high = "darkred",
        name = "Adjusted Gross Income (AGI)",
        breaks = c(min(flows$total_agi, na.rm = TRUE), 
                   max(flows$total_agi, na.rm = TRUE)),
        labels = function(x) format(x, scientific = FALSE, big.mark = ",")
      ) +
      scale_fill_gradient(
        low  = "lightyellow",
        high = "red",
        na.value = "grey90",
        name = "Unemployment Rate",
        limits = c(3, 7.5),  
        breaks = c(4, 5, 6, 7)
      ) + annotate("text", x = -125, y = 25, 
                 label = "Gray coloring indicates missing data", 
                 size = 3, color = "gray40", hjust = 0) +
      coord_fixed(1.3) +
      theme_void() +
      theme(
        legend.position = "bottom",
        plot.title = element_text(hjust = 0.5, face = "bold")
      ) +
      labs(
        title = paste(
          "State-to-State Migration by Adjusted Gross Income (AGI) and Unemployment Rate,",
          as.character(unique(flows$flow_period))
        )
      )
  })
  
  output$top_dest <- renderTable({
    flows <- filtered_flows()
    if (nrow(flows) == 0) return(NULL)
    
    flows %>%
      group_by(dest_abb) %>%
      summarise(total_agi = sum(total_agi, na.rm = TRUE), .groups = "drop") %>%
      arrange(desc(total_agi)) %>%
      slice_head(n = 5) %>%
      rename(
        `Destination` = dest_abb,
        `Total AGI` = total_agi
      )
  })
  
  output$top_origin <- renderTable({
    flows <- filtered_flows()
    if (nrow(flows) == 0) return(NULL)
    
    flows %>%
      group_by(origin_abb) %>%
      summarise(total_agi = sum(total_agi, na.rm = TRUE), .groups = "drop") %>%
      arrange(desc(total_agi)) %>%
      slice_head(n = 5) %>%
      rename(
        `Origin` = origin_abb,
        `Total AGI` = total_agi
    )
    
  })
  
  selected_years <- reactive({
    as.numeric(strsplit(input$year_range, "_")[[1]])
  })
  
  data_for_year <- reactive({
    yrs <- selected_years()
    
    unemp_long %>%
      filter(year %in% yrs) %>%
      group_by(region, state_abbr) %>%
      summarise(
        unemployment_rate = mean(unemployment_rate, na.rm = TRUE),
        .groups = "drop"
      )
  })
  
  output$unemp_map <- renderPlot({
    df <- data_for_year()
    
    plot_df <- states_map %>%
      left_join(df, by = "region")
    
    ggplot(plot_df,
           aes(long, lat, group = group, fill = unemployment_rate)) +
      geom_polygon(color = "white", linewidth = 0.2) +
      scale_fill_gradient(
        low = "lightblue",    # Light color for LOW unemployment
        high = "darkblue",         # Dark color for HIGH unemployment
        na.value = "grey90",
        name = "Unemployment Rate (%)",
        limits = c(3, 7.5),   # SAME limits as Migration tab
        breaks = c(3, 4, 5, 6, 7, 7.5)
      ) + annotate("text", x = -125, y = 25, 
                   label = "Gray coloring indicaets missing data", 
                   size = 3, color = "gray40", hjust = 0) +
      coord_fixed(1.3) +
      labs(
        title = paste("Average unemployment rate (", input$year_range, ")", sep = ""),
        fill  = "Rate"
      ) +
      theme_void()
  })
  
  state_series <- reactive({
    unemp_long %>%
      filter(state_abbr == input$state) %>%
      arrange(year)
  })
  
  output$state_ts <- renderPlot({
    df <- state_series()
    
    ggplot(df, aes(x = year, y = unemployment_rate)) +
      geom_line() +
      geom_point() +
      scale_x_continuous(breaks = all_years) +
      scale_y_continuous(
        limits = c(3.5, 7.5),  
        breaks = c(4, 5, 6, 7)
      ) +
      labs(
        title = paste(
          "Unemployment rate for",
          unique(df$state_abbr),
          "(", unique(df$region), ")"
        ),
        x = "Year",
        y = "Unemployment rate"
      ) +
      theme_minimal()
  })
}

# run app
shinyApp(ui, server)
