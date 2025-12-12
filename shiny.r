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
  "migration_flows_combined_2011_2021.csv",
  show_col_types = FALSE
) %>%
  mutate(
    n_returns    = as.numeric(n_returns),
    n_exemptions = as.numeric(n_exemptions),
    total_agi    = as.numeric(total_agi)
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

migration_geo <- migration_geo %>%
  mutate(
    avg_agi = (total_agi / n_returns) * 1000
  )


max_n <- as.numeric(quantile(migration_geo$n_returns, 0.99, na.rm = TRUE))

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
            "min_avg_agi",
            "Minimum average AGI per household ($):",
            min   = round(quantile(migration_geo$avg_agi, 0.05, na.rm = TRUE), -3),
            max   = round(quantile(migration_geo$avg_agi, .9998, na.rm = TRUE), -3),
            value = round(median(migration_geo$avg_agi, na.rm = TRUE), -3),
            step  = 500,
            pre   = "$",
            sep   = ","
          )
          ,
          
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
          h4("Top destinations"),
          tableOutput("top_dest"),
          br(),
          h4("Top origins"),
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
        ),
        mainPanel(
          plotOutput("unemp_map", height = "550px"),
          tags$hr(),
        )
      )
    )
  )
)


server <- function(input, output, session) {
  slider_domain <- reactive({
    df <- migration_geo %>%
      filter(flow_period == input$flow_period)
    
    if (input$origin_state != "All") df <- df %>% filter(origin_abb == input$origin_state)
    if (input$dest_state   != "All") df <- df %>% filter(dest_abb   == input$dest_state)
    
    # avoid weird outliers so the slider stays usable
    rng <- quantile(df$avg_agi, probs = c(0.05, 0.995), na.rm = TRUE)
    
    list(
      min = round(rng[[1]], -3),
      max = round(rng[[2]], -3)
    )
  })
  
  observeEvent(
    list(input$flow_period, input$origin_state, input$dest_state),
    {
      dom <- slider_domain()
      
      # keep current value if it's still in-range; otherwise clamp
      new_value <- input$min_avg_agi
      if (is.null(new_value) || is.na(new_value)) new_value <- dom$min
      new_value <- max(dom$min, min(dom$max, new_value))
      
      updateSliderInput(
        session, "min_avg_agi",
        min = dom$min,
        max = dom$max,
        value = new_value,
        step = 500
      )
    },
    ignoreInit = TRUE
  )
  
  
  filtered_flows <- reactive({
    out <- migration_geo %>%
      filter(
        flow_period == input$flow_period,
        avg_agi >= input$min_avg_agi
        
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
          size   = n_returns,
          colour = avg_agi
        )
        ,
        curvature = 0.25,
        alpha = 0.9,
        arrow = arrow(length = unit(0.12, "inches"))
      ) +
      scale_size_continuous(range = c(0.2, 4), guide = "none") +
      scale_colour_gradient(
        low  = "blue",
        high = "red",
        name = "Average AGI per household ($)",
        breaks = scales::pretty_breaks(n = 4),
        labels = scales::dollar_format()
      ) +
      scale_fill_gradient(
        low  = "white",
        high = "orange",
        na.value = "grey90",
        name = "Unemployment rate"
      ) +
      coord_fixed(1.3) +
      theme_void() +
      theme(
        legend.position = "bottom",
        legend.box = "vertical",
        legend.box.just = "right",
        legend.justification = "right",
        legend.key.width = unit(2, "cm")
      ) +
      labs(
        title = paste(
          "State-to-state migration of tax returns (households) and unemployment,",
          as.character(unique(flows$flow_period))
        )
      )
  })
  
  output$top_dest <- renderTable({
    flows <- filtered_flows()
    if (nrow(flows) == 0) return(NULL)
    
    flows %>%
      group_by(dest_abb) %>%
      summarise(
        `Households` = sum(n_returns, na.rm = TRUE),
        `Avg AGI ($)` = sum(avg_agi * n_returns, na.rm = TRUE) / sum(n_returns, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      arrange(desc(`Households`)) %>%
      slice_head(n = 5) %>%
      mutate(
        `Households` = scales::comma(round(`Households`)),
        `Avg AGI ($)` = scales::dollar(round(`Avg AGI ($)`))
      ) %>%
      rename(`Destination` = dest_abb)
  })
  
  
  
  output$top_origin <- renderTable({
    flows <- filtered_flows()
    if (nrow(flows) == 0) return(NULL)
    
    flows %>%
      group_by(origin_abb) %>%
      summarise(
        `Households` = sum(n_returns, na.rm = TRUE),
        `Avg AGI ($)` = sum(avg_agi * n_returns, na.rm = TRUE) / sum(n_returns, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      arrange(desc(`Households`)) %>%
      slice_head(n = 5) %>%
      mutate(
        `Households` = scales::comma(round(`Households`)),
        `Avg AGI ($)` = scales::dollar(round(`Avg AGI ($)`))
      ) %>%
      rename(`Origin` = origin_abb)
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

# Run the app
shinyApp(ui, server)
