# app.R ---------------------------------------------------------------
# Shiny app: US map with state-to-state migration arrows

library(shiny)
library(readr)
library(dplyr)
library(ggplot2)
library(stringr)
library(maps)

# --------------------- Load & prepare data ---------------------------

# 1) Path to your cleaned CSV  ----
csv_path <- "~/Downloads/migration_cleaned.csv"   # <-- change if needed

migration_raw <- read_csv(csv_path, show_col_types = FALSE) %>%
  mutate(
    n1  = as.numeric(n1),
    n2  = as.numeric(n2),
    AGI = as.numeric(AGI)
  ) %>%
  # use only outflow rows; this still gives both directions between states
  filter(flow_type == "outflow")

# 2) FIPS -> state abbreviation lookup (50 states + DC)  ----
fips_lut <- tibble::tibble(
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

# 3) Attach abbreviations for both y1_statefips and y2_statefips  ----
migration_labeled <- migration_raw %>%
  left_join(fips_lut, by = c("y1_statefips" = "fips")) %>%
  rename(y1_abb_from_fips = state_abb) %>%
  left_join(fips_lut, by = c("y2_statefips" = "fips")) %>%
  rename(y2_abb_from_fips = state_abb)

# 4) Build unified origin / destination abbreviations  ----
migration_flows <- migration_labeled %>%
  mutate(
    origin_abb = y1_abb_from_fips,  # origin is focal state
    dest_abb   = y2_state           # destination 2-letter code
  ) %>%
  filter(origin_abb %in% state.abb,
         dest_abb   %in% state.abb)

# 5) Get state centroids for plotting arrows  ----
state_centers <- tibble::tibble(
  state_abb  = state.abb,
  state_name = state.name,
  lon        = state.center$x,
  lat        = state.center$y
)

# add lon/lat for origin and destination
migration_geo <- migration_flows %>%
  left_join(state_centers, by = c("origin_abb" = "state_abb")) %>%
  rename(orig_lon = lon, orig_lat = lat) %>%
  left_join(state_centers, by = c("dest_abb" = "state_abb")) %>%
  rename(dest_lon = lon, dest_lat = lat)

# 6) US map polygons for background  ----
usa_map <- map_data("state")

# Nicely ordered yearpairs, drop same-state flows
migration_geo <- migration_geo %>%
  mutate(
    yearpair = factor(yearpair, levels = sort(unique(yearpair)))
  ) %>%
  filter(origin_abb != dest_abb)

# use a more realistic upper bound for the slider
max_n1 <- as.numeric(quantile(migration_geo$n1, 0.99, na.rm = TRUE))

# --------------------------- UI --------------------------------------

ui <- fluidPage(
  titlePanel("US State-to-State Migration of Tax Returns (Households)"),
  
  sidebarLayout(
    sidebarPanel(
      width = 4,
      
      selectInput(
        "yearpair", "Migration year:",
        choices = levels(migration_geo$yearpair),
        selected = levels(migration_geo$yearpair)[1]
      ),
      
      sliderInput(
        "min_n1",
        "Minimum number of tax returns (households) to show:",
        min   = 0,
        max   = max_n1,
        value = round(max_n1 * 0.1),
        step  = round(max_n1 / 1000)
      ),
      
      selectInput(
        "origin_state", "Origin state (from):",
        choices = c("All", sort(unique(migration_geo$origin_abb))),
        selected = "All"
      ),
      selectInput(
        "dest_state", "Destination state (to):",
        choices = c("All", sort(unique(migration_geo$dest_abb))),
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

# -------------------------- SERVER -----------------------------------

server <- function(input, output, session) {
  
  filtered_flows <- reactive({
    out <- migration_geo %>%
      filter(
        yearpair == input$yearpair,
        n1 >= input$min_n1
      )
    
    if (input$origin_state != "All") {
      out <- out %>% filter(origin_abb == input$origin_state)
    }
    if (input$dest_state != "All") {
      out <- out %>% filter(dest_abb == input$dest_state)
    }
    
    out
  })
  
  output$flow_map <- renderPlot({
    flows <- filtered_flows()
    
    ggplot() +
      # USA background map
      geom_polygon(
        data = usa_map,
        aes(x = long, y = lat, group = group),
        fill = "grey95", colour = "white"
      ) +
      # Flow arrows; size + color map to n1
      geom_curve(
        data = flows,
        aes(
          x = orig_lon,  y = orig_lat,
          xend = dest_lon, yend = dest_lat,
          size = n1,
          colour = n1
        ),
        curvature = 0.25,
        alpha = 0.9,
        arrow = arrow(length = unit(0.12, "inches"))
      ) +
      scale_size_continuous(range = c(0.2, 4), guide = "none") +
      scale_colour_gradient(
        low  = "blue",
        high = "red",
        name = "Tax returns (households)"
      ) +
      coord_fixed(1.3) +
      theme_void() +
      theme(
        legend.position = "bottom",
        plot.title = element_text(hjust = 0.5, face = "bold")
      ) +
      labs(
        title = paste(
          "State-to-state migration of tax returns (households),",
          as.character(unique(flows$yearpair))
        )
      )
  })
  
  # --------- side rankings -----------------
  
  output$top_dest <- renderTable({
    flows <- filtered_flows()
    if (nrow(flows) == 0) return(NULL)
    
    flows %>%
      group_by(dest_abb) %>%
      summarise(total_returns = sum(n1, na.rm = TRUE), .groups = "drop") %>%
      arrange(desc(total_returns)) %>%
      slice_head(n = 5) %>%
      rename(
        `Destination`                = dest_abb,
        `Tax returns (households)`   = total_returns
      )
  })
  
  output$top_origin <- renderTable({
    flows <- filtered_flows()
    if (nrow(flows) == 0) return(NULL)
    
    flows %>%
      group_by(origin_abb) %>%
      summarise(total_returns = sum(n1, na.rm = TRUE), .groups = "drop") %>%
      arrange(desc(total_returns)) %>%
      slice_head(n = 5) %>%
      rename(
        `Origin`                     = origin_abb,
        `Tax returns (households)`   = total_returns
      )
  })
}

# --------------------------- Run app ---------------------------------
shinyApp(ui, server)
