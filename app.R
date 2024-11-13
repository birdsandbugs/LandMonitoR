# LandMonitoR
# Graham Montgomery & Ben Tonelli
# System requirements: R v. 4.3.1 or greater
# note that working directory should be set to same as script

# 11/12/2024 notes
# upload to github
# deploy to shiny
# populate google feedback form (keep it short and simple)
# make a sheet to put your email down if you want updates.

################################################################################
# setup

# dependencies
if (!require("pacman")) install.packages("pacman")
pacman::p_load(shiny, 
               shinydashboard,
               shinyjs,
               tinytex,
               htmltools, 
               quarto,
               tidyverse,
               showtext,
               sysfonts,
               patchwork,
               rsi, # ndvi
               terra, # ndvi
               rgbif, # biodiversity data
               rtide, # tides
               scales, # for tides
               MoonShineR, # moon illumination
               rnpn, # phenology/GDD
               sf, # spatial stuff
               gt, # tables
               hrbrthemes, # figures
               nasapower, # climatic data
               vegan, # community ecology
               elevatr, # elevation
               leaflet, # mapping
               rcrossref) # lit data

# MoonShineR
# devtools is required to install MoonShineR. use:
# devtools::install_github("Crampton-Lab/MoonShineR_package")
# library(MoonShineR)

# other setup
sysfonts::font_add_google("Work Sans", "worksans")
showtext::showtext_auto()

################################################################################
#input: getting that sweet, sweet data

# what is your site's name?
site_name <- "Fort Ord Natural Reserve"

# spatial
# if you DON'T have a shape file, make a WKT at XXXXX.com and read it in with the next two lines.
# site_WKT <- "POLYGON((-109.20927974232089 31.875781565692634,-109.19614788797449 31.881545348825625,-109.19897373004905 31.884109528270415,-109.2081992732924 31.88695591909628,-109.21448815712495 31.884650584537596,-109.21537469581502 31.879969166404678,-109.20927974232089 31.875781565692634))"
# site_sf <- sf::st_as_sfc(site_WKT, crs = 4326)
# Now, skip to line 56 below.

# if you DO have a shape file, rename all four files (.dbf, .prj, .shp, & .shx) to "site":
site_sf <- st_read("site.shp") # Fort Ord currently
site_sf <- st_transform(site_sf, crs = 4326)
site_sf_valid <- st_make_valid(site_sf) # makes coords counterclockwise (for rgbif)
site_WKT <- st_as_text(site_sf_valid$geometry)

site_centroid <- sf::st_centroid(site_sf)
site_coords <- sf::st_coordinates(site_centroid)
site_centroid_sf <- sf::st_sf(site_centroid)
site_elev_centroid <- elevatr::get_elev_point(site_centroid_sf)
site_bbox <- st_bbox(site_sf_valid) # for iN widget
nelat <- as.character(site_bbox$ymax)
nelong <- as.character(site_bbox$xmax)
swlat <- as.character(site_bbox$ymin) 
swlong <- as.character(site_bbox$xmin)

# for NDVI
asi = spectral_indices(download_indices = TRUE)
site_ndvi_pt = st_point(c(site_coords[1], site_coords[2]))
site_ndvi_pt2 = st_sfc(site_ndvi_pt, crs = "EPSG:4326")
site_buffer <- st_buffer(st_transform(site_ndvi_pt2, "EPSG:3081"), 5000)


# GDD
# Note: for the average, need to figure out a way that calls the server less.

GDD_10 <- 0:7
for (i in 0:7) {
  date_to_use <- Sys.Date() - years(i)
  
  data <- rnpn::npn_get_agdd_point_data(
    layer = 'gdd:agdd_50f',
    lat = as.character(site_coords[2]),
    long = as.character(site_coords[1]),
    date = date_to_use
  )
  
  GDD_10[i] <- data
  
}

GDD_10yravg <- round(mean(unlist(GDD_10)), digits = 0)

GDD_site_today <- rnpn::npn_get_agdd_point_data(layer = 'gdd:agdd_50f', 
                                          lat = as.character(site_coords[2]), 
                                          long = as.character(site_coords[1]), 
                                          date = Sys.Date())

GDD_site_lastyear <- rnpn::npn_get_agdd_point_data(layer = 'gdd:agdd_50f', 
                                                lat = as.character(site_coords[2]), 
                                                long = as.character(site_coords[1]), 
                                                date = Sys.Date()-years(1))

GDD_site_today <- round(GDD_site_today$value, digits = 0)
GDD_site_lastyear <- round(GDD_site_lastyear, digits = 0)
GDD_site_avg <- GDD_10yravg # note: just the last 8 years until a good rnpn solution found
linevalue <- GDD_10yravg # note: just the last 8 years until a good rnpn solution found

GDD_site_df <- data.frame(
  gdd = GDD_site_today,
  category = "GDD_site_today") # df setup for plotting.

# moon
moonlight_this_wk <- MoonShineR::predict_lux(latitude = site_coords[2],
                                             longitude = site_coords[1],
                                             date_start = as.character(Sys.Date()),
                                             time_start = "18:00:00",
                                             duration = 7,
                                             time_zone = "PST",
                                             site_elev = site_elev_centroid$elevation,
                                             export_table = FALSE)

# tides
# tide station names: https://opendap.co-ops.nos.noaa.gov/stations/index.jsp
# time zone identifiers (tz): https://en.wikipedia.org/wiki/List_of_tz_database_time_zones
tides = tide_height("Santa Monica", from = Sys.Date(), 
                    to = Sys.Date()+6, minutes = 10, tz ='America/Los_Angeles')

# soil wetness
# may have to go with smap/smapr (but it requires an API key...)
# I have tried hard to avoid API keys/logins, but it may be unavoidable.
soil_wetness_swrs_now <- get_power(
  community = "ag",
  pars = "GWETTOP",
  lonlat = c(site_coords[1], site_coords[2]),
  temporal_api = "daily",
  dates = Sys.Date()-365 # last year, not this year!
)

soil_wetness_swrs_avg <- get_power(
  community = "ag",
  pars = "GWETTOP",
  lonlat = c(site_coords[1], site_coords[2]),
  temporal_api = "daily",
  dates = Sys.Date()-365*2 # two years ago, not the actual average!
)

ssw_today <- round(soil_wetness_swrs_now$GWETTOP, digits = 2) # test only, previous year data, bar to this level
ssw_avg <- soil_wetness_swrs_avg$GWETTOP # test only, previous year data, dashed line at this level

ssw_test <- data.frame(
  ssw = ssw_today,
  category = "ssw_today")

# snowpack
# may have to go with smap/smapr (but it requires an API key...)
snowpack_today <- 0 # TESTING # bar to this level
snowpack_avg <- 0.1 # TESTING # dashed line at this level

snowpack_test <- data.frame(
  var = snowpack_today,
  category = "snowpack_today")

# precip ytd
precip_ytd <- 13.2 # TESTING # bar to this level
precip_ytd_avg <- 17.6 # TESTING # dashed line at this level

precip_ytd_test <- data.frame(
  precip = precip_ytd,
  category = "precip_ytd")

# NDVI map
site_sentinel <- get_sentinel2_imagery(
  site_buffer,
  start_date = paste(Sys.Date()-4),
  end_date = paste(Sys.Date()-1),
  output_filename = tempfile(fileext = ".tif")
)

site_dem = get_dem(
  site_buffer,
  output_filename = tempfile(fileext = ".tif")
)

site_sentinel_rast = rast(site_sentinel)
site_dem_rast = rast(site_dem)

plotRGB(site_sentinel_rast, r = 4, g = 3, b = 2, stretch = "lin")
plot(site_dem_rast)

ndvi = asi[asi$short_name == "NDVI", ]

site_sentinel_ndvi_current = calculate_indices(
  site_sentinel,
  ndvi,
  output_filename = tempfile(fileext = ".tif")
)

site_rast <- rast(site_sentinel_ndvi_current)
NDVI_df <- as.data.frame(site_rast, xy = TRUE)


# Biodiversity initial data pull (GBIF)
cols <-  c("acceptedScientificName", "decimalLatitude", "decimalLongitude", "basisOfRecord",
           "kingdom", "phylum", "class", "order", "family", "genus", "species", "taxonRank",
           "iucnRedListCategory", "year", "month", "day", "eventDate","startDayOfYear",
           "references")
site_obs_raw <- occ_search(geometry = site_WKT, 
                           hasCoordinate = TRUE, 
                           basisOfRecord = "HUMAN_OBSERVATION;PRESERVED_SPECIMEN", 
                           fields = cols,
                           limit = 100)

site_obs <- site_obs_raw$data

# Biodiversity summary table setup
df_key <- data.frame(class = c("Aves", "Insecta", "Liliopsida", "Squamata", 
                                  "Mammalia", "Arachnida", "Amphibia", "Magnoliopsida"), 
                     display_text = c("# Bird spp:", "# Insect spp:", "# Monocot sp:", 
                                      "# Reptile spp:", "# Mammal spp:", "# Arachnid spp:", 
                                      "# Amphibian spp:", "# Dicot sp:"))

sp_counts <- count(site_obs, species)
tax_classes <- site_obs %>% select(species, class)
sp_counts <- left_join(sp_counts, tax_classes)

summary_table_a <- sp_counts %>% count(class) %>% filter(class %in% df_key$class)
summary_table_a <- left_join(df_key, summary_table_a, by = "class")
summary_table_a <- summary_table_a %>% select(-class)

summary_table_b <- data.frame(Total_obs = as.character(nrow(site_obs)),
                              num_sp = as.character(length(unique(site_obs$acceptedScientificName))),
                              num_types_new_sp = "0",
                              num_special_stat = as.character(nrow(filter(site_obs, iucnRedListCategory != "LC" & iucnRedListCategory != "DD"))),
                              shannon = round(diversity(sp_counts$n, index = "shannon"), 2),
                              simpson = round(diversity(sp_counts$n, index = "simpson"), 2))

summary_table_b2 <- as.data.frame(t(summary_table_b))
summary_table_b2 <- cbind(ColumnNames = rownames(summary_table_b2), summary_table_b2)
rownames(summary_table_b2) <- NULL
summary_table_b2$ColumnNames <- c("# of observations:",
                                  "# of spp:",
                                  "# of type spp:",
                                  "# of special status spp:",
                                  "Shannon diversity:",
                                  "Simpson diversity:")

summary_table_all <- bind_rows(summary_table_b2, summary_table_a)

f <- function(x){
  no_rows <- length(x)
  v <- x[!is.na(x)]
  length(v) <- no_rows
  v
}

summary_table_all <- as.data.frame(lapply(summary_table_all, f))
summary_table_all <- summary_table_all %>% filter(!if_all(everything(), is.na)) %>% 
  mutate(n = coalesce(n, 0))
colnames(summary_table_all) <- c("ColumnNames", "V1", "display_text", "n")


# species list table setup
splist_cols <- c("kingdom", "phylum", "class", "order", "family", "genus",
                 "species", "iucnRedListCategory", "references")

site_species_list <- site_obs %>% 
  filter(taxonRank == "SPECIES" | taxonRank == "SUBSPECIES") %>%
  select(all_of(splist_cols)) %>%
  distinct(species, .keep_all = TRUE) %>%
  arrange(kingdom, phylum, class, order, family, genus, species)

make_hyperlink = function(myurl,mytext=myurl) {
  paste('<a href="',myurl,'">',"link to observation",'</a>')
}

# SAC curve
SAC_curve <- site_obs %>% arrange(eventDate) %>% 
  select(year, month, day, eventDate, species) %>% mutate(obs_number = row_number(),
                                                          sum = NA)

# for loop time (ideally, an apply/dplyr solution is probably more efficient)
for(i in 1:nrow(SAC_curve)) {
  SAC_curve$sum[i] <- length(unique(SAC_curve$species[1:i]))
}

# relative abundance figure setup
spp_totals <- site_obs %>% count(species)

# use data
site_use <- read.csv("dummy_use_data.csv") # dummy data

# citation data
bib_data <- read.csv("bib_data_angelo.csv") # dummy data
bib_summary <- bib_data %>% count(Publication.Year)

# iNaturalist widget
inat <- paste0(
  "<style type=\"text/css\" media=\"screen\">
  .inat-widget { font-family: Georgia, serif; padding: 10px; line-height: 1;}
  .inat-widget-header {margin-bottom: 10px;}
  .inat-widget td {vertical-align: top; padding-bottom: 10px;}
  .inat-label { color: #888; }
  .inat-meta { font-size: smaller; margin-top: 3px; line-height: 1.2;}
  .inat-observation-body, .inat-user-body { padding-left: 10px; }
  .inat-observation-image {text-align: center;}
  .inat-observation-image, .inat-user-image { width: 48px; display: inline-block; }
  .inat-observation-image img, .inat-user-image img { max-width: 48px; }
  .inat-observation-image img { vertical-align: middle; }
  .inat-widget-small .inat-observation-image { display:block; float: left; margin: 0 3px 3px 0; height:48px;}
  .inat-label, .inat-value, .inat-user { font-family: \"Trebuchet MS\", Arial, sans-serif; }
  .inat-user-body {vertical-align: middle;}
  .inat-widget td.inat-user-body {vertical-align: middle;}
  .inat-widget .inat-footer td.inat-value {vertical-align: middle; padding-left: 10px;}
  </style>
  <div class=\"inat-widget\">
      <div class=\"inat-widget-header\">
        <a href=\"https://www.inaturalist.org\"><img alt=\"iNaturalist\" src=\"https://www.inaturalist.org/assets/logo-small.png\" /></a>  
      </div>
    <script type=\"text/javascript\" charset=\"utf-8\" src=\"https://www.inaturalist.org/observations.widget?nelat=",
  nelat,
  "&nelng=",
  nelong,
  "&swlat=",
  swlat,
  "&swlng=",
  swlong,
  "&layout=small&limit=5&order=desc&order_by=observed_on\"></script>
    <table class=\"inat-footer\">
      <tr class=\"inat-user\">
        <td class=\"inat-value\" colspan=\"2\">
          <strong>
              <a href=\"https://www.inaturalist.org/places/fort-ord-natural-reserve\">View more observations near the reserve on <nobr>iNaturalist »</nobr></a>
          </strong>
        </td>
      </tr>
    </table>
  </div>"
)

# recent citations feed

get_recent_papers <- function(keyword, num_results = 5) {
  results <- cr_works(query = keyword, limit = num_results, sort = "relevance")
  papers <- results$data %>%
    select(title, published.print, url) %>%
    mutate(date = case_when(grepl("^\\d{4}-\\d{2}-\\d{2}$", published.print) ~ as.Date(published.print, format = "%Y-%m-%d"),
                             TRUE ~ NA)) %>%
    arrange(desc(date))
  return(papers)
}

recent_papers <- get_recent_papers(site_name, 5) %>% select(-date) %>% mutate()
colnames(recent_papers) <- c("Title", "Publication date", "link")


################################################################################
# Define UI

ui <- dashboardPage(
  dashboardHeader(
    title = "LandMonitoR"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Map", tabName = "map", icon = icon("map")),
      menuItem("Conditions this week", tabName = "this_week", icon = icon("dashboard")),
      menuItem("Reserve Summary", tabName = "summary", icon = icon("chart-line")),
      menuItem("Tutorial", tabName = "tutorial", icon = icon("book")),
      menuItem("About", tabName = "about", icon = icon("cog"))
    )
  ),
  
  dashboardBody(
    tags$style(HTML(".box.box-solid.box-primary>.box-header {color: #4f4e4e;background-color: #9AD1F8;}
    .box.box-solid.box-primary {border-color: #9AD1F8;}
    .gt_table_scroll_small{max-height: 240px !important; overflow-y: auto; !important}
    .gt_table_scroll{max-height: 340px !important; overflow-y: auto; !important}")),
    
    tags$head(
      tags$style(HTML(".content-wrapper {background-color: #fffbf3;} 
       .main-sidebar {background-color: #9AD1F8 !important;} 
       .navbar {background-color: #9AD1F8 !important;}
       .sidebar-menu .active a {background-color: #858585 !important;}
       .sidebar-menu li a:hover {background-color: #858585 !important;}
       .sidebar-menu li a {color: #4f4e4e !important;}
       .logo {background-color: #9AD1F8 !important;}"))),
    tabItems(
      # "Maps" tab content
      tabItem(tabName = "map",
              h1("Your site"),
              fluidPage(leafletOutput("basic_map", width = "100%", height = 500))),
      
      # "This week" tab content
      tabItem(tabName = "this_week",
              h1("Current conditions & forecast"),
              useShinyjs(),
              fluidRow(
                # Tide & moon
                box(title = "Moon illumination", status = "primary", solidHeader = TRUE, width = 6, height = 300, plotOutput("moon", height = "240px")),
                box(title = "Tide chart", status = "primary", solidHeader = TRUE, width = 6, height = 300, plotOutput("tides", height = "240px"))),
              
              fluidRow(  
                # Weather vars
                box(title = "Accumulated growing degree days", status = "primary", solidHeader = TRUE, width = 4, height = 300, plotOutput("GDD", height = "240px")),
                box(title = "Precipitation (year to date)*", status = "primary", solidHeader = TRUE, width = 4, height = 300, plotOutput("precipytd", height = "240px")),
                box(title = "Current snowpack level*", status = "primary", solidHeader = TRUE, width = 4, height = 300, plotOutput("snowpack", height = "240px")),
              ),
              fluidRow(
                box(title = "Current soil moisture*", status = "primary", solidHeader = TRUE, width = 4, height = 300, plotOutput("soilwet", height = "240px")),
                
                # iNaturalist widget
                box(
                  title = "iNaturalist feed",
                  width = 4, height = 300,
                  status = "primary",
                  solidHeader = TRUE,
                  HTML(paste(inat))),
            
                # Recent citations widget
                  column(width = 4, height = 300,
                         box(title = "Recent studies", status = "primary", solidHeader = TRUE, width = 400, height = 300, div(class = "gt_table_scroll_small", gt_output("recentcites"))))),
              # NDVI
                fluidRow(
                  box(title = "NDVI", status = "primary", solidHeader = TRUE, width = 4, height = 400, plotOutput("NDVI", height = "340px")),    
                  box(
                    title = "Report Generation",
                    status = "primary",
                    solidHeader = TRUE,
                    width = 4,  # Adjust width as needed
                    
                    # Generate report button inside the box
                    actionButton("generate_report", "Generate Report", icon = icon("file-pdf")),
                    
                    # Download button (initially hidden), also inside the box
                    downloadButton("download_report", "Download Report", style = "display: none;")
                  )
                  
                  )
      ),
      
      # "Reserve summary" tab content
      tabItem(tabName = "summary",
              h1("Reserve summary"),
              
              # row with biodiversity summary table & SAC
              fluidRow(
                column(width = 6,
                       box(title = "Biodiversity summary", status = "primary", solidHeader = TRUE, width = 600, height = 400, div(class = "gt_table_scroll", gt_output("biosummary")))),
                column(width = 6,
                       box(title = "Species accumulation curve*", status = "primary", solidHeader = TRUE, width = 600, height = 400, plotOutput("SAC_curve", height = "340px")))),

              # Relative abundance table row
              fluidRow(
                box(title = "Relative abundance*", status = "primary", solidHeader = TRUE, width = 12, height = 500, plotOutput("relabund", height = "440px"))),
              
              # Species table row
              fluidRow(
                column(width = 12,
                       div(
                         gt_output("species_table"),
                         style = "max-width: 1600px; max-height: 400px; overflow-y: auto; margin-bottom: 40px;"))),
           
              # row with user & bibliographic data
              fluidRow(
                box(title = "User hours by year", status = "primary", solidHeader = TRUE, width = 6, height = 300, plotOutput("user_hours", height = "240px")),
                box(title = "Literature citations by year", status = "primary", solidHeader = TRUE, width = 6, height = 300, plotOutput("bibliographic", height = "240px"))),
      ),
      
      # "Tutorial" tab content (rmd/quarto doc)
      tabItem(tabName = "tutorial",
              fluidPage(mainPanel(includeHTML("LandMonitoR_tutorial.html")))
      ),
      
      # "About" tab content: references, etc. (also an rmd/quarto doc)
      tabItem(tabName = "about",
              fluidPage(mainPanel(includeHTML("LandMonitoR_about.html"))
              )
      )
    )
  )
)

################################################################################
# Define server logic

server <- function(input, output) {
  # "Maps" tab
  output$basic_map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%  # Add default OpenStreetMap tiles
      addPolygons(data = site_sf, color = "blue", weight = 2, fillOpacity = 0.2)
  })
  
  # This week's report
  observeEvent(input$generate_report, {
    # Show download button after clicking generate
    shinyjs::show("download_report")
  })
  
  output$download_report <- downloadHandler(
    filename = function() {
      paste("report", Sys.Date(), ".pdf", sep = "")
    },
    content = function(file) {
      # Path to the pre-made PDF file
      file.copy("LandMonitoR_report.pdf", file)
    }
  )
  
  # "This week" tab
  # moon illumination
  output$moon <- renderPlot({
    plot_lux(df = moonlight_this_wk, illuminance_type_plot = "total_illuminance_all",
             plot_y_max = 0.3,  plot_dayttime_gray_mask = TRUE, plot_eclipse_red_mask = TRUE,
             plot_twilight = "astro", vertical_time_label = TRUE, time_label_interval_hr = 24,
             time_label_shift_hr = 0) + theme_ipsum() + theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 10, hjust=1))
  })
  
  # tides
  output$tides <- renderPlot({
    ggplot(data = tides, aes(x = DateTime, y = TideHeight)) + 
      geom_line(color = "skyblue", size = 1) + 
      scale_x_datetime(name = "November 7-13, 2024", 
                       labels = date_format("%H:%M", tz="PST8PDT")) +
      scale_y_continuous(name = "Tide Height (m)") +
      ggtitle("Tides this week") + 
      theme_ipsum()
  })
  
  # GDD
  output$GDD <- renderPlot({
    ggplot(GDD_site_df, aes(x=category, y=gdd)) + 
      geom_col(color = "#66CCCC", fill = "#66CCCC", alpha = 1, width = 0.3, position = position_nudge(x = -0.25)) + 
      labs(title = "GDD today (>10°C)",
           y = "Accumulated GDD") + 
      theme_minimal(base_family = "worksans") + 
      geom_hline(yintercept=linevalue,linetype=2, size = 1.1, color = "#696969") +
      annotate("text", x = 1.15, y = linevalue*1.07, label = paste("30-yr avg"), color = "#696969", size = 5) +
      annotate("text", x = 1.15, y = linevalue*.95, label = paste("(", linevalue, ")", sep = ""), color = "#696969", size = 4) + 
      annotate("text", x = .75, y = GDD_site_today*1.07, label = paste(GDD_site_today), color = "#54b3b3", size = 5) + 
      theme(axis.title.x=element_blank(),
            axis.title.y=element_blank(),
            axis.text.x = element_blank(),
            plot.background = element_rect(fill = "white", color = NA),     
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            plot.title = element_text(hjust = 0.5, size = 18)
      )
  })
  
  # soil wetness
  output$soilwet <- renderPlot({
    ggplot(ssw_test, aes(x=category, y=ssw)) + 
      geom_col(color = "#88674E", fill = "#88674E", alpha = 1, width = 0.3, position = position_nudge(x = -0.25)) + 
      labs(title = "Topsoil wetness today (x units)",
           y = "wetness units...") + 
      theme_minimal(base_family = "worksans") + 
      geom_hline(yintercept=ssw_avg,linetype=2, size = 1.1, color = "#696969") +
      annotate("text", x = 1.15, y = ssw_avg*1.07, label = paste("20-yr avg"), color = "#696969", size = 5) +
      annotate("text", x = 1.15, y = ssw_avg*.95, label = paste("(", ssw_avg, ")", sep = ""), color = "#696969", size = 4) + 
      annotate("text", x = .75, y = ssw_today*1.12, label = paste(ssw_today), color = "#88674E", size = 5) + 
      theme(axis.title.x=element_blank(),
            axis.title.y=element_blank(),
            axis.text.x = element_blank(),
            plot.background = element_rect(fill = "white", color = NA),     
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            plot.title = element_text(hjust = 0.5, size = 18))
  })
  
  # snowpack
  output$snowpack <- renderPlot({
    ggplot(snowpack_test, aes(x=category, y=var)) + 
      geom_col(color = "#8FADF1", fill = "#8FADF1", alpha = 1, width = 0.3, position = position_nudge(x = -0.25)) + 
      labs(title = "Snowpack today",
           y = "snowpack in inches") + 
      theme_minimal(base_family = "worksans") + 
      geom_hline(yintercept=snowpack_avg,linetype=2, size = 1.1, color = "#696969") +
      annotate("text", x = 1.15, y = snowpack_avg*1.07, label = paste("30-yr avg"), color = "#696969", size = 5) +
      annotate("text", x = 1.15, y = snowpack_avg*.95, label = paste("(", snowpack_avg, ")", sep = ""), color = "#696969", size = 4) + 
      annotate("text", x = .75, y = snowpack_today+0.01, label = paste(snowpack_today), color = "#8FADF1", size = 5) + 
      theme(axis.title.x=element_blank(),
            axis.title.y=element_blank(),
            axis.text.x = element_blank(),
            plot.background = element_rect(fill = "white", color = NA),     
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            plot.title = element_text(hjust = 0.5, size = 18))
  })
  
  # Recent citations feed
  output$recentcites <- render_gt({
    citations_gt <- recent_papers %>%
      gt() %>%
      tab_header(title = md("**Recent citations**")) %>%
      tab_style(style = cell_fill(color = "#fffbf3"),
                locations = cells_body(rows = seq(1, nrow(recent_papers), 2))) %>%
      fmt(columns = "link", fns = make_hyperlink)
  })
  
  # precip ytd
  output$precipytd <- renderPlot({
      ggplot(precip_ytd_test, aes(x=category, y=precip)) + 
      geom_col(color = "#53789E", fill = "#53789E", alpha = 1, width = 0.3, position = position_nudge(x = -0.25)) + 
      labs(title = "YTD Precipitation (in)",
           y = "Accumulated GDD") + 
      theme_minimal(base_family = "worksans") + 
      geom_hline(yintercept=precip_ytd_avg,linetype=2, size = 1.1, color = "#696969") +
      annotate("text", x = 1.15, y = precip_ytd_avg*1.07, label = paste("30-yr avg"), color = "#696969", size = 5) +
      annotate("text", x = 1.15, y = precip_ytd_avg*.95, label = paste("(", precip_ytd_avg, ")", sep = ""), color = "#696969", size = 4) + 
      annotate("text", x = .75, y = precip_ytd*1.07, label = paste(precip_ytd), color = "#53789E", size = 5) + 
      theme(axis.title.x=element_blank(),
            axis.title.y=element_blank(),
            axis.text.x = element_blank(),
            plot.background = element_rect(fill = "white", color = NA),     
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            plot.title = element_text(hjust = 0.5, size = 18))
  })
    
    # NDVI
    output$NDVI <- renderPlot({
      ggplot(data = NDVI_df) +
        geom_raster(aes(x = x, y = y, fill = NDVI)) +  # 'layer' is the name of the raster band column
        coord_equal() +                                # Ensures square pixels for spatial accuracy
        scale_fill_viridis_c() +                       # Optional: a nice color scale for continuous data
        theme_minimal(base_family = "worksans") + 
        labs(caption = "Note: if you see gaps, those are probably from clouds!")
  })
  
  
  
  # Summary tab ################################################################
  # species list
  output$species_table <- render_gt({
    site_splist_gt <- site_species_list %>% 
      gt() %>% cols_label(kingdom = md("**Kingdom**"), 
                          phylum = md("**Phylum**"),
                          class = md("**Class**"),
                          order = md("**Order**"),
                          family = md("**Family**"),
                          genus = md("**Genus**"),
                          species = md("**Species**"),
                          iucnRedListCategory = md("**Red List Category**"), 
                          references = md("**Evidence of Presence**")) %>%
      tab_header(title = md("**Species List**")) %>%
      tab_style(style = cell_fill(color = "#fffbf3"),
                locations = cells_body(rows = seq(1, nrow(site_species_list), 2))) %>%
      fmt (
        columns = "references",
        fns = make_hyperlink
      )
  })
  
  # Biodiversity summary table
  output$biosummary <- render_gt({
    summary_gt <- summary_table_all %>%
      gt() %>% sub_missing(
        missing_text = "") %>% 
      tab_style(style = cell_text(weight = "bolder"),
                         locations = cells_body(columns = c(display_text, ColumnNames))) %>%
      cols_add('TEST1' = '', .after = 'V1') %>%
      cols_label('TEST1' = md('&emsp;&emsp;&emsp;')) %>%
      cols_label(display_text ="", n = "", ColumnNames = "", V1 = "") %>%
      tab_header(title = md("**Biodiversity summary**")) %>%
      tab_style(style = cell_fill(color = "#fffbf3"),
                locations = cells_body(rows = seq(1, nrow(summary_table_all), 2)))
      })
  
  # SAC curve
  output$SAC_curve <- renderPlot({
    ggplot(data = SAC_curve, mapping = aes(x = obs_number, y = sum)) +
      geom_point(alpha = 0.7, color = "skyblue", size = 3) +
      labs(title = "Species Accumulation Curve*",
           x = "Observations",
           y = "Total Species",
           caption = "Note: interpret with caution! (see documentation for details)") +
      theme_ipsum() +
      theme(text=element_text(size = 12),
            plot.caption = element_text(hjust = 0.5))
  })
  
  # Relative abundance
  output$relabund <- renderPlot({
    ggplot(spp_totals, aes(reorder(x=species, -n), y=n)) + 
      geom_bar(stat = "identity", color = "skyblue", fill = "skyblue", alpha = 0.8) + 
      labs(title = "Relative abundance*",
           x = "Species",
           y = "Total Individuals",
           caption = "Note: not necessarily related to abundance! (see documentation for details)") +
      theme_ipsum() + 
      theme(text=element_text(size = 12),
            plot.caption = element_text(hjust = 0.5),
            axis.title.x=element_blank(),
            axis.text.x = element_text(angle = 45, vjust = 1, size = 13, hjust=1))
  })
  
  # Use data
  output$user_hours <- renderPlot({
    ggplot(data = site_use, mapping = aes(x = year, y = user_days)) +
      geom_point(alpha = 0.7, color = "#53789E", size = 3) +
      labs(title = "Yearly user days",
           x = "Year",
           y = "Total user days") +
      theme_ipsum() +
      theme(text=element_text(size = 12))
  })
  
  # Bibliographic data
  output$bibliographic <- renderPlot({
    ggplot(data = bib_summary, mapping = aes(x = Publication.Year, y = n)) +
      geom_point(alpha = 0.7, color = "#53789E", size = 3) +
      labs(title = "Yearly publications",
           x = "Year",
           y = "Number of publications mentioning X reserve") +
      theme_ipsum() +
      theme(text=element_text(size = 12))
  })
}

################################################################################
# Run the app
shinyApp(ui = ui, server = server)
