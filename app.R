# ==============================================================================
# Chinese Engineers Relational Database (CERD) Web Interface
# ==============================================================================
# Author:       Dr. Thorben Pelzer
# Institute:    HKUST (originally Leipzig University)
# Year:         2025
# License:      CC BY-SA 4.0
# Project:      2020–2024: DFG SFB 1199-A6
#
# R Version:    4.5.1+
# ==============================================================================

# ------------------------------------------------------------------------------
# Required Libraries
# ------------------------------------------------------------------------------

# Core Shiny packages
library(shiny)            # Web application framework
library(shinyjs)          # JavaScript operations in Shiny
library(shinythemes)      # Bootstrap themes for Shiny
library(shinyWidgets)     # Custom input widgets
library(shinycssloaders)  # Loading animations
library(htmlwidgets)      # HTML widget framework
library(bslib)            # Bootstrap styling

# Data manipulation and analysis
library(tidyverse)        # Data wrangling ecosystem
library(DT)
library(tmcn)

# Network and graph analysis
library(igraph)           # Network analysis
library(visNetwork)       # Interactive network visualization

# Visualization packages
library(ggplot2)          # Grammar of graphics
library(ggtext)           # Enhanced text rendering for ggplot2
library(ggforce)
library(ggrepel)          # Label positioning for ggplot2
library(ggridges)
library(fmsb)
library(viridisLite)      # Color scales for visualization

# Geospatial analysis
library(sf)               # Simple features for spatial data

# File export
library(writexl)          # Excel file export

# ------------------------------------------------------------------------------
# Application Configuration
# ------------------------------------------------------------------------------

# Set maximum file size
options(shiny.maxRequestSize = 100 * 1024^2)
options(bslib.precompiled = TRUE)
options(sass.cache = TRUE)
options(shiny.minified = TRUE)

# ------------------------------------------------------------------------------
# Data Mappings and Configuration
# ------------------------------------------------------------------------------

known_fields <- c("civil", "mechanical", "electrical", "mining", "chemical", "textile")

# ------------------------------------------------------------------------------
# Data Loading
# ------------------------------------------------------------------------------

world_1938 <- read_sf("CERD/world_1938.geojson")
china_1928 <- read_sf("china_tw_combined.geojson")
taiwan_1946 <- read_sf("taiwan_1946.geojson")

# Persons
CERD_persons <- read_csv("CERD/pelzer_cerd_180_persons_bio.csv", show_col_types = F) %>%
left_join(read_csv("CERD/pelzer_cerd_180_persons_societies.csv", show_col_types = F), by="person_id", relationship="many-to-many") %>%
left_join(read_csv("CERD/pelzer_cerd_180_persons_ids.csv", show_col_types = F), by="person_id", relationship="many-to-many") %>%
left_join(read_csv("CERD/pelzer_cerd_180_persons_names.csv", show_col_types = F), by="person_id", relationship="many-to-many") %>%
unique()
CERD_locations <-read_sf("CERD/pelzer_cerd_180_locations.geojson") %>%
rename(longlat=geometry)
CERD_degrees <-read_csv("CERD/pelzer_cerd_180_degrees.csv", show_col_types = F)
CERD_colleges <-read_csv("CERD/pelzer_cerd_180_colleges.csv", show_col_types = F)
CERD_jobs <-read_csv("CERD/pelzer_cerd_180_jobs.csv", show_col_types = F)
CERD_employers <-read_csv("CERD/pelzer_cerd_180_employers.csv", show_col_types = F)
CERD_sources <-read_csv("CERD/pelzer_cerd_180_persons_sources.csv", show_col_types = F)

# CERD-Taiwan
CERD_TW_persons <- read_csv("CERD_Taiwan/CERD_TW_persons_bio.csv", show_col_types = F) %>%
left_join(read_csv("CERD_Taiwan/CERD_TW_persons_societies.csv", show_col_types = F), by="person_id", relationship="many-to-many") %>%
left_join(read_csv("CERD_Taiwan/CERD_TW_persons_ids.csv", show_col_types = F), by="person_id", relationship="many-to-many") %>%
left_join(read_csv("CERD_Taiwan/CERD_TW_persons_names.csv", show_col_types = F), by="person_id", relationship="many-to-many") %>%
unique()
CERD_TW_locations <-read_sf("CERD_Taiwan/CERD_TW_locations.geojson") %>%
rename(longlat=geometry)
CERD_TW_degrees <-read_csv("CERD_Taiwan/CERD_TW_degrees.csv", show_col_types = F)
CERD_TW_jobs <-read_csv("CERD_Taiwan/CERD_TW_jobs.csv", show_col_types = F)
CERD_TW_employers <-read_csv("CERD_Taiwan/CERD_TW_employers.csv", show_col_types = F)

CERD_TW_sources <-read_csv("CERD_Taiwan/CERD_TW_persons_sources.csv", show_col_types = F)

CERD_persons <- CERD_persons %>%
rbind(CERD_TW_persons) %>%
unique()

CERD_sources <- CERD_sources %>%
rbind(CERD_TW_sources) %>%
unique()

CERD_locations <- CERD_locations %>%
rbind(CERD_TW_locations) %>%
unique()

CERD_degrees <- CERD_degrees %>%
rbind(CERD_TW_degrees) %>%
unique()

CERD_jobs <- CERD_jobs %>%
rbind(CERD_TW_jobs) %>%
unique()

CERD_employers <- CERD_employers %>%
rbind(CERD_TW_employers) %>%
unique()

rm(CERD_TW_persons)
rm(CERD_TW_sources)
rm(CERD_TW_employers)
rm(CERD_TW_jobs)
rm(CERD_TW_degrees)
rm(CERD_TW_locations)

# ==============================================================================
# UI DEFINITION
# ==============================================================================

ui <- fluidPage(
  useShinyjs(),
  tags$head(
    tags$script(HTML("
  $(document).on('click', '.college-link', function(e) {
    e.preventDefault();
    const cid = $(this).data('id');
    Shiny.setInputValue('clicked_college_id', cid, {priority: 'event'});
  });
  
  $(document).on('click', '.employer-link', function(e) {
    e.preventDefault();
    const eid = $(this).data('id');
    Shiny.setInputValue('clicked_employer_id', eid, {priority: 'event'});
  });
  
    $(document).on('click', '.subsidy-link', function(e) {
    e.preventDefault();
    const eid = $(this).data('id');
    Shiny.setInputValue('clicked_subsidy_id', eid, {priority: 'event'});
  });
  
      $(document).on('click', '.society-link', function(e) {
    e.preventDefault();
    const eid = $(this).data('id');
    Shiny.setInputValue('clicked_society', eid, {priority: 'event'});
  });
  
      $(document).on('click', '.entry-link', function(e) {
    e.preventDefault();
    const eid = $(this).data('id');
    Shiny.setInputValue('clicked_entry_id', eid, {priority: 'event'});
  });
  
  ")),
    
    tags$meta(charset = "UTF-8"),
    tags$meta(name = "author", content = "Thorben Pelzer"),
    tags$meta(name = "description", content = "CERD is a historical biographical database of engineers from Chinese-speaking regions."),
    tags$meta(property = "og:title", content = "Chinese Engineers Relational Database (CERD)"),
    tags$meta(property = "og:description", content = "CERD is a historical biographical database of engineers from Chinese-speaking regions."),
    tags$meta(property = "og:url", content = "https://dhi.ust.hk/cerd/"),
    tags$meta(property = "og:type", content = "website"),
    tags$meta(property = "og:image", content = "https://tp451.github.io/projects/cerd/cover_cerd.png"),
    
    # Favicon for standard browsers
    tags$link(rel = "icon", type = "image/x-icon", href = "favicon.ico"),
    # Mobile Web App Icons
    tags$link(rel = "apple-touch-icon", sizes = "180x180", href = "apple-touch-icon.png"),
    tags$link(rel = "icon", type = "image/png", sizes = "32x32", href = "favicon-32x32.png"),
    tags$link(rel = "icon", type = "image/png", sizes = "16x16", href = "favicon-16x16.png"),
    
    tags$script(HTML("
    $(document).on('shown.bs.collapse', function (e) {
      $(e.target).prev('.panel-heading').find('i.fa').removeClass('fa-plus').addClass('fa-minus');
    });
    $(document).on('hidden.bs.collapse', function (e) {
      $(e.target).prev('.panel-heading').find('i.fa').removeClass('fa-minus').addClass('fa-plus');
    });
  ")),
    
    # Custom CSS styling
    tags$style(HTML("
    
/* --- Force full-viewport width (robust fallback) --- */
 html, body {
   box-shadow: none !important;
     margin: 0;
     padding: 0;
     height: 100%;
     min-height: 100vh;
     width: 100%;
     box-sizing: border-box;
 }
/* Make sure common high-level containers won't limit width */
 #page-wrapper, .container, .container-fluid, .app-wrapper, .shiny-app {
     display: block !important;
    /* avoid inline-block shrinkage */
     width: 100% !important;
    /* always take full width */
     min-width: 100% !important;
     max-width: 100% !important;
    /* override any Bootstrap max-width */
     box-sizing: border-box;
}
/* If body is ever set to flex (your media queries do this), ensure the child grows */
 body {
    /* If you need body as flex for wide-aspect centering, keep it. If not, this keeps body a normal block. */
    /* display: flex;
     justify-content:center;
     */
 }
 body > #page-wrapper, body > .container, body > .container-fluid {
     flex: 1 1 auto;
    /* allow wrapper to expand inside a flex body */
     align-self: stretch;
 }
 
 h4 {
        font-size: 1.6rem;
        font-weight: 550;
 }
 
 .footer {
 font-family: 'Open Sans', -apple-system, BlinkMacSystemFont, Segoe UI, Roboto, Helvetica Neue, Arial, sans-serif !default;
        position: fixed;
        headings-font-weight: 300 !default;
        left: 0;
        bottom: 0;
        width: 100%;
        background-color: #6d94b3;
        color: #fff;
        text-align: center;
        padding: 10px;
        border-top: 1px solid #ccc;
        a,
a:link,
a:visited,
a:hover,
a:active {
  color: #fff !important;
  text-decoration: none; /* optional: removes underline */
}
 }
    
 @media(min-aspect-ratio: 16/9) {
     body {
         display: flex;
         justify-content: center;
    }
     #page-wrapper {
         max-width: calc(100vh * (16 / 9));
         width: 100%;
    }
 }
/*  .irs-from, .irs-to {
  display: none !important;
} */
/* Dynamic Heights */
 .dynamic-height {
     height: 90vh;
     margin: 0 auto;
    /* center horizontally */
}
 .dynamic-height-network {
     height: 85vh !important;
}
 .full-width-plot {
     width: 100% !important;
     padding: 0 !important;
     margin: 0 !important;
}
/* Scrollable Content */
 .scrollable-content {
     flex-grow: 1;
     overflow-y: auto;
     padding: 0;
     margin: 0;
     overflow-x: auto;
    /* allow horizontal scroll */
     -webkit-overflow-scrolling: touch;
    /* smooth on mobile */
}
/* Centered Content */
 .center-content {
     width: 100%;
     justify-content: top;
     justify-content: flex-start;
     align-items: center;
     margin: 0;
     padding: 0;
 }
 .center-content button {
  display: block;
  margin: 0;
 }
.well {
  box-sizing: border-box;
}
    
.center-content p {
  margin: .1em;
  padding: .1em;
}
/* Navbar Styling */
 .navbar {
     background-color: #6d94b3;
}
/* Hover Effect for Buttons */
 .btn-group-toggle > .btn:hover, .btn-group-toggle > .btn.active:hover, .btn-group-toggle > .btn:focus {
     background-color: #428bca !important;
     color: #FFFFFF !important;
}
/* Remove Padding and Margin between Fluid Rows */
 .row > .col-sm-4, .row > .col-sm-3, .row > .col-sm-6 {
    /* padding: 0.0% !important;
     */
     padding: 0% !important;
     margin: 0 !important;
}
 .row {
     margin-left: 0 !important;
     margin-right: 0 !important;
}
/* Customize Button */
 .well .btn {
     white-space: normal !important;
     word-break: break-word;
}
 .btn.checkbtn.btn-custom {
     font-size: 14px !important;
     line-height: 1 !important;
}
/* Body Background Color */
 body {
     background-color: #ffffff;
}
 small {
     font-size: 14px;
}
/* Mobile Responsive Design */
 @media (max-width: 768px) {
     .dynamic-height {
         height: auto !important;
         min-height: 300px;
    }
     .col-sm-3, .col-sm-4, .col-sm-6 {
         width: 100% !important;
         margin-bottom: 5px;
    }
     .navbar-brand {
         font-size: 12px !important;
    }
     .btn {
         font-size: 10px !important;
         padding: 2px 2px !important;
    }
     .well, .wellPanel {
         padding: 1px !important;
         margin: 1px 0 !important;
    }
}
/* Show plot by default, hide the message */
 #sfPlotUnavailable {
     display: none;
}
 #networkUnavailable {
     display: none;
     height: 100%;
     justify-content: center;
     align-items: center;
     display: flex;
}
/* On small screens, hide plot and show the message */
 @media (max-width: 1000px), (max-height: 500px) {
     #sfPlotWrapper {
         display: none;
    }
     #sfPlotUnavailable {
         display: block;
    }
}
/* On small screens: hide the network output container, show the message */
 @media (max-width: 768px) {
     #mynetworkid {
         display: none !important;
    }
     #networkUnavailable {
         display: flex !important;
    }
}
 @media (max-width: 480px) {
     .dynamic-height {
         height: auto !important;
         min-height: 250px;
    }
     .navbar-brand {
         font-size: 10px !important;
    }
     .btn {
         font-size: 10px !important;
         padding: 4px 8px !important;
    }
}
    
}
    "))
  ),
  title = "Chinese Engineers Relational Database (CERD)",
  theme = shinytheme("yeti"),
  navbarPage(id = "main_navbar",
  "⚙️ Chinese Engineers Relational Database (CERD) 2.0",
  tabPanel("Catalogue",
  fluidPage(
    fluidRow(
      # Left panel for queries
      column(3, div(class = "dynamic-height",
      wellPanel(
        
        div(
          style = "text-align: center;",
          h4(
            "Showing ",
            textOutput("total_count", inline = TRUE),
            " of over 29,000 engineers"
          )),
          div(
            style = "text-align: center; margin-bottom: 0.5em;",
            uiOutput("buttons")  # Dynamically generated button centered
          ),
          
          # Label row with title + tooltip grouped tightly, toggle aligned right
          div(
            style = "display: flex; align-items: center; justify-content: start; gap: 0.5em; margin-bottom: 0.5em;",
            
            # # Group title and tooltip together
            div(
              style = "display: flex; align-items: center; gap: 0.25em;",
              h4("Datasets"),
              tooltip(
                fontawesome::fa("info-circle", a11y = "sem",
                title = "CERD holds records of Republican-era China (1912–1949). CERD-Taiwan focuses on the post-1945 era. If an engineer is part of both datasets, all available information is displayed regardless the selection."),
                "CERD holds records of Republican-era China (1912–1949). CERD-Taiwan focuses on the post-1945 era. If an engineer is part of both datasets, all available information is displayed regardless the selection."
              ),
              div(style = "margin-top: 5px;",
              checkboxGroupButtons(
                inputId = "datasets",
                choices = c("️🔵️️ CERD" = "cerd",
                "🟢️ CERD-Taiwan" = "cerd-taiwan"
              ),
              size = "sm",
              selected=c("cerd"),
              status = "custom",
              checkIcon = list(
                yes = icon("ok",
                lib = "glyphicon"),
                no = icon("remove",
                lib = "glyphicon"))
              ))
            ),
          ),
                    
          # Label row with title + tooltip grouped tightly, toggle aligned right
          div(
            style = "display: flex; align-items: center; justify-content: start; gap: 0.5em; margin-bottom: 0.25em;",
                                   
            # Group title and tooltip together
            div(
              style = "display: flex; align-items: center; gap: 0.25em;",
              h4("Name of individual"),
            ),
            
            # Toggle with label, nudged up for baseline alignment
            div(
              style = "margin-top: 15px;",
              radioButtons(
                inputId = "query_name_logic_radio",
                label = NULL,
                inline = TRUE,
                choices = c("Any keyword" = "or", "All" = "and"),
                selected = "and"
              )
            )
          ),
          
          # Text input below
          div(
            style = "margin-top: -5px;",  # pulls input closer to the header group
            textInput("query_name", label = NULL, value = "", placeholder = "Pinyin or 中文 (or leave blank)")
          ),
          
          div(
            style = "display: flex; align-items: center; justify-content: start; gap: 0.5em; margin-bottom: 0.25em;",
            # Group title and tooltip together
            div(
              style = "display: flex; align-items: center; gap: 0.25em;",
              h4("Year of birth")
            ),
            # Toggle with label, nudged up for baseline alignment
            div(
              style = "margin-top: 5.5px;",
              checkboxInput("include_unknown_birthyear", "Include unknown", value = TRUE)
            )
          ),
          
          div(style = "margin-top: -20px;",
          sliderInput(
            inputId = "time_birth",
            label = "",
            sep="",
            min=1850,
            max=1950,
            value = c(1850,1950),
            step = 1)
            # ),
          ),
          
          div(
            style = "display: flex; align-items: center; justify-content: start; gap: 0.5em; margin-bottom: 0.25em;",
            # Group title and tooltip together
            div(
              style = "display: flex; align-items: center; gap: 0.25em;",
              h4("Fields")
            ),
            # Toggle with label, nudged up for baseline alignment
            div(
              style = "margin-top: 5.5px;",
              checkboxInput("select_all_fields", "All / none", value = TRUE)
            )
          ),
          
          div(style = "margin-top: -5px;",
          checkboxGroupButtons(
            inputId = "fields",
            choices = c("🏗️ Civil" = "civil",
            "🔧 Mechanical" = "mechanical",
            "💡️️ Electrical" = "electrical",
            "⛏️ Mining" = "mining",
            "🧪 Chemical" = "chemical",
            "🧵️ Textile" = "textile",
            "🧩️️ Other" = "other"
          ),
          size = "sm",
          selected=c("civil","mechanical","textile","chemical","electrical","other","mining"),
          status = "custom",
          checkIcon = list(
            yes = icon("ok",
            lib = "glyphicon"),
            no = icon("remove",
            lib = "glyphicon"))
          )),
          
          div(
            style = "display: flex; align-items: center; justify-content: start; gap: 0.5em; margin-bottom: 0.25em;",
            # Group title and tooltip together
            div(
              style = "display: flex; align-items: center; gap: 0.25em;",
              h4("Year of graduation")
            ),
            # Toggle with label, nudged up for baseline alignment
            div(
              style = "margin-top: 5.5px;",
              checkboxInput("include_unknown_graduation", "Include unknown", value = TRUE)
            )
          ),
          
          # div(h4("Year of graduation", style = "margin: 0;")),
          div(style = "margin-top: -20px;",
          sliderInput(
            inputId = "time_graduation",
            label = "",
            sep="",
            min=1870,
            max=1970,
            value = c(1870,1970),
            step = 1)
          ),
          
          # Label row with title + tooltip grouped tightly, toggle aligned right
          div(
            style = "display: flex; align-items: center; justify-content: start; gap: 0.5em; margin-bottom: 0.25em;",
            
            # Group title and tooltip together
            div(
              style = "display: flex; align-items: center; gap: 0.25em;",
              h4("Native places")
            ),
            
            # Toggle with label, nudged up for baseline alignment
            div(
              style = "margin-top: 15px;",
              radioButtons(
                inputId = "query_jiguan_logic_radio",
                label = NULL,
                inline = TRUE,
                choices = c("Any keyword" = "or", "All" = "and"),
                selected = "or"
              )
            )
          ),
          
          div(
            style = "margin-top: -5px;",  # pulls input closer to the header group
            textInput("query_jiguan", label = NULL, value = "", placeholder = "Pinyin or 中文 (or leave blank)")
          ),

          # Label row with title + tooltip grouped tightly, toggle aligned right
          div(
            style = "display: flex; align-items: center; justify-content: start; gap: 0.5em; margin-bottom: 0.25em;",
            
            # Group title and tooltip together
            div(
              style = "display: flex; align-items: center; gap: 0.25em;",
              h4("Colleges")
            ),
            
            # Toggle with label, nudged up for baseline alignment
            div(
              style = "margin-top: 15px;",
              radioButtons(
                inputId = "query_college_logic_radio",
                label = NULL,
                inline = TRUE,
                choices = c("Any keyword" = "or", "All" = "and"),
                selected = "and"
              )
            )
          ),
          
          # Text input below
          div(
            style = "margin-top: -5px;",  # pulls input closer to the header group
            textInput("query_college", label = NULL, value = "", placeholder = "English or 中文 (or leave blank)")
          ),
          
          # Label row with title + tooltip grouped tightly, toggle aligned right
          div(
            style = "display: flex; align-items: center; justify-content: start; gap: 0.5em; margin-bottom: 0.25em;",
            
            # Group title and tooltip together
            div(
              style = "display: flex; align-items: center; gap: 0.25em;",
              h4("Employer and/or job title")
            ),
            
            # Toggle with label, nudged up for baseline alignment
            div(
              style = "margin-top: 15px;",
              radioButtons(
                inputId = "query_employer_logic_radio",
                label = NULL,
                inline = TRUE,
                choices = c("Any keyword" = "or", "All" = "and"),
                selected = "and"
              )
            )
          ),
          
          # Text input below
          div(
            style = "margin-top: -5px;",  # pulls input closer to the header group
            textInput("query_employer", label = NULL, value = "", placeholder = "English or 中文 (or leave blank)")
          ),
          
          div(
            style = "display: flex; align-items: center; justify-content: start; gap: 0.5em; margin-bottom: 0.5em;",
            # Group title and tooltip together
            div(
              style = "display: flex; align-items: center; gap: 0.3em;",
              h4("Genders")
            ),
            
            div(style = "margin-top: 6px;",
            checkboxGroupButtons(
              inputId = "genders",
              choices = c("♂ Male" = "male",
              "♀ Female" = "female",
              "◆️ Unknown" = "unknown"),
              size = "sm",
              selected=c("male","female","unknown"),
              status = "custom",
              checkIcon = list(
                yes = icon("ok",
                lib = "glyphicon"),
                no = icon("remove",
                lib = "glyphicon"))
              )
            )
          ),
          
          div(
            style = "display: flex; align-items: center; justify-content: start; gap: 0.5em; margin-bottom: 0em;",
            # Group title and tooltip together
            div(
              style = "display: flex; align-items: center; gap: 0.3em;",
              h4("Search by ID")
            ),
            
            div(
              style = "margin-top: 6px;",  # pulls input closer to the header group
              textInput("query_id", label = NULL, value = "", placeholder = "")
            )
          ),
          
          tags$script(HTML("
                                            $(document).on('keyup', function(e) {
                                            if (e.which == 13 && $(e.target).is('#query_name, #query_jiguan, #query_college, #query_employer, #query_id')) {  // Check if Enter is released inside the textInput
                                            $('#go').click();
                                            }
                                            });
                                          "))
        ))
      ),
      
      column(9, 
        tabsetPanel(id = "main_tabs",
        tabPanel("Engineers", value="Engineers", 
        column(8,
          div(class = "center-content scrollable-content",
          wellPanel(class = "center-content  scrollable-content",
          withSpinner(DT::dataTableOutput("dataTablePerson", width = "100%"))
        ))),
        column(4, div(class = "center-content",
        wellPanel(div(style="text-align: center",
        p(downloadButton("download_excel_person", "Save selection (Excel)"),
        downloadButton("download_csv_person", "Save selection (CSV)")))
      ),
      wellPanel(
        uiOutput("dynamic_ui_person")
      )
    )
  ),
),
tabPanel("Colleges", value = "Colleges",
column(8,
  div(class = "center-content scrollable-content",
  wellPanel(class = "center-content scrollable-content",
  withSpinner(DT::dataTableOutput("dataTableCol", width = "100%"))
))),
column(4, div(class = "center-content",
wellPanel(div(style="text-align: center",
p(downloadButton("download_excel_col", "Save selection (Excel)"),
downloadButton("download_csv_col", "Save selection (CSV)")))
),wellPanel(
  uiOutput("dynamic_ui_cols")
)
)
),
),
tabPanel("Employers", value = "Employers",
column(8,
  div(class = "center-content scrollable-content",
  wellPanel(class = "center-content scrollable-content",
  withSpinner(DT::dataTableOutput("dataTablePub", width = "100%"))
))),
column(4, div(class = "center-content",
wellPanel(div(style="text-align: center",
p(downloadButton("download_excel_pub", "Save selection (Excel)"),
downloadButton("download_csv_pub", "Save selection (CSV)")))
),wellPanel(
  uiOutput("dynamic_ui_pubs")
)
)
),
),
tabPanel("Maps", div(class = "center-content full-width-plot",
fluidRow(
  column(8,
    div(class = "center-content scrollable-content",
    wellPanel(
      withSpinner(plotOutput("barChart_region", width = "100%", height = "50vh"))
    )
  )
),
column(4,
  div(class = "center-content scrollable-content",
  wellPanel(
    withSpinner(plotOutput("barChart_region_taiwan", width = "100%", height = "50vh"))
  )
)
)
),
fluidRow(
  column(12,
    div(class = "center-content scrollable-content",
    wellPanel(
      withSpinner(plotOutput("barChart_region_world", width = "100%", height = "50vh"))
    )
  )
),
)

)),
tabPanel(
  "Network",
  column(
    8,
    div(
      class = "center-content",
      wellPanel(
        class = "dynamic-height-network center-content",
        
        visNetworkOutput("mynetworkid", height = "95%"),
        div(
          id = "networkUnavailable",
          style = "display: none; text-align: center; font-weight: bold;",
          "Network graph is not available on small display resolutions."
        )
      )
    )
  ),
  column(
    4,
    div(
      class = "center-content",
      wellPanel(
        uiOutput("dynamic_ui_netzwerk")
      )
    )
  )
),

tabPanel("Statistics",
# First row
fluidRow(
  column(6,
    div(class = "center-content scrollable-content",
    wellPanel(
      withSpinner(plotOutput("barChart_birthyear", width = "100%", height = "38vh"))
    )
  )
),
column(6,
  div(class = "center-content scrollable-content",
  wellPanel(
    withSpinner(plotOutput("barChart_fields", width = "100%", height = "38vh"))
  )
)
)
),
fluidRow(
  column(6,
    div(class = "center-content scrollable-content",
    wellPanel(
      withSpinner(plotOutput("barChart_jobs", width = "100%", height = "38vh"))
    )
  )
),
column(6,
  div(class = "center-content scrollable-content",
  wellPanel(
    withSpinner(plotOutput("barChart_activity", width = "100%", height = "38vh"))
  )
)
)
),

)
),

),
),
)),

tabPanel("Publications",

fluidRow(
  column(12, div(class = "dynamic-height scrollable-content",
  wellPanel(tags$small(HTML(paste0(
    
    "
    <p><h4>Publications</h4></p>
                                         <p>
                                         <a href=\"https://doi.org/10.1080/18752160.2025.2546742\" target=\"_blank\"><img height=200px src=\"pub_societies.jpg\" alt=\"Publication cover\"></a>
                                         <a href=\"https://doi.org/10.1515/9783111374437-004\" target=\"_blank\"><img height=200px src=\"pub_flux.jpg\" alt=\"Publication cover\"></a>
                                         <a href=\"https://doi.org/10.1353/tcc.2025.a950426\" target=\"_blank\"><img height=200px src=\"pub_twentieth.jpg\" alt=\"Publication cover\"></a>
                                         <a href=\"https://doi.org/10.1163/9789004549555\" target=\"_blank\"><img height=200px src=\"pub_engineering.jpg\" alt=\"Publication cover\"></a>
                                         <a href=\"https://www.univerlag-leipzig.de/catalog/bookstore/article/2069-Chinese_Engineers_Relational_Database_CERD_Design_Manual\" target=\"_blank\"><img height=200px src=\"pub_manual.jpg\" alt=\"Publication cover\"></a>
                                         <a href=\"https://doi.org/10.1353/tech.2022.0154\" target=\"_blank\"><img height=200px src=\"pub_spinning.jpg\" alt=\"Publication cover\"></a>
                                         </p>
                                         <p><b>Discussing the database</b>
                                         <ul>
                                         <li>\"[...] a groundbreaking example of applying modern social sciences methodologies to Chinese history.\" —Ben Kletzer, <i>Journal of Chinese History</i>, 2025.</li>
                                         <li>Ren, Bamboo Yunzhu. <i>Professional Education and Employment in China: Health and Engineering, 1905–1952</i>. Diss. Hong Kong University of Science and Technology (Hong Kong), 2025.</li>
                                         <li>Pelzer, Thorben. <i>Chinese Engineers Relational Database (CERD) Design Manual</i>. Leipziger Universitätsverlag, 2020.</li>
                                         </ul></p>
                                         <p><b>Using the database</b>
                                         <ul>
                                         <li>Pelzer, Thorben. \"Engineering Societies in China: Spaces of Professionalization and Participation, 1912–1949.\" <i>East Asian Science, Technology and Society: An International Journal</i> (2025).</li>
                                         <li>Pelzer, Thorben. \"Engineers on the Move: Elite Geographic Mobility in Republican China.\" <i>Twentieth-Century China</i> 50.1 (2025): 25–55.</li>
                                         <li>Pelzer, Thorben. \"Technocracy and Technostructure in Nationalist China.\" <i>Modern China in Flux: Networks, Mobility, and Transformation</i> (2025): 99–134.</li>
                                         <li>Pelzer, Thorben. <i>Engineering Trouble: US–Chinese Experiences of Professional Discontent, 1905–1945</i>. China Studies Vol. 52. Brill, 2023.</li>
                                         <li>Yi, Yuan. \"Crafted for Mass Production: Imported Spinning Machinery on the Shop Floor, China, 1910s–1920s.\" <i>Technology and Culture</i> 63.4 (2022): 979–1004.</li>
                                         </ul>
                                         </p>
                                         <p><b>Raw data download</b></p>
                                         <p>The raw data is available in a repository under doi <a href=\"http://doi.org/10.5281/zenodo.4075601\" target=\"_blank\">10.5281/zenodo.4075601</a>.</p>
                                         <p><b>Legacy interface</b></p>
                                         <p>The legacy online interface, refered to in earlier publications, remains available <a href=\"https://home.uni-leipzig.de/cerd/index.php\" target=\"_blank\">here</a>. The data is no longer up to date.</p> 
                                         "
  )))),
  
)),
)

),

tabPanel("Sources",

fluidRow(
  column(12, div(class = "dynamic-height scrollable-content",
  wellPanel(tags$small(HTML(paste0(
    "<p><h4>Sources</h4></p>
                                         <p><b>CERD v.1.8</b></p>
<p>Primary</p>
<ul>
<li>
Chinese Who's Who (1944):
Tseng, H. P., and Jean Lyon, eds. <i>China Handbook 1937–1944: A Comprehensive Survey of Major Developments in China in Seven Years of War</i>. Chongqing: Chinese Ministry of Information, 1944: 555–626.
</li><li>
Diangong (1947):
“Zhōngguó diànjī gōngchéngshī xuéhuì huìyuán lù.” 中國電機工程師學會會員錄 <i>Diàngōng</i> 16, no. 1 (1947): 85–91.
</li><li>
Dongli gongcheng (1947):
“Zhōngguó dònglì gōngchéng xuéhuì huìyuán diàochá biǎo.” 中國動力工程學會會員調查表 <i>Dònglì gōngchéng jìkān</i> 1, 1, 4 (1947): 100, 196.
</li><li>
Gongchengren minglu (1941):
Zīyuán wěiyuánhuì, 資源委員會. <i>Zhōngguó gōngchéngrén mínglù.</i> 中國工程人名錄. Changsha: Shāngwù yìnshūguǎn, 1941.
</li><li>
Gongcheng xuebao (1925):
“Directory of Nanyang Alumni Association in America.” <i>Gōngchéng xuébào</i> 1, no. 1 (1925): 130–34.
</li><li>
Gongcheng xuehui huibao (1919):
“Zhōngguó gōngchéng xuéhuì huìyuán lù.” 中國工程學會會員錄 <i>Zhōngguó gōngchéng xuéhuì huìbào</i> 1 (1919): 49–65.
</li>
<li>
Guoli Zhongyang daxue fuxiao (1945):
<i>Guólì Zhōngyāng dàxué fùxiào dì èr jiè bìyè jìniàn kān</i>. 國立中央大學復校第二屆畢業紀念刊. Nanjing: Nanjing Central University, 1945.
</li>
<li>
Jiaotong (1922):
Jiāotōng dàxué 交通大學, ed. <i>Jiāotōng dàxué bìyè jìniàn cè</i>. 交通大學畢業紀念冊. Shanghai: s. n., 1922.
</li><li>
Journal of the ACAE (1927):
“List of Members.” <i>The Oriental Engineer</i> 8, no.2 (1927): 31–44.
</li><li>
Journal of the ACAE (1938):
“List of Members.” <i>Journal of the Association of Chinese & American Engineers</i> 19, no.2 (1938): 95–102.
</li><li>
Kuangye gongcheng xuehui huiyuan lu (1943):
Zhōngguó kuàngyě gōngchéng xuéhuì, 中國礦冶工程學會, ed. <i>Huìyuán lù</i>. 會員錄. Chongqing: Zhōngguó kuàngyě gōngchéng xuéhuì, 1943.
</li>
<li>
Nanjing zhongyang daxue xiaoyou tongxun (1989):
Zhōngyāng dàxué xiàoyǒu huì Nánjīng (1940–1945) xiàoyǒu liánluò chù. <i>Nánjīng zhōngyāng dàxué (1940–1945) xiàoyǒu tōngxùn.</i> 南京中央大學（1940–1945）校友通訊. Nanjing: s. n., 1989.
</li>
<li>
Qinghua daxue tumu gongcheng xuehui huikan (1944):
“Huìyuán lù.” 會員錄 <i>Guólì Qīnghuá dàxué tǔmù gōngchéng xuéhuì</i> 6 (1944): 79–95.
</li><li>
Qinghua daxue tumu gongcheng xuehui huikan (1949):
“Huìyuán lù.” 會員錄 <i>Guólì Qīnghuá dàxué tǔmù gōngchéng xuéhuì</i> 7 (1949): 11–14.
</li><li>
Shuili (1933):
“Zhōngguó shuǐlì gōngchéng xuéhuì huìyuán lù.” 中國水利工程學會會員錄 <i>Shuǐlì</i> 5, no. 4 (1933): 49–53.
</li><li>The Chinese Students Directory (1931):
Koo, Eugene Chen, ed. <i>Chinese Students Directory for the Past Fifty Years</i>. Cambridge: Massachusetts Institute of Technology, 1931.
</li><li>
Tumu gongcheng (1935):
“Běn huì lìjiè bìyè huìyuán yīlǎn.” 本會歷届畢業會員一覽 <i>Tǔmù gōngchéng</i> 3, no. 1 (1935): 197–201.
</li><li>
Who's Who in China (1936):
The China Weekly Review. <i>Who's Who in China. Biographies of Chinese Leaders.</i> 5th ed. Shanghai: The China Weekly Review, 1936.
</li><li>
Zhongguo gongchengshi xuehui Wuhan fenhui (1933):
Zhōngguó gōngchéngshī xuéhuì wǔhàn fēnhuì 中國工程師學會武漢分會, ed. <i>Zhōngguó gōngchéngshī xuéhuì Wǔhàn fēnhuì huìyuán lù</i> 中國工程師學會武漢分會會員錄. Wuhuan: s. n., 1933.
</li><li>
Zhongguo ranhua jinian kan (1940):
“Huìyuán lù.” 會員錄 <i>Zhōngguó rǎnhuà gōngchéng xuéhuì chénglì jìniàn kān</i> 2 (1940): 115–124.
</li><li>
Zhonghua gongchengshi xuehui huiyuan lu (1924):
Zhōnghuá gōngchéngshī xuéhuì 中華工程師學會, ed. <i>Zhōnghuá gōngchéngshī xuéhuì huìyuán lù.</i> 中華工程師學會會員錄. S. l.: Zhōnghuá gōngchéngshī xuéhuì, 1924.
</li></ul></p>
<p>Supplements
<ul><li>
Freeman Papers at MIT (1916–1932):
Papers of John Ripley Freeman. MC.0051. MIT Libraries Distinctive Collections.
</li><li>
Jiaoda jikan (1930):
Waddell, John Alexander Low. “In Memoria[m]. K. Y. Kwong.” <i>Jiāodà jìkān</i> no. 3 (1930): 79–83.
</li><li>
Journal of the ACAE (1924):
“Yang Pao-ling.” <i>Journal of the Association of Chinese & American Engineers</i> 5, no.4 (1924): i–ii.
</li><li>
Journal of the ACAE (1928):
“The New President.” <i>Journal of the Association of Chinese & American Engineers</i> 9, no.1 (1928): i.
</li><li>
Ling Hongxun Papers (1962):
Líng, Hóngxūn 淩鴻勛. Hung-Hsun Ling Papers, Duì rì kàngzhàn bā nián jiāotōng dàshìjì chūgǎo. 對日抗戰八年交通大事記初稿. 1962, MS#0786. Columbia University Archival Collections.
</li><li>
Oliver Julian Todd Papers (1899–1973):
O. J. (Oliver Julian) Todd papers. 73087. Hoover Institution Archives.
</li></ul></p>
                                         <p><b>CERD-Taiwan v.1.0</b></p>
                                         <p><ul>
                                         <li>CIE 1947: “Zhōngguó gōngchéngshī xuéhuì táiwān fēnhuì huìyuán lù.” 中國工程師學會臺灣分會會員錄 <i>Táiwān gōngchéng jiè</i> 1, no. 1 (1947): 21–24.</li>
                                         <li>CIE 1953: Zhōngguó gōngchéngshī xuéhuì 中國工程師學會, ed. <i>Zhōngguó gōngchéngshī xuéhuì huìyuán lù</i> 中國工程師學會會員錄. Taipei: s.n., 1953.</li>
                                         <li>CIE 1958: Zhōngguó gōngchéngshī xuéhuì 中國工程師學會, ed. <i>Zhōngguó gōngchéngshī xuéhuì yīlǎn</i> 中國工程師學會一覽. S.l.: s.n., 1958.</li>
                                         <li>CIE 1974: Zhōngguó gōngchéngshī xuéhuì 中國工程師學會, ed. <i>Zhōngguó gōngchéngshī xuéhuì huìyuán lù</i> 中國工程師學會會員錄. Taipei: s.n., 1974.</li>
                                         <li>Taiwan Chemical Engineers 1954: Zhōngguó huàxué gōngchéng xuéhuì 中國化學工程學會, ed.<i>Huàxué gōngchéng xuéhuì huìyuán lù</i> 化學工程學會會員錄. S.l.: s.n., 1954.</li>
                                         <li>Taiwan Mechanical Engineers 1964: Zhōngguó jīxiè gōngchéng xuéhuì 中國機械工程學會, ed. <i>Zhōngguó jīxiè gōngchéng xuéhuì huìyuán lù</i> 中國機械工程學會會員錄. Taipei: s.n., 1964.</li>
                                         </ul></p>
                                         "
  )))),
  
)),
)

),

tabPanel("Credits",

fluidRow(
  column(12, div(class = "dynamic-height scrollable-content",
  wellPanel(tags$small(HTML(paste0(
    "<p><img height=55px src=\"logo_hkust.png\" alt=\"HKUST logo\"><img height=95px src=\"logo_lu.png\" alt=\"LU logo\"></p>
    
                                  <p><b>Project lead (Concept, code, oversight)</b>
                                  <ul><li><a href=\"https://pelzer.blog\" target=\"_blank\">Thorben Pelzer</a> (Hong Kong University of Science and Technology)</li></ul></p>
                                 
                                  <p><b>Research funding</b>
                                  <ul><li>“Chinese Engineers and their Spatial Imaginations”</br>
                                  Grant SFB-1199-A6 awarded by DFG (2020–2024)</br>
                                  Principal investigator:	<a href=\"https://www.uni-leipzig.de/personenprofil/mitarbeiter/prof-dr-elisabeth-kaske\" target=\"_blank\">Elisabeth Kaske</a></br>
                                  Postdoctoral researcher: Thorben Pelzer</li>
                                  <li>“The Pioneers of China’s Rise to Technological Power”</br>
                                  Grant 01UL1909X awarded by BMFTR (2019–2022)</br>
                                  Principal investigator:	<a href=\"https://www.oaw.ruhr-uni-bochum.de/gc/team/Hailian_Chen.html.en\" target=\"_blank\">Hailian Chen</a>
                                  </li></ul></p>
                                 
                                 <p><b>Student assistants</b>
                                 <ul>
                                 <li><a href=\"https://www.sin-aps.fau.de/people/gus-tsz-kit-chan/\" target=\"_blank\">Tsz-kit Gus Chan</a></li>
                                 <li>Chun Yeung Chung</li>
                                 <li><a href=\"https://www.linkedin.com/in/oliver-dieckmann-b24392132\" target=\"_blank\">Oliver Dieckmann</a></li>
                                 <li>Ariane Kolden</li>
                                 <li><a href=\"https://orcid.org/0000-0002-8155-8844\" target=\"_blank\">Chin Fung Ng</a></li>
                                 <li>Felix Opper</li>
                                 <li>Christian Staudte</li>
                                 <li>Mirjam Stretz</li>
                                 <li>Mo Wang</li>
                                 </ul></p>
                                 
                                 <p><b>Special thanks</b>
                                 <ul>
                                 <li>Christian Henriot <i>(online basemap)</i></li>
                                 <li>Jonathan Henshaw <i>(data provision)</i></li>
                                 <li>Ian Johnson <i>(original data input realised with <a href=\"https://heurist.huma-num.fr/Heurist_Contacts/web/\" target=\"_blank\">Heurist</a>)</i></li>
                                 <li>Elisabeth Köll <i>(data provision)</i></li>
                                 <li>André Ourednik <i>(online basemap under <a href=\"https://www.gnu.org/licenses/gpl-3.0.html\" target=\"_blank\">GPL-3.0</a>)</i></li>
                                 </ul></p>
                                                                   
                                 <p><b>Project contact</b>
                                 <ul><li>Thorben Pelzer</br>
                                  Assistant Professor</br>
                                  3351 Academic Building, HKUST</br>
                                  Clear Water Bay, Hong Kong</br>
                                  Tel.: +852 2358-8908</li></ul></p>
                                  
                                  <p><b>Citation</b>
                                  <ul><li>Pelzer, Thorben, et al., eds. (2022). “Chinese Engineers Relational Database (CERD).” Geneva, Budapest: Zenodo. <a href=\"http://doi.org/10.5281/zenodo.4075601\" target=\"_blank\">http://doi.org/10.5281/zenodo.4075601</a>.</li>
                                 <li>Data available under <a href=\"https://creativecommons.org/licenses/by/4.0/legalcode\" target=\"_blank\">Creative Commons Attribution 4.0 International</a> licence: Citation required.</li>
                                 <li>BibTeX file available <a href=\"citation.bib\"  download=\"citation.bib\">here</a>.</li></ul></p>
                                 
                                 <p><img height=75px src=\"logo_sfb.png\" alt=\"SFB logo\"><img height=80px src=\"logo_dfg.png\" alt=\"DFG logo\"><img height=85px src=\"logo_bmftr.png\" alt=\"BMFTR logo\"></p>
                                 
                                 "
  )))),
  
)),
)

)

),
tags$footer(
  class = "footer",
  HTML("Pelzer, Thorben, et al., eds. (2020–2025). “Chinese Engineers Relational Database (CERD).” Geneva, Budapest: Zenodo. <a href=\"http://doi.org/10.5281/zenodo.4075601\" target=\"_blank\">http://doi.org/10.5281/zenodo.4075601</a>."
)))

# ==============================================================================
# SERVER LOGIC
# ==============================================================================
server <- function(input, output, session) {
  
  expanded_fields <- reactive({
    unlist(lapply(input$fields, function(x) {
      switch(x,
        civil = c("civil", "hydraulic"),
        mechanical = "mechanical",
        electrical = c("electric","telecomm","radio"),
        mining = "mining",
        chemical = c("chemical","chemistry"),
        textile = "textile",
        other = "other"
      )
    }))
  })
  
  rv <- reactiveValues(reset_done = FALSE)
  # Reactive values for data filtering and state management
  filtered_persons <- reactiveVal(data.frame())
  filtered_fields <- reactiveVal()
  filtered_locations <- reactiveVal()
  filtered_colleges <- reactiveVal()
  filtered_employers <- reactiveVal()
  filtered_degrees <- reactiveVal()
  filtered_jobs <- reactiveVal()
  
  selected_node <- reactiveVal(NULL)
  selected_node_col <- reactiveVal(NULL)
  selected_node_pub <- reactiveVal(NULL)
  
  # User interaction tracking
  clicked_person <- reactiveVal()
  clicked_organization <- reactiveVal()
  has_started <- reactiveVal()
  has_refiltered <- reactiveVal()
  
  # Visualization data
  plotted_points_var <- reactiveVal()
  plotted_points_countries_var <- reactiveVal()
  network_edges_shared <- reactiveVal()
  network_nodes_shared <- reactiveVal()
  
  first_run <- reactiveVal(TRUE)
  
  output$buttons <- renderUI({
    div(
      style = "display: flex; gap: 10px; justify-content: center;",  # horizontal layout
      actionBttn(
        inputId = "reset_button",
        label = HTML("🔄 Reset"),
        style = "simple", 
        color = "warning"
      ),
      actionBttn(
        inputId = "go",
        label = HTML("🔍 Apply filter"),
        style = "simple", 
        color = "primary"
      )
    )
  })
  
  observeEvent(input$reset_button, {
    # Reset text inputs
    updateTextInput(session, "query_name", value = "")
    updateTextInput(session, "query_jiguan", value = "")
    updateTextInput(session, "query_college", value = "")
    updateTextInput(session, "query_employer", value = "")
    updateTextInput(session, "query_id", value = "")
    
    # Reset sliders
    updateSliderInput(session, "time_birth", value = c(1850, 1950))
    updateSliderInput(session, "time_graduation", value = c(1870, 1970))
    
    # Reset checkbox groups
    updateCheckboxGroupButtons(session, "fields", selected = c("civil","mechanical","textile","chemical","electrical","other","mining"))
    updateCheckboxGroupButtons(session, "genders", selected = c("male","female","unknown"))
    updateCheckboxGroupButtons(session, "datasets", selected = "cerd")
    
    # Reset single checkboxes / radios
    updateCheckboxInput(session, "include_unknown_birthyear", value = TRUE)
    updateCheckboxInput(session, "include_unknown_graduation", value = TRUE)
    updateCheckboxInput(session, "select_all_fields", value = TRUE)
    
    updateRadioButtons(session, "query_name_logic_radio", selected = "and")
    updateRadioButtons(session, "query_jiguan_logic_radio", selected = "or")
    updateRadioButtons(session, "query_college_logic_radio", selected = "and")
    updateRadioButtons(session, "query_employer_logic_radio", selected = "and")
    
    
    rv$reset_done <- FALSE
    later::later(function() { rv$reset_done <- TRUE }, 0.3)
  })
  
  observeEvent(rv$reset_done, {
    req(rv$reset_done)
    filter_data()
    has_started("T")
  })
  
  # Wherever you detect a node click (e.g., in observeEvent)
  observeEvent(input$dataTablePerson_rows_selected, {
    filtered_data <- filtered_persons()
    
    if (length(input$dataTablePerson_rows_selected) > 0) {
      clicked <- filtered_data %>%
      select(person_id, familyname, givenname, hanzi, birthyear, location_id, gender) %>%
      unique() %>%
      arrange(familyname, givenname, hanzi, birthyear, location_id) %>%
      slice(input$dataTablePerson_rows_selected)
      
      selected_node(clicked)
    } else {
      selected_node(NULL)
    }
  })
  
  # Wherever you detect a node click (e.g., in observeEvent)
  observeEvent(input$dataTablePub_rows_selected, {
    filtered_data <- filtered_employers()
    
    if (length(input$dataTablePub_rows_selected) > 0) {
      clicked <- filtered_data %>%
      select(n, employer_id, employer_name, location_id) %>%
      unique() %>%
      arrange(-n, employer_name, location_id) %>%
      slice(input$dataTablePub_rows_selected)
      
      selected_node_pub(clicked)

    } else {
      selected_node_pub(NULL)
    }
  })
  
  # Wherever you detect a node click (e.g., in observeEvent)
  observeEvent(input$dataTableCol_rows_selected, {
    filtered_data <- filtered_colleges()
    
    if (length(input$dataTableCol_rows_selected) > 0) {
      clicked <- filtered_data %>%
      select(n, college_id, college_name, location_id) %>%
      unique() %>%
      arrange(-n, college_name, location_id) %>%
      slice(input$dataTableCol_rows_selected)
      
      selected_node_col(clicked)

    } else {
      selected_node_col(NULL)
    }
  })
  
  observeEvent(input$select_all_fields, {
    all_choices <- c("civil", "mechanical", "electrical", "mining", "textile", 
    "chemical","other","NA")
    
    updateCheckboxGroupButtons(
      session,
      inputId = "fields",
      selected = if (input$select_all_fields) all_choices else character(0)
    )
  }, ignoreInit = TRUE)  # <-- this prevents firing on load
  
  observeEvent(input$select_all_datasets, {
    all_choices <- c("cerd", "cerd-taiwan")
    
    updateCheckboxGroupButtons(
      session,
      inputId = "datasets",
      selected = if (input$select_all_datasets) all_choices else character(0)
    )
  }, ignoreInit = TRUE)  # <-- this prevents firing on load
  
  observeEvent(input$select_all_genders, {
    
    all_choices <- c("male", "female", "unknown")
    
    updateCheckboxGroupButtons(
      session,
      inputId = "genders",
      selected = if (input$select_all_genders) all_choices else character(0)
    )
  }, ignoreInit = TRUE)  # <-- this prevents firing on load
  
  make_network_nodes <- function() {
    filtered_persons() %>%
    mutate(id = paste0(person_id, "_person"), title = paste(familyname,givenname,hanzi), label = "", group = "person") %>%
    select(group, title, id, label) %>%
    rbind(
      filtered_colleges() %>%
      mutate(id = paste0(college_id, "_college"), title = college_name, label = "", group = "college") %>%
      select(group, title, id, label) %>%
      unique()
    ) %>%
    rbind(
      filtered_employers() %>%
      mutate(id = paste0(employer_id, "_employer"), title = employer_name, label = "", group = "employer") %>%
      select(group, title, id, label)
    ) %>%
    rbind(
      CERD_locations %>%
      st_drop_geometry() %>%
      select(place_name, location_id) %>%
      filter(location_id %in% filtered_persons()$location_id) %>%
      mutate(id = paste0(location_id, "_location"), title = place_name, label = "", group = "location") %>%
      select(group, title, id, label)
    ) %>%
    unique() %>%
    group_by(id) %>%
    slice(1) %>%
    ungroup() %>%
    drop_na()
  }
  
  make_network_edges <- function(nodes_df) {
    filtered_persons() %>%
    rename(from = person_id, to = location_id) %>%
    select(from, to) %>%
    drop_na() %>%
    mutate(from = paste0(from, "_person"), to = paste0(to, "_location")) %>%
    rbind(
      filtered_degrees() %>%
      rename(from = person_id, to = college_id) %>%
      select(from, to) %>%
      mutate(from = paste0(from, "_person"), to = paste0(to, "_college"))
    ) %>%
    rbind(
      filtered_jobs() %>%
      rename(from = person_id, to = employer_id) %>%
      select(from, to) %>%
      drop_na() %>%
      mutate(from = paste0(from, "_person"), to = paste0(to, "_employer"))
    ) %>%
    rbind(
      filtered_employers() %>%
      filter(is.na(parent_id)==F) %>%
      rename(from = employer_id, to = parent_id) %>%
      select(from, to) %>%
      drop_na() %>%
      mutate(from = paste0(from, "_employer"), to = paste0(to, "_employer"))
    ) %>%
    drop_na() %>%
    filter(from != to, from %in% nodes_df$id, to %in% nodes_df$id) %>%
    unique()
  }
  
  # ------------------------------------------------------------------------------
  # Network Graph Output  
  # ------------------------------------------------------------------------------
  
  output$total_count <- renderText({
    format(total_rows(), big.mark = ",", scientific = FALSE)
  })
  
  output$mynetworkid <- renderVisNetwork({
    
    if (total_rows() >= 2000) {
      return(visNetwork(
        nodes = data.frame(id = "msg_large", label = "Network too large to compute (>2,000 engineers). Please limit your query."),
        edges = data.frame()
      ) %>%
      visOptions(highlightNearest = FALSE, nodesIdSelection = FALSE) %>%
      visInteraction(dragNodes = FALSE, dragView = FALSE, zoomView = FALSE))
    }
    
    g_subgraph <- g_sub()
    req(g_subgraph)
    
    if (vcount(g_subgraph) > 4000) {
      return(visNetwork(
        nodes = data.frame(id = "msg_dense", label = "Network computed, but too dense to visualise (>4,000 nodes). Please limit your query."),
        edges = data.frame()
      ) %>%
      visOptions(highlightNearest = FALSE, nodesIdSelection = FALSE) %>%
      visInteraction(dragNodes = FALSE, dragView = FALSE, zoomView = FALSE))
    }
    
    nodes <- network_nodes_shared() %>% filter(id %in% V(g_subgraph)$name)
    edges <- as_data_frame(g_subgraph, what = "edges")
    
    set.seed(1337)
    visNetwork(nodes, edges) %>%
    addFontAwesome() %>%
    visGroups(groupname = "college",  shape = "icon", icon = list(code = "f19c", size = 35, color = "#9b0a7d")) %>%
    visGroups(groupname = "degree",   shape = "icon", icon = list(code = "f19d", size = 5,  color = "#9b0a7d")) %>%
    visGroups(groupname = "person",   shape = "icon", icon = list(code = "f183", size = 30, color = "#c34113")) %>%
    visGroups(groupname = "employer", shape = "icon", icon = list(code = "f1ad", size = 25, color = "#428bca")) %>%
    visGroups(groupname = "job",      shape = "icon", icon = list(code = "f0b1", size = 5,  color = "#428bca")) %>%
    visGroups(groupname = "location", shape = "icon", icon = list(code = "f3c5", size = 25, color = "darkgreen")) %>%
    visIgraphLayout(layout = "layout_with_fr") %>%
    visPhysics(stabilization = FALSE, timestep = .35, minVelocity = 10,
      maxVelocity = 50, solver = "forceAtlas2Based") %>%
      visEdges(smooth = FALSE) %>%
      visOptions(highlightNearest = list(enabled = TRUE, degree = 2, hover = TRUE)) %>%
      visEvents(click = "function(nodes) {
      Shiny.onInputChange('clicked_node',
        nodes.nodes.length > 0 ? nodes.nodes[0] : null);
    }")
    })
    
    # Reactive observer to filter data — listens to button click
    observeEvent(input$go, {
      filter_data()
      has_started("T")
    })
    
    observeEvent(input$main_tabs, {
      if(is_null(has_started())==F & is_null(has_refiltered())==F & input$main_tabs!="Employers"){
        filter_data()
        has_refiltered(NULL)
      }
    })
    
    # --- Colleges ---
    observeEvent(input$clicked_college_id, {
      req(input$clicked_college_id)
      updateTabsetPanel(session, "main_tabs", selected = "Engineers")
      updateTextInput(session, "query_id", value = input$clicked_college_id)
      filter_data(input$clicked_college_id)
    }, ignoreInit = TRUE)
    
    # --- Employers ---
    observeEvent(input$clicked_employer_id, {
      req(input$clicked_employer_id)
      updateTabsetPanel(session, "main_tabs", selected = "Engineers")
      updateTextInput(session, "query_id", value = input$clicked_employer_id)
      filter_data(input$clicked_employer_id)
    }, ignoreInit = TRUE)
    
    # --- Subsidies ---
    observeEvent(input$clicked_subsidy_id, {
      req(input$clicked_subsidy_id)
      updateTabsetPanel(session, "main_tabs", selected = "Employers")
      subsidies <- CERD_employers %>% filter(parent_id %in% input$clicked_subsidy_id | employer_id %in% input$clicked_subsidy_id)
      filter_data(subsidies$employer_id)
    }, ignoreInit = TRUE)
    
    # --- Entry filter ---
    observeEvent(input$clicked_entry_id, {
      req(input$clicked_entry_id)
      updateTabsetPanel(session, "main_tabs", selected = "Engineers")
      updateTextInput(session, "query_id", value = input$clicked_entry_id)
      filter_data(input$clicked_entry_id)
    }, ignoreInit = TRUE)
    
    # --- Society filter ---
    observeEvent(input$clicked_society, {
      req(input$clicked_society)
      updateTabsetPanel(session, "main_tabs", selected = "Engineers")
      society_filter <- CERD_persons %>% filter(societies %in% input$clicked_society)
      filter_data(society_filter$person_id)
    }, ignoreInit = TRUE)
    
    # ------------------------------------------------------------------------------
    # Core Data Filtering Function
    # ------------------------------------------------------------------------------
    # This function applies all user-selected filters to the research data
    
    filter_data <- function(id="") {
      req(input$fields)  # Make sure places are selected
      req(input$datasets)
      
      query_name_logic <- input$query_name_logic_radio
      query_jiguan_logic <- input$query_jiguan_logic_radio
      query_employer_logic <- input$query_employer_logic_radio
      query_college_logic <- input$query_college_logic_radio
      
      query_jiguan <- toTrad(input$query_jiguan)
      query_employer <- toTrad(input$query_employer)
      query_college <- toTrad(input$query_college)
      
      if (length(id) == 0 || all(id == "")) {
        query_id <- input$query_id
        
        # Parse user search query
        query_name <- toTrad(input$query_name)
        
      }else{
        query_id <- id
        query_name <-  NULL
        updateTextInput(session, "query_name", value = "")
      }

      # Store selected faculty filters for use across reactive expressions
      selected_fields <- input$fields
      filtered_fields(selected_fields)
      
      filtered_locations(
        CERD_locations %>%
        {  # handle name query separately
          if (is.null(query_jiguan) || query_jiguan == "") {
            .
          } else {
            query_jiguan_words <- strsplit(query_jiguan, "\\s+")[[1]]
            
            word_matches <- lapply(query_jiguan_words, function(word) {
              pattern <- paste0("\\b", word, "\\b")
              grepl(pattern, .$province, ignore.case = TRUE) |
              grepl(pattern, .$place_name, ignore.case = TRUE) |
              grepl(pattern, .$country, ignore.case = TRUE)
            })
            
            if (query_jiguan_logic == "or") {
              filter(., Reduce(`|`, word_matches))
            } else if (query_jiguan_logic == "and") {
              filter(., Reduce(`&`, word_matches))
            } else {
              stop("Query logic must be 'or' or 'and'")
            }
          }
        }
      )
      
      filtered_degrees(
        CERD_degrees %>%
        
        filter({
          # Use the expanded fields
          selected_fields <- expanded_fields()
          
          if(length(selected_fields) == 0) {
            TRUE
          } else {
            # Keep "other" separate for filtering
            main_fields <- setdiff(selected_fields, "other")
            main_pattern <- paste(main_fields, collapse = "|")
            
            if("other" %in% selected_fields & length(main_fields) == 0) {
              # only "other" selected -> keep degrees not matching any known field
              !grepl("civil|hydraulic|mechanical|electric|telecomm|radio|mining|chemical|chemistry|textile", field, ignore.case = TRUE)
            } else if("other" %in% selected_fields) {
              # "other" plus main fields -> keep main fields OR anything not matching known fields
              grepl(main_pattern, field, ignore.case = TRUE) |
              !grepl("civil|hydraulic|mechanical|electric|telecomm|radio|mining|chemical|chemistry|textile", field, ignore.case = TRUE)
            } else {
              # only main fields
              grepl(main_pattern, field, ignore.case = TRUE)
            }
          }
        }) %>%
        
        filter(
          graduation_year >= input$time_graduation[[1]] &
          graduation_year <= input$time_graduation[[2]] |
          (input$include_unknown_graduation & is.na(graduation_year))
        ) %>%
        
        left_join(CERD_colleges %>% select(college_id,college_name), by="college_id") %>%
        
        {
          
          if (!is.null(query_id) && length(query_id) > 0 && any(query_id %in% .$college_id)) {
            filter(., college_id %in% query_id)
          } else {
            .
          }
        } %>%
        
        {  # handle name query separately
          if (is.null(query_college) || query_college == "") {
            .
          } else {
            query_college_words <- strsplit(query_college, "\\s+")[[1]]
            
            word_matches <- lapply(query_college_words, function(word) {
              pattern <- paste0("\\b", word, "\\b")
              grepl(pattern, .$college_name, ignore.case = TRUE)
            })
            
            if (query_college_logic == "or") {
              filter(., Reduce(`|`, word_matches))
            } else if (query_college_logic == "and") {
              filter(., Reduce(`&`, word_matches))
            } else {
              stop("Query logic must be 'or' or 'and'")
            }
          }
        }
      )
      
      filtered_jobs(
        CERD_jobs %>%
        
        left_join(CERD_employers %>% select(employer_id,employer_name,parent_name,parent_id), by="employer_id") %>%
        
        {
          if (!is.null(query_id) && length(query_id) > 0 && any(query_id %in% .$employer_id)) {
            filter(., employer_id %in% query_id)
          } else {
            .
          }
        } %>%
        
        {  # handle name query separately
          if (is.null(query_employer) || query_employer == "") {
            .
          } else {
            query_employer_words <- strsplit(query_employer, "\\s+")[[1]]
            
            word_matches <- lapply(query_employer_words, function(word) {
              pattern <- paste0("\\b", word, "\\b")
              grepl(pattern, .$employer_name, ignore.case = TRUE) |
              grepl(pattern, .$job_title, ignore.case = TRUE) |
              grepl(pattern, .$parent_name, ignore.case = TRUE)
            })
            
            if (query_employer_logic == "or") {
              filter(., Reduce(`|`, word_matches))
            } else if (query_employer_logic == "and") {
              filter(., Reduce(`&`, word_matches))
            } else {
              stop("query_name_logic must be 'or' or 'and'")
            }
          }
        }
      )
      
      filtered_persons(
        CERD_persons %>%

        {
          if (!is.null(query_id) && length(query_id) > 0 && any(query_id %in% .$person_id)) {
            filter(., person_id %in% query_id)
          } else {
            .
          }
        } %>%
        
        filter(person_id %in% (CERD_persons %>% filter(dataset %in% input$datasets))$person_id) %>%
        
        filter({
          if(length(input$fields) == 0) {
            TRUE
          } else {
            main_fields <- setdiff(input$fields, "other")
            main_pattern <- paste(main_fields, collapse = "|")
            if("other" %in% input$fields & length(main_fields) == 0) {
              TRUE
            } else if("other" %in% input$fields) {
              # "other" plus main fields -> keep main fields OR anything not matching any known field
              TRUE
            } else {
              # only main fields
              person_id %in% CERD_degrees$person_id
            }
          }
        }) %>%
        
        filter(
          (person_id %in% filtered_degrees()$person_id) |
          (is.na(degree_id) & 
          (is.null(query_id) || length(query_id) == 0 || all(query_id == "")) & 
          (is.null(query_college) || length(query_college) == 0 || all(query_college == "")))
        ) %>%
        
        filter(
          (person_id %in% filtered_jobs()$person_id) |
          (is.na(job_id) &
          (is.null(query_id) || length(query_id) == 0 || all(query_id == "")) &
          (is.null(query_employer) || length(query_employer) == 0 || all(query_employer == "")))
        ) %>%
        
        filter(
          (location_id %in% filtered_locations()$location_id) |
          (is.na(location_id) &
          (is.null(query_jiguan) || length(query_jiguan) == 0 || all(query_jiguan == "")))
        ) %>%
        
        filter(tolower(gender) %in% tolower(input$genders)) %>%
        
        filter(
          birthyear >= input$time_birth[[1]] &
          birthyear <= input$time_birth[[2]] |
          (input$include_unknown_birthyear & is.na(birthyear))
        ) %>%

        {
          if (is.null(query_name) || query_name == "") {
            .
          } else {
            query_name_words <- strsplit(query_name, "\\s+")[[1]]
            
            word_matches <- lapply(query_name_words, function(word) {
              if (query_name_logic == "or") {
                # Allow partial matches (no word boundaries)
                pattern <- paste0(word)
              } else if (query_name_logic == "and") {
                # Require exact word boundaries
                pattern <- paste0("\\b", word, "\\b")
              } else {
                stop("Query logic must be 'or' or 'and'")
              }
              
              grepl(pattern, .$familyname, ignore.case = TRUE) |
              grepl(pattern, .$givenname, ignore.case = TRUE) |
              grepl(pattern, .$familyname_postal, ignore.case = TRUE) |
              grepl(pattern, .$givenname_postal, ignore.case = TRUE) |
              grepl(pattern, .$names_alt, ignore.case = TRUE) |
              grepl(pattern, .$hanzi, ignore.case = TRUE)
            })
            
            if (query_name_logic == "or") {
              filter(., Reduce(`|`, word_matches))
            } else if (query_name_logic == "and") {
              filter(., Reduce(`&`, word_matches))
            } else {
              stop("Query logic must be 'or' or 'and'")
            }
          }
        }
      )
      
      filtered_jobs(
        filtered_jobs() %>%
        filter(person_id %in% filtered_persons()$person_id)
      )
      
      filtered_degrees(
        filtered_degrees() %>%
        filter(person_id %in% filtered_persons()$person_id)
      )
      
      filtered_colleges(
        filtered_degrees() %>%
        select(person_id,college_id) %>%
        unique() %>%
        count(college_id) %>%
        
        left_join(CERD_colleges %>% select(college_id,college_name,location_id), by="college_id") %>%
        filter(!college_name=="") %>% filter(!is.na(college_name)) %>% filter(!college_name=="NA")
      )
      
      filtered_employers(
        filtered_jobs() %>%
        select(person_id,employer_id) %>%
        unique() %>%
        count(employer_id) %>%
        
        left_join(CERD_employers %>% select(employer_id,employer_name,location_id,parent_id,parent_name), by="employer_id")
      )
      
      
      plotted_points <- CERD_persons %>% 
      filter(location_id %in% filtered_persons()$location_id) %>%
      left_join(CERD_locations,by="location_id",relationship="many-to-many") %>%
      select(person_id,longlat) %>%
      unique() %>%
      count(longlat) %>%
      arrange(-n)
      
      plotted_points_var(plotted_points)
      
    }
    
    # --- Reactive nodes / edges ---
    network_nodes_shared <- reactive({
      req(total_rows() < 2000)
      make_network_nodes()
    })
    
    network_edges_shared <- reactive({
      req(total_rows() < 2000)
      make_network_edges(network_nodes_shared())
    })
    
    # --- Full graph ---
    g <- reactive({
      req(total_rows() < 2000)
      edges <- network_edges_shared()
      req(nrow(edges) > 0)
      
      graph_from_edgelist(as.matrix(edges[, c("from", "to")]), directed = FALSE)
    })
    
    # --- Subgraph ---
    g_sub <- reactive({
      full_graph <- g()
      comps <- components(full_graph)
      induced_subgraph(
        full_graph,
        V(full_graph)[comps$membership %in%
          which(comps$csize >= 0.05 * max(comps$csize))]
        )
      })
      
      total_rows <- reactive({
        req(!is.null(filtered_persons()), "person_id" %in% names(filtered_persons()))
        nrow(filtered_persons() %>% select(person_id) %>% distinct())
      })
      
      # ------------------------------------------------------------------------------
      # Logics First Startup
      # ------------------------------------------------------------------------------
      
      # --- Trigger manually once on startup ---
      observeEvent(TRUE, {
        if (!is.null(input$fields)) {
          filter_data()
          has_started("T")
        }
      }, once = TRUE)
      
      # ------------------------------------------------------------------------------
      # Data Table: Engineers
      # ------------------------------------------------------------------------------
      
      # One canonical reactive for the table displayed in the UI
      dataTablePerson <- reactive({
        filtered_persons() %>%
        select(person_id, familyname, givenname, hanzi, birthyear, location_id, gender, mcbd_id)  %>%
        unique() %>%
        left_join(CERD_locations, by = "location_id", relationship = "many-to-many") %>%
        select(person_id, familyname, givenname, hanzi, birthyear, place_name, location_id, gender, mcbd_id)  %>%
        unique() %>%
        arrange(familyname, givenname, hanzi, birthyear, place_name)
      })
      
      # Render it
      output$dataTablePerson <- DT::renderDataTable({
        DT::datatable(
          dataTablePerson() %>%
          select(familyname,givenname,hanzi,birthyear,place_name) %>%
          rename('Family name' = familyname, 'Given name' = givenname, 'Birth year' = birthyear,
          'Native place' = place_name, Characters = hanzi) ,
          selection = list(mode = "single"),
          options = list(
            orderClasses = TRUE,
            pageLength = 25,
            dom = 'tip'
          )
        )
      })
      
      # ------------------------------------------------------------------------------
      # Data Table: Employers
      # ------------------------------------------------------------------------------
      
      dataTablePub <- reactive ({
        datatable_employers <- filtered_employers()
        
        # Check if the filtered data is empty
        if (is.null(datatable_employers) || nrow(datatable_employers) == 0) {
          return(data.frame("No results" = "Please change your filter.", check.names = FALSE))
        }
        
        datatable_employers %>%
        arrange(-n, employer_name,location_id) %>%
        left_join(CERD_locations,by="location_id", relationship="many-to-many") %>%
        select(employer_name, place_name, n) %>%
        rename(Count = n, 'Employer name' = employer_name, Location = place_name)
      })
      
      # Render the DataTable
      output$dataTablePub <- DT::renderDataTable({
        DT::datatable(dataTablePub(),
        selection = list(mode = "single"),  # <-- only one row selectable
        options = list(
          
          orderClasses = TRUE, 
          pageLength = 25,
          dom = 'tip',
          initComplete = htmlwidgets::JS(
            "function(settings, json) {",
            "$(this.api().table().body()).css({'background-color': 'transparent'});",
            "$(this.api().table().header()).css({'background-color': 'transparent'});",
            "$('.dataTables_info').css({'background-color': 'transparent'});",
            "$('.dataTables_paginate').css({'background-color': 'transparent'});",
            "}"
          )
        ),escape=F
      )
    })
    
    # ------------------------------------------------------------------------------
    # Data Table: Colleges
    # ------------------------------------------------------------------------------
    
    dataTableCol <- reactive ({
      
      datatable_colleges <- filtered_colleges()
      
      # Check if the filtered data is empty
      if (is.null(datatable_colleges) || nrow(datatable_colleges) == 0) {
        return(data.frame("No results" = "Please change your filter.", check.names = FALSE))
      }
      
      datatable_colleges %>%
      arrange(-n, college_name, location_id) %>%
      left_join(CERD_locations,by="location_id", relationship="many-to-many") %>%
      select(college_name, place_name, n) %>%
      rename(Count = n, 'College name' = college_name, Location = place_name)
    })
    
    # Render the DataTable
    output$dataTableCol <- DT::renderDataTable({
      req(has_started)
      DT::datatable(dataTableCol(),
      selection = list(mode = "single"),  # <-- only one row selectable
      options = list(
        
        orderClasses = TRUE, 
        pageLength = 25,
        dom = 'tip',
        initComplete = htmlwidgets::JS(
          "function(settings, json) {",
          "$(this.api().table().body()).css({'background-color': 'transparent'});",
          "$(this.api().table().header()).css({'background-color': 'transparent'});",
          "$('.dataTables_info').css({'background-color': 'transparent'});",
          "$('.dataTables_paginate').css({'background-color': 'transparent'});",
          "}"
        )
      ),escape=F
    )
  })
  
  # ------------------------------------------------------------------------------
  # Engineers Table UI
  # ------------------------------------------------------------------------------
  
  output$dynamic_ui_person <- renderUI({

    table_data <- dataTablePerson()
    
    # get clicked person row safely
    clicked_node <- table_data[input$dataTablePerson_rows_selected, ]
    
    if (nrow(clicked_node) == 0) return(NULL)
    
    aka_names <- filtered_persons() %>%
    filter(person_id %in% clicked_node$person_id & is.na(familyname_postal)==F & is.na(givenname_postal)==F) %>%
    # Combine postal names
    transmute(names_alt = paste(familyname_postal, givenname_postal)) %>%
    # Bind with existing names_alt column
    select(names_alt) %>%
    rbind(
      filtered_persons() %>%
      filter(person_id %in% clicked_node$person_id & is.na(names_alt) == F) %>%
      select(names_alt)
    ) %>%
    drop_na() %>%
    unique()
    
    # Degrees for clicked person
    
    if (!is.null(clicked_node) && nrow(clicked_node) > 0) {
      
      # --- Native place
      native_loc <- CERD_locations %>%
      filter(location_id == clicked_node$location_id)
      
      # --- Degree locations
      clicked_degrees <- CERD_degrees %>%
      filter(person_id == clicked_node$person_id) %>%
      left_join(CERD_colleges, by = "college_id") %>%
      left_join(CERD_locations, by = c("location_id" = "location_id"), suffix = c("_degree", "_loc")) %>%
      arrange(graduation_year)
      
      # Jobs for clicked person
      clicked_jobs <- CERD_jobs %>%
      filter(person_id == clicked_node$person_id) %>%
      left_join(CERD_employers, by = "employer_id") %>%
      left_join(CERD_locations, by = c("location_id" = "location_id"), suffix = c("_job", "_loc")) %>%
      arrange(job_start)
      
      clicked_societies <- CERD_persons %>%
      filter(person_id %in% clicked_node$person_id) %>%
      select(societies,societies_date,societies_number,societies_status) %>% filter(!is.na(societies)) %>% unique() %>% arrange(societies)
      
      tagList(
        div(
          style = "display: flex; justify-content: space-between; align-items: flex-start;",
          h4(style = "margin-top: 0; margin-bottom: 0;",
          paste(
            clicked_node$familyname, clicked_node$givenname,
            
            if (!is.na(clicked_node$hanzi)) {
              clicked_node$hanzi
            } else {
              NULL
            },
            
            if (!is.na(clicked_node$gender) && clicked_node$gender != "Unknown") {
              if (clicked_node$gender == "Male") "♂"
              else if (clicked_node$gender == "Female") "♀"
              else ""  # optional for unisex/unknown
            } else ""
          )
        ),
        span(
          paste("ID #",clicked_node$person_id)
        )
      ),
      
      if (!is.na(clicked_node$birthyear) || !is.na(clicked_node$location_id)) {
        p(
          HTML(
            paste0(
              # Birth year
              if (!is.na(clicked_node$birthyear)) paste0("born ", clicked_node$birthyear) else "",
              # " in " only if both birthyear and location exist
              if (!is.na(clicked_node$birthyear) & !is.na(clicked_node$location_id)) " in " else "",
              # Place name
              if (!is.na(clicked_node$location_id)) {
                CERD_locations %>%
                filter(location_id == clicked_node$location_id) %>%
                pull(place_name)
              } else "",
              # Province in parentheses if it exists
              if (!is.na(clicked_node$location_id)) {
                province <- CERD_locations %>%
                filter(location_id == clicked_node$location_id) %>%
                pull(province)
                if (!is.na(province)) paste0(" (", province, ")") else ""
              } else ""
            )
          )
        )
      } else {
        NULL
      },
      
      if (!is_empty(aka_names) & nrow(aka_names)!=0) {
        p(HTML(paste("a.k.a.",paste(aka_names$names_alt, collapse = ", "))))
      } else {
        NULL
      },
      
      # Only render if there is at least one degree
      if (nrow(clicked_degrees) > 0) {
        tagList(
          p(HTML("<b>Degrees</b>:")),
          HTML(
            paste0(
              "<ol>",
              paste(
                sapply(1:nrow(clicked_degrees), function(i) {
                  degree <- clicked_degrees[i, ]
                  # Build text for each degree
                  paste0(
                    "<li>",
                    if (!is.na(degree$field)) degree$field else "",
                    if (!is.na(degree$college_id)) {
                      college_name <- CERD_colleges %>%
                      filter(college_id == degree$college_id) %>%
                      pull(college_name)
                      
                      if (length(college_name) > 0) {
                        paste0(
                          ' at <a href="#" class="college-link" data-id="', degree$college_id, '">',
                          college_name,
                          '</a>'
                        )
                      } else ""
                    } else "",
                    # Degree and graduation year logic
                    if (!is.na(degree$degree) || !is.na(degree$graduation_year)) {
                      paste0(
                        " (",
                        if (!is.na(degree$degree)) degree$degree else "",
                        if (!is.na(degree$degree) & !is.na(degree$graduation_year)) ", " else "",
                        if (!is.na(degree$graduation_year)) degree$graduation_year else "",
                        ")"
                      )
                    } else "",
                    "</li>"
                  )
                }),
                collapse = ""
              ),
              "</ol>"
            )
          )
        )
      } else {
        NULL
      },
      # Only render if there is at least one job
      if (nrow(clicked_jobs) > 0) {
        tagList(
          p(HTML("<b>Employments</b>:")),
          HTML(
            paste0(
              "<ol>",
              paste(
                sapply(1:nrow(clicked_jobs), function(i) {
                  job <- clicked_jobs[i, ]
                  # Build text for each job
                  paste0(
                    "<li>",
                    if (!is.na(job$job_title)) gsub("\\|", " & ", job$job_title) else "",
                    if (!is.na(job$employer_id)) {
                      employer_name <- CERD_employers %>%
                      filter(employer_id == job$employer_id) %>%
                      pull(employer_name)
                      if (length(employer_name) > 0) {
                        paste0(
                          ' at <a href="#" class="employer-link" data-id="', job$employer_id, '">',
                          employer_name,
                          '</a>'
                        )
                      } else ""
                    } else "",
                    # Job start year in parentheses
                    if (!is.na(job$job_start)) {
                      paste0(" (", job$job_start, ")")
                    } else if (is.na(job$job_start) && !is.na(job$year_approx)) {
                      paste0(" (", job$year_approx, ")")
                    } else {
                      ""
                    },
                    "</li>"
                  )
                }),
                collapse = ""
              ),
              "</ol>"
            )
          )
        )
      } else {
        NULL
      },
      
      # Only render if there is at least one society
      if (nrow(clicked_societies) > 0) {
        tagList(
          p(HTML("<b>Memberships</b>:")),
          HTML(
            paste0(
              "<ol>",
              paste(
                sapply(1:nrow(clicked_societies), function(i) {
                  society <- clicked_societies[i, ]
                  # Build text for each job
                  paste0(
                    "<li>",
                    paste0(
                      '<a href="#" class="society-link" data-id="', society$societies, '">',
                      society$societies,
                      '</a>'
                    ),
                    if (!is.na(society$societies_number) | !is.na(society$societies_status) | !is.na(society$societies_date)) paste(" (") else "",
                    if (!is.na(society$societies_number)) paste0("#", society$societies_number) else "",
                    if (!is.na(society$societies_number) & !is.na(society$societies_status) || !is.na(society$societies_number) & !is.na(society$societies_date)) paste(", ") else "",
                    if (!is.na(society$societies_status)) gsub("\\|", " & ", society$societies_status) else "",
                    if (!is.na(society$societies_date) & !is.na(society$societies_status)) paste(" ") else "",
                    if (!is.na(society$societies_date)) gsub("\\|", " & ", society$societies_date) else "",
                    if (!is.na(society$societies_number) | !is.na(society$societies_status) | !is.na(society$societies_date)) paste(")") else "",
                    "</li>"
                  )
                }),
                collapse = ""
              ),
              "</ol>"
            )
          )
        )
      } else {
        NULL
      },
      
      p(HTML(paste0(
        '<a href="#" class="entry-link" data-id="', clicked_node$person_id, '">',
        '🔍 Add as ID filter to existing query',
        '</a>'
      ))),
      
      plotOutput("person_map", height = "380px"),
      
      if (nrow(CERD_sources %>% 
        filter(person_id %in% clicked_node$person_id))>0) {
          p(
            HTML(
              paste0(
                "<span style='font-size:0.85em; color:#666;'>Source(s): ",
                paste((unique(CERD_sources %>% arrange(sources) %>%
                filter(person_id %in% clicked_node$person_id))$sources),
                collapse = ", "),
                "</span>"
              )
            )
          )
        } else {
          NULL
        },
        if (!is.na(clicked_node$mcbd_id)) {
          p(
            HTML(
              paste0(
                "<span style='font-size:0.85em; color:#666;'>This entry can also be found at <a href=\"https://heurist.huma-num.fr/ModernChinaBiographicalDatabase/\">MCBD</a> as ID #",
                clicked_node$mcbd_id,
                ".</span>"
              )
            )
          )
        } else {
          NULL
        }
        
      )
    } else {
      h4("Click on an entry for additional information.")
    }
    
  })
  
  output$person_map <- renderPlot({
    req(selected_node())  # you need a reactive for clicked_node
    
    pid <- selected_node()$person_id
    locid <- selected_node()$location_id
    
    # Native place
    native_loc <- CERD_locations %>%
    filter(location_id %in% locid) %>%
    mutate(
      type = "Native Place",
      person_id = pid  # add the person_id explicitly
    )
    
    # Degree locations
    degree_locs <- CERD_degrees %>%
    filter(person_id %in% pid) %>%
    left_join(CERD_colleges, by = "college_id") %>%
    left_join(CERD_locations, by = "location_id") %>%
    mutate(type = "College Location")
    
    # Job locations
    job_locs <- CERD_jobs %>%
    filter(person_id %in% pid) %>%
    left_join(CERD_employers, by = "employer_id") %>%
    left_join(CERD_locations, by = "location_id") %>%
    mutate(type = "Job Location")
    
    # Combine all
    plot_data <- bind_rows(native_loc, degree_locs, job_locs) %>%
    filter(!st_is_empty(longlat) & !is.na(longlat)) %>%
    st_as_sf(sf_column_name = "longlat", crs = 4326)
    
    # Plot
    # Ensure type is a factor in desired order
    plot_data <- plot_data %>%
    mutate(type = factor(type, levels = c("Native Place", "College Location", "Job Location")))
    
    # Extract coordinates
    plot_data_coords <- plot_data %>%
    mutate(
      lon = st_coordinates(longlat)[,1],
      lat = st_coordinates(longlat)[,2]
    )
    
    ggplot() +
    geom_sf(data = china_1928, fill = "white", color = "grey50", linewidth = 0.2, linetype = "longdash") +
    geom_point(
      data = plot_data_coords,
      aes(x = lon, y = lat, color = type),
      size = 3,
      alpha = 0.7,
      position = position_jitter(width = 0.4, height = 0.4)
    ) +
    scale_color_manual(values = c(
      "Native Place" = "#0072B2",
      "College Location" = "#E69F00",
      "Job Location" = "#009E73"
    )) +
    labs(color = "") + # removes the legend title completely
    coord_sf(xlim = c(71, 139), ylim = c(18, 54), expand = FALSE) +
    theme_minimal(base_size = 12) +
    theme(
      legend.position = "bottom",
      panel.background = element_rect(fill = "#fafafa", color = NA),
      plot.background  = element_rect(fill = "#fafafa", color = NA),
      panel.grid       = element_blank(),
      axis.title       = element_blank(),
      axis.text        = element_blank(),
      axis.ticks       = element_blank()
    )
  }, bg = "transparent")
  
  # ------------------------------------------------------------------------------
  # Employer Table UI
  # ------------------------------------------------------------------------------
  
  output$dynamic_ui_pubs <- renderUI({
    
    filtered_data <- filtered_employers()
    datatable_pubs <- filtered_employers() %>% arrange(-n, employer_name, location_id)
    
    clicked_node <- datatable_pubs[input$dataTablePub_rows_selected, ]
    
    if (!is.null(clicked_node) && nrow(clicked_node) > 0) {
      
      # Create dynamic UI
      tagList(
        
        div(
          style = "display: flex; justify-content: space-between; align-items: flex-start;",
          h4(style = "margin-top: 0; margin-bottom: 0;",
          HTML(paste0(clicked_node$employer_name))
        ),
        span(
          paste("ID #",clicked_node$employer_id)
        )
      ),
      
      if (!is.na(clicked_node$location_id)) {
        p(HTML(paste0(CERD_locations[CERD_locations$location_id==clicked_node$location_id,]$place_name,
          " (",CERD_locations[CERD_locations$location_id==clicked_node$location_id,]$province,")")))
        } else {
          NULL
        },
        if (!is.na(clicked_node$parent_id) & !is.na(clicked_node$parent_name)) {
          p(HTML(paste0(
            'Part of <a href="#" class="employer-link" data-id="', clicked_node$parent_id, '">',
            clicked_node$parent_name,
            '</a>'
          )))
        } else {
          NULL
        },
        p(),p(),
        if (nrow(CERD_employers %>% filter(is.na(parent_id)==F & parent_id %in% clicked_node$employer_id)) > 0) {
          p(HTML(paste0(
            '<a href="#" class="subsidy-link" data-id="', clicked_node$employer_id, '">',
            '🏢 Include subsidiaries',
            '</a>'
          )))
        },
        p(HTML(paste0(
          '<a href="#" class="entry-link" data-id="', clicked_node$employer_id, '">',
          '🔍 Add as ID filter to existing query',
          '</a>'
        )))
      )
    } else {
      h4("Click on an entry for further information.")
    }
  })
  
  # ------------------------------------------------------------------------------
  # College Table UI
  # ------------------------------------------------------------------------------
  
  output$dynamic_ui_cols <- renderUI({
    
    filtered_data <- filtered_colleges()
    datatable_cols <- filtered_colleges() %>% arrange(-n, college_name, location_id)
    
    clicked_node <- datatable_cols[input$dataTableCol_rows_selected, ]
    
    if (!is.null(clicked_node) && nrow(clicked_node) > 0) {
      
      tagList(
        
        div(
          style = "display: flex; justify-content: space-between; align-items: flex-start;",
          h4(style = "margin-top: 0; margin-bottom: 0;",
          HTML(paste0(clicked_node$college_name))
        ),
        span(
          paste("ID #",clicked_node$college_id)
        )
      ),
      
      if (!is.na(clicked_node$location_id)) {
        p(HTML(paste0(CERD_locations[CERD_locations$location_id==clicked_node$location_id,]$place_name,
          " (",CERD_locations[CERD_locations$location_id==clicked_node$location_id,]$province,")")))
        } else {
          NULL
        },
        
        # --- Add radar chart
        div(
          style = "text-align: center;",
          plotOutput("college_radar", height = "250px"),
          HTML(
            paste0(
              "<span style='font-size:0.85em; color:#666;'>Sub-disciplines distribution<br/>(queried engineers only)</span>"
            )
          )
        ),
        p(),
        p(HTML(paste0(
          '<a href="#" class="entry-link" data-id="', clicked_node$college_id, '">',
          '🔍 Add as ID filter to existing query',
          '</a>'
        )))
        
      )
    } else {
      h4("Click on an entry for further information.")
    }
  })
  
  # --- Then define the radar plot separately in server
  output$college_radar <- renderPlot({
    
    # 5 main disciplines
    disciplines <- c("CE 🏗️", "ME 🔧", "EE 💡️️", "MiE ⛏️", "ChE 🧪")
    # Prepare counts per discipline for selected college
    radar_counts <- filtered_degrees() %>%
    filter(college_id == selected_node_col()$college_id) %>%
    mutate(discipline = case_when(
      grepl("Civil", field, ignore.case = TRUE) ~ "CE 🏗️",
      grepl("Mechanical", field, ignore.case = TRUE) ~ "ME 🔧",
      grepl("Electrical", field, ignore.case = TRUE) ~ "EE 💡️️",
      grepl("Mining", field, ignore.case = TRUE) ~ "MiE ⛏️",
      grepl("Chemical", field, ignore.case = TRUE) ~ "ChE 🧪",
      TRUE ~ NA_character_
    )) %>%
    drop_na(discipline) %>%
    count(discipline) %>%
    complete(discipline = disciplines, fill = list(n = 0)) %>%
    arrange(match(discipline, disciplines)) 
    
    # Create dataframe for fmsb radarchart
    radar_data <- data.frame(
      rbind(
        rep(max(radar_counts$n, 1), length(disciplines)),  # max row
        rep(0, length(disciplines)),                       # min row
        radar_counts$n                                     # actual counts
      )
    )
    colnames(radar_data) <- disciplines
    
    # Draw the radar chart
    par(mar = c(0, 0, 0, 0))  # remove all inner margins
    radarchart(
      radar_data,
      cglcol = "grey", cglty = 1, cglwd = 0.8
    )
  }, bg = "transparent")
  
  # ------------------------------------------------------------------------------
  # Visualization: Location Distribution Bar Chart
  # ------------------------------------------------------------------------------
  output$barChart_standort <- renderPlot({
    researcher_counts <- filtered_persons() %>%
    mutate(
      organization_address_city = ifelse(
        is.na(organization_address_city) | 
        !organization_address_city %in% unlist(city_mapping, use.names = FALSE),
        "außerhalb",
        organization_address_city
      )
    ) %>%
    count(organization_address_city) %>%
    arrange(desc(n))
    
    ggplot(researcher_counts, aes(x = organization_address_city, y = n)) +
    geom_bar(stat = 'identity', fill = "#9b0a7d") +
    theme_minimal() +
    labs(title = "Letztdokumentierter Standort", x = "Standort", y = "Anzahl Forschende",
    caption = "Suchanfrage via: Pelzer 2025, Atlas der Ostasien-Forschung")  +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12))
  })
  
  # ------------------------------------------------------------------------------
  # Visualization: Fields Distribution Bar Chart  
  # ------------------------------------------------------------------------------
  output$barChart_fields <- renderPlot({
    
    # Map faculty codes to German display labels and filter by publication authors
    stat_fields <- filtered_degrees() %>%
    select(field, person_id) %>%
    unique() %>%
    count(field) %>%
    arrange(desc(n)) %>%
    slice(1:10)
    
    ggplot(stat_fields, aes(x = field, y = n)) +
    geom_bar(stat = 'identity', fill = "#6d94b3") +
    theme_minimal() +
    labs(
      title = "Top-10 queried engineering sub-disciplines",
      x = "Sub-discipline",
      y = "Count",
      caption = "Query via: Pelzer et al., CERD"
    ) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
      plot.title = element_text(face = "bold", size = 14)
    )
    
  })
  
  # ------------------------------------------------------------------------------
  # Shared reactive computations
  # ------------------------------------------------------------------------------
  
  # Base table: unique person-location pairs
  stat_region_base <- reactive({
    req(filtered_persons(), filtered_locations())
    
    filtered_persons() %>%
    left_join(filtered_locations(), by = "location_id", relationship = "many-to-many") %>%
    select(province, country, person_id) %>%
    distinct()
  })
  
  # Province-level counts (used for Asia map)
  province_counts <- reactive({
    stat_region_base() %>%
    filter(!is.na(province)) %>%
    count(province, name = "n") %>%
    arrange(desc(n))
  })
  
  # Country-level counts (used for World map)
  country_counts <- reactive({
    stat_region_base() %>%
    filter(!is.na(country)) %>%
    count(country, name = "n") %>%
    arrange(desc(n))
  })
  
  # Colleges
  stat_region_college <- reactive({
    req(filtered_degrees(), filtered_colleges(), CERD_locations)
    
    filtered_degrees() %>%
    select(college_id, person_id) %>%
    left_join(filtered_colleges() %>% select(college_id, location_id),
    by = "college_id", relationship = "many-to-many") %>%
    left_join(CERD_locations %>% select(location_id, longlat),
    by = "location_id", relationship = "many-to-many") %>%
    select(person_id, longlat) %>%
    count(longlat, name = "n") %>%
    drop_na() %>%
    mutate(type = "College")
  })
  
  # Employers
  stat_region_employer <- reactive({
    req(filtered_jobs(), filtered_employers(), CERD_locations)
    
    filtered_jobs() %>%
    select(employer_id, person_id) %>%
    left_join(filtered_employers() %>% select(employer_id, location_id),
    by = "employer_id", relationship = "many-to-many") %>%
    left_join(CERD_locations %>% select(location_id, longlat),
    by = "location_id", relationship = "many-to-many") %>%
    select(person_id, longlat) %>%
    count(longlat, name = "n") %>%
    drop_na() %>%
    mutate(type = "Employer")
  })
  
  # ------------------------------------------------------------------------------
  # Visualization: Asia Map
  # ------------------------------------------------------------------------------
  
  output$barChart_region <- renderPlot({
    req(province_counts())
    
    stat_region <- china_1928 %>%
    left_join(province_counts(), by = "province")
    
    ggplot() +
    geom_sf(data = world_1938, fill = "gray95", color = "gray95", size = 0.1) +
    geom_sf(data = stat_region, aes(fill = n), color = "white", size = 0.2) +
    geom_sf(data = stat_region_employer(),
    aes(geometry = longlat, size = n, color = type),
    alpha = 0.6) +
    geom_sf(data = stat_region_college(),
    aes(geometry = longlat, size = n, color = type),
    alpha = 0.4) +
    coord_sf(xlim = c(73, 149), ylim = c(16.5, 54.5)) +
    scale_fill_gradient(low = "#dce7ef", high = "#6d94b3",
    na.value = "grey90", name = "Native province") +
    scale_color_manual(values = c("College" = "#E69F00", "Employer" = "#009E73"),
    name = "Event type") +
    scale_size_continuous(name = "Events per location",
    breaks = c(1, 10, 100, 250, 500, 1000),
    range = c(1, 10)) +
    theme_minimal() +
    labs(
      title = "Geographic mobility of queried engineers (Asia)",
      caption = "Query via: Pelzer et al., CERD. Basemaps: China 1928, Christian Henriot (CC 0) & Ministry of the Interior / Taiwan Atlas.",
      x = NULL, y = NULL
    ) +
    theme(
      legend.position = "right",
      plot.title = element_text(face = "bold", size = 14),
      legend.text = element_text(size = 10)
    )
  })
  
  # ------------------------------------------------------------------------------
  # Visualization: World Map
  # ------------------------------------------------------------------------------
  
  output$barChart_region_world <- renderPlot({
    req(country_counts())
    
    stat_region <- world_1938 %>%
    rename(country = NAME) %>%
    mutate(country = trimws(as.character(country))) %>%
    left_join(country_counts() %>%
    mutate(country = trimws(as.character(country))),
    by = "country")
    
    ggplot(stat_region) +
    geom_sf(aes(fill = n), color = "white", size = 0.2) +
    geom_sf(data = stat_region_employer(),
    aes(geometry = longlat, size = n, color = type),
    alpha = 0.6) +
    geom_sf(data = stat_region_college(),
    aes(geometry = longlat, size = n, color = type),
    alpha = 0.4) +
    coord_sf(xlim = c(-130, 40), ylim = c(23, 70)) +
    scale_fill_gradient(low = "#dce7ef", high = "#6d94b3",
    na.value = "gray95", name = "Native country") +
    scale_color_manual(values = c("College" = "#E69F00", "Employer" = "#009E73"),
    name = "Event type") +
    scale_size_continuous(name = "Events per location",
    breaks = c(1, 10, 25, 50, 100, 250),
    range = c(1, 10)) +
    theme_minimal() +
    labs(
      title = "Geographic mobility of queried engineers (USA & Europe)",
      caption = "Query via: Pelzer et al., CERD. Basemap: World 1938 André Ourednik (GPL 3).",
      x = NULL, y = NULL
    ) +
    theme(
      legend.position = "right",
      plot.title = element_text(face = "bold", size = 14),
      legend.text = element_text(size = 10)
    )
  })
  
  # ------------------------------------------------------------------------------
  # Visualization: Taiwan Map
  # ------------------------------------------------------------------------------
  
  output$barChart_region_taiwan <- renderPlot({
    req(stat_region_employer())
    
    ggplot() +
    geom_sf(data = world_1938, fill = "gray95", color = "gray95", size = 0.1) +
    geom_sf(data = taiwan_1946, fill = "grey90", color = "white", size = 0.2) +
    geom_sf(data = stat_region_employer(),
    aes(geometry = longlat, size = n, color = type),
    alpha = 0.6) +
    coord_sf(xlim = c(119.3, 122), ylim = c(21.5, 25.5)) +
    scale_color_manual(values = c("Employer" = "#009E73"),
    name = "Event type") +
    scale_size_continuous(name = "Events per location",
    breaks = c(1, 10, 25, 50, 100, 250),
    range = c(1, 10)) +
    theme_minimal() +
    labs(
      title = "Geographic mobility of queried engineers (Taiwan)",
      caption = "Query via: Pelzer et al., CERD. Basemap: Ministry of the Interior / Taiwan Atlas.",
      x = NULL, y = NULL
    ) +
    theme(
      legend.position = "right",
      plot.title = element_text(face = "bold", size = 14),
      legend.text = element_text(size = 10)
    )
  })
  
  # ------------------------------------------------------------------------------
  # Visualization: Birth Year Line Plot
  # ------------------------------------------------------------------------------
  
  output$barChart_birthyear <- renderPlot({
    
    stat_birthyear <- filtered_persons() %>%
    select(birthyear,person_id) %>%
    unique() %>%
    mutate(birthyear = as.numeric(birthyear)) %>%  # ensure numeric
    count(birthyear) %>%
    arrange(desc(n)) %>%
    drop_na()
    
    stat_graduation <- filtered_degrees() %>%
    select(graduation_year,person_id) %>%
    unique() %>%
    mutate(graduation_year = as.numeric(graduation_year)) %>%  # ensure numeric
    count(graduation_year) %>%
    arrange(desc(n)) %>%
    drop_na()
    
    ggplot() +
    # Birthyear line
    geom_line(
      data = stat_birthyear,
      aes(x = birthyear, y = n, group = 1, color = "Birth year"),
      linewidth = 0.6
    ) +
    geom_point(
      data = stat_birthyear,
      aes(x = birthyear, y = n, color = "Birth year"),
      size = 2
    ) +
    
    # Graduation line (stroked / dashed)
    geom_line(
      data = stat_graduation,
      aes(x = graduation_year, y = n, group = 1, color = "Graduation year"),
      linewidth = 0.6,
      linetype = "longdash"
    ) +
    geom_point(
      data = stat_graduation,
      aes(x = graduation_year, y = n, color = "Graduation year"),
      size = 2,
      color = "#6d94b3",  # explicitly set color
      shape = 1  # hollow circle for contrast
    ) +
    
    scale_x_continuous(
      name = "Year",
      breaks = function(x) round(scales::breaks_pretty(n = 5)(x)),  # ≈5 nice breaks
      labels = function(x) as.integer(x)  # ensures integer labels
    ) +
    
    scale_color_manual(
      name = "",
      values = c("Birth year" = "#6d94b3", "Graduation year" = "#6d94b3")
    ) +
    
    scale_y_continuous(name = "Count") +
    labs(
      title = "Years of birth and graduation of queried engineers",
      subtitle = "",
      caption = "Query via: Pelzer et al., CERD"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
      plot.title = element_text(face = "bold", size = 14),
      legend.position = "top",
      legend.title = element_blank(),
      legend.text = element_text(size = 12)  # increase legend text size
    )
  })
  
  # ------------------------------------------------------------------------------
  # Visualization: Employer Activity Plot
  # ------------------------------------------------------------------------------
  
  output$barChart_activity <- renderPlot({
    
    stat_jobs <- filtered_jobs() %>%
    select(job_start, person_id, employer_id) %>%
    unique() %>%
    left_join(CERD_employers %>% select(location_id, employer_id), by = "employer_id") %>%
    left_join(CERD_locations %>% select(location_id, province), by = "location_id") %>%
    mutate(job_start = as.numeric(job_start)) %>%
    drop_na()
    
    top10 <- stat_jobs %>%
    count(province, sort = TRUE) %>%
    slice_head(n = 10) %>%
    pull(province)
    
    stat_jobs <- stat_jobs %>%
    filter(province %in% top10) %>%
    mutate(province = fct_rev(fct_infreq(province)))
    
    # ggridges plot
    ggplot(stat_jobs, aes(x = job_start, y = province)) +
    geom_density_ridges(scale = 3, alpha = 0.6, color = "#6d94b3", fill = "#6d94b3",
    bandwidth = 1) +
    theme_minimal() +
    labs(
      x = "Job start year",
      y = "Province",
      title = "Most active regions by job commencements, queried engineers only",
      caption = "Query via: Pelzer et al., CERD"
    ) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
      axis.text.y = element_text(angle = 45, hjust = 1, size = 12),
      plot.title = element_text(face = "bold", size = 14),
      legend.position = "none",
      legend.title = element_blank(),
      legend.text = element_text(size = 12)  # increase legend text size
    )
  })
  
  # ------------------------------------------------------------------------------
  # Visualization: Job Title Bars
  # ------------------------------------------------------------------------------
  
  output$barChart_jobs <- renderPlot({
    
    stat_jobs <- filtered_jobs() %>%
    select(job_title, person_id, job_id) %>%
    unique() %>%
    drop_na() %>%
    count(job_title) %>%
    arrange(desc(n)) %>%
    slice(1:10)
    
    ggplot(stat_jobs, aes(x = reorder(job_title, n), y = n)) +
    geom_bar(stat = 'identity', fill = "#6d94b3") +
    coord_flip() +
    geom_text(aes(label = job_title), 
    hjust = 1, 
    color = "white", 
    size = 9) +
    theme_minimal() +
    labs(
      title = "Top-10 job titles of queried engineers",
      x = "Job title",
      y = "Count",
      caption = "Query via: Pelzer et al., CERD"
    ) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
      axis.text.y = element_blank(),  # hide y-axis labels (job titles)
      axis.ticks.y = element_blank(),
      plot.title = element_text(face = "bold", size = 14)
    )

  })
  
  # ------------------------------------------------------------------------------
  # Data Export Logics
  # ------------------------------------------------------------------------------
  
  # Download as Excel
  output$download_excel_person <- downloadHandler(
    filename = function() {
      paste0("cerd_filter_", Sys.Date(), ".xlsx")
    },
    content = function(file) {
      write_xlsx(dataTablePerson(), path = file)
    }
  )
  
  # Download as CSV
  output$download_csv_person <- downloadHandler(
    filename = function() {
      paste0("cerd_filter_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(dataTablePerson(), file, row.names = FALSE, 
      fileEncoding = "windows-1252")
    }
  )
  
  # Download as Excel (employers)
  output$download_excel_col <- downloadHandler(
    filename = function() {
      paste0("cerd_filter_", Sys.Date(), ".xlsx")
    },
    content = function(file) {
      write_xlsx(dataTableCol(), path = file)
    }
  )
  
  # Download as CSV (employers)
  output$download_csv_col <- downloadHandler(
    filename = function() {
      paste0("cerd_filter_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(dataTableCol(), file, row.names = FALSE)
    }
  )
  
  # Download as Excel (employers)
  output$download_excel_pub <- downloadHandler(
    filename = function() {
      paste0("cerd_filter_", Sys.Date(), ".xlsx")
    },
    content = function(file) {
      write_xlsx(dataTablePub(), path = file)
    }
  )
  
  # Download as CSV (employers)
  output$download_csv_pub <- downloadHandler(
    filename = function() {
      paste0("cerd_filter_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(dataTablePub(), file, row.names = FALSE)
    }
  )
  
  # ------------------------------------------------------------------------------
  # Network Graph UI
  # ------------------------------------------------------------------------------

  output$dynamic_ui_netzwerk <- renderUI({
    req(g_sub())
    
    # Compute centrality measures
    deg <- degree(g_sub())
    btw <- betweenness(g_sub())
    clo <- closeness(g_sub(), normalized = TRUE, mode = "all")
    eig <- eigen_centrality(g_sub())$vector
    
    # Build summary using $title
    tagList(
      h4("Network summary"),
      p(paste("Number of nodes:", format(vcount(g_sub()), big.mark = ","))),
      p(paste("Number of edges:", format(ecount(g_sub()), big.mark = ","))),
      h4("Top nodes by centrality"),
      
      # Degree centrality
      p("Degree:"),
      tags$ul(
        lapply(1:3, function(i) {
          node_name <- V(g_sub())$name[order(deg, decreasing = TRUE)][i]
          node_title <- network_nodes_shared() %>% filter(id == node_name) %>% pull(title)
          node_value <- round(deg[which(V(g_sub())$name == node_name)], 2)
          tags$li(paste(node_title, "(", node_value, ")"))
        })
      ),
      
      # Betweenness centrality
      p("Betweenness:"),
      tags$ul(
        lapply(1:3, function(i) {
          node_name <- V(g_sub())$name[order(btw, decreasing = TRUE)][i]
          node_title <- network_nodes_shared() %>% filter(id == node_name) %>% pull(title)
          node_value <- round(btw[which(V(g_sub())$name == node_name)], 2)
          tags$li(paste(node_title, "(", node_value, ")"))
        })
      ),
      
      # Closeness centrality
      p("Closeness:"),
      tags$ul(
        lapply(1:3, function(i) {
          node_name <- V(g_sub())$name[order(clo, decreasing = TRUE)][i]
          node_title <- network_nodes_shared() %>% filter(id == node_name) %>% pull(title)
          node_value <- round(clo[which(V(g_sub())$name == node_name)], 2)
          tags$li(paste(node_title, "(", node_value, ")"))
        })
      ),
      
      # Eigenvector centrality
      p("Eigenvector:"),
      tags$ul(
        lapply(1:3, function(i) {
          node_name <- V(g_sub())$name[order(eig, decreasing = TRUE)][i]
          node_title <- network_nodes_shared() %>% filter(id == node_name) %>% pull(title)
          node_value <- round(eig[which(V(g_sub())$name == node_name)], 2)
          tags$li(paste(node_title, "(", node_value, ")"))
        })
      )
    )
  })
  
}

# ==============================================================================
# RUN APPLICATION
# ==============================================================================
shinyApp(ui = ui, server = server)
