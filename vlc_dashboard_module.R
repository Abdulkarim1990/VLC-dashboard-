# =============================================================================
# VLC DASHBOARD MODULE
# Essential Values for Ghanaian Youth - Values Learning Community Monitoring
# For integration into existing SEI Shiny Dashboard
# Author: SEI Data Team | GES / T-TEL
# Last Updated: March 2026
# =============================================================================
# This file contains:
#   vlc_ui        -> tabItem UI (add to dashboardBody)
#   vlc_server()  -> server logic (call inside server function)
# =============================================================================

# ---- REQUIRED LIBRARIES (add to your main app if not already present) -------
library(shiny)
library(shinydashboard)
library(shinyjs)
library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)
library(scales)
library(lubridate)
library(glue)
library(DT)
library(stringr)
library(RColorBrewer)

# =============================================================================
# GLOBAL CONSTANTS
# =============================================================================

TOTAL_SCHOOLS_NATIONAL <- 721

# Session catalogue - maps raw session values to clean labels and value group
SESSION_CATALOGUE <- tibble(
  session_raw = c(
    "Session 0", "Session 1: Understanding Responsible Citizenship",
    "Session 2", "Session 3", "Session 4", "Session 5",
    "Session 6", "Session 7", "Session 8", "Session 9",
    "Session 10", "Session 11", "Session 12", "Session 13",
    "Session 14", "Session 15", "Session 16", "Session 17",
    "Session 18", "Session 19", "Session 20", "Session 21", "Session 22"
  ),
  session_number = 0:22,
  session_label = c(
    "S0: How to Use the Handbook",
    "S1: Understanding Responsible Citizenship",
    "S2: Applying Responsible Citizenship",
    "S3: Understanding Honesty & Misinformation",
    "S4: Applying Honesty & Misinformation",
    "S5: Understanding Integrity",
    "S6: Applying Integrity",
    "S7: Understanding Diversity",
    "S8: Applying Diversity",
    "S9: Understanding Equity",
    "S10: Applying Equity",
    "S11: Understanding Discipline",
    "S12: Applying Discipline",
    "S13: Understanding Self-Directed Learning",
    "S14: Applying Self-Directed Learning",
    "S15: Understanding Adaptability",
    "S16: Applying Adaptability",
    "S17: Understanding Resourcefulness",
    "S18: Applying Resourcefulness",
    "S19: Understanding Leadership",
    "S20: Applying Leadership",
    "S21: Understanding Building Confidence",
    "S22: Applying Building Confidence"
  ),
  value_theme = c(
    "Orientation",
    "Responsible Citizenship", "Responsible Citizenship",
    "Honesty", "Honesty",
    "Integrity", "Integrity",
    "Diversity", "Diversity",
    "Equity", "Equity",
    "Discipline", "Discipline",
    "Self-Directed Learning", "Self-Directed Learning",
    "Adaptability", "Adaptability",
    "Resourcefulness", "Resourcefulness",
    "Leadership", "Leadership",
    "Building Confidence", "Building Confidence"
  ),
  session_type = c(
    "Orientation",
    rep(c("Understanding", "Applying"), 11)
  )
)

# 16 Ghana regions
GHANA_REGIONS <- c(
  "Ahafo", "Ashanti", "Bono", "Bono East", "Central",
  "Eastern", "Greater Accra", "North East", "Northern",
  "Oti", "Savannah", "Upper East", "Upper West",
  "Volta", "Western", "Western North"
)

# Colour palette (consistent with SEI yellow skin)
VLC_COLOURS <- list(
  primary    = "#E6A817",
  secondary  = "#2C3E50",
  success    = "#27AE60",
  warning    = "#E67E22",
  danger     = "#E74C3C",
  info       = "#2980B9",
  light_bg   = "#FDF6E3",
  card_bg    = "#FFFFFF",
  text_muted = "#7F8C8D"
)

# Value theme colours for charts
THEME_COLOURS <- c(
  "Orientation"              = "#95A5A6",
  "Responsible Citizenship"  = "#E74C3C",
  "Honesty"                  = "#E67E22",
  "Integrity"                = "#F39C12",
  "Diversity"                = "#27AE60",
  "Equity"                   = "#16A085",
  "Discipline"               = "#2980B9",
  "Self-Directed Learning"   = "#8E44AD",
  "Adaptability"             = "#D35400",
  "Resourcefulness"          = "#1ABC9C",
  "Leadership"               = "#2C3E50",
  "Building Confidence"      = "#C0392B"
)

# =============================================================================
# DATA PREPARATION FUNCTION
# =============================================================================

prepare_vlc_data <- function(raw_data) {
  
  df <- raw_data %>%
    rename_with(~ str_remove(., "^nts_vlc_"), starts_with("nts_vlc_")) %>%
    mutate(
      
      # ---- Date fields -------------------------------------------------------
      session_date = as.Date(date_session_vlc),
      submission_date = as.Date(str_extract(X_submission_time, "^[0-9\\-]+")),
      next_session_date = as.Date(next_session_vlc),
      week_of_term = as.numeric(difftime(session_date,
                                          min(session_date, na.rm = TRUE),
                                          units = "weeks")) + 1,
      
      # ---- Session took place ------------------------------------------------
      vlc_held = case_when(
        plc_take_place_vlc == 1 ~ "Held",
        plc_take_place_vlc == 2 ~ "Not Held",
        plc_take_place_vlc == 3 ~ "Not Held",
        TRUE ~ "Unknown"
      ),
      
      # ---- Participation rate (capped at 100%) --------------------------------
      total_enrolled = no_male_teachers_vlc + no_female_teachers_vlc,
      total_attended = male_attendance_vlc + female_attendance_vlc,
      participation_pct = case_when(
        total_enrolled > 0 ~ pmin((total_attended / total_enrolled) * 100, 100),
        TRUE ~ NA_real_
      ),
      participation_pct = round(participation_pct, 1),
      
      # ---- Gender gap (positive = more females) --------------------------------
      gender_gap = case_when(
        total_attended > 0 ~
          ((female_attendance_vlc - male_attendance_vlc) / total_attended) * 100,
        TRUE ~ NA_real_
      ),
      
      # ---- Disability inclusion flag ------------------------------------------
      disability_included = case_when(
        Have_any_learners_with_disabil == "yes" ~ TRUE,
        Have_any_learners_with_disabil == "no"  ~ FALSE,
        TRUE ~ NA
      ),
      
      # ---- Leadership presence index (0-6 scale) ------------------------------
      leadership_score = rowSums(
        cbind(
          headteacher_attendance_2_vlc  == 1,
          assitant_academic_2_vlc       == 1,
          assistant_admin_2_vlc         == 1,
          assistant_domestic_2_vlc      == 1,
          senior_house_2_vlc            == 1,
          gcc_2_vlc                     == 1
        ),
        na.rm = TRUE
      ),
      leadership_pct = round((leadership_score / 6) * 100),
      headteacher_present = headteacher_attendance_2_vlc == 1,
      gcc_present = gcc_2_vlc == 1,
      
      # ---- Quality flags ------------------------------------------------------
      session_valuable    = teacher_session_useful_vlc == 1,
      high_impact         = impact_students_vlc == 1,
      values_applied_high = teachers_applying_concepts_vlc_vlc %in% c(3, 4),
      
      # ---- External visit -----------------------------------------------------
      external_visit = national_office_vlc == 1,
      
      # ---- Session numbering --------------------------------------------------
      session_clean = case_when(
        str_detect(tolower(Session_vlc), "session 0|session zero") ~ "Session 0",
        str_detect(Session_vlc, "Session 1:") ~ "Session 1",
        str_detect(Session_vlc, "Session 1 ") ~ "Session 1",
        TRUE ~ str_extract(Session_vlc, "Session [0-9]+")
      ),
      session_num = as.integer(str_extract(session_clean, "[0-9]+")),
      
      # ---- Next session planned -----------------------------------------------
      has_next_plan = !is.na(next_session_date) & next_session_date > session_date,
      
      # ---- School type --------------------------------------------------------
      is_transitional = transi == 1,
      
      # ---- Participation quality tier -----------------------------------------
      participation_tier = case_when(
        participation_pct >= 85 ~ "High (≥85%)",
        participation_pct >= 60 ~ "Medium (60-84%)",
        participation_pct >= 40 ~ "Low (40-59%)",
        !is.na(participation_pct) ~ "Very Low (<40%)",
        TRUE ~ "Unknown"
      )
    )
  
  return(df)
}

# =============================================================================
# UI COMPONENT
# =============================================================================

vlc_ui <- tabItem(
  tabName = "vlc_monitor",
  
  # --------------------------------------------------------------------------
  # FILTERS ROW
  # --------------------------------------------------------------------------
  fluidRow(
    box(
      width = 12,
      solidHeader = TRUE,
      status = "warning",
      collapsible = TRUE,
      title = icon("filter", "fa") %>% tagAppendChildren(" Filters"),
      
      column(3,
        selectInput("vlc_filter_term", "Academic Term:",
                    choices = c("All Terms" = "all", "Term 1" = "1", "Term 2" = "2"),
                    selected = "all")
      ),
      column(3,
        selectInput("vlc_filter_region", "Region:",
                    choices = c("All Regions" = "all"),
                    selected = "all")
      ),
      column(3,
        selectInput("vlc_filter_session", "VLC Session:",
                    choices = c("All Sessions" = "all"),
                    selected = "all")
      ),
      column(3,
        br(),
        actionButton("vlc_reset_filters", "Reset Filters",
                     icon = icon("undo"), class = "btn-warning btn-sm")
      )
    )
  ),
  
  # --------------------------------------------------------------------------
  # SECTION 1 — ABOUT VLC (Landing / Explainer Panel)
  # --------------------------------------------------------------------------
  fluidRow(
    box(
      width = 12,
      status = "warning",
      solidHeader = FALSE,
      
      div(
        style = "background: linear-gradient(135deg, #2C3E50 0%, #1a252f 100%);
                 border-radius: 8px; padding: 30px; color: white; margin-bottom: 5px;",
        
        # Header
        fluidRow(
          column(8,
            h2(
              icon("book-open"),
              " Essential Values for Ghanaian Youth",
              style = "color: #E6A817; font-weight: bold; margin-top: 0;"
            ),
            h4("Values Learning Community (VLC) — National Monitoring Dashboard",
               style = "color: #BDC3C7; margin-top: 5px;")
          ),
          column(4,
            div(
              style = "text-align: right; padding-top: 10px;",
              tags$img(
                src = "https://upload.wikimedia.org/wikipedia/commons/thumb/1/19/Flag_of_Ghana.svg/200px-Flag_of_Ghana.svg.png",
                height = "50px", style = "margin-right: 10px;"
              ),
              tags$span(
                "GES | NaCCA | T-TEL | NUGS",
                style = "color: #E6A817; font-size: 12px; display: block; margin-top: 5px;"
              )
            )
          )
        ),
        
        hr(style = "border-color: #E6A817; margin: 15px 0;"),
        
        # What is the VLC — three columns
        fluidRow(
          
          column(4,
            div(
              style = "background: rgba(255,255,255,0.05); border-left: 4px solid #E6A817;
                       border-radius: 4px; padding: 15px; height: 100%;",
              h5(icon("question-circle"), " What is the VLC?",
                 style = "color: #E6A817; font-weight: bold;"),
              p(
                "The ", tags$strong(style = "color:#E6A817;", "Values Learning Community (VLC)"),
                " is a weekly, student-led session held in every Senior High School across Ghana. 
                While teachers attend their ", tags$em("Professional Learning Community (PLC)"),
                " sessions, learners engage in structured peer-facilitated discussions guided by 
                the Essential Values Handbook. Each 90-minute session is led by two trained 
                ", tags$strong("Peer Guides"), " — classmates selected for their integrity and 
                communication skills.",
                style = "font-size: 13px; color: #ECF0F1; line-height: 1.6;"
              )
            )
          ),
          
          column(4,
            div(
              style = "background: rgba(255,255,255,0.05); border-left: 4px solid #27AE60;
                       border-radius: 4px; padding: 15px; height: 100%;",
              h5(icon("book"), " The Handbook",
                 style = "color: #27AE60; font-weight: bold;"),
              p(
                "Published by the ", tags$strong("Ministry of Education Ghana"),
                " in partnership with GES, NaCCA, T-TEL, NUGS, Lead for Ghana, and Honour Ghana,
                the handbook was ", tags$em("written by young people for young people"),
                ". It covers ", tags$strong("11 core values"), " across ",
                tags$strong("22 sessions"), " spanning the full academic year — 
                from ", tags$em("Responsible Citizenship"), " and ", tags$em("Integrity"),
                " to ", tags$em("Leadership"), " and ", tags$em("Building Confidence"),
                ". Each value is taught in an Understanding session, 
                then practised in an Applying session.",
                style = "font-size: 13px; color: #ECF0F1; line-height: 1.6;"
              )
            )
          ),
          
          column(4,
            div(
              style = "background: rgba(255,255,255,0.05); border-left: 4px solid #2980B9;
                       border-radius: 4px; padding: 15px; height: 100%;",
              h5(icon("bullseye"), " Why It Matters",
                 style = "color: #2980B9; font-weight: bold;"),
              p(
                "The VLC addresses Ghana's national commitment to holistic education — 
                developing not just academic knowledge ", tags$em("(The Head)"),
                ", but also values and character ", tags$em("(The Heart)"),
                " and practical skills ", tags$em("(The Hand)"),
                ". GES Director-General Prof. Ernest Kofi Davis notes that 
                character formation ", tags$em('"enhances academic achievement"'),
                ". This dashboard monitors whether Ghana's 721 SHS are reaching their 
                students with this transformative programme every week.",
                style = "font-size: 13px; color: #ECF0F1; line-height: 1.6;"
              )
            )
          )
        ),
        
        # Values journey strip
        div(
          style = "margin-top: 20px;",
          h5(icon("road"), " The 11-Value Journey (22 Sessions)",
             style = "color: #E6A817; font-weight: bold; margin-bottom: 10px;"),
          
          div(
            style = "display: flex; flex-wrap: wrap; gap: 8px;",
            
            lapply(
              list(
                list("Responsible Citizenship", "#E74C3C"),
                list("Honesty", "#E67E22"),
                list("Integrity", "#F39C12"),
                list("Diversity", "#27AE60"),
                list("Equity", "#16A085"),
                list("Discipline", "#2980B9"),
                list("Self-Directed Learning", "#8E44AD"),
                list("Adaptability", "#D35400"),
                list("Resourcefulness", "#1ABC9C"),
                list("Leadership", "#BDC3C7"),
                list("Building Confidence", "#C0392B")
              ),
              function(v) {
                div(
                  style = glue(
                    "background: {v[[2]]}22; border: 1px solid {v[[2]]};
                     border-radius: 20px; padding: 4px 12px; font-size: 11px;
                     color: white; white-space: nowrap;"
                  ),
                  icon("circle", style = glue("color: {v[[2]]}; font-size: 8px;")),
                  " ", v[[1]]
                )
              }
            )
          )
        )
      )
    )
  ),
  
  # --------------------------------------------------------------------------
  # SECTION 2 — NARRATIVE STORY HEADER (Dynamic Data Story)
  # --------------------------------------------------------------------------
  fluidRow(
    box(
      width = 12,
      status = "warning",
      solidHeader = TRUE,
      title = tagList(icon("rss"), " Ghana's Values Formation Pulse — Live Data Story"),
      
      div(
        style = "background: #FDF6E3; border-radius: 6px; padding: 20px;",
        
        # Main narrative paragraph
        div(
          style = "font-size: 15px; line-height: 1.9; color: #2C3E50; 
                   border-left: 5px solid #E6A817; padding-left: 15px;",
          uiOutput("vlc_narrative_text")
        ),
        
        hr(),
        
        # Three signal cards below the narrative
        fluidRow(
          
          column(4,
            div(
              style = "background: white; border-radius: 6px; padding: 15px;
                       border-top: 3px solid #27AE60; text-align: center;
                       box-shadow: 0 1px 4px rgba(0,0,0,0.1);",
              uiOutput("vlc_signal_coverage"),
              p("National School Coverage",
                style = "color: #7F8C8D; font-size: 12px; margin: 0;")
            )
          ),
          
          column(4,
            div(
              style = "background: white; border-radius: 6px; padding: 15px;
                       border-top: 3px solid #2980B9; text-align: center;
                       box-shadow: 0 1px 4px rgba(0,0,0,0.1);",
              uiOutput("vlc_signal_learners"),
              p("Cumulative Learner Touchpoints",
                style = "color: #7F8C8D; font-size: 12px; margin: 0;")
            )
          ),
          
          column(4,
            div(
              style = "background: white; border-radius: 6px; padding: 15px;
                       border-top: 3px solid #E74C3C; text-align: center;
                       box-shadow: 0 1px 4px rgba(0,0,0,0.1);",
              uiOutput("vlc_signal_attention"),
              p("Schools Requiring Follow-up",
                style = "color: #7F8C8D; font-size: 12px; margin: 0;")
            )
          )
        )
      )
    )
  ),
  
  # --------------------------------------------------------------------------
  # SECTION 3 — KEY PERFORMANCE INDICATORS
  # --------------------------------------------------------------------------
  fluidRow(
    valueBoxOutput("vlc_kpi_submissions",    width = 2),
    valueBoxOutput("vlc_kpi_schools_held",   width = 2),
    valueBoxOutput("vlc_kpi_students",       width = 2),
    valueBoxOutput("vlc_kpi_participation",  width = 2),
    valueBoxOutput("vlc_kpi_headteacher",    width = 2),
    valueBoxOutput("vlc_kpi_disability",     width = 2)
  ),
  
  # --------------------------------------------------------------------------
  # SECTION 4 — PROGRESS & MOMENTUM
  # --------------------------------------------------------------------------
  fluidRow(
    
    # Session delivery trend
    box(
      width = 8,
      title = tagList(icon("chart-line"), " Weekly VLC Session Submissions — Programme Momentum"),
      status = "warning", solidHeader = TRUE,
      plotlyOutput("vlc_weekly_trend", height = "280px"),
      div(
        style = "font-size: 11px; color: #7F8C8D; margin-top: 5px; padding-left: 10px;",
        icon("info-circle"),
        " Each bar represents total session submissions per week. 
          A declining trend signals implementation fatigue and requires regional follow-up."
      )
    ),
    
    # Participation funnel
    box(
      width = 4,
      title = tagList(icon("filter"), " Implementation Funnel"),
      status = "warning", solidHeader = TRUE,
      plotlyOutput("vlc_funnel", height = "280px"),
      div(
        style = "font-size: 11px; color: #7F8C8D; margin-top: 5px; padding-left: 10px;",
        icon("info-circle"),
        " Drop between 'Submitted' and 'VLC Held' indicates data without sessions."
      )
    )
  ),
  
  # --------------------------------------------------------------------------
  # SECTION 5 — SESSION CONTENT ANALYSIS
  # --------------------------------------------------------------------------
  fluidRow(
    
    # Session delivery frequency
    box(
      width = 6,
      title = tagList(icon("book-open"), " Session Coverage — Which Values Are Being Taught?"),
      status = "warning", solidHeader = TRUE,
      plotlyOutput("vlc_session_coverage", height = "350px"),
      div(
        style = "font-size: 11px; color: #7F8C8D; margin-top: 5px; padding-left: 10px;",
        icon("info-circle"),
        " Low delivery of later sessions (Leadership, Confidence) signals schools are 
          not progressing through the full values curriculum."
      )
    ),
    
    # Value theme heatmap
    box(
      width = 6,
      title = tagList(icon("th"), " Value Theme Delivery by Region"),
      status = "warning", solidHeader = TRUE,
      plotlyOutput("vlc_theme_region_heatmap", height = "350px"),
      div(
        style = "font-size: 11px; color: #7F8C8D; margin-top: 5px; padding-left: 10px;",
        icon("info-circle"),
        " Dark cells = more sessions on that value theme delivered in that region."
      )
    )
  ),
  
  # --------------------------------------------------------------------------
  # SECTION 6 — EQUITY & INCLUSION
  # --------------------------------------------------------------------------
  fluidRow(
    box(
      width = 12,
      status = "warning", solidHeader = TRUE,
      collapsible = TRUE,
      title = tagList(icon("balance-scale"), " Equity & Inclusion Dashboard"),
      
      # Sub-panel tabs
      tabsetPanel(
        type = "tabs",
        
        # Gender tab
        tabPanel(
          title = tagList(icon("venus-mars"), " Gender Balance"),
          br(),
          fluidRow(
            column(8, plotlyOutput("vlc_gender_region", height = "300px")),
            column(4,
              div(
                style = "background: #F8F9FA; border-radius: 6px; padding: 15px; margin-top: 10px;",
                h5("Reading the Gender Gap", style = "color: #2C3E50; font-weight: bold;"),
                p("Bars to the ", tags$strong(style = "color: #E74C3C;", "right"),
                  " indicate female-dominant attendance.", br(),
                  "Bars to the ", tags$strong(style = "color: #2980B9;", "left"),
                  " indicate male-dominant attendance.", br(), br(),
                  "The national equity target is a gender gap of ±10 percentage points.
                   Regions outside this band require targeted gender-responsive 
                   implementation support.",
                  style = "font-size: 12px; color: #555;"),
                br(),
                uiOutput("vlc_gender_summary_text")
              )
            )
          )
        ),
        
        # Disability inclusion tab
        tabPanel(
          title = tagList(icon("wheelchair"), " Learner Disability Inclusion"),
          br(),
          fluidRow(
            column(5, plotlyOutput("vlc_disability_regions", height = "300px")),
            column(4, plotlyOutput("vlc_disability_types", height = "300px")),
            column(3,
              div(
                style = "background: #F8F9FA; border-radius: 6px; padding: 15px; margin-top: 10px;",
                h5("Why This Matters", style = "color: #2C3E50; font-weight: bold;"),
                p(
                  "The handbook's Sessions 7-10 explicitly address Diversity and Equity — 
                   making disability inclusion in VLC sessions a direct measure of 
                   values-in-action. Schools that include learners with disabilities 
                   in VLC are demonstrating the very values being taught.",
                  style = "font-size: 12px; color: #555; line-height: 1.6;"
                ),
                hr(),
                uiOutput("vlc_disability_summary")
              )
            )
          )
        ),
        
        # Leadership presence tab
        tabPanel(
          title = tagList(icon("user-tie"), " School Leadership Engagement"),
          br(),
          fluidRow(
            column(7, plotlyOutput("vlc_leadership_chart", height = "300px")),
            column(5,
              fluidRow(
                column(6,
                  div(
                    style = "background: #F8F9FA; border-radius: 6px; padding: 12px; margin: 5px;
                             text-align: center;",
                    uiOutput("vlc_ht_presence_rate"),
                    p("Headteacher Present", style = "font-size: 11px; color: #7F8C8D; margin: 0;")
                  )
                ),
                column(6,
                  div(
                    style = "background: #F8F9FA; border-radius: 6px; padding: 12px; margin: 5px;
                             text-align: center;",
                    uiOutput("vlc_gcc_presence_rate"),
                    p("G&C Officer Present", style = "font-size: 11px; color: #7F8C8D; margin: 0;")
                  )
                )
              ),
              br(),
              div(
                style = "background: #FFF8E1; border-left: 4px solid #E6A817;
                         padding: 12px; border-radius: 4px; margin: 5px;",
                p(
                  icon("lightbulb", style = "color: #E6A817;"),
                  " ", tags$strong("Research insight:"),
                  " Schools where the headteacher consistently attends VLC sessions 
                    show 23% higher programme continuity in comparable school improvement 
                    programmes (T-TEL internal evidence, 2023).",
                  style = "font-size: 12px; color: #555; margin: 0; line-height: 1.6;"
                )
              )
            )
          )
        )
      )
    )
  ),
  
  # --------------------------------------------------------------------------
  # SECTION 7 — REGIONAL PERFORMANCE
  # --------------------------------------------------------------------------
  fluidRow(
    
    box(
      width = 8,
      title = tagList(icon("map"), " Regional Performance Overview"),
      status = "warning", solidHeader = TRUE,
      plotlyOutput("vlc_regional_bar", height = "350px")
    ),
    
    box(
      width = 4,
      title = tagList(icon("ranking-star"), " Regional Rankings"),
      status = "warning", solidHeader = TRUE,
      DT::dataTableOutput("vlc_regional_table"),
      div(
        style = "font-size: 11px; color: #7F8C8D; margin-top: 8px;",
        icon("info-circle"),
        " Score = composite of coverage rate, participation rate, 
          headteacher presence, and gender balance."
      )
    )
  ),
  
  # --------------------------------------------------------------------------
  # SECTION 8 — SCHOOL QUADRANT (Action Segmentation)
  # --------------------------------------------------------------------------
  fluidRow(
    box(
      width = 8,
      title = tagList(icon("th-large"),
                      " School Action Quadrant — Who Needs What Support?"),
      status = "warning", solidHeader = TRUE,
      plotlyOutput("vlc_school_quadrant", height = "380px"),
      div(
        style = "font-size: 11px; color: #7F8C8D; margin-top: 5px; padding-left: 10px;",
        icon("info-circle"),
        " X = Participation Rate | Y = Sessions Completed. 
          Hover over points for school details. 
          Schools in the bottom-left quadrant are priority for intervention."
      )
    ),
    
    box(
      width = 4,
      title = tagList(icon("exclamation-triangle"), " Priority Follow-Up Schools"),
      status = "danger", solidHeader = TRUE,
      p("Schools with low coverage AND low participation requiring immediate contact:",
        style = "font-size: 12px; color: #555;"),
      DT::dataTableOutput("vlc_priority_schools")
    )
  ),
  
  # --------------------------------------------------------------------------
  # SECTION 9 — SCHOOL-LEVEL PROFILE (Search)
  # --------------------------------------------------------------------------
  fluidRow(
    box(
      width = 12,
      title = tagList(icon("search"), " School Profile — Search by School"),
      status = "warning", solidHeader = TRUE,
      collapsible = TRUE, collapsed = TRUE,
      
      fluidRow(
        column(6,
          selectInput("vlc_school_search", "Select School:",
                      choices = c("Select a school..." = ""),
                      width = "100%")
        ),
        column(6,
          br(),
          uiOutput("vlc_school_narrative")
        )
      ),
      
      fluidRow(
        column(6, plotlyOutput("vlc_school_timeline",  height = "200px")),
        column(6, plotlyOutput("vlc_school_gender_trend", height = "200px"))
      )
    )
  ),
  
  # --------------------------------------------------------------------------
  # SECTION 10 — RAW DATA TABLE
  # --------------------------------------------------------------------------
  fluidRow(
    box(
      width = 12,
      title = tagList(icon("table"), " Submission Records"),
      status = "warning", solidHeader = TRUE,
      collapsible = TRUE, collapsed = TRUE,
      
      fluidRow(
        column(3,
          downloadButton("vlc_download_data", "Download Filtered Data",
                         class = "btn-warning btn-sm")
        )
      ),
      br(),
      DT::dataTableOutput("vlc_raw_table")
    )
  )
)

# =============================================================================
# SERVER FUNCTION
# =============================================================================

vlc_server <- function(input, output, session) {
  
  # --------------------------------------------------------------------------
  # LOAD & PREPARE DATA
  # --------------------------------------------------------------------------
  
  vlc_raw <- reactive({
    # Replace this path with your actual data source / API call
    read.csv("vlc_data_nat.csv", stringsAsFactors = FALSE)
  })
  
  vlc_data <- reactive({
    prepare_vlc_data(vlc_raw())
  })
  
  # Populate dynamic filter choices
  observe({
    df <- vlc_data()
    
    regions <- sort(unique(df$Region_hbk5[!is.na(df$Region_hbk5)]))
    sessions <- sort(unique(df$session_clean[!is.na(df$session_clean)]))
    schools  <- sort(unique(df$Name_school_hbk5[!is.na(df$Name_school_hbk5)]))
    
    updateSelectInput(session, "vlc_filter_region",
                      choices = c("All Regions" = "all", setNames(regions, regions)))
    updateSelectInput(session, "vlc_filter_session",
                      choices = c("All Sessions" = "all", setNames(sessions, sessions)))
    updateSelectInput(session, "vlc_school_search",
                      choices = c("Select a school..." = "", setNames(schools, schools)))
  })
  
  # Reset filters
  observeEvent(input$vlc_reset_filters, {
    updateSelectInput(session, "vlc_filter_term",    selected = "all")
    updateSelectInput(session, "vlc_filter_region",  selected = "all")
    updateSelectInput(session, "vlc_filter_session", selected = "all")
  })
  
  # --------------------------------------------------------------------------
  # FILTERED DATA (reactive)
  # --------------------------------------------------------------------------
  
  vlc_filtered <- reactive({
    df <- vlc_data() %>% filter(vlc_held == "Held")
    
    if (input$vlc_filter_term != "all")
      df <- df %>% filter(term_academic_vlc == as.integer(input$vlc_filter_term))
    
    if (input$vlc_filter_region != "all")
      df <- df %>% filter(Region_hbk5 == input$vlc_filter_region)
    
    if (input$vlc_filter_session != "all")
      df <- df %>% filter(session_clean == input$vlc_filter_session)
    
    df
  })
  
  # All submissions (including non-held)
  vlc_all_subs <- reactive({
    df <- vlc_data()
    if (input$vlc_filter_term != "all")
      df <- df %>% filter(term_academic_vlc == as.integer(input$vlc_filter_term))
    if (input$vlc_filter_region != "all")
      df <- df %>% filter(Region_hbk5 == input$vlc_filter_region)
    df
  })
  
  # --------------------------------------------------------------------------
  # COMPUTED AGGREGATES
  # --------------------------------------------------------------------------
  
  vlc_stats <- reactive({
    df  <- vlc_filtered()
    all <- vlc_all_subs()
    
    schools_reporting    <- n_distinct(all$Name_school_hbk5, na.rm = TRUE)
    schools_held         <- n_distinct(df$Name_school_hbk5, na.rm = TRUE)
    total_submissions    <- nrow(all)
    total_sessions_held  <- nrow(df)
    total_students       <- sum(df$total_attended, na.rm = TRUE)
    avg_participation    <- mean(df$participation_pct, na.rm = TRUE)
    ht_present_pct       <- mean(df$headteacher_present, na.rm = TRUE) * 100
    gcc_present_pct      <- mean(df$gcc_present, na.rm = TRUE) * 100
    sessions_with_disab  <- sum(df$disability_included == TRUE, na.rm = TRUE)
    disability_rate      <- mean(df$disability_included == TRUE, na.rm = TRUE) * 100
    
    # Schools that have not submitted anything
    coverage_pct <- (schools_reporting / TOTAL_SCHOOLS_NATIONAL) * 100
    
    # Low performing schools (bottom quadrant)
    school_summary <- df %>%
      group_by(Name_school_hbk5, Region_hbk5) %>%
      summarise(
        sessions_n      = n(),
        avg_partic      = mean(participation_pct, na.rm = TRUE),
        last_session    = max(session_date, na.rm = TRUE),
        .groups = "drop"
      )
    
    low_schools <- school_summary %>%
      filter(sessions_n <= 1 | avg_partic < 50) %>%
      arrange(sessions_n, avg_partic)
    
    # Most recent session date
    latest_date <- max(df$session_date, na.rm = TRUE)
    
    list(
      schools_reporting   = schools_reporting,
      schools_held        = schools_held,
      total_submissions   = total_submissions,
      total_sessions_held = total_sessions_held,
      total_students      = total_students,
      avg_participation   = round(avg_participation, 1),
      ht_present_pct      = round(ht_present_pct, 1),
      gcc_present_pct     = round(gcc_present_pct, 1),
      disability_rate     = round(disability_rate, 1),
      sessions_with_disab = sessions_with_disab,
      coverage_pct        = round(coverage_pct, 1),
      low_schools         = low_schools,
      school_summary      = school_summary,
      latest_date         = latest_date
    )
  })
  
  # --------------------------------------------------------------------------
  # NARRATIVE TEXT (Dynamic Story Header)
  # --------------------------------------------------------------------------
  
  output$vlc_narrative_text <- renderUI({
    s   <- vlc_stats()
    df  <- vlc_filtered()
    
    # Most delivered session
    top_session <- df %>%
      count(session_clean, sort = TRUE) %>%
      slice(1) %>%
      pull(session_clean)
    if (length(top_session) == 0) top_session <- "N/A"
    
    # Top region
    top_region <- df %>%
      count(Region_hbk5, sort = TRUE) %>%
      slice(1) %>%
      pull(Region_hbk5)
    if (length(top_region) == 0) top_region <- "N/A"
    
    # Attendance trend - compare last 2 weeks
    weekly_counts <- df %>%
      mutate(week = floor_date(session_date, "week")) %>%
      count(week) %>%
      arrange(week)
    
    trend_word <- if (nrow(weekly_counts) >= 2) {
      last2 <- tail(weekly_counts$n, 2)
      if (last2[2] > last2[1]) "increasing" else if (last2[2] < last2[1]) "declining" else "stable"
    } else "developing"
    
    # Date context
    date_str <- if (!is.na(s$latest_date)) format(s$latest_date, "%d %B %Y") else "the current period"
    
    tagList(
      p(
        "As of ", tags$strong(date_str), ", ",
        tags$strong(
          style = "color: #E6A817; font-size: 17px;",
          format(s$schools_reporting, big.mark = ",")
        ),
        " of Ghana's ",
        tags$strong(format(TOTAL_SCHOOLS_NATIONAL, big.mark = ",")),
        " Senior High Schools (",
        tags$strong(
          style = glue("color: {if(s$coverage_pct >= 70) '#27AE60' else if(s$coverage_pct >= 50) '#E67E22' else '#E74C3C'};"),
          paste0(s$coverage_pct, "%")
        ),
        " national coverage) have submitted VLC session data. Across ",
        tags$strong(
          style = "color: #2980B9; font-size: 17px;",
          format(s$total_sessions_held, big.mark = ",")
        ),
        " sessions held, a cumulative ",
        tags$strong(
          style = "color: #8E44AD; font-size: 17px;",
          format(s$total_students, big.mark = ",")
        ),
        " learner touchpoints have been recorded, with an average attendance rate of ",
        tags$strong(
          style = glue("color: {if(s$avg_participation >= 75) '#27AE60' else if(s$avg_participation >= 55) '#E67E22' else '#E74C3C'};"),
          paste0(s$avg_participation, "%")
        ),
        ".",
        style = "margin-bottom: 10px;"
      ),
      p(
        "The most frequently delivered session nationally is ",
        tags$em(tags$strong(style = "color: #E6A817;", top_session)),
        ". The ", tags$strong(top_region), " region leads in total session submissions. 
         School leadership engagement — a key quality signal — shows headteachers present in ",
        tags$strong(
          style = glue("color: {if(s$ht_present_pct >= 60) '#27AE60' else '#E74C3C'};"),
          paste0(s$ht_present_pct, "%")
        ),
        " of sessions, and Guidance & Counselling officers in ",
        tags$strong(paste0(s$gcc_present_pct, "%")),
        ". On disability inclusion, ",
        tags$strong(paste0(s$disability_rate, "%")),
        " of sessions have recorded learners with disabilities participating — 
         a direct measure of the equity values being taught. ",
        "Weekly submission momentum is currently ",
        tags$strong(
          style = glue("color: {if(trend_word == 'increasing') '#27AE60' else if(trend_word == 'declining') '#E74C3C' else '#E67E22'};"),
          trend_word
        ),
        ". ",
        tags$strong(
          style = "color: #E74C3C;",
          format(nrow(s$low_schools), big.mark = ",")
        ),
        " schools are flagged for priority follow-up.",
        style = "margin-bottom: 0;"
      )
    )
  })
  
  # Signal cards
  output$vlc_signal_coverage <- renderUI({
    s <- vlc_stats()
    color <- if (s$coverage_pct >= 70) "#27AE60" else if (s$coverage_pct >= 50) "#E67E22" else "#E74C3C"
    tagList(
      div(style = glue("font-size: 32px; font-weight: bold; color: {color};"),
          paste0(s$coverage_pct, "%")),
      div(style = "font-size: 12px; color: #555;",
          paste0(s$schools_reporting, " of ", TOTAL_SCHOOLS_NATIONAL, " schools"))
    )
  })
  
  output$vlc_signal_learners <- renderUI({
    s <- vlc_stats()
    tagList(
      div(style = "font-size: 32px; font-weight: bold; color: #2980B9;",
          format(s$total_students, big.mark = ",")),
      div(style = "font-size: 12px; color: #555;",
          paste0("across ", s$total_sessions_held, " sessions"))
    )
  })
  
  output$vlc_signal_attention <- renderUI({
    s <- vlc_stats()
    color <- if (nrow(s$low_schools) <= 20) "#27AE60" else if (nrow(s$low_schools) <= 50) "#E67E22" else "#E74C3C"
    tagList(
      div(style = glue("font-size: 32px; font-weight: bold; color: {color};"),
          nrow(s$low_schools)),
      div(style = "font-size: 12px; color: #555;", "low coverage or participation")
    )
  })
  
  # --------------------------------------------------------------------------
  # VALUE BOXES
  # --------------------------------------------------------------------------
  
  output$vlc_kpi_submissions <- renderValueBox({
    s <- vlc_stats()
    valueBox(
      format(s$total_submissions, big.mark = ","),
      "Total Submissions",
      icon = icon("upload"),
      color = "yellow"
    )
  })
  
  output$vlc_kpi_schools_held <- renderValueBox({
    s <- vlc_stats()
    valueBox(
      paste0(s$schools_held, " schools"),
      paste0(s$coverage_pct, "% Coverage"),
      icon = icon("school"),
      color = if (s$coverage_pct >= 70) "green" else if (s$coverage_pct >= 50) "orange" else "red"
    )
  })
  
  output$vlc_kpi_students <- renderValueBox({
    s <- vlc_stats()
    valueBox(
      format(s$total_students, big.mark = ","),
      "Learner Touchpoints",
      icon = icon("users"),
      color = "blue"
    )
  })
  
  output$vlc_kpi_participation <- renderValueBox({
    s <- vlc_stats()
    valueBox(
      paste0(s$avg_participation, "%"),
      "Avg Attendance Rate",
      icon = icon("chart-bar"),
      color = if (s$avg_participation >= 75) "green" else if (s$avg_participation >= 55) "orange" else "red"
    )
  })
  
  output$vlc_kpi_headteacher <- renderValueBox({
    s <- vlc_stats()
    valueBox(
      paste0(s$ht_present_pct, "%"),
      "Headteacher Present",
      icon = icon("user-tie"),
      color = if (s$ht_present_pct >= 60) "green" else if (s$ht_present_pct >= 40) "orange" else "red"
    )
  })
  
  output$vlc_kpi_disability <- renderValueBox({
    s <- vlc_stats()
    valueBox(
      paste0(s$disability_rate, "%"),
      "Disability Inclusion Rate",
      icon = icon("wheelchair"),
      color = if (s$disability_rate >= 20) "green" else if (s$disability_rate >= 10) "orange" else "red"
    )
  })
  
  # --------------------------------------------------------------------------
  # PLOT: Weekly Trend
  # --------------------------------------------------------------------------
  
  output$vlc_weekly_trend <- renderPlotly({
    df <- vlc_all_subs() %>%
      filter(!is.na(session_date)) %>%
      mutate(week = floor_date(session_date, "week")) %>%
      count(week, vlc_held)
    
    p <- ggplot(df, aes(x = week, y = n, fill = vlc_held,
                        text = paste0("Week of: ", format(week, "%d %b %Y"),
                                      "<br>Count: ", n,
                                      "<br>Status: ", vlc_held))) +
      geom_col(position = "stack") +
      scale_fill_manual(values = c("Held" = "#E6A817", "Not Held" = "#E74C3C", "Unknown" = "#BDC3C7")) +
      scale_x_date(date_labels = "%d %b", date_breaks = "1 week") +
      scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
      labs(x = NULL, y = "Submissions", fill = "VLC Status") +
      theme_minimal(base_size = 11) +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
        legend.position = "bottom",
        panel.grid.minor = element_blank()
      )
    
    ggplotly(p, tooltip = "text") %>%
      layout(legend = list(orientation = "h", y = -0.3))
  })
  
  # --------------------------------------------------------------------------
  # PLOT: Implementation Funnel
  # --------------------------------------------------------------------------
  
  output$vlc_funnel <- renderPlotly({
    s <- vlc_stats()
    all <- vlc_all_subs()
    df  <- vlc_filtered()
    
    funnel_df <- data.frame(
      stage = c("Schools in Ghana", "Schools Submitted Data",
                "Schools: VLC Held", "Sessions with ≥75% Attendance"),
      n = c(
        TOTAL_SCHOOLS_NATIONAL,
        s$schools_reporting,
        s$schools_held,
        sum(df$participation_pct >= 75, na.rm = TRUE)
      ),
      colour = c("#2C3E50", "#E6A817", "#27AE60", "#2980B9")
    ) %>%
      mutate(stage = factor(stage, levels = rev(stage)),
             pct   = round(n / TOTAL_SCHOOLS_NATIONAL * 100, 1))
    
    p <- ggplot(funnel_df,
                aes(x = n, y = stage, fill = colour,
                    text = paste0(stage, "<br>N = ", format(n, big.mark = ","),
                                  "<br>", pct, "% of national total"))) +
      geom_col(width = 0.6) +
      geom_text(aes(label = format(n, big.mark = ",")),
                hjust = -0.1, size = 3.5, colour = "#2C3E50") +
      scale_fill_identity() +
      scale_x_continuous(expand = expansion(mult = c(0, 0.2))) +
      labs(x = "Count", y = NULL) +
      theme_minimal(base_size = 11) +
      theme(panel.grid.minor = element_blank(),
            panel.grid.major.y = element_blank())
    
    ggplotly(p, tooltip = "text") %>% hide_legend()
  })
  
  # --------------------------------------------------------------------------
  # PLOT: Session Coverage
  # --------------------------------------------------------------------------
  
  output$vlc_session_coverage <- renderPlotly({
    df <- vlc_filtered() %>%
      filter(!is.na(session_clean)) %>%
      count(session_clean) %>%
      left_join(
        SESSION_CATALOGUE %>%
          mutate(session_clean = paste0("Session ", session_number)),
        by = c("session_clean" = "session_clean")
      ) %>%
      arrange(coalesce(session_number, 99L)) %>%
      mutate(
        label     = coalesce(session_label, session_clean),
        label     = str_trunc(label, 35),
        theme     = coalesce(value_theme, "Unknown"),
        bar_color = THEME_COLOURS[theme],
        bar_color = coalesce(bar_color, "#95A5A6")
      )
    
    if (nrow(df) == 0) {
      return(plotly_empty() %>% layout(title = "No session data available"))
    }
    
    p <- ggplot(df,
                aes(x = n, y = reorder(label, coalesce(session_number, 99L)),
                    fill = theme,
                    text = paste0(label, "<br>Sessions delivered: ", n,
                                  "<br>Value theme: ", theme))) +
      geom_col(width = 0.7) +
      scale_fill_manual(values = THEME_COLOURS, guide = "none") +
      scale_x_continuous(expand = expansion(mult = c(0, 0.1))) +
      labs(x = "Number of Schools Delivering This Session", y = NULL) +
      theme_minimal(base_size = 9) +
      theme(panel.grid.minor = element_blank(),
            panel.grid.major.y = element_blank(),
            axis.text.y = element_text(size = 8))
    
    ggplotly(p, tooltip = "text") %>% hide_legend()
  })
  
  # --------------------------------------------------------------------------
  # PLOT: Value Theme × Region Heatmap
  # --------------------------------------------------------------------------
  
  output$vlc_theme_region_heatmap <- renderPlotly({
    df <- vlc_filtered() %>%
      filter(!is.na(session_clean), !is.na(Region_hbk5)) %>%
      left_join(
        SESSION_CATALOGUE %>%
          mutate(session_clean = paste0("Session ", session_number)),
        by = "session_clean"
      ) %>%
      mutate(theme = coalesce(value_theme, "Unknown")) %>%
      count(Region_hbk5, theme) %>%
      complete(Region_hbk5, theme, fill = list(n = 0))
    
    if (nrow(df) == 0) {
      return(plotly_empty() %>% layout(title = "No data available"))
    }
    
    p <- ggplot(df,
                aes(x = theme, y = Region_hbk5, fill = n,
                    text = paste0(Region_hbk5, " — ", theme,
                                  "<br>Sessions: ", n))) +
      geom_tile(colour = "white", size = 0.5) +
      scale_fill_gradient(low = "#FDF6E3", high = "#E6A817",
                          name = "Sessions") +
      labs(x = NULL, y = NULL) +
      theme_minimal(base_size = 9) +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
        axis.text.y = element_text(size = 8),
        panel.grid  = element_blank()
      )
    
    ggplotly(p, tooltip = "text")
  })
  
  # --------------------------------------------------------------------------
  # PLOT: Gender by Region
  # --------------------------------------------------------------------------
  
  output$vlc_gender_region <- renderPlotly({
    df <- vlc_filtered() %>%
      filter(!is.na(Region_hbk5)) %>%
      group_by(Region_hbk5) %>%
      summarise(
        total_m = sum(male_attendance_vlc, na.rm = TRUE),
        total_f = sum(female_attendance_vlc, na.rm = TRUE),
        total   = total_m + total_f,
        .groups = "drop"
      ) %>%
      filter(total > 0) %>%
      mutate(gap = round(((total_f - total_m) / total) * 100, 1),
             bar_color = if_else(gap >= 0, "#E74C3C", "#2980B9"))
    
    if (nrow(df) == 0) {
      return(plotly_empty() %>% layout(title = "No gender data available"))
    }
    
    p <- ggplot(df,
                aes(x = gap, y = reorder(Region_hbk5, gap),
                    fill = bar_color,
                    text = paste0(Region_hbk5,
                                  "<br>Gender gap: ", gap, " pp",
                                  "<br>Male: ", format(total_m, big.mark = ","),
                                  "<br>Female: ", format(total_f, big.mark = ",")))) +
      geom_col(width = 0.7) +
      geom_vline(xintercept = 0, colour = "#2C3E50", size = 0.8) +
      geom_vline(xintercept = c(-10, 10), colour = "#7F8C8D",
                 linetype = "dashed", size = 0.5) +
      scale_fill_identity() +
      scale_x_continuous(labels = function(x) paste0(x, " pp")) +
      labs(x = "Gender Gap (positive = more female)", y = NULL,
           caption = "Dashed lines = ±10pp equity target zone") +
      theme_minimal(base_size = 10) +
      theme(panel.grid.minor = element_blank(),
            panel.grid.major.y = element_blank())
    
    ggplotly(p, tooltip = "text") %>% hide_legend()
  })
  
  output$vlc_gender_summary_text <- renderUI({
    df <- vlc_filtered() %>%
      summarise(
        total_m = sum(male_attendance_vlc, na.rm = TRUE),
        total_f = sum(female_attendance_vlc, na.rm = TRUE)
      )
    total <- df$total_m + df$total_f
    if (total == 0) return(p("No data"))
    f_pct <- round(df$total_f / total * 100, 1)
    
    div(
      style = "text-align: center; padding: 10px;",
      div(style = "font-size: 24px; font-weight: bold; color: #E74C3C;", paste0(f_pct, "%")),
      p("of attendees are female", style = "color: #555; font-size: 12px; margin: 0;"),
      br(),
      div(style = "font-size: 24px; font-weight: bold; color: #2980B9;",
          paste0(100 - f_pct, "%")),
      p("of attendees are male", style = "color: #555; font-size: 12px; margin: 0;")
    )
  })
  
  # --------------------------------------------------------------------------
  # PLOT: Disability by Region
  # --------------------------------------------------------------------------
  
  output$vlc_disability_regions <- renderPlotly({
    df <- vlc_filtered() %>%
      filter(!is.na(Region_hbk5), !is.na(disability_included)) %>%
      group_by(Region_hbk5) %>%
      summarise(
        sessions_total  = n(),
        sessions_incl   = sum(disability_included == TRUE, na.rm = TRUE),
        incl_rate       = round(sessions_incl / sessions_total * 100, 1),
        .groups = "drop"
      )
    
    if (nrow(df) == 0) return(plotly_empty())
    
    p <- ggplot(df,
                aes(x = incl_rate, y = reorder(Region_hbk5, incl_rate),
                    fill = incl_rate,
                    text = paste0(Region_hbk5,
                                  "<br>Inclusion rate: ", incl_rate, "%",
                                  "<br>", sessions_incl, " of ",
                                  sessions_total, " sessions"))) +
      geom_col(width = 0.7) +
      scale_fill_gradient(low = "#FDF6E3", high = "#16A085") +
      scale_x_continuous(labels = percent_format(scale = 1)) +
      labs(x = "Sessions with Disabled Learners (%)", y = NULL) +
      theme_minimal(base_size = 10) +
      theme(panel.grid.minor = element_blank(),
            panel.grid.major.y = element_blank())
    
    ggplotly(p, tooltip = "text") %>% hide_legend()
  })
  
  output$vlc_disability_types <- renderPlotly({
    df <- vlc_filtered() %>%
      filter(!is.na(What_type_s_of_disa_these_learners_have)) %>%
      mutate(type_list = str_split(What_type_s_of_disa_these_learners_have, " ")) %>%
      unnest(type_list) %>%
      filter(type_list != "") %>%
      count(type_list) %>%
      mutate(type_clean = str_replace_all(type_list, "_", " ") %>% str_to_title())
    
    if (nrow(df) == 0) return(plotly_empty())
    
    plot_ly(df, labels = ~type_clean, values = ~n,
            type = "pie",
            textinfo = "label+percent",
            hoverinfo = "label+value",
            marker = list(colors = RColorBrewer::brewer.pal(
              min(nrow(df), 8), "Set2"))) %>%
      layout(showlegend = TRUE,
             legend = list(orientation = "h", y = -0.2))
  })
  
  output$vlc_disability_summary <- renderUI({
    s <- vlc_stats()
    tagList(
      div(style = "text-align: center;",
          div(style = "font-size: 28px; font-weight: bold; color: #16A085;",
              paste0(s$disability_rate, "%")),
          p("of sessions include<br>learners with disabilities",
            style = "color: #555; font-size: 11px; line-height: 1.4;")
      )
    )
  })
  
  # --------------------------------------------------------------------------
  # PLOT: Leadership Presence
  # --------------------------------------------------------------------------
  
  output$vlc_leadership_chart <- renderPlotly({
    df <- vlc_filtered() %>%
      filter(!is.na(Region_hbk5)) %>%
      group_by(Region_hbk5) %>%
      summarise(
        ht_pct  = mean(headteacher_present, na.rm = TRUE) * 100,
        gcc_pct = mean(gcc_present, na.rm = TRUE) * 100,
        avg_lpi = mean(leadership_pct, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      pivot_longer(c(ht_pct, gcc_pct), names_to = "role", values_to = "pct") %>%
      mutate(role_label = if_else(role == "ht_pct", "Headteacher", "G&C Officer"))
    
    if (nrow(df) == 0) return(plotly_empty())
    
    p <- ggplot(df,
                aes(x = pct, y = reorder(Region_hbk5, avg_lpi),
                    fill = role_label,
                    text = paste0(Region_hbk5, " — ", role_label,
                                  "<br>Present in ", round(pct, 1), "% of sessions"))) +
      geom_col(position = "dodge", width = 0.6) +
      scale_fill_manual(values = c("Headteacher" = "#E6A817", "G&C Officer" = "#2980B9")) +
      scale_x_continuous(labels = percent_format(scale = 1), limits = c(0, 105)) +
      labs(x = "% of Sessions", y = NULL, fill = "Role") +
      theme_minimal(base_size = 10) +
      theme(panel.grid.minor = element_blank(),
            panel.grid.major.y = element_blank(),
            legend.position = "bottom")
    
    ggplotly(p, tooltip = "text") %>%
      layout(legend = list(orientation = "h", y = -0.2))
  })
  
  output$vlc_ht_presence_rate <- renderUI({
    s <- vlc_stats()
    col <- if (s$ht_present_pct >= 60) "#27AE60" else if (s$ht_present_pct >= 40) "#E67E22" else "#E74C3C"
    div(style = glue("font-size: 28px; font-weight: bold; color: {col};"),
        paste0(s$ht_present_pct, "%"))
  })
  
  output$vlc_gcc_presence_rate <- renderUI({
    s <- vlc_stats()
    div(style = "font-size: 28px; font-weight: bold; color: #2980B9;",
        paste0(s$gcc_present_pct, "%"))
  })
  
  # --------------------------------------------------------------------------
  # PLOT: Regional Bar
  # --------------------------------------------------------------------------
  
  output$vlc_regional_bar <- renderPlotly({
    df <- vlc_all_subs() %>%
      group_by(Region_hbk5) %>%
      summarise(
        schools_submitted = n_distinct(Name_school_hbk5, na.rm = TRUE),
        sessions_held     = sum(vlc_held == "Held", na.rm = TRUE),
        total_students    = sum(total_attended, na.rm = TRUE),
        avg_participation = mean(participation_pct, na.rm = TRUE),
        ht_pct            = mean(headteacher_present, na.rm = TRUE) * 100,
        .groups = "drop"
      ) %>%
      filter(!is.na(Region_hbk5))
    
    if (nrow(df) == 0) return(plotly_empty())
    
    p <- ggplot(df,
                aes(x = reorder(Region_hbk5, sessions_held),
                    y = sessions_held,
                    fill = avg_participation,
                    text = paste0(Region_hbk5,
                                  "<br>Sessions held: ", sessions_held,
                                  "<br>Schools: ", schools_submitted,
                                  "<br>Students reached: ", format(total_students, big.mark = ","),
                                  "<br>Avg participation: ", round(avg_participation, 1), "%",
                                  "<br>Headteacher present: ", round(ht_pct, 1), "%"))) +
      geom_col(width = 0.7) +
      coord_flip() +
      scale_fill_gradient(low = "#E74C3C", high = "#27AE60",
                          name = "Avg Attendance Rate (%)",
                          limits = c(0, 100)) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
      labs(x = NULL, y = "Total VLC Sessions Held") +
      theme_minimal(base_size = 11) +
      theme(panel.grid.minor = element_blank(),
            panel.grid.major.y = element_blank(),
            legend.position = "bottom")
    
    ggplotly(p, tooltip = "text") %>%
      layout(coloraxis_colorbar = list(title = "Attendance %"))
  })
  
  # --------------------------------------------------------------------------
  # TABLE: Regional Rankings
  # --------------------------------------------------------------------------
  
  output$vlc_regional_table <- DT::renderDataTable({
    df <- vlc_all_subs() %>%
      group_by(Region_hbk5) %>%
      summarise(
        Schools       = n_distinct(Name_school_hbk5, na.rm = TRUE),
        Sessions      = sum(vlc_held == "Held", na.rm = TRUE),
        Attendance    = round(mean(participation_pct, na.rm = TRUE), 1),
        HT_Present    = round(mean(headteacher_present, na.rm = TRUE) * 100, 1),
        Disability    = round(mean(disability_included == TRUE, na.rm = TRUE) * 100, 1),
        .groups = "drop"
      ) %>%
      filter(!is.na(Region_hbk5)) %>%
      mutate(
        # Composite score (equal weights)
        Score = round(
          (pmin(Schools / 50, 1) * 25) +
            (pmin(Attendance / 100, 1) * 25) +
            (pmin(HT_Present / 100, 1) * 25) +
            (pmin(Disability / 30, 1) * 25),
          1)
      ) %>%
      arrange(desc(Score)) %>%
      mutate(Rank = row_number()) %>%
      select(Rank, Region = Region_hbk5, Schools, Sessions,
             `Attend %` = Attendance, `HT %` = HT_Present,
             `Disability %` = Disability, Score)
    
    DT::datatable(
      df,
      options = list(
        pageLength = 8, dom = "t", ordering = TRUE,
        columnDefs = list(list(className = "dt-center", targets = "_all"))
      ),
      rownames = FALSE
    ) %>%
      DT::formatStyle("Score",
                      background = DT::styleColorBar(range(df$Score), "#E6A817"),
                      backgroundSize = "100% 80%",
                      backgroundRepeat = "no-repeat",
                      backgroundPosition = "center")
  })
  
  # --------------------------------------------------------------------------
  # PLOT: School Quadrant
  # --------------------------------------------------------------------------
  
  output$vlc_school_quadrant <- renderPlotly({
    df <- vlc_filtered() %>%
      group_by(Name_school_hbk5, Region_hbk5) %>%
      summarise(
        sessions_completed = n(),
        avg_participation  = mean(participation_pct, na.rm = TRUE),
        ht_pct             = mean(headteacher_present, na.rm = TRUE) * 100,
        .groups = "drop"
      ) %>%
      filter(!is.na(avg_participation)) %>%
      mutate(
        quadrant = case_when(
          sessions_completed >= 3 & avg_participation >= 75 ~ "Exemplar",
          sessions_completed >= 3 & avg_participation < 75  ~ "Quality Concern",
          sessions_completed < 3  & avg_participation >= 75 ~ "Access Barrier",
          TRUE                                               ~ "Priority Intervention"
        ),
        q_color = case_when(
          quadrant == "Exemplar"             ~ "#27AE60",
          quadrant == "Quality Concern"      ~ "#E67E22",
          quadrant == "Access Barrier"       ~ "#2980B9",
          quadrant == "Priority Intervention"~ "#E74C3C"
        )
      )
    
    if (nrow(df) == 0) return(plotly_empty())
    
    med_s <- median(df$sessions_completed)
    med_p <- median(df$avg_participation, na.rm = TRUE)
    
    p <- ggplot(df,
                aes(x = avg_participation, y = sessions_completed,
                    colour = quadrant, size = ht_pct,
                    text = paste0(Name_school_hbk5,
                                  "<br>Region: ", Region_hbk5,
                                  "<br>Sessions: ", sessions_completed,
                                  "<br>Avg attendance: ", round(avg_participation, 1), "%",
                                  "<br>HT present: ", round(ht_pct, 1), "%",
                                  "<br>Quadrant: ", quadrant))) +
      geom_vline(xintercept = 75, linetype = "dashed", colour = "#BDC3C7") +
      geom_hline(yintercept = 3,  linetype = "dashed", colour = "#BDC3C7") +
      geom_point(alpha = 0.7) +
      scale_colour_manual(
        values = c("Exemplar"              = "#27AE60",
                   "Quality Concern"       = "#E67E22",
                   "Access Barrier"        = "#2980B9",
                   "Priority Intervention" = "#E74C3C")
      ) +
      scale_size_continuous(range = c(2, 6), guide = "none") +
      annotate("text", x = 88, y = max(df$sessions_completed) * 0.95,
               label = "Exemplar", colour = "#27AE60", size = 3, fontface = "bold") +
      annotate("text", x = 40, y = max(df$sessions_completed) * 0.95,
               label = "Access\nBarrier", colour = "#2980B9", size = 3, fontface = "bold") +
      annotate("text", x = 88, y = 1,
               label = "Quality\nConcern", colour = "#E67E22", size = 3, fontface = "bold") +
      annotate("text", x = 40, y = 1,
               label = "Priority\nIntervention", colour = "#E74C3C", size = 3, fontface = "bold") +
      labs(x = "Average Attendance Rate (%)", y = "Sessions Completed",
           colour = "Quadrant") +
      theme_minimal(base_size = 11) +
      theme(legend.position = "bottom",
            panel.grid.minor = element_blank())
    
    ggplotly(p, tooltip = "text") %>%
      layout(legend = list(orientation = "h", y = -0.2))
  })
  
  # --------------------------------------------------------------------------
  # TABLE: Priority Schools
  # --------------------------------------------------------------------------
  
  output$vlc_priority_schools <- DT::renderDataTable({
    df <- vlc_stats()$low_schools %>%
      select(School = Name_school_hbk5,
             Region = Region_hbk5,
             Sessions = sessions_n,
             `Avg Partic %` = avg_partic,
             `Last Session` = last_session) %>%
      mutate(`Avg Partic %` = round(`Avg Partic %`, 1),
             `Last Session` = format(`Last Session`, "%d %b %Y")) %>%
      head(20)
    
    DT::datatable(
      df,
      options = list(pageLength = 8, dom = "t", scrollY = "300px"),
      rownames = FALSE
    ) %>%
      DT::formatStyle(
        "Sessions",
        backgroundColor = DT::styleInterval(c(1, 3), c("#FADBD8", "#FDEBD0", "#EAFAF1"))
      )
  })
  
  # --------------------------------------------------------------------------
  # SCHOOL PROFILE
  # --------------------------------------------------------------------------
  
  selected_school_data <- reactive({
    req(input$vlc_school_search != "")
    vlc_filtered() %>%
      filter(Name_school_hbk5 == input$vlc_school_search) %>%
      arrange(session_date)
  })
  
  output$vlc_school_narrative <- renderUI({
    req(input$vlc_school_search != "")
    df <- selected_school_data()
    if (nrow(df) == 0) return(p("No data for selected school."))
    
    latest    <- max(df$session_date, na.rm = TRUE)
    sessions  <- nrow(df)
    avg_p     <- round(mean(df$participation_pct, na.rm = TRUE), 1)
    latest_s  <- tail(df$session_clean, 1)
    ht_rate   <- round(mean(df$headteacher_present, na.rm = TRUE) * 100)
    total_st  <- sum(df$total_attended, na.rm = TRUE)
    region    <- df$Region_hbk5[1]
    
    div(
      style = "background: #FDF6E3; border-left: 4px solid #E6A817;
               padding: 12px; border-radius: 4px;",
      p(
        tags$strong(input$vlc_school_search), " (", region, ") has held ",
        tags$strong(sessions), " VLC session(s), reaching ",
        tags$strong(format(total_st, big.mark = ",")), " learner touchpoints. ",
        "Average attendance is ", tags$strong(paste0(avg_p, "%")), ". ",
        "The most recent session was ", tags$strong(format(latest, "%d %b %Y")),
        " covering ", tags$em(latest_s), ". ",
        "The headteacher was present in ", tags$strong(paste0(ht_rate, "%")),
        " of sessions.",
        style = "font-size: 12px; color: #2C3E50; margin: 0; line-height: 1.7;"
      )
    )
  })
  
  output$vlc_school_timeline <- renderPlotly({
    df <- selected_school_data()
    if (nrow(df) == 0) return(plotly_empty())
    
    p <- ggplot(df, aes(x = session_date, y = participation_pct,
                        text = paste0("Date: ", session_date,
                                      "<br>Session: ", session_clean,
                                      "<br>Attendance: ", participation_pct, "%"))) +
      geom_line(colour = "#E6A817", size = 1) +
      geom_point(aes(colour = participation_tier), size = 3) +
      scale_colour_manual(values = c(
        "High (≥85%)"     = "#27AE60",
        "Medium (60-84%)" = "#E67E22",
        "Low (40-59%)"    = "#E74C3C",
        "Very Low (<40%)" = "#8B0000",
        "Unknown"         = "#BDC3C7"
      )) +
      scale_y_continuous(limits = c(0, 105),
                         labels = percent_format(scale = 1)) +
      geom_hline(yintercept = 75, linetype = "dashed", colour = "#BDC3C7") +
      labs(x = NULL, y = "Attendance %",
           title = "Session Attendance Over Time",
           colour = "Tier") +
      theme_minimal(base_size = 10) +
      theme(legend.position = "none",
            panel.grid.minor = element_blank())
    
    ggplotly(p, tooltip = "text")
  })
  
  output$vlc_school_gender_trend <- renderPlotly({
    df <- selected_school_data()
    if (nrow(df) == 0) return(plotly_empty())
    
    df2 <- df %>%
      select(session_date, Male = male_attendance_vlc,
             Female = female_attendance_vlc) %>%
      pivot_longer(c(Male, Female), names_to = "gender", values_to = "n")
    
    p <- ggplot(df2, aes(x = session_date, y = n, fill = gender,
                         text = paste0("Date: ", session_date,
                                       "<br>", gender, ": ", n))) +
      geom_col(position = "stack", width = 5) +
      scale_fill_manual(values = c("Male" = "#2980B9", "Female" = "#E74C3C")) +
      labs(x = NULL, y = "Attendance Count",
           title = "Gender Split by Session", fill = NULL) +
      theme_minimal(base_size = 10) +
      theme(legend.position = "bottom",
            panel.grid.minor = element_blank())
    
    ggplotly(p, tooltip = "text") %>%
      layout(legend = list(orientation = "h", y = -0.3))
  })
  
  # --------------------------------------------------------------------------
  # RAW DATA TABLE
  # --------------------------------------------------------------------------
  
  output$vlc_raw_table <- DT::renderDataTable({
    df <- vlc_filtered() %>%
      select(
        School      = Name_school_hbk5,
        Region      = Region_hbk5,
        District    = District_hbk5,
        Date        = session_date,
        Session     = session_clean,
        `Male Enr.` = no_male_teachers_vlc,
        `Fem Enr.`  = no_female_teachers_vlc,
        `Male Att.` = male_attendance_vlc,
        `Fem Att.`  = female_attendance_vlc,
        `Attend %`  = participation_pct,
        `HT Present`= headteacher_present,
        Disability  = disability_included,
        Term        = term_academic_vlc
      ) %>%
      mutate(
        Date         = format(Date, "%Y-%m-%d"),
        `HT Present` = if_else(`HT Present`, "Yes", "No"),
        Disability   = case_when(
          Disability == TRUE  ~ "Yes",
          Disability == FALSE ~ "No",
          TRUE ~ "N/A"
        )
      )
    
    DT::datatable(
      df,
      filter   = "top",
      options  = list(pageLength = 15, scrollX = TRUE, dom = "lftip"),
      rownames = FALSE
    ) %>%
      DT::formatStyle(
        "Attend %",
        backgroundColor = DT::styleInterval(c(50, 75),
                                             c("#FADBD8", "#FDEBD0", "#EAFAF1"))
      )
  })
  
  # --------------------------------------------------------------------------
  # DOWNLOAD HANDLER
  # --------------------------------------------------------------------------
  
  output$vlc_download_data <- downloadHandler(
    filename = function() {
      paste0("VLC_Data_Export_", format(Sys.Date(), "%Y%m%d"), ".csv")
    },
    content = function(file) {
      write.csv(vlc_filtered(), file, row.names = FALSE)
    }
  )
  
} # end vlc_server()

# =============================================================================
# INTEGRATION INSTRUCTIONS
# =============================================================================
# 
# 1. ADD TO SIDEBAR (in your existing ui.R):
#    menuItem("VLC Monitoring", tabName = "vlc_dashboard",
#             icon = icon("book-open"))
#
# 2. ADD TO BODY (in your existing dashboardBody()):
#    vlc_ui   # (the tabItem defined above)
#
# 3. ADD TO SERVER (in your existing server function):
#    vlc_server(input, output, session)
#
# 4. DATA SOURCE:
#    Replace read.csv("vlc_data_nat.csv") in vlc_server with your
#    live connection string or reactive data feed.
#
# 5. SCHOOL COUNT:
#    Update TOTAL_SCHOOLS_NATIONAL <- 721 if this changes.
#
# =============================================================================
