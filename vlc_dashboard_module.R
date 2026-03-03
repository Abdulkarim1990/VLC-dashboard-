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

# Official number of SHS per region (GES registry)
REGIONAL_SCHOOL_TOTALS <- tibble::tibble(
  Region_hbk5 = c(
    "Ahafo", "Ashanti", "Bono", "Bono East", "Central", "Eastern",
    "Greater Accra", "North East", "Northern", "Oti", "Savannah",
    "Upper East", "Upper West", "Volta", "Western", "Western North"
  ),
  region_total = c(18, 144, 37, 29, 75, 94, 47, 18, 31, 25, 13, 35, 31, 68, 38, 18)
)

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
    "Confidence", "Confidence"
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
  "Confidence"               = "#C0392B"
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

      # ---- Academic Year (Sept–Aug cycle) ------------------------------------
      academic_year = case_when(
        month(session_date) >= 9 ~
          paste0(year(session_date), "/", year(session_date) + 1),
        !is.na(session_date) ~
          paste0(year(session_date) - 1, "/", year(session_date)),
        TRUE ~ NA_character_
      ),
      
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
      # ---- Count of learners with disabilities in the session -----------------
      disability_learners_n = suppressWarnings(
        as.integer(How_many_learners_wi_d_in_the_VLC_session)
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

  # ── Custom CSS ─────────────────────────────────────────────────────────────
  tags$style(HTML("

    /* ── Main Tab Bar ──────────────────────────────────────────────────── */
    #vlc_main_tabs > .nav-tabs {
      background: #1B2A4A;
      border-bottom: 3px solid #E6A817;
      padding: 0 16px;
      border-radius: 6px 6px 0 0;
      margin-bottom: 0;
    }
    #vlc_main_tabs > .nav-tabs > li > a {
      color: #A0AEC0 !important;
      border: none !important;
      border-bottom: 3px solid transparent !important;
      border-radius: 0 !important;
      padding: 13px 20px;
      font-size: 12px;
      font-weight: 700;
      letter-spacing: 0.6px;
      text-transform: uppercase;
      background: transparent !important;
      margin-bottom: -3px;
      transition: color 0.2s;
    }
    #vlc_main_tabs > .nav-tabs > li > a:hover {
      color: #E6A817 !important;
      border-bottom: 3px solid #E6A817 !important;
    }
    #vlc_main_tabs > .nav-tabs > li.active > a,
    #vlc_main_tabs > .nav-tabs > li.active > a:focus,
    #vlc_main_tabs > .nav-tabs > li.active > a:hover {
      color: #E6A817 !important;
      border-bottom: 3px solid #E6A817 !important;
      background: transparent !important;
    }
    #vlc_main_tabs > .tab-content {
      background: #F4F6F9;
      border: 1px solid #DDE1E7;
      border-top: none;
      border-radius: 0 0 8px 8px;
      padding: 28px 24px;
      min-height: 600px;
    }

    /* ── Hero Banner ───────────────────────────────────────────────────── */
    .vlc-hero {
      background: #1B2A4A;
      border-radius: 10px;
      padding: 40px 40px 32px;
      color: white;
      margin-bottom: 24px;
      position: relative;
      overflow: hidden;
      border-bottom: 3px solid #C9A227;
    }
    .vlc-hero-badge {
      display: inline-block;
      background: rgba(201, 162, 39, 0.12);
      border: 1px solid rgba(201, 162, 39, 0.35);
      color: #C9A227;
      border-radius: 4px;
      padding: 4px 12px;
      font-size: 10px;
      font-weight: 700;
      text-transform: uppercase;
      letter-spacing: 1.2px;
      margin-bottom: 16px;
    }
    .vlc-hero-title {
      font-size: 26px;
      font-weight: 800;
      color: #FFFFFF;
      margin: 0 0 6px;
      line-height: 1.25;
    }
    .vlc-hero-sub {
      font-size: 14px;
      color: #A0AEC0;
      margin: 0 0 28px;
    }
    .vlc-stat-pill {
      background: #FFFFFF;
      border-radius: 8px;
      padding: 16px 14px;
      text-align: center;
      box-shadow: 0 2px 10px rgba(0,0,0,0.18);
    }
    .vlc-stat-num {
      font-size: 26px;
      font-weight: 800;
      color: #1B2A4A;
      line-height: 1;
      display: block;
    }
    .vlc-stat-lbl {
      font-size: 10px;
      color: #718096;
      text-transform: uppercase;
      letter-spacing: 0.5px;
      margin-top: 5px;
    }

    /* ── KPI Group Cards ───────────────────────────────────────────────── */
    .kpi-group {
      background: white;
      border-radius: 10px;
      padding: 16px 20px 12px;
      box-shadow: 0 2px 10px rgba(0,0,0,0.06);
      margin-bottom: 14px;
    }
    .kpi-group-title {
      font-size: 10px;
      font-weight: 700;
      text-transform: uppercase;
      letter-spacing: 0.8px;
      color: #718096;
      margin: 0 0 12px;
      padding-bottom: 8px;
      border-bottom: 1px solid #EDF2F7;
    }
    .kpi-metric {
      padding: 8px 4px;
      text-align: center;
      border-right: 1px solid #EDF2F7;
    }
    .kpi-metric:last-child { border-right: none; }
    .kpi-value {
      font-size: 24px;
      font-weight: 800;
      color: #1B2A4A;
      line-height: 1;
    }
    .kpi-label {
      font-size: 11px;
      color: #4A5568;
      margin-top: 5px;
      line-height: 1.3;
    }
    .kpi-sub {
      font-size: 10px;
      color: #A0AEC0;
      margin-top: 2px;
    }

    /* ── Info Cards ────────────────────────────────────────────────────── */
    .vlc-info-card {
      background: white;
      border-radius: 10px;
      padding: 22px 20px;
      height: 100%;
      box-shadow: 0 2px 10px rgba(0,0,0,0.06);
      border-top: 4px solid;
      transition: transform 0.2s, box-shadow 0.2s;
    }
    .vlc-info-card:hover {
      transform: translateY(-3px);
      box-shadow: 0 8px 22px rgba(0,0,0,0.11);
    }
    .vlc-info-card .card-icon {
      font-size: 26px;
      margin-bottom: 10px;
    }
    .vlc-info-card h5 {
      font-size: 13px;
      font-weight: 700;
      text-transform: uppercase;
      letter-spacing: 0.5px;
      margin: 0 0 10px;
    }
    .vlc-info-card p {
      font-size: 12.5px;
      color: #4A5568;
      line-height: 1.75;
      margin: 0;
    }

    /* ── Values Journey Strip ──────────────────────────────────────────── */
    .vlc-values-strip {
      background: white;
      border-radius: 10px;
      padding: 22px 20px;
      box-shadow: 0 2px 10px rgba(0,0,0,0.06);
      margin-top: 20px;
    }
    .vlc-values-strip .strip-title {
      font-size: 11px;
      font-weight: 700;
      color: #1B2A4A;
      text-transform: uppercase;
      letter-spacing: 0.7px;
      margin-bottom: 14px;
    }
    .vlc-value-badge {
      display: inline-flex;
      align-items: center;
      gap: 6px;
      border-radius: 20px;
      padding: 5px 13px;
      font-size: 11.5px;
      font-weight: 600;
      white-space: nowrap;
      margin: 3px;
    }
    .vlc-meta-chip {
      background: #F4F6F9;
      border-radius: 6px;
      padding: 8px 12px;
      font-size: 11.5px;
      color: #4A5568;
      display: flex;
      align-items: center;
      gap: 7px;
    }

    /* ── Section Headers ───────────────────────────────────────────────── */
    .vlc-section-hdr {
      display: flex;
      align-items: center;
      gap: 10px;
      margin: 6px 0 18px;
      padding-bottom: 12px;
      border-bottom: 2px solid #E6A817;
    }
    .vlc-section-hdr .hdr-icon {
      background: #E6A817;
      color: white;
      border-radius: 7px;
      width: 34px; height: 34px;
      display: flex; align-items: center; justify-content: center;
      font-size: 15px;
      flex-shrink: 0;
    }
    .vlc-section-hdr h4 {
      margin: 0;
      font-size: 15px;
      font-weight: 700;
      color: #1B2A4A;
    }
    .vlc-section-hdr .hdr-sub {
      margin: 1px 0 0;
      font-size: 11px;
      color: #718096;
    }

    /* ── Narrative Card ────────────────────────────────────────────────── */
    .vlc-narrative-card {
      background: white;
      border-radius: 10px;
      padding: 24px 26px;
      box-shadow: 0 2px 10px rgba(0,0,0,0.06);
      border-left: 5px solid #E6A817;
      margin-bottom: 18px;
    }
    .vlc-narrative-card .narr-eyebrow {
      font-size: 10px;
      font-weight: 700;
      color: #E6A817;
      text-transform: uppercase;
      letter-spacing: 1.4px;
      margin-bottom: 12px;
    }
    .vlc-narrative-card .narr-body {
      font-size: 13.5px;
      line-height: 1.9;
      color: #2C3E50;
    }

    /* ── Signal Cards ──────────────────────────────────────────────────── */
    .vlc-signal-card {
      background: white;
      border-radius: 10px;
      padding: 18px 14px;
      text-align: center;
      box-shadow: 0 2px 10px rgba(0,0,0,0.06);
      border-top: 4px solid;
      height: 100%;
    }
    .vlc-signal-card .sig-label {
      font-size: 10px;
      color: #718096;
      text-transform: uppercase;
      letter-spacing: 0.5px;
      margin-top: 7px;
    }

    /* ── Chart Cards ───────────────────────────────────────────────────── */
    .vlc-chart-card {
      background: white;
      border-radius: 10px;
      padding: 20px;
      box-shadow: 0 2px 10px rgba(0,0,0,0.06);
      margin-bottom: 20px;
      height: 100%;
    }
    .vlc-chart-card .chart-ttl {
      font-size: 13px;
      font-weight: 700;
      color: #1B2A4A;
      margin-bottom: 2px;
    }
    .vlc-chart-card .chart-sub {
      font-size: 11px;
      color: #718096;
      margin-bottom: 14px;
    }
    .vlc-chart-note {
      font-size: 11px;
      color: #718096;
      margin-top: 10px;
      padding-top: 8px;
      border-top: 1px solid #EDF2F7;
    }

    /* ── Equity Pills ──────────────────────────────────────────────────── */
    .vlc-eq-tabs .nav-pills { margin-bottom: 16px; }
    .vlc-eq-tabs .nav-pills > li > a {
      border-radius: 20px;
      font-size: 12px;
      font-weight: 600;
      color: #2C3E50;
      padding: 6px 16px;
    }
    .vlc-eq-tabs .nav-pills > li.active > a,
    .vlc-eq-tabs .nav-pills > li.active > a:hover,
    .vlc-eq-tabs .nav-pills > li.active > a:focus {
      background-color: #E6A817 !important;
      color: white !important;
    }

    /* ── Quadrant Legend ───────────────────────────────────────────────── */
    .vlc-qlgnd {
      background: #F7FAFC;
      border-radius: 8px;
      padding: 14px 16px;
      margin-top: 10px;
    }
    .vlc-qlgnd-item {
      display: flex;
      align-items: center;
      gap: 8px;
      margin-bottom: 7px;
      font-size: 11.5px;
      color: #4A5568;
    }
    .vlc-qlgnd-dot {
      width: 12px; height: 12px;
      border-radius: 3px;
      flex-shrink: 0;
    }

    /* ── Insight Box ───────────────────────────────────────────────────── */
    .vlc-insight {
      background: #FFF8E1;
      border-left: 4px solid #E6A817;
      border-radius: 4px 8px 8px 4px;
      padding: 13px 15px;
      font-size: 12px;
      color: #4A5568;
      line-height: 1.65;
    }

    /* ── Priority card accent ──────────────────────────────────────────── */
    .vlc-priority-card {
      border-top: 4px solid #E74C3C !important;
    }
    .vlc-priority-card .chart-ttl { color: #E74C3C !important; }

    /* ── Spacing helpers ───────────────────────────────────────────────── */
    .vlc-gap { margin-bottom: 20px; }
    .vlc-gap-sm { margin-bottom: 12px; }
    .vlc-filter-row .box-header { background: #1B2A4A !important; }

  ")),

  # ── Filters Row ────────────────────────────────────────────────────────────
  fluidRow(
    box(
      width = 12,
      solidHeader = TRUE,
      status = "warning",
      collapsible = TRUE,
      title = tagList(icon("sliders-h"), " Filters"),

      column(3,
        selectInput("vlc_filter_year", "Academic Year:",
                    choices = c("All Years"  = "all",
                                "2025/2026"  = "2025/2026",
                                "2026/2027"  = "2026/2027",
                                "2027/2028"  = "2027/2028"),
                    selected = "2025/2026")
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
  
  # ── Main 4-Tab Layout ──────────────────────────────────────────────────────
  fluidRow(
    column(12,
      tabsetPanel(
        id = "vlc_main_tabs",
        type = "tabs",

        # ════════════════════════════════════════════════════════════════════
        # TAB 1 — INTRODUCTION
        # ════════════════════════════════════════════════════════════════════
        tabPanel(
          title = tagList(icon("info-circle"), " Introduction"),

          # Hero Banner
          div(
            class = "vlc-hero",
            div(class = "vlc-hero-badge", "GES | NaCCA | T-TEL | NUGS"),
            h1(class = "vlc-hero-title",
               icon("book-open"), " Essential Values for Ghanaian Youth"),
            p(class = "vlc-hero-sub",
              "Values Learning Community (VLC) — National Monitoring Dashboard"),
            hr(style = "border-color: rgba(230,168,23,0.3); margin: 22px 0;"),
            fluidRow(
              column(3,
                div(class = "vlc-stat-pill",
                  tags$span(class = "vlc-stat-num", "721"),
                  div(class = "vlc-stat-lbl", "Senior High Schools")
                )
              ),
              column(3,
                div(class = "vlc-stat-pill",
                  tags$span(class = "vlc-stat-num", "11"),
                  div(class = "vlc-stat-lbl", "Core Values")
                )
              ),
              column(3,
                div(class = "vlc-stat-pill",
                  tags$span(class = "vlc-stat-num", "22"),
                  div(class = "vlc-stat-lbl", "Sessions Per Year")
                )
              ),
              column(3,
                div(class = "vlc-stat-pill",
                  tags$span(class = "vlc-stat-num", "16"),
                  div(class = "vlc-stat-lbl", "Regions")
                )
              )
            )
          ),

          # Three info cards
          fluidRow(
            column(4,
              div(class = "vlc-info-card", style = "border-top-color: #E6A817;",
                div(class = "card-icon", style = "color: #E6A817;",
                    icon("users")),
                h5(style = "color: #E6A817;", "What is the VLC?"),
                p("The ", tags$strong("Values Learning Community (VLC)"),
                  " is a weekly, student-led session held in every Senior High School
                   across Ghana. While teachers attend their Professional Learning
                   Community (PLC) meetings, learners engage in peer-facilitated
                   discussions guided by the Essential Values Handbook. Each
                   90-minute session is facilitated by two trained ",
                  tags$strong("Peer Guides"), " — classmates selected for their
                   integrity and communication skills.")
              )
            ),
            column(4,
              div(class = "vlc-info-card", style = "border-top-color: #27AE60;",
                div(class = "card-icon", style = "color: #27AE60;",
                    icon("book")),
                h5(style = "color: #27AE60;", "The Handbook"),
                p("Published by the ", tags$strong("Ministry of Education Ghana"),
                  " in partnership with GES, NaCCA, T-TEL, NUGS, Lead for Ghana,
                   and Honour Ghana — and ", tags$em("written by young people for
                   young people"), ". It covers ", tags$strong("11 core values"),
                  " across ", tags$strong("22 sessions"), " spanning the full
                   academic year. Each value is explored in an Understanding
                   session, then reinforced in an Applying session.")
              )
            ),
            column(4,
              div(class = "vlc-info-card", style = "border-top-color: #2980B9;",
                div(class = "card-icon", style = "color: #2980B9;",
                    icon("bullseye")),
                h5(style = "color: #2980B9;", "Why It Matters"),
                p("The VLC addresses Ghana's commitment to holistic education —
                   developing ", tags$em("The Head"), " (knowledge), ",
                  tags$em("The Heart"), " (values and character), and ",
                  tags$em("The Hand"), " (practical skills). This dashboard
                   tracks whether all 721 SHS are delivering the programme
                   each week — and identifies where support is needed most.")
              )
            )
          ),

          # Values journey strip
          div(
            class = "vlc-values-strip",
            div(class = "strip-title",
                icon("road"), " The 11-Value Journey — 22 Sessions Across the Year"),
            div(
              style = "display: flex; flex-wrap: wrap; align-items: center;",
              lapply(
                list(
                  list("Responsible Citizenship", "#E74C3C"),
                  list("Honesty",                 "#E67E22"),
                  list("Integrity",               "#F39C12"),
                  list("Diversity",               "#27AE60"),
                  list("Equity",                  "#16A085"),
                  list("Discipline",              "#2980B9"),
                  list("Self-Directed Learning",  "#8E44AD"),
                  list("Adaptability",            "#D35400"),
                  list("Resourcefulness",         "#1ABC9C"),
                  list("Leadership",              "#2C3E50"),
                  list("Confidence",              "#C0392B")
                ),
                function(v) {
                  div(
                    class = "vlc-value-badge",
                    style = glue(
                      "background: {v[[2]]}18; border: 1.5px solid {v[[2]]}55;
                       color: {v[[2]]};"
                    ),
                    tags$span(style = glue("color: {v[[2]]};"), "\u25cf"),
                    tags$span(v[[1]])
                  )
                }
              )
            ),
            div(
              style = "margin-top: 16px; padding-top: 14px;
                       border-top: 1px solid #EDF2F7;",
              fluidRow(
                column(4,
                  div(class = "vlc-meta-chip",
                    icon("lightbulb", style = "color: #E6A817;"),
                    "S0 = Orientation. S1\u2013S22 = Understanding & Applying pairs."
                  )
                ),
                column(4,
                  div(class = "vlc-meta-chip",
                    icon("clock", style = "color: #2980B9;"),
                    "Each session lasts approximately 90 minutes."
                  )
                ),
                column(4,
                  div(class = "vlc-meta-chip",
                    icon("calendar-alt", style = "color: #27AE60;"),
                    "Sessions run weekly throughout both academic terms."
                  )
                )
              )
            )
          )
        ), # end Tab 1

        # ════════════════════════════════════════════════════════════════════
        # TAB 2 — NATIONAL SUMMARY
        # ════════════════════════════════════════════════════════════════════
        tabPanel(
          title = tagList(icon("chart-bar"), " National Summary"),

          # Section A: Live Data Story
          div(class = "vlc-section-hdr",
            div(class = "hdr-icon", icon("rss")),
            div(
              h4("Live Data Summary"),
              p(class = "hdr-sub", "Current programme data — national overview")
            )
          ),

          div(
            class = "vlc-narrative-card",
            div(class = "narr-eyebrow",
                icon("align-left"), " Live Data Summary"),
            div(class = "narr-body",
                uiOutput("vlc_narrative_text"))
          ),

          fluidRow(
            column(4,
              div(class = "vlc-signal-card", style = "border-top-color: #27AE60;",
                uiOutput("vlc_signal_coverage"),
                div(class = "sig-label", "National School Coverage")
              )
            ),
            column(4,
              div(class = "vlc-signal-card", style = "border-top-color: #2980B9;",
                uiOutput("vlc_signal_attendance"),
                div(class = "sig-label", "Avg Attendance per Session")
              )
            ),
            column(4,
              div(class = "vlc-signal-card", style = "border-top-color: #E74C3C;",
                uiOutput("vlc_signal_attention"),
                div(class = "sig-label", "Schools Requiring Follow-up")
              )
            )
          ),

          div(class = "vlc-gap"),

          # Section B: KPIs
          div(class = "vlc-section-hdr",
            div(class = "hdr-icon", icon("tachometer-alt")),
            div(
              h4("Key Performance Indicators"),
              p(class = "hdr-sub", "Core metrics for the current filtered view")
            )
          ),

          # Group 1: National Coverage
          div(
            class = "kpi-group",
            div(class = "kpi-group-title",
                icon("school"), " National Coverage"),
            fluidRow(
              column(4,
                div(class = "kpi-metric",
                  uiOutput("vlc_kpi_active_schools"),
                  div(class = "kpi-label", "Active Schools"),
                  div(class = "kpi-sub",
                      paste0("of ", TOTAL_SCHOOLS_NATIONAL, " nationally"))
                )
              ),
              column(4,
                div(class = "kpi-metric",
                  uiOutput("vlc_kpi_coverage_pct"),
                  div(class = "kpi-label", "National Coverage"),
                  div(class = "kpi-sub", "schools with submissions")
                )
              ),
              column(4,
                div(class = "kpi-metric",
                  uiOutput("vlc_kpi_followup"),
                  div(class = "kpi-label", "Schools for Follow-Up"),
                  div(class = "kpi-sub", "low coverage or attendance")
                )
              )
            )
          ),

          # Group 2: Programme Delivery
          div(
            class = "kpi-group",
            div(class = "kpi-group-title",
                icon("chart-bar"), " Programme Delivery"),
            fluidRow(
              column(6,
                div(class = "kpi-metric",
                  uiOutput("vlc_kpi_sessions"),
                  div(class = "kpi-label", "Sessions Delivered"),
                  div(class = "kpi-sub", "VLC sessions held")
                )
              ),
              column(6,
                div(class = "kpi-metric",
                  uiOutput("vlc_kpi_avg_attend"),
                  div(class = "kpi-label", "Avg Attendance per Session"),
                  div(class = "kpi-sub", "learner participation rate")
                )
              )
            )
          ),

          # Group 3: Quality & Inclusion
          div(
            class = "kpi-group",
            div(class = "kpi-group-title",
                icon("check-circle"), " Quality & Inclusion"),
            fluidRow(
              column(6,
                div(class = "kpi-metric",
                  uiOutput("vlc_kpi_headteacher_new"),
                  div(class = "kpi-label", "Headteacher Participation"),
                  div(class = "kpi-sub", "% of sessions attended")
                )
              ),
              column(6,
                div(class = "kpi-metric",
                  uiOutput("vlc_kpi_disability_new"),
                  div(class = "kpi-label", "Disability Inclusion Rate"),
                  div(class = "kpi-sub", "sessions with learners with disabilities")
                )
              )
            )
          ),

          div(class = "vlc-gap"),

          # Section C: Progress & Momentum
          div(class = "vlc-section-hdr",
            div(class = "hdr-icon", icon("chart-line")),
            div(
              h4("Progress & Momentum"),
              p(class = "hdr-sub",
                "Weekly submission trends and implementation pipeline")
            )
          ),

          fluidRow(
            column(8,
              div(class = "vlc-chart-card",
                div(class = "chart-ttl", "Weekly VLC Session Submissions"),
                div(class = "chart-sub",
                    "Programme momentum — sessions submitted by week of term"),
                plotlyOutput("vlc_weekly_trend", height = "265px"),
                div(class = "vlc-chart-note",
                  icon("info-circle"),
                  " Week-by-week submission volumes. A sustained decline may
                    indicate schools requiring follow-up."
                )
              )
            ),
            column(4,
              div(class = "vlc-chart-card",
                div(class = "chart-ttl", "Implementation Funnel"),
                div(class = "chart-sub",
                    "From all schools to high-attendance sessions"),
                plotlyOutput("vlc_funnel", height = "265px"),
                div(class = "vlc-chart-note",
                  icon("info-circle"),
                  " Drop between Submitted and VLC Held indicates reporting
                    without delivery."
                )
              )
            )
          ),

          div(class = "vlc-gap"),

          # Section D: Session Content Analysis
          div(class = "vlc-section-hdr",
            div(class = "hdr-icon", icon("book-open")),
            div(
              h4("Session Content Analysis"),
              p(class = "hdr-sub",
                "Programme rollout stage and value theme distribution")
            )
          ),

          # Block 1: Rollout Progression
          fluidRow(
            column(12,
              div(class = "vlc-chart-card",
                div(class = "chart-ttl", "Current Rollout Stage"),
                div(class = "chart-sub",
                    "Distribution of sessions delivered per school"),
                fluidRow(
                  column(4,
                    div(style = "text-align: center; padding: 8px;",
                      uiOutput("vlc_rollout_s0_pct"))
                  ),
                  column(4,
                    div(style = "text-align: center; padding: 8px;",
                      uiOutput("vlc_rollout_2plus_pct"))
                  ),
                  column(4,
                    div(style = "text-align: center; padding: 8px;",
                      uiOutput("vlc_rollout_avg_sessions"))
                  )
                ),
                br(),
                plotlyOutput("vlc_rollout_chart", height = "190px"),
                div(class = "vlc-chart-note",
                  icon("info-circle"),
                  " Schools are required to begin at Session 0.",
                  " Distribution reflects current rollout stage."
                )
              )
            )
          ),

          div(class = "vlc-gap-sm"),

          # Block 2 & Block 3 side by side
          fluidRow(
            column(6,
              div(class = "vlc-chart-card",
                div(class = "chart-ttl", "Value Themes Delivered to Date"),
                div(class = "chart-sub",
                    "Total sessions delivered nationally per value theme"),
                plotlyOutput("vlc_theme_national", height = "340px")
              )
            ),
            column(6,
              div(class = "vlc-chart-card",
                div(class = "chart-ttl", "Value Theme Delivery by Region"),
                div(class = "chart-sub",
                    "Sessions per active school — normalized across regions"),
                plotlyOutput("vlc_theme_region_heatmap", height = "340px"),
                div(class = "vlc-chart-note",
                  icon("info-circle"),
                  " Values shown represent sessions delivered per active school
                    in each region."
                )
              )
            )
          )
        ), # end Tab 2

        # ════════════════════════════════════════════════════════════════════
        # TAB 3 — REGIONAL INSIGHTS
        # ════════════════════════════════════════════════════════════════════
        tabPanel(
          title = tagList(icon("map-marked-alt"), " Regional Insights"),

          # Section A: Regional performance
          div(class = "vlc-section-hdr",
            div(class = "hdr-icon", icon("map")),
            div(
              h4("Regional Performance Overview"),
              p(class = "hdr-sub",
                "Comparing session delivery and outcomes across all 16 regions")
            )
          ),

          fluidRow(
            column(12,
              div(class = "vlc-chart-card",
                div(class = "chart-ttl", "Regional Summary"),
                div(class = "chart-sub",
                    "Coverage, participation and composite performance score — bars coloured by avg attendance rate (red \u2192 green)"),
                DT::dataTableOutput("vlc_regional_summary"),
                div(class = "vlc-chart-note",
                  icon("info-circle"),
                  " Composite Score = equal weighting of coverage %, avg attendance %, headteacher presence % and disability inclusion %.",
                  " Coverage uses official regional school totals."
                )
              )
            )
          ),

          div(class = "vlc-gap"),

          # Section B: Equity & Inclusion
          div(class = "vlc-section-hdr",
            div(class = "hdr-icon", icon("balance-scale")),
            div(
              h4("Equity & Inclusion"),
              p(class = "hdr-sub",
                "Gender balance, disability inclusion, and leadership engagement
                 by region")
            )
          ),

          div(
            class = "vlc-chart-card vlc-eq-tabs",
            tabsetPanel(
              type = "pills",
              id   = "vlc_equity_tabs",

              # Gender tab
              tabPanel(
                title = tagList(icon("venus-mars"), " Gender Balance"),
                br(),
                fluidRow(
                  column(8,
                    plotlyOutput("vlc_gender_region", height = "310px")
                  ),
                  column(4,
                    div(
                      style = "background: #F7FAFC; border-radius: 8px; padding: 18px;",
                      h5(style = "color: #1B2A4A; font-weight: 700; font-size: 13px;",
                         "Reading the Gender Gap"),
                      p(style = "font-size: 12px; color: #4A5568; line-height: 1.75;",
                        "Bars to the ",
                        tags$strong(style = "color: #E74C3C;", "right"),
                        " indicate female-dominant attendance.", br(),
                        "Bars to the ",
                        tags$strong(style = "color: #2980B9;", "left"),
                        " indicate male-dominant attendance.", br(), br(),
                        "The national equity target is a gender gap of \u00b110
                         percentage points. Regions outside this band require
                         targeted support."
                      ),
                      hr(style = "margin: 12px 0;"),
                      uiOutput("vlc_gender_summary_text")
                    )
                  )
                )
              ),

              # Disability tab
              tabPanel(
                title = tagList(icon("wheelchair"), " Disability Inclusion"),
                br(),
                fluidRow(
                  column(5,
                    plotlyOutput("vlc_disability_regions", height = "310px")
                  ),
                  column(4,
                    plotlyOutput("vlc_disability_types", height = "310px")
                  ),
                  column(3,
                    div(
                      style = "background: #F7FAFC; border-radius: 8px; padding: 18px;",
                      h5(style = "color: #1B2A4A; font-weight: 700; font-size: 13px;",
                         "Why This Matters"),
                      p(style = "font-size: 12px; color: #4A5568; line-height: 1.75;",
                        "Sessions 7\u201310 explicitly address Diversity and Equity,
                         making disability inclusion a direct measure of values-in-action.
                         Schools that include learners with disabilities are demonstrating
                         the very values being taught."
                      ),
                      hr(style = "margin: 12px 0;"),
                      uiOutput("vlc_disability_summary")
                    )
                  )
                )
              ),

              # Leadership tab
              tabPanel(
                title = tagList(icon("user-tie"), " Leadership Engagement"),
                br(),
                fluidRow(
                  column(7,
                    plotlyOutput("vlc_leadership_chart", height = "310px")
                  ),
                  column(5,
                    fluidRow(
                      column(6,
                        div(
                          style = "background: #F7FAFC; border-radius: 8px;
                                   padding: 14px; text-align: center; margin-bottom: 10px;",
                          uiOutput("vlc_ht_presence_rate"),
                          p(style = "font-size: 11px; color: #718096; margin: 0;",
                            "Headteacher Present")
                        )
                      ),
                      column(6,
                        div(
                          style = "background: #F7FAFC; border-radius: 8px;
                                   padding: 14px; text-align: center; margin-bottom: 10px;",
                          uiOutput("vlc_gcc_presence_rate"),
                          p(style = "font-size: 11px; color: #718096; margin: 0;",
                            "G&C Officer Present")
                        )
                      )
                    ),
                    div(
                      class = "vlc-insight",
                      icon("lightbulb", style = "color: #E6A817;"),
                      " ", tags$strong("Research insight:"),
                      " Schools where the headteacher consistently attends VLC
                        sessions show 23% higher programme continuity in comparable
                        school improvement programmes (T-TEL internal evidence, 2023)."
                    )
                  )
                )
              )
            ) # end equity tabsetPanel
          )
        ), # end Tab 3

        # ════════════════════════════════════════════════════════════════════
        # TAB 4 — SCHOOL-LEVEL ANALYSIS
        # ════════════════════════════════════════════════════════════════════
        tabPanel(
          title = tagList(icon("school"), " School-Level Analysis"),

          # Section A: School Performance Summary Table
          div(class = "vlc-section-hdr",
            div(class = "hdr-icon", icon("table")),
            div(
              h4("School Performance Summary"),
              p(class = "hdr-sub",
                "All schools — delivery, attendance, gender and disability inclusion")
            )
          ),

          fluidRow(
            column(12,
              div(class = "vlc-chart-card",
                fluidRow(
                  column(8,
                    div(class = "chart-sub",
                        "Scroll to explore all schools \u2022 Region and School columns stay frozen \u2022 Attendance % coloured red \u2192 green")
                  ),
                  column(4,
                    div(style = "text-align: right; padding-top: 2px;",
                      radioButtons(
                        "vlc_summary_view",
                        label    = NULL,
                        choices  = c("Cumulative (All Sessions)" = "cumulative",
                                     "Most Recent Session"       = "recent"),
                        selected = "cumulative",
                        inline   = TRUE
                      )
                    )
                  )
                ),
                DT::dataTableOutput("vlc_school_summary_table"),
                div(class = "vlc-chart-note",
                  icon("info-circle"),
                  " Cumulative view: totals across all sessions held.",
                  " \u2018% Sess w/ Disab\u2019 = share of sessions that reported learners with disabilities."
                )
              )
            )
          ),

          div(class = "vlc-gap"),

          # Section B: School Action Quadrant
          div(class = "vlc-section-hdr",
            div(class = "hdr-icon", icon("th-large")),
            div(
              h4("School Action Quadrant"),
              p(class = "hdr-sub",
                "Segmenting all schools by delivery frequency and participation quality")
            )
          ),

          fluidRow(
            column(8,
              div(class = "vlc-chart-card",
                div(class = "chart-ttl",
                    "Who Needs What Support?"),
                div(class = "chart-sub",
                    "X = Attendance Rate \u2022 Y = Sessions Completed \u2022 Bubble size = Enrolled learners \u2022 Dashed lines = national medians"),
                div(
                  style = "background: #F7FAFC; border: 1px solid #E2E8F0; border-radius: 6px;
                           padding: 8px 12px; margin-bottom: 10px; font-size: 11px; color: #4A5568;",
                  icon("info-circle", style = "color: #2980B9;"), " ",
                  tags$strong("Classification: "),
                  tags$strong("High Delivery"), " = sessions \u2265 national median. ",
                  tags$strong("High Attendance"), " = attendance \u2265 national median. ",
                  "Thresholds adapt to the current data. Bubble size = total enrolled learners."
                ),
                plotlyOutput("vlc_school_quadrant", height = "370px"),
                div(class = "vlc-chart-note",
                  icon("info-circle"),
                  " Hover over any point for school details.",
                  " Use the global Region filter above to focus on a specific region."
                ),
                div(
                  class = "vlc-qlgnd",
                  fluidRow(
                    column(6,
                      div(class = "vlc-qlgnd-item",
                        div(class = "vlc-qlgnd-dot",
                            style = "background: #27AE60;"),
                        tags$strong("High Delivery / High Attendance")
                      ),
                      div(class = "vlc-qlgnd-item",
                        div(class = "vlc-qlgnd-dot",
                            style = "background: #E67E22;"),
                        tags$strong("High Delivery / Low Attendance")
                      )
                    ),
                    column(6,
                      div(class = "vlc-qlgnd-item",
                        div(class = "vlc-qlgnd-dot",
                            style = "background: #2980B9;"),
                        tags$strong("Low Delivery / High Attendance")
                      ),
                      div(class = "vlc-qlgnd-item",
                        div(class = "vlc-qlgnd-dot",
                            style = "background: #E74C3C;"),
                        tags$strong("Low Delivery / Low Attendance")
                      )
                    )
                  )
                )
              )
            ),
            column(4,
              div(class = "vlc-chart-card vlc-priority-card",
                div(class = "chart-ttl",
                    icon("exclamation-triangle"), " Top 10 Priority Schools"),
                div(class = "chart-sub",
                    "Below-median delivery or attendance \u2014 with reason flagged"),
                DT::dataTableOutput("vlc_priority_schools")
              )
            )
          ),

          div(class = "vlc-gap"),

          # Section B: School Profile
          div(class = "vlc-section-hdr",
            div(class = "hdr-icon", icon("search")),
            div(
              h4("School Profile — Deep Dive"),
              p(class = "hdr-sub",
                "Select any school to view its attendance and gender trend over time")
            )
          ),

          div(
            class = "vlc-chart-card",
            fluidRow(
              column(5,
                selectInput("vlc_school_search", "Select a School:",
                            choices  = c("Select a school..." = ""),
                            width    = "100%")
              ),
              column(7,
                div(style = "padding-top: 6px;",
                    uiOutput("vlc_school_narrative"))
              )
            ),
            br(),
            fluidRow(
              column(6,
                div(class = "chart-ttl", "Attendance Trend Over Time"),
                div(class = "chart-sub vlc-gap-sm",
                    "Participation rate per session — dots coloured by tier"),
                plotlyOutput("vlc_school_timeline", height = "220px")
              ),
              column(6,
                div(class = "chart-ttl", "Gender Participation by Session"),
                div(class = "chart-sub vlc-gap-sm",
                    "Male and female attendance stacked by session date"),
                plotlyOutput("vlc_school_gender_trend", height = "220px")
              )
            )
          ),

          div(class = "vlc-gap"),

          # Section C: Raw Data (collapsible box)
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

        ) # end Tab 4

      ) # end tabsetPanel
    )   # end column
  )     # end fluidRow
)       # end tabItem

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
    updateSelectInput(session, "vlc_filter_year",    selected = "2025/2026")
    updateSelectInput(session, "vlc_filter_region",  selected = "all")
    updateSelectInput(session, "vlc_filter_session", selected = "all")
  })
  
  # --------------------------------------------------------------------------
  # FILTERED DATA (reactive)
  # --------------------------------------------------------------------------
  
  vlc_filtered <- reactive({
    df <- vlc_data() %>% filter(vlc_held == "Held")

    if (input$vlc_filter_year != "all")
      df <- df %>% filter(academic_year == input$vlc_filter_year)

    if (input$vlc_filter_region != "all")
      df <- df %>% filter(Region_hbk5 == input$vlc_filter_region)

    if (input$vlc_filter_session != "all")
      df <- df %>% filter(session_clean == input$vlc_filter_session)

    df
  })

  # All submissions (including non-held)
  vlc_all_subs <- reactive({
    df <- vlc_data()
    if (input$vlc_filter_year != "all")
      df <- df %>% filter(academic_year == input$vlc_filter_year)
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
    
    # Schools in any submission that have ZERO held sessions
    no_session_schools <- all %>%
      select(Name_school_hbk5, Region_hbk5) %>%
      distinct() %>%
      anti_join(school_summary, by = "Name_school_hbk5") %>%
      mutate(sessions_n = 0L, avg_partic = NA_real_,
             last_session = as.Date(NA))

    # Schools with held sessions but average participation below 50 %
    low_partic_schools <- school_summary %>%
      filter(avg_partic < 50)

    low_schools <- bind_rows(no_session_schools, low_partic_schools) %>%
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
          style = "font-size: 16px;",
          format(s$schools_reporting, big.mark = ",")
        ),
        " of Ghana's ",
        tags$strong(format(TOTAL_SCHOOLS_NATIONAL, big.mark = ",")),
        " Senior High Schools, representing ",
        tags$strong(
          style = glue("color: {if(s$coverage_pct >= 70) '#27AE60' else if(s$coverage_pct >= 50) '#E67E22' else '#E74C3C'};"),
          paste0(s$coverage_pct, "%")
        ),
        " national coverage, have submitted VLC session data.",
        " A total of ",
        tags$strong(
          style = "font-size: 16px;",
          format(s$total_sessions_held, big.mark = ",")
        ),
        " sessions have been reported, with an average attendance rate of ",
        tags$strong(
          style = glue("color: {if(s$avg_participation >= 75) '#27AE60' else if(s$avg_participation >= 55) '#E67E22' else '#E74C3C'};"),
          paste0(s$avg_participation, "%")
        ),
        " per session.",
        style = "margin-bottom: 10px;"
      ),
      p(
        tags$strong(top_session),
        " is currently the most frequently delivered session nationwide.",
        " The ", tags$strong(top_region),
        " Region has recorded the highest number of session submissions to date.",
        " Headteachers were present in ",
        tags$strong(
          style = glue("color: {if(s$ht_present_pct >= 60) '#27AE60' else '#E74C3C'};"),
          paste0(s$ht_present_pct, "%")
        ),
        " of sessions, while Guidance and Counselling officers participated in ",
        tags$strong(paste0(s$gcc_present_pct, "%")),
        ".",
        " Learners with disabilities were recorded in ",
        tags$strong(paste0(s$disability_rate, "%")),
        " of sessions, indicating progress toward inclusive participation.",
        " Weekly submission rates are currently ",
        tags$strong(
          style = glue("color: {if(trend_word == 'increasing') '#27AE60' else if(trend_word == 'declining') '#E74C3C' else '#E67E22'};"),
          trend_word
        ),
        ", and ",
        tags$strong(
          style = "color: #E74C3C;",
          format(nrow(s$low_schools), big.mark = ",")
        ),
        " schools have been identified for follow-up.",
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
  
  output$vlc_signal_attendance <- renderUI({
    s <- vlc_stats()
    col <- if (s$avg_participation >= 75) "#27AE60" else if (s$avg_participation >= 55) "#E67E22" else "#E74C3C"
    tagList(
      div(style = glue("font-size: 32px; font-weight: bold; color: {col};"),
          paste0(s$avg_participation, "%")),
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
  
  # ── Group 1: National Coverage ──────────────────────────────────────────────
  output$vlc_kpi_active_schools <- renderUI({
    s <- vlc_stats()
    col <- if (s$coverage_pct >= 70) "#27AE60" else if (s$coverage_pct >= 50) "#E67E22" else "#1B2A4A"
    div(class = "kpi-value", style = glue("color: {col};"),
        paste0(s$schools_reporting, " of ", TOTAL_SCHOOLS_NATIONAL))
  })

  output$vlc_kpi_coverage_pct <- renderUI({
    s <- vlc_stats()
    col <- if (s$coverage_pct >= 70) "#27AE60" else if (s$coverage_pct >= 50) "#E67E22" else "#E74C3C"
    div(class = "kpi-value", style = glue("color: {col};"),
        paste0(s$coverage_pct, "%"))
  })

  output$vlc_kpi_followup <- renderUI({
    s <- vlc_stats()
    col <- if (nrow(s$low_schools) <= 20) "#27AE60" else if (nrow(s$low_schools) <= 50) "#E67E22" else "#E74C3C"
    div(class = "kpi-value", style = glue("color: {col};"),
        nrow(s$low_schools))
  })

  # ── Group 2: Programme Delivery ─────────────────────────────────────────────
  output$vlc_kpi_sessions <- renderUI({
    s <- vlc_stats()
    div(class = "kpi-value", style = "color: #1B2A4A;",
        format(s$total_sessions_held, big.mark = ","))
  })

  output$vlc_kpi_avg_attend <- renderUI({
    s <- vlc_stats()
    col <- if (s$avg_participation >= 75) "#27AE60" else if (s$avg_participation >= 55) "#E67E22" else "#E74C3C"
    div(class = "kpi-value", style = glue("color: {col};"),
        paste0(s$avg_participation, "%"))
  })

  # ── Group 3: Quality & Inclusion ────────────────────────────────────────────
  output$vlc_kpi_headteacher_new <- renderUI({
    s <- vlc_stats()
    col <- if (s$ht_present_pct >= 60) "#27AE60" else if (s$ht_present_pct >= 40) "#E67E22" else "#E74C3C"
    div(class = "kpi-value", style = glue("color: {col};"),
        paste0(s$ht_present_pct, "%"))
  })

  output$vlc_kpi_disability_new <- renderUI({
    s <- vlc_stats()
    col <- if (s$disability_rate >= 20) "#27AE60" else if (s$disability_rate >= 10) "#E67E22" else "#E74C3C"
    div(class = "kpi-value", style = glue("color: {col};"),
        paste0(s$disability_rate, "%"))
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
  # BLOCK 1: Rollout Progression
  # --------------------------------------------------------------------------

  ALL_VALUE_THEMES <- c(
    "Responsible Citizenship", "Honesty", "Integrity", "Diversity",
    "Equity", "Discipline", "Self-Directed Learning", "Adaptability",
    "Resourcefulness", "Leadership", "Confidence"
  )

  output$vlc_rollout_s0_pct <- renderUI({
    df <- vlc_filtered()
    if (nrow(df) == 0) return(div(class = "kpi-value", "—"))
    school_sessions <- df %>%
      group_by(Name_school_hbk5) %>%
      summarise(has_s0 = any(session_num == 0, na.rm = TRUE), .groups = "drop")
    pct <- round(mean(school_sessions$has_s0, na.rm = TRUE) * 100, 1)
    tagList(
      div(style = "font-size: 22px; font-weight: 800; color: #1B2A4A;",
          paste0(pct, "%")),
      div(style = "font-size: 11px; color: #718096; margin-top: 3px;",
          "Schools Delivered Session 0")
    )
  })

  output$vlc_rollout_2plus_pct <- renderUI({
    df <- vlc_filtered()
    if (nrow(df) == 0) return(div(class = "kpi-value", "—"))
    school_sessions <- df %>%
      group_by(Name_school_hbk5) %>%
      summarise(n = n(), .groups = "drop")
    pct <- round(mean(school_sessions$n >= 2, na.rm = TRUE) * 100, 1)
    tagList(
      div(style = "font-size: 22px; font-weight: 800; color: #1B2A4A;",
          paste0(pct, "%")),
      div(style = "font-size: 11px; color: #718096; margin-top: 3px;",
          "Schools with At Least 2 Sessions")
    )
  })

  output$vlc_rollout_avg_sessions <- renderUI({
    df <- vlc_filtered()
    if (nrow(df) == 0) return(div(class = "kpi-value", "—"))
    avg <- df %>%
      group_by(Name_school_hbk5) %>%
      summarise(n = n(), .groups = "drop") %>%
      summarise(avg = mean(n, na.rm = TRUE)) %>%
      pull(avg) %>%
      round(1)
    tagList(
      div(style = "font-size: 22px; font-weight: 800; color: #1B2A4A;", avg),
      div(style = "font-size: 11px; color: #718096; margin-top: 3px;",
          "Avg Sessions per Active School")
    )
  })

  output$vlc_rollout_chart <- renderPlotly({
    df <- vlc_filtered() %>%
      filter(!is.na(session_num)) %>%
      group_by(Name_school_hbk5) %>%
      summarise(max_session = max(session_num, na.rm = TRUE), .groups = "drop") %>%
      mutate(stage = case_when(
        max_session == 0 ~ "Session 0 only",
        max_session <= 3 ~ "Sessions 1–3",
        max_session <= 6 ~ "Sessions 4–6",
        TRUE             ~ "Sessions 7+"
      )) %>%
      count(stage) %>%
      mutate(stage = factor(stage,
               levels = c("Session 0 only", "Sessions 1–3",
                           "Sessions 4–6", "Sessions 7+")))

    if (nrow(df) == 0) {
      return(plotly_empty() %>% layout(title = "No session data available"))
    }

    p <- ggplot(df, aes(x = stage, y = n,
                        text = paste0(stage, ": ", n, " schools"))) +
      geom_col(fill = "#1B2A4A", width = 0.55) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
      labs(x = NULL, y = "Number of Schools") +
      theme_minimal(base_size = 11) +
      theme(panel.grid.minor = element_blank(),
            panel.grid.major.x = element_blank())

    ggplotly(p, tooltip = "text") %>% hide_legend()
  })

  # --------------------------------------------------------------------------
  # BLOCK 2: National Value Theme Distribution
  # --------------------------------------------------------------------------

  output$vlc_theme_national <- renderPlotly({
    df <- vlc_filtered() %>%
      filter(!is.na(session_clean)) %>%
      left_join(
        SESSION_CATALOGUE %>%
          mutate(session_clean = paste0("Session ", session_number)),
        by = "session_clean"
      ) %>%
      filter(!is.na(value_theme), value_theme != "Orientation") %>%
      count(value_theme) %>%
      complete(value_theme = ALL_VALUE_THEMES, fill = list(n = 0)) %>%
      arrange(n) %>%
      mutate(
        bar_color = case_when(
          n == max(n) ~ "#1B2A4A",
          n == min(n) ~ "#8FA8C0",
          TRUE        ~ "#4A6FA5"
        )
      )

    if (nrow(df) == 0) {
      return(plotly_empty() %>% layout(title = "No session data available"))
    }

    p <- ggplot(df,
                aes(x = n, y = reorder(value_theme, n),
                    fill = bar_color,
                    text = paste0(value_theme, ": ", n, " sessions"))) +
      geom_col(width = 0.7) +
      scale_fill_identity() +
      scale_x_continuous(expand = expansion(mult = c(0, 0.1))) +
      labs(x = "Total Sessions Delivered Nationally", y = NULL) +
      theme_minimal(base_size = 9) +
      theme(panel.grid.minor = element_blank(),
            panel.grid.major.y = element_blank(),
            axis.text.y = element_text(size = 9))

    ggplotly(p, tooltip = "text") %>% hide_legend()
  })

  # --------------------------------------------------------------------------
  # BLOCK 3: Value Theme × Region Heatmap (normalized)
  # --------------------------------------------------------------------------

  output$vlc_theme_region_heatmap <- renderPlotly({
    # Active schools per region (denominator for normalization)
    active_by_region <- vlc_filtered() %>%
      filter(!is.na(Region_hbk5)) %>%
      group_by(Region_hbk5) %>%
      summarise(active_schools = n_distinct(Name_school_hbk5, na.rm = TRUE),
                .groups = "drop")

    df <- vlc_filtered() %>%
      filter(!is.na(session_clean), !is.na(Region_hbk5)) %>%
      left_join(
        SESSION_CATALOGUE %>%
          mutate(session_clean = paste0("Session ", session_number)),
        by = "session_clean"
      ) %>%
      mutate(theme = coalesce(value_theme, "Unknown")) %>%
      filter(theme != "Orientation", theme != "Unknown") %>%
      count(Region_hbk5, theme) %>%
      left_join(active_by_region, by = "Region_hbk5") %>%
      mutate(sessions_per_school = round(n / pmax(active_schools, 1), 2)) %>%
      complete(Region_hbk5, theme = ALL_VALUE_THEMES,
               fill = list(n = 0, sessions_per_school = 0))

    if (nrow(df) == 0) {
      return(plotly_empty() %>% layout(title = "No data available"))
    }

    p <- ggplot(df,
                aes(x = theme, y = Region_hbk5,
                    fill = sessions_per_school,
                    text = paste0(Region_hbk5, " — ", theme,
                                  "<br>Sessions per active school: ",
                                  sessions_per_school))) +
      geom_tile(colour = "white", size = 0.5) +
      scale_fill_gradient(low = "#EBF0F7", high = "#1B2A4A",
                          name = "Sessions\nper school") +
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
  # TABLE: Regional Summary (coverage + performance + composite rank)
  # --------------------------------------------------------------------------

  output$vlc_regional_summary <- DT::renderDataTable({
    raw <- vlc_all_subs() %>%
      group_by(Region_hbk5) %>%
      summarise(
        schools_vlc   = n_distinct(Name_school_hbk5, na.rm = TRUE),
        sessions_held = sum(vlc_held == "Held", na.rm = TRUE),
        avg_attend    = round(mean(participation_pct[vlc_held == "Held"], na.rm = TRUE), 1),
        ht_pct        = round(mean(headteacher_present[vlc_held == "Held"], na.rm = TRUE) * 100, 1),
        disab_pct     = round(mean((disability_included == TRUE)[vlc_held == "Held"], na.rm = TRUE) * 100, 1),
        .groups = "drop"
      ) %>%
      filter(!is.na(Region_hbk5)) %>%
      left_join(REGIONAL_SCHOOL_TOTALS, by = "Region_hbk5") %>%
      mutate(
        region_total  = replace_na(region_total, 0),
        coverage_pct  = round(schools_vlc / region_total * 100, 1),
        avg_sessions  = round(sessions_held / pmax(schools_vlc, 1), 1),
        # Composite: equal 25 % weight each pillar, all normalised 0–100
        Score = round(
          (pmin(coverage_pct, 100) * 0.25) +
          (pmin(avg_attend,   100) * 0.25) +
          (pmin(ht_pct,       100) * 0.25) +
          (pmin(disab_pct,    100) * 0.25),
          1)
      ) %>%
      arrange(desc(Score)) %>%
      mutate(Rank = row_number()) %>%
      select(
        Rank,
        Region          = Region_hbk5,
        `Total Schools` = region_total,
        `Schools w/ VLC`= schools_vlc,
        `Coverage %`    = coverage_pct,
        `Sessions Held` = sessions_held,
        `Avg Sessions`  = avg_sessions,
        `Avg Attend %`  = avg_attend,
        `HT Present %`  = ht_pct,
        `Inclusion %`   = disab_pct,
        Score
      )

    # Colour scale for Avg Attend %: red → green mapped over 0–100
    attend_vals  <- raw$`Avg Attend %`
    score_vals   <- raw$Score

    DT::datatable(
      raw,
      options = list(
        pageLength = 16, dom = "t", ordering = TRUE,
        columnDefs = list(list(className = "dt-center", targets = "_all"))
      ),
      rownames = FALSE
    ) %>%
      DT::formatStyle(
        "Avg Attend %",
        background = DT::styleColorBar(c(0, 100), "#27AE60"),
        backgroundSize = "90% 60%",
        backgroundRepeat = "no-repeat",
        backgroundPosition = "center"
      ) %>%
      DT::formatStyle(
        "Avg Attend %",
        color = DT::styleInterval(
          c(40, 60, 85),
          c("#C0392B", "#E67E22", "#27AE60", "#1A5276")
        )
      ) %>%
      DT::formatStyle(
        "Coverage %",
        background = DT::styleColorBar(c(0, 100), "#2980B9"),
        backgroundSize = "90% 60%",
        backgroundRepeat = "no-repeat",
        backgroundPosition = "center"
      ) %>%
      DT::formatStyle(
        "Score",
        background = DT::styleColorBar(range(score_vals, na.rm = TRUE), "#E6A817"),
        backgroundSize = "90% 70%",
        backgroundRepeat = "no-repeat",
        backgroundPosition = "center"
      ) %>%
      DT::formatStyle(
        "Rank",
        fontWeight = "bold",
        color = DT::styleInterval(c(4, 8, 12), c("#27AE60", "#2980B9", "#E67E22", "#C0392B"))
      )
  })
  
  # --------------------------------------------------------------------------
  # TABLE: School Performance Summary
  # --------------------------------------------------------------------------

  output$vlc_school_summary_table <- DT::renderDataTable({
    base_df <- vlc_filtered()

    # Apply view mode: filter each school to only its most recent session date
    if (isTRUE(input$vlc_summary_view == "recent")) {
      base_df <- base_df %>%
        group_by(Name_school_hbk5) %>%
        filter(session_date == max(session_date, na.rm = TRUE)) %>%
        ungroup()
    }

    df <- base_df %>%
      group_by(Region_hbk5, Name_school_hbk5) %>%
      summarise(
        meetings_held   = n(),
        sessions_list   = paste(sort(unique(na.omit(session_clean))), collapse = ", "),
        total_enrolled  = sum(total_enrolled,         na.rm = TRUE),
        total_attended  = sum(total_attended,         na.rm = TRUE),
        male_enrolled   = sum(no_male_teachers_vlc,   na.rm = TRUE),
        male_attended   = sum(male_attendance_vlc,    na.rm = TRUE),
        female_enrolled = sum(no_female_teachers_vlc, na.rm = TRUE),
        female_attended = sum(female_attendance_vlc,  na.rm = TRUE),
        # % of sessions that reported any learner with a disability
        pct_disab       = round(mean(disability_included == TRUE, na.rm = TRUE) * 100, 0),
        .groups = "drop"
      ) %>%
      mutate(
        overall_att_pct = round(pmin(total_attended  / pmax(total_enrolled,  1), 1) * 100, 1),
        male_att_pct    = round(pmin(male_attended   / pmax(male_enrolled,   1), 1) * 100, 1),
        female_att_pct  = round(pmin(female_attended / pmax(female_enrolled, 1), 1) * 100, 1),
        total_learners  = paste0(format(total_attended, big.mark = ","), " / ",
                                 format(total_enrolled, big.mark = ","))
      ) %>%
      arrange(Region_hbk5, Name_school_hbk5) %>%
      select(
        Region                  = Region_hbk5,
        School                  = Name_school_hbk5,
        `Sessions`              = meetings_held,
        `Topics Covered`        = sessions_list,
        `Attended / Enrolled`   = total_learners,
        `Att %`                 = overall_att_pct,
        `Male Att %`            = male_att_pct,
        `Female Att %`          = female_att_pct,
        `% Sess w/ Disab`       = pct_disab
      )

    att_range <- c(0, 100)

    DT::datatable(
      df,
      filter     = "top",
      extensions = c("Scroller", "FixedHeader", "FixedColumns"),
      options    = list(
        # Infinite scroll — no pagination
        scrollY      = "520px",
        scrollX      = TRUE,
        scroller     = TRUE,
        deferRender  = TRUE,
        fixedHeader  = TRUE,
        fixedColumns = list(leftColumns = 2),
        dom          = "fti",
        columnDefs   = list(
          list(className = "dt-center",
               targets   = c(2, 4, 5, 6, 7, 8)),
          list(width = "220px", targets = 3),
          list(className = "dt-wrap", targets = 3)
        )
      ),
      rownames = FALSE
    ) %>%
      # Overall Att %: single green colour bar + text colour thresholds
      DT::formatStyle(
        "Att %",
        background         = DT::styleColorBar(att_range, "#27AE60"),
        backgroundSize     = "90% 55%",
        backgroundRepeat   = "no-repeat",
        backgroundPosition = "center",
        color = DT::styleInterval(
          c(40, 60, 85),
          c("#C0392B", "#E67E22", "#16A085", "#1A5276")
        ),
        fontWeight = "bold"
      ) %>%
      # Male Att %: text colour only (no bar)
      DT::formatStyle(
        "Male Att %",
        color = DT::styleInterval(
          c(40, 70),
          c("#C0392B", "#E67E22", "#2980B9")
        )
      ) %>%
      # Female Att %: text colour only (no bar)
      DT::formatStyle(
        "Female Att %",
        color = DT::styleInterval(
          c(40, 70),
          c("#C0392B", "#E67E22", "#8E44AD")
        )
      ) %>%
      # Sessions count: warm-to-cool gradient
      DT::formatStyle(
        "Sessions",
        backgroundColor = DT::styleInterval(
          c(2, 5, 10),
          c("#FADBD8", "#FDEBD0", "#D5F5E3", "#A9DFBF")
        )
      ) %>%
      # Disability share: highlight schools with any reported inclusion
      DT::formatStyle(
        "% Sess w/ Disab",
        backgroundColor = DT::styleInterval(0, c("#FDFEFE", "#EBF5FB")),
        color = DT::styleInterval(0, c("#95A5A6", "#1A5276"))
      )
  })

  # --------------------------------------------------------------------------
  # PLOT: School Quadrant
  # --------------------------------------------------------------------------
  
  output$vlc_school_quadrant <- renderPlotly({
    df <- vlc_filtered() %>%
      group_by(Name_school_hbk5, Region_hbk5) %>%
      summarise(
        sessions_completed = n(),
        avg_participation  = mean(participation_pct,  na.rm = TRUE),
        total_enrolled     = sum(total_enrolled,       na.rm = TRUE),
        .groups = "drop"
      ) %>%
      filter(!is.na(avg_participation))

    if (nrow(df) == 0) return(plotly_empty())

    # Data-driven thresholds: national medians from the current filtered dataset
    med_sessions <- median(df$sessions_completed, na.rm = TRUE)
    med_att      <- median(df$avg_participation,  na.rm = TRUE)

    df <- df %>%
      mutate(
        quadrant = case_when(
          sessions_completed >= med_sessions & avg_participation >= med_att ~
            "High Delivery / High Attendance",
          sessions_completed >= med_sessions & avg_participation < med_att  ~
            "High Delivery / Low Attendance",
          sessions_completed <  med_sessions & avg_participation >= med_att ~
            "Low Delivery / High Attendance",
          TRUE ~
            "Low Delivery / Low Attendance"
        )
      )

    # Annotation positions: corners of each quadrant
    x_lo <- min(df$avg_participation,  na.rm = TRUE)
    x_hi <- max(df$avg_participation,  na.rm = TRUE)
    y_lo <- min(df$sessions_completed, na.rm = TRUE)
    y_hi <- max(df$sessions_completed, na.rm = TRUE)
    x_pad <- (x_hi - x_lo) * 0.06
    y_pad <- max((y_hi - y_lo) * 0.04, 0.3)

    p <- ggplot(df,
                aes(x = avg_participation, y = sessions_completed,
                    colour = quadrant,
                    size   = total_enrolled,
                    text   = paste0(
                      "<b>", Name_school_hbk5, "</b>",
                      "<br>Region: ",      Region_hbk5,
                      "<br>Sessions: ",    sessions_completed,
                        " (median: ", round(med_sessions, 1), ")",
                      "<br>Attendance: ",  round(avg_participation, 1), "%",
                        " (median: ", round(med_att, 1), "%)",
                      "<br>Enrolled: ",    format(total_enrolled, big.mark = ","),
                      "<br>", quadrant
                    ))) +
      geom_vline(xintercept = med_att,      linetype = "dashed",
                 colour = "#95A5A6", linewidth = 0.7) +
      geom_hline(yintercept = med_sessions, linetype = "dashed",
                 colour = "#95A5A6", linewidth = 0.7) +
      geom_point(alpha = 0.7) +
      scale_colour_manual(
        values = c(
          "High Delivery / High Attendance" = "#27AE60",
          "High Delivery / Low Attendance"  = "#E67E22",
          "Low Delivery / High Attendance"  = "#2980B9",
          "Low Delivery / Low Attendance"   = "#E74C3C"
        )
      ) +
      scale_size_continuous(range = c(2, 9), guide = "none") +
      annotate("text", x = x_hi - x_pad, y = y_hi - y_pad,
               label = "High Delivery\nHigh Attendance",
               colour = "#27AE60", size = 2.7, fontface = "bold", hjust = 1) +
      annotate("text", x = x_lo + x_pad, y = y_hi - y_pad,
               label = "Low Delivery\nHigh Attendance",
               colour = "#2980B9", size = 2.7, fontface = "bold", hjust = 0) +
      annotate("text", x = x_hi - x_pad, y = y_lo + y_pad,
               label = "High Delivery\nLow Attendance",
               colour = "#E67E22", size = 2.7, fontface = "bold", hjust = 1) +
      annotate("text", x = x_lo + x_pad, y = y_lo + y_pad,
               label = "Low Delivery\nLow Attendance",
               colour = "#E74C3C", size = 2.7, fontface = "bold", hjust = 0) +
      labs(
        x      = paste0("Average Attendance Rate (%)  \u2014  median = ",
                         round(med_att, 1), "%"),
        y      = paste0("Sessions Completed  \u2014  median = ",
                         round(med_sessions, 1)),
        colour = NULL
      ) +
      theme_minimal(base_size = 11) +
      theme(legend.position  = "bottom",
            panel.grid.minor = element_blank())

    ggplotly(p, tooltip = "text") %>%
      layout(legend = list(orientation = "h", y = -0.25))
  })
  
  # --------------------------------------------------------------------------
  # TABLE: Priority Schools
  # --------------------------------------------------------------------------
  
  output$vlc_priority_schools <- DT::renderDataTable({
    school_data <- vlc_filtered() %>%
      group_by(Name_school_hbk5, Region_hbk5) %>%
      summarise(
        sessions_n   = n(),
        avg_partic   = round(mean(participation_pct, na.rm = TRUE), 1),
        .groups      = "drop"
      )

    # Data-driven thresholds from current filtered data
    med_sessions <- median(school_data$sessions_n, na.rm = TRUE)
    med_att      <- median(school_data$avg_partic,  na.rm = TRUE)

    df <- school_data %>%
      filter(
        sessions_n < med_sessions |
        (!is.na(avg_partic) & avg_partic < med_att)
      ) %>%
      mutate(
        why_flagged = case_when(
          sessions_n < med_sessions & (!is.na(avg_partic) & avg_partic < med_att) ~
            paste0(sessions_n, " sessions (median: ", round(med_sessions),
                   "); ", avg_partic, "% att (median: ", round(med_att, 1), "%)"),
          sessions_n < med_sessions ~
            paste0("Only ", sessions_n, " session(s) \u2014 median is ",
                   round(med_sessions)),
          TRUE ~
            paste0(avg_partic, "% attendance below median (",
                   round(med_att, 1), "%)")
        )
      ) %>%
      arrange(sessions_n, avg_partic) %>%
      select(
        School        = Name_school_hbk5,
        Region        = Region_hbk5,
        Sessions      = sessions_n,
        `Att %`       = avg_partic,
        `Why Flagged` = why_flagged
      ) %>%
      head(10)

    DT::datatable(
      df,
      options  = list(dom = "t", scrollY = "330px", scrollX = TRUE),
      rownames = FALSE
    ) %>%
      DT::formatStyle(
        "Sessions",
        backgroundColor = DT::styleInterval(
          c(0, 3), c("#FADBD8", "#FDEBD0", "#EAFAF1")
        )
      ) %>%
      DT::formatStyle(
        "Att %",
        color = DT::styleInterval(
          c(40, 60),
          c("#C0392B", "#E67E22", "#27AE60")
        ),
        fontWeight = "bold"
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
        tags$strong(sessions), " VLC session(s), with a total of ",
        tags$strong(format(total_st, big.mark = ",")),
        " session attendances recorded. ",
        "Average attendance per session is ", tags$strong(paste0(avg_p, "%")), ". ",
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
