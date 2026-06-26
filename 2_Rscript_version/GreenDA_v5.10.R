#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
################################################################################
# Shiny app for GreenFeed data analysis
# Outlier: based on Arbre & Coppa
# Analysis: Yoan's Macro
# Made by SJeon
# v5.1X (2026-June-16): Repeatability calculation model update
################################################################################

library(shiny)
library(readxl)
library(openxlsx)
library(dplyr)
library(tibble)
library(lme4) # diurnal adjustment; repeatability calculation
library(DT)
library(psych)
library(ggplot2)
library(plotly) # interactive plot
library(shinyjs)
library(chron)
library(shinyWidgets) # selecting animal list

########################################################################################################################
################################################# UI DEFINITION SCRIPT##################################################
########################################################################################################################

ui <- fluidPage(
  shinyjs::useShinyjs(), # restart shinyjs
  
  # Style define
  tags$head(
    tags$style(HTML("
                    .section-title {
                      font-size: 20px;
                      font-weight: bold;
                      margin-bottom: 10px;
                    }
                
                    .custom-well {
                      border: 1px solid #cccccc !important;
                      padding: 15px;
                      border-radius: 6px;
                      background-color: #f9f9f9;
                    }
                
                    .shiny-output-error-validation {
                      color: red;
                      font-weight: bold;
                    }
                
                    .watermark {
                      position: fixed;
                      bottom: 10px;
                      right: 15px;
                      color: rgba(150, 150, 150, 0.3);
                      font-size: 14px;
                      font-style: italic;
                      z-index: 9999;
                      pointer-events: none;
                    }
                    
                    .custom-step-tabs .tabbable{
                      display:block;
                    }
                    
                    .custom-step-tabs .nav{
                      display:flex;
                      flex-direction:row;
                      margin-bottom:0;
                      border-bottom:none;
                    }
                    
                    /* STEP BUTTON */
                    
                    .custom-step-tabs .nav > li{
                      flex:1;
                      text-align:center;
                    }
                    
                    .custom-step-tabs .nav > li > a{
                      border:2px solid #0b2a3a;
                      border-bottom:none;
                      background-color:#f9f9f9;
                      color:black;
                      font-size:15px;
                      font-weight:600;
                      padding:8px 4px;
                      border-radius:10px 10px 0 0;
                      margin-right:6px;
                    }
                    
                    .custom-step-tabs .nav > li:last-child > a{
                      margin-right:0;
                    }
                    
                    /* hover */
                    
                    .custom-step-tabs .nav > li > a:hover{
                      background-color:#f0f0f0;
                    }
                    
                    /* ACTIVE STEP */
                    
                    .custom-step-tabs .nav > li.active > a,
                    .custom-step-tabs .nav > li.active > a:hover{
                      background-color:black;
                      color:white;
                      font-weight:700;
                    }
                    
                    
                    /* =====================================================
                    STEP CONTENT
                    ===================================================== */
                    
                    .custom-step-tabs .tab-content{
                      background:#f9f9f9;
                      border:2px solid #0b2a3a;
                      border-top:none;
                      border-radius:0 0 8px 8px;
                      padding:15px;
                    }
                    
                    
                    /* =====================================================
                    STEP PAGE TITLE
                    ===================================================== */
                    
                    .custom-step-tabs .page-title{
                      font-size:22px;
                      font-weight:bold;
                      margin-bottom:12px;
                    }

                    .modal-lg {
                      width: 95% !important;
                      max-width: 1400px !important;
                    }
                  "))),

  ########################################################################################################################
  # TITLE PANEL #
  ########################################################################################################################
  # titlePanel(HTML("GreenDA: GreenFeed<sup>&reg;</sup> data analyzer (INRAE-UMRH)")),
  div(style = "display: flex; align-items: center; position: relative; left: 0px;",
      img(src = "Logo2_v3.png", height = "80px"),
      span(
        "GreenDA: GreenFeed",
        HTML("<sup>&reg;</sup>"),
        " data analyzer (INRAE)",
        style = "font-size: 30px; margin-left: 10px; margin-right: 10px; vertical-align: middle; 
                font-family: Times New Roman, sans-serif; font-weight: bold;"
      )),
  
  ########################################################################################################################
  # SIDE PANEL #
  ########################################################################################################################
  sidebarLayout(
    sidebarPanel(
      width = 5,
      #==================================================================================================================#
      div(
        class = "custom-step-tabs",
        tabsetPanel(
          id = "step_tabs",
          selected = "manual", # First page is manual; if you want to start it from step1, change it as "gf"
          
          tabPanel(
            "Manual",
            value = "manual",
            div(class = "page-title", "Manual"),
            div(
              class = "custom-well",
              div("- MANUAL DOWNLOAD -", class = "section-title"),
              p("This is an overview of the application"),
              downloadButton("Manual_download", "Download Manual")
            ),
            div(
              class = "custom-well",
              div("- COLUMN MAPPING TEMPLATE -", class = "section-title"),
              p("Download this template, edit only the « Column » column, and upload it in STEP 1."),
              downloadButton("download_col_temp", "Download column mapping template")
            )
          ),
          
          tabPanel(
            "STEP 1",
            value = "gf",
            div(class = "page-title", "1.GreenFeed File"),
            
            div(
              class = "custom-well",
              div("[1] UPLOAD GREENFEED FILE", class = "section-title"),
              fileInput("file", "GreenFeed File upload (xlsx, xlsm, xltm, csv)",
                        accept = c(".xlsx", ".xlsm", ".xltm", ".csv")),
              
              fileInput("col_map_file", "[OPTIONAL] Column mapping file upload (.xlsx)", accept = c(".xlsx")),
              
              checkboxGroupInput(
                "available_data",
                "Select additional data (CO2 & CH4 are mandatory): ",
                choices = c("O2", "H2", "Airflow"),
                inline = TRUE
              )
            ),
            
            div(
              class = "custom-well",
              div("[2] SELECT DATA COLUMNS", class = "section-title"),
              uiOutput("sheet_ui"),
              uiOutput("dynamic_ui")
            ),
            tags$hr(),
            div(
              style = "display:flex; align-items:center; gap:10px;",
              actionButton("Done_GF", "Go to next step", icon = icon("play"), class = "btn btn-primary"),
              uiOutput("step2_ready_text")
            )
            ),
          
          tabPanel(
            "STEP 2",
            value = "treatment",
            div(class = "page-title", "2. Factor File"),
            
            div(
              class = "custom-well",
              div("[1] UPLOAD FACTOR FILE", class = "section-title"),
              checkboxInput("treat_info", "I want to consider some factors for analysis (e.g. Treatment)", value = FALSE),
              conditionalPanel(
                condition = "input.treat_info",
                checkboxInput("same_file", "I want to use the same file as GreenFeed", value = FALSE),
                conditionalPanel(
                  condition = "!input.same_file",
                  fileInput("file_trt", "Factor information upload (xlsx, xlsm, xltm, csv)",
                            accept = c(".xlsx", ".xlsm", ".xltm", ".csv"))
                )
              )
            ),
            
            conditionalPanel(
              condition = "input.treat_info",
              tagList(
                div(
                  class = "custom-well",
                  div("[2] SELECT DATA COLUMNS", class = "section-title"),
                  uiOutput("sheet_ui_trt"),
                  uiOutput("column_ui_trt")
                )
              )
            ),
            tags$hr(),
            
            div(
              style = "display:flex; align-items:center; gap:10px;",
              conditionalPanel(
                condition = "input.treat_info",
                actionButton("Done_trt", "Go to next step", ico = icon("play"), class = "btn btn-primary")
              ),
              conditionalPanel(
                condition = "!input.treat_info",
                actionButton("Done_trt", "Skip this step", icon = icon("forward"), class = "btn btn-secondary")
              ),
              uiOutput("step3_ready_text")
            )
          ),
          
          tabPanel(
            "STEP 3",
            value = "screen",
            div(class = "page-title", "3. Dataset Screening"),
            div(
              class = "custom-well",
              div("DATA RANGE", class = "section-title"),
              uiOutput("date_scr_ui")
            ),
            
            div(
              class = "custom-well",
              div("GREENFEED MACHINE", class = "section-title"),
              uiOutput("machine_scr_ui")
            ),
            
            conditionalPanel(
              condition = "input.treat_info",
              div(
                class = "custom-well",
                div("FACTOR", class = "section-title"),
                uiOutput("dynamic_treat_scr_ui")
              )
            ),
            
            div(
              class = "custom-well",
              div("ANIMAL", class = "section-title"),
              uiOutput("animal_scr_ui")
            ),
            
            tags$hr(),
            div(
              style = "display:flex; align-items:center; gap:10px;",
              actionButton("data_scr_done", "Go to next step", icon = icon("play"), class = "btn btn-primary"),
              uiOutput("step4_ready_text")
              
            )
          ),
          
          tabPanel(
            "STEP 4",
            value = "outlier",
            div(class = "page-title", "4. Outlier Cleaning"),
            
            div(
              class = "custom-well",
              checkboxInput("outlier_info", "I want to remove outliers", value = FALSE)
            ),
            
            conditionalPanel(
              condition = "!input.outlier_info",
              tags$hr(),
              div(
                style = "display:flex; align-items:center; gap:10px;",
                actionButton("skip_cln", "Skip this step", icon = icon("forward"), class = "btn btn-secondary"),
                uiOutput("step5_ready_text")
              )
            ),
            
            conditionalPanel(
              condition = "input.outlier_info",
              tagList(
                div(
                  class = "custom-well",
                  div("STAY DURATION", class = "section-title"),
                  fluidRow(
                    column(
                      6,
                      selectInput(
                        "duration_outlier", "Remove outliers based on stay duration?",
                        choices = c("No", "Yes"), selected = "No"
                      )
                    ),
                    column(
                      6,
                      conditionalPanel(
                        condition = "input.duration_outlier == 'Yes'",
                        numericInput(
                          "duration_lim", "Stay duration (sec) limitation (default = 120)",
                          value = 120, min = 0, step = 10
                        )
                      )
                    )
                  ),
                  
                  conditionalPanel(
                    condition = "input.available_data && input.available_data.includes('Airflow')",
                    tagList(
                      br(),
                      div("AIRFLOW", class = "section-title"),
                      fluidRow(
                        column(
                          6,
                          selectInput(
                            "airflow_outlier", "Remove outliers based on Airflow?",
                            choices = c("No", "Yes"), selected = "No"
                          )
                        ),
                        column(
                          6,
                          conditionalPanel(
                            condition = "input.airflow_outlier == 'Yes'",
                            numericInput(
                              "airflow_lim", "Airflow (L/s) limitation (default=26)",
                              value = 26, min = 0, step = 0.1
                            )
                          )
                        )
                      )
                    )
                  ),
                  
                  br(),
                  div("NO. OF VISIT", class = "section-title"),
                  fluidRow(
                    column(
                      6,
                      selectInput(
                        "no_visit_outlier", "Remove data from poor visit?",
                        choices = c("No", "Yes"), selected = "No"
                      )
                    ),
                    column(
                      6,
                      conditionalPanel(
                        condition = "input.no_visit_outlier == 'Yes'",
                        numericInput(
                          "no_visit_lim", "Minimum number of visit",
                          value = 20, min = 0, step = 1
                        )
                      )
                    )
                  ),
                  
                  br(),
                  actionButton("1st_cln", "1st Cleaning", icon = icon("play"), class = "btn btn-primary")
                ),
                
                conditionalPanel(
                  condition = "input['1st_cln'] > 0",
                  div(
                    class = "custom-well",
                    div(
                      style = "display:flex; align-items:center; gap:10px;",
                      div("STATISTICALLY - CO2 & CH4", class = "section-title"),
                      conditionalPanel(
                        condition = "input.analysis_method == 'Standard_deviation'",
                        actionButton(
                          "ch4_dist_check", "CH4 Distribution check",
                          icon = icon("chart-bar"), class = "btn btn-info btn-sm"
                        )
                      )
                    ),
                    
                    fluidRow(
                      column(
                        6,
                        selectInput(
                          "analysis_method", "Select a method to remove CO2, CH4 outlier",
                          choices = c("No", "Standard_deviation", "Arbre_2016", "Coppa_2021")
                        )
                      ),
                      column(
                        6,
                        conditionalPanel(
                          condition = "input.analysis_method == 'Standard_deviation'",
                          tagList(
                            radioButtons(
                              "ch4co2_split_cleaning", "Split dataset by factor before CH4/CO2 cleaning?",
                              choices = c("No","Yes"), selected = "No", inline = TRUE
                            ),
                            conditionalPanel(
                              condition = "input.ch4co2_split_cleaning == 'Yes'",
                              uiOutput("ch4co2_split_vars_ui")
                            )
                          ),
                          numericInput(
                            "sd_multiplier", "SD multiplier (default = 3)",
                            value = 3, min = 1, step = 0.1
                          )
                        ),
                        conditionalPanel(
                          condition = "input.analysis_method == 'Arbre_2016' || input.analysis_method == 'Coppa_2021'",
                          actionButton("ch4co2_r2_calc", "R² calculation"),
                          br(), br(),
                          tableOutput("ch4co2_corr_summary")
                        )
                      )
                    ),
                    br(),
                    actionButton("2nd_cln", "2nd Cleaning", icon = icon("play"), class = "btn btn-primary")
                  )
                ),
                
                conditionalPanel(
                  condition = "input['2nd_cln'] > 0 && input.available_data && (input.available_data.includes('O2') || input.available_data.includes('H2'))",
                  div(
                    class = "custom-well",
                    
                    div(
                      style = "display:flex; align-items:center; gap:10px;",
                      div("STATISTICALLY - O2 & H2", class = "section-title"),
                      
                      conditionalPanel(
                        condition = "input.available_data && input.available_data.includes('O2')",
                        actionButton(
                          "o2_dist_check", "O2 Distribution check",
                          icon = icon("chart-bar"), class = "btn btn-info btn-sm"
                        )
                      ),
                      
                      conditionalPanel(
                        condition = "input.available_data && input.available_data.includes('H2')",
                        actionButton(
                          "h2_dist_check", "H2 Distribution check",
                          icon = icon("chart-bar"), class = "btn btn-info btn-sm"
                        )
                      )
                    ),
                    
                    conditionalPanel(
                      condition = "input.available_data && input.available_data.includes('O2')",
                      tagList(
                        radioButtons(
                          "o2_split_cleaning", "Split dataset by factor before O2 cleaning?",
                          choices = c("No", "Yes"), selected = "No", inline = TRUE
                        ),
                        conditionalPanel(
                          condition = "input.o2_split_cleaning == 'Yes'",
                          uiOutput("o2_split_vars_ui")
                        ),
                        fluidRow(
                          column(
                            6,
                            selectInput(
                              "O2_outlier", "Remove O2 outliers?",
                              choices = c("No", "Yes"), selected = "No"
                            )
                          ),
                          column(
                            6,
                            conditionalPanel(
                              condition = "input.O2_outlier == 'Yes'",
                              numericInput(
                                "O2_lim", "SD multiplier (default = 3)",
                                value = 3, min = 1, step = 0.1
                              )
                            )
                          )
                        )
                      )
                    ),
                    
                    conditionalPanel(
                      condition = "input.available_data && input.available_data.includes('H2')",
                      tagList(
                        radioButtons(
                          "h2_split_cleaning", "Split dataset by factor before H2 cleaning?",
                          choices = c("No", "Yes"), selected = "No", inline = TRUE
                        ),
                        conditionalPanel(
                          condition = "input.h2_split_cleaning == 'Yes'",
                          uiOutput("h2_split_vars_ui")
                        ),
                        fluidRow(
                          column(
                            6,
                            selectInput(
                              "H2_outlier", "Remove H2 outliers?",
                              choices = c("No", "Yes"), selected = "No"
                            )
                          ),
                          column(
                            6,
                            conditionalPanel(
                              condition = "input.H2_outlier == 'Yes'",
                              numericInput(
                                "H2_lim", "H2 IQR muliplier (default = 1.5)",
                                value = 1.5, min = 0, step = 0.1
                              )
                            )
                          )
                        )
                      )
                    ),
                    
                    br(),
                    actionButton("3rd_cln", "3rd Cleaning", icon = icon("play"), class = "btn btn-primary")
                  )
                ),
                
                tags$hr(),
                div(
                  actionButton("data_cln_done", "Go to next step", icon = icon("check"), class = "btn btn-success", disabled = TRUE),
                  downloadButton("download_clean_data", "Cleaned data", disabled = TRUE, style = "margin-left: 10px")
                ),
                br(),
                uiOutput("cleaning_summary")
              )
            )
          ),
          
          
          tabPanel("STEP 5", value = "diurnal",
                   div(class = "page-title", "5. Diurnal adjust of CH4 data"),
                   
                   div(
                     class = "custom-well",
                     checkboxInput("time_smooth", "I want to adjust Diurnal variation", value = FALSE)
                   ),
                   
                   conditionalPanel(
                     condition = "input.time_smooth",
                     tagList(
                       div(
                         class = "custom-well",
                         div("ADJUSTMENT FACTORS", class = "section-title"),
                         uiOutput("adj_factor_ui")
                       )
                     )
                   ),
                   
                   tags$hr(),
                   
                   div(
                     style = "display:flex; align-items:center; gap:10px; flex-wrap:wrap;",
                     
                     conditionalPanel(
                       condition = "input.time_smooth",
                       actionButton("adj_ch4", "Diurnal variation adjust",
                                    icon = icon("play"), class = "btn btn-primary")
                     ),
                     
                     conditionalPanel(
                       condition = "input.time_smooth",
                       downloadButton("download_adj_data", "Adjusted data",
                                      disabled = TRUE, style = "margin-left: 0px")
                     ),
                     
                     conditionalPanel(
                       condition = "!input.time_smooth",
                       actionButton("adj_ch4", "Skip this step",
                                    icon = icon("forward"), class = "btn btn-secondary")
                     ),
                     
                     uiOutput("step6_ready_text")
                   )
          ),

          tabPanel("STEP 6", value = "range",
                   div(class = "page-title", "6. Analysis Range Selection"),
                   
                   div(
                     class = "custom-well",
                     checkboxInput("range_info", "I want to analyze only a part of the prepared dataset", value = FALSE)
                   ),
                   
                   div(
                     class = "custom-well",
                     style = "display:none;",
                     div("DATE RANGE", class = "section-title"),
                     uiOutput("date_range_ui")
                   ),
                   
                   conditionalPanel(
                     condition = "input.range_info",
                     div(
                       class = "custom-well",
                       div("DATE RANGE", class = "section-title"),
                       uiOutput("date_range_ui")
                     ),
                     
                     div(
                       class = "custom-well",
                       div("GREENFEED MACHINE", class = "section-title"),
                       uiOutput("machine_filter_ui")
                     ),
                     
                     conditionalPanel(
                       condition = "input.treat_info",
                       div(
                         class = "custom-well",
                         div("FACTOR", class = "section-title"),
                         uiOutput("dynamic_treat_filter_ui")
                       )
                     ),
                     
                     div(
                       class ="custom-well",
                       div("ANIMAL", class = "section-title"),
                       uiOutput("animal_filter_ui")
                     )
                   ),
                   
                   tags$hr(),
                   
                   div(
                     style = "display:flex; align-items:center; gap:10px;",
                     actionButton("range_sel_done", "Ready for analysis", icon = icon("play"), class = "btn btn-primary"),
                     downloadButton("download_ready_data", "Ready data", disabled = TRUE, style = "margin-left: 10px"),
                     uiOutput("analysis_ready_text")
                   )
          )
        ),
        div(style ="text-align: right; margin-top: 20px;", 
            actionButton("reset_button", "Restart App", icon = icon("redo"),
                         style = "background-color: black; color: white; border-color: black;"))
      )
      ),
    
    ########################################################################################################################
    # MAIN PANEL #
    ########################################################################################################################
    
    mainPanel(
      width = 7,
      div(class = "watermark", "© INRAE UMRH 2026"),
      tabsetPanel(id = "tabs",
                  #==================================================================================================================#
                  tabPanel("CH4_distribution", value = "tab_ch4dist", br(),
                           selectInput("ch4_dist_group", "Select CH4 distribution grouping", choices = "Entire", selected = "Entire"),
                           actionButton("plot_dist", "Distribution graph"), plotlyOutput("CH4_dist")),
                  #==================================================================================================================#
                  tabPanel("Repeatability", value = "tab_repeatability",
                          
                    div(class = "custom-well", 
                        div("REPEATABILITY RANGE", class = "section-title"),
                      
                      radioButtons("rep_date_mode", "Date indexing method",
                        choices = c("Calendar date" = "calendar", "Ordinal day" = "ordinal"),
                        selected = "calendar",
                        inline = TRUE
                      ),
                      
                      uiOutput("rep_date_range_ui"),
                      
                      actionButton("rep_range_done", "Select Done", icon = icon("check"), class = "btn btn-primary"),
                      br(),
                      
                      # verbatimTextOutput("divider_list"),
                      textOutput("divider_list"),
                      
                      div(
                        style = "display:flex; align-items:center; white-space:nowrap; margin-top:10px;",
                        tags$input(
                          type = "checkbox",
                          id = "rep_use_fixed",
                          name = "rep_use_fixed",
                          style = "margin:0; width:18px; height:18px;"
                        ),
                        tags$label(
                          "Consider fixed effect for repeatability calculation",
                          `for` = "rep_use_fixed",
                          style = "margin-left:8px; margin-bottom:0; font-size:16px; font-weight:normal; white-space:nowrap;"
                        )
                      ),
                      
                      conditionalPanel(
                        condition = "input.rep_use_fixed",
                        uiOutput("rep_fixed_effect_ui")
                      )
                    ),
                    
                    div(
                      br(),
                      actionButton("calc_repeatability", "Calculate Repeatability", icon = icon("play")),
                      downloadButton("download_repeatability", "Download Repeatability Results",
                                     disabled = TRUE, style = "margin-left: 10px;")
                    ),
                    
                    br(),
                    
                    div(
                      style = "display: flex; align-items: center; ",
                      tags$input(type = "checkbox", id = "show_hline", name = "show_hline",
                                 style = "margin: 0; width: 18px; height: 18px; vertical-align: middle;"),
                      tags$label("Show threshold line", `for` = "show_hline",
                                 style = "margin-left: 6px; font-size: 16px; font-weight: normal; line-height: 1; 
                                 display: inline-block; vertical-align: middle;"),
                      div(style = "display: flex; align-items: center; gap: 6px; margin-left: 10px;",
                          tags$label("→ Threshold: ", `for` = "hline_value",
                                     style = "font-size: 16px; font-weight: bold; margin: 0;"),
                          numericInput("hline_value", "", value = 0.7, step = 0.05, width = "80px"))
                    ),
                    
                    br(),
                    plotlyOutput("Rep_grp"),
                    
                    div(style = "margin-top: 30px; "),
                    
                    conditionalPanel(
                      condition = "input.time_smooth == true",
                      plotlyOutput("adjRep_grp")
                    )
                  ),
                  #==================================================================================================================#
                  tabPanel("Kinetics Visit", value = "tab_kinetics_visit", br(),
                           selectInput("visit_group", "Select plot grouping", choices = c("Whole", "Animal"), selected = "Whole"),
                           selectInput("visit_ytype", "Y-axis type", choices = c("Count", "Percent"), selected = "Count"),
                           actionButton("plot_visit", "Plot Visit graph"), plotlyOutput("visit_time_grp")),
                  #==================================================================================================================#
                  tabPanel("Kinetics CH4", value = "tab_kinetics_ch4", br(),
                           selectInput("error_bar", "Select error bar", choices = c("None", "SEM", "SD")),
                           selectInput("ch4_group", "Select plot grouping", choices = c("Whole", "Animal"), selected = "Whole"),
                           actionButton("plot_ch4", "Plot CH4 graph"), plotlyOutput("CH4_time_grp"), 
                           div(
                             conditionalPanel(
                               condition = "input.time_smooth",
                               plotlyOutput("CH4_time_adj_grp")
                             ))),
                  #==================================================================================================================#
                  tabPanel("Descriptive statistics", value = "tab_file_download",
                           br(),
                           
                           tags$div(class = "custom-well",
                                    
                                    tags$div(
                                      tags$span("1. WHOLE PERIOD ANALYSIS", class = "section-title"),
                                      actionButton("calc_all_whole", "Run All (Whole period)", icon = icon("play"),
                                                   class = "btn btn-success", style = "background-color: black; color: white; 
                                                   border-color: black; margin-left: 6px; padding: 3px 8px; font-size: 12px;")
                                    ),
                                    
                                    uiOutput("whole_analysis_tabs")
                                    ),

                           br(),
                           
                           tags$div(class = "custom-well",
                                    
                                    tags$div(
                                      tags$span("2. DAILY ANALYSIS", class = "section-title"),
                                      actionButton("calc_all_daily", "Run All (Daily)", icon = icon("play"),
                                                   class = "btn btn-success", style = "background-color: black; color: white; 
                                                   border-color: black; margin-left: 6px; padding: 3px 8px; font-size: 12px;")
                                    ),
                                    
                                    uiOutput("daily_analysis_tabs")
                                    ),
                           
                           br(),

                           tags$div(class = "custom-well",
                                    
                                    tags$div(
                                      tags$span("3. HOURLY ANALYSIS", class = "section-title"),
                                      actionButton("calc_all_hourly", "Run All (Hourly)", icon = icon("play"),
                                                   class = "btn btn-success", style = "background-color: black; color: white; 
                                                   border-color: black; margin-left: 6px; padding: 3px 8px; font-size: 12px;")
                                    ),
                                    
                                    uiOutput("hourly_analysis_tabs")
                                   ),
                           
                           br(),
                           
                           tags$div(class = "custom-well",
                                    
                                    tags$div(
                                      tags$span("4. DAILY & HOURLY ANALYSIS", class = "section-title"),
                                      actionButton("calc_all_dhly", "Run All (Daily&Hourly)", icon = icon("play"),
                                                   class = "btn btn-success", style = "background-color: black; color: white; 
                                                   border-color: black; margin-left: 6px; padding: 3px 8px; font-size: 12px;")
                                    ),
                                    
                                    uiOutput("dhly_analysis_tabs")
                                    ),
                           
                           br(),
                           
                           tags$div(class = "custom-well",
                                    
                                    tags$div(
                                      tags$span("5. REPEATABILITY PERIOD BASED ANALYSIS", class = "section-title"),
                                      actionButton("calc_all_rep", "Run All (Period)", icon = icon("play"),
                                                   class = "btn btn-success", style = "background-color: black; color: white; 
                                                   border-color: black; margin-left: 6px; padding: 3px 8px; font-size: 12px;")
                                    ),
                                    
                                    uiOutput("rep_analysis_tabs")
                                    )
                           
                           )))))

########################################################################################################################
################################################## CALCULATION SCRIPT ##################################################
########################################################################################################################

server <- function(input, output, session) {
  options(shiny.maxRequestSize = 60*1024^2)

  ########################################################################################################################
  # annotated_data <- reactiveVal(NULL)
  # cached_cleaned_data <- reactiveVal(NULL)
  
  # session$onFlushed(function(){
  #   hideTab("tabs", "treatment")
  # }, once = TRUE)
  # 
  # observeEvent(input$Done_GF, {
  #   showTab("tabs", "treatment")
  #   updateTabsetPanel(session, "tabs", selected = "treatment")
  # })
  
  
  # Reset module
  plot_state <- reactiveValues(
    ch4_dist_ready = FALSE,
    ch4_dist_trt1_ready = FALSE,
    ch4_dist_trt2_ready = FALSE,
    ch4_dist_trt3_ready = FALSE,
    rep_plot_ready = FALSE,
    adjrep_plot_ready = FALSE,
    visit_plot_ready = FALSE, 
    CH4_plot_ready = FALSE,
    adjCH4_plot_ready = FALSE
  )
  
  trt_dup_alert <- reactiveVal(NULL)
  
  observeEvent(input$reset_button, {
    showModal(
      modalDialog(
        title = "Restart App",
        "Do you want to restart the application?",
        footer = tagList(
          actionButton("confirm_reset", "Yes", class = "btn btn-danger"),
          modalButton("Cancel")
        ),
        easyClose = TRUE
      )
    )
  })

  observeEvent(input$confirm_reset,{
    session$userData$reloading <- TRUE
    session$reload()
  })
  
  # Reset when new input file is uploaded
  observeEvent(input$file, {
    # Basic GF data reset
    # output$divider_list <- renderText({""})
    # output$date_length <- renderText({""})
       
    # Plot reset
    plot_state$rep_plot_ready <- FALSE
    plot_state$adjrep_plot_ready <- FALSE
    plot_state$visit_plot_ready <- FALSE
    plot_state$CH4_plot_ready <- FALSE
    plot_state$adjCH4_plot_ready <- FALSE
    })


  ########################################################################################################################
  # Instruction file download #
  output$Manual_download <- downloadHandler(
    filename = function(){
      "GreenDA_User_Guide_v5.10_SJ.pdf"
    },
    content = function(file){
      file.copy("www/GreenDA_User_Guide_v5.10_SJ.pdf", file)
    })
  
  # Column name mapping file #
  output$download_col_temp <- downloadHandler(
    filename = function(){
      "GreenDA_column_mapping_template.xlsx"
    },
    content = function(file){
      file.copy("www/GreenDA_column_mapping_template.xlsx", file)
    }
  )

  ########################################################################################################################
  # GreenFeed datafile manipulate #

  # After file upload, select sheet
  
  # Column matching based on uploaded file
  column_map <- reactive({
    if(is.null(input$col_map_file)){
      return(NULL)
    }
    
    map <- read_excel(input$col_map_file$datapath)
    required_cols <- c("Field", "Column")
    
    if (!all(required_cols %in% names(map))){
      return(NULL)
    }
    
    map
    
  })
  
  get_selected_col <- function(map, field, cols, default = ""){
    
    if (is.null(map)) {
      return(default)
    }
    
    matched <- map$Column[map$Field == field]
    
    if (length(matched) == 0){
      return(default)
    }
    
    matched <- matched[1]
    
    if (is.na(matched) || matched == ""){
      return(default)
    }
    
    if(!(matched %in% cols)){
      return(default)
    }
    matched
  }
  
  sheets <- reactive({
    req(input$file)
    inFile <- input$file
    if (grepl("\\.xlsx$|\\.xlsm$|\\.xltm$", inFile$name, ignore.case = TRUE)) {
      excel_sheets(inFile$datapath)
    } else if (grepl("\\.csv$", inFile$name, ignore.case = TRUE)) {
      "CSV"
    } else {
      NULL
    }
  })
  
  output$sheet_ui <- renderUI({
    req(sheets())
    selectInput("sheet", "Select GreenFeed data sheet", choices = sheets())
  })
  
  # Data read after file uploading
  data <- reactive({
    req(input$file)
    inFile <- input$file
    if (grepl("\\.xlsx$|\\.xlsm$|\\.xltm$", inFile$name, ignore.case = TRUE)) {
      req(input$sheet)
      read_excel(inFile$datapath, sheet = input$sheet)
    } else if (grepl("\\.csv$", inFile$name, ignore.case = TRUE)){
      read.csv(inFile$datapath)
    } else {
      NULL
    }
  })
  
  # Select target column
  output$dynamic_ui <- renderUI({
    req(data())
    cols <- colnames(data())
    selected_vars <- input$available_data # H2, O2, airflow
    map <- column_map()
    
    ui_list <- list(
      # If your dataset always has same column name, you can edit the "" part of the commented out line #
      # selectInput("animal", "Select Animal ID column", choices = cols, selected = if ("ANIM" %in% cols) "ANIM"),
      # selectInput("machine", "Select GreenFeed machine ID column", choices = cols, selected = if ("FID" %in% cols) "FID"),
      # selectInput("startdt", "Select Start date column", choices = cols, selected = if ("DEBVIS" %in% cols) "DEBVIS"),
      # selectInput("enddt", "Select End date column", choices = cols, selected = if ("FINVIS" %in% cols) "FINVIS"),
      # selectInput("duree_day", "Select Good Data Duration", choices = cols, selected = if ("DURVIS" %in% cols) "DURVIS"),
      # selectInput("hour_day", "Select Hour of day column", choices = cols, selected = if ("HORDEB" %in% cols) "HORDEB"),
      # selectInput("CO2", "Select CO2 column", choices = cols, selected = if ("CO224H" %in% cols) "CO224H"),
      # selectInput("CH4", "Select CH4 column", choices = cols, selected = if ("CH424H" %in% cols) "CH424H")
      # 
      selectInput("animal", "Select Animal ID column", choices = c("None" = "", as.character(cols)), selected = get_selected_col(map, "animal", cols, "")),
      selectInput("machine", "Select GreenFeed machine ID column", choices = c("None" = "", as.character(cols)), selected = get_selected_col(map, "machine", cols, "")),
      selectInput("startdt", "Select Start date column", choices = c("None" = "", as.character(cols)), selected = get_selected_col(map, "startdt", cols, "")),
      selectInput("enddt", "Select End date column", choices = c("None" = "", as.character(cols)), selected = get_selected_col(map, "enddt", cols, "")),
      selectInput("duree_day", "Select Good Data Duration", choices = c("None" = "", as.character(cols)), selected = get_selected_col(map, "duree_day", cols, "")),
      selectInput("hour_day", "Select Hour of day column", choices = c("None" = "", as.character(cols)), selected = get_selected_col(map, "hour_day", cols, "")),
      selectInput("CO2", "Select CO2 column", choices = c("None" = "", as.character(cols)), selected = get_selected_col(map, "CO2", cols, "")),
      selectInput("CH4", "Select CH4 column", choices = c("None" = "", as.character(cols)), selected = get_selected_col(map, "CH4", cols, ""))

    )
    
    if ("O2" %in% selected_vars){
      ui_list <- append(ui_list, list(
        selectInput("O2", "Select O2 column", choices = c("None" = "", as.character(cols)), selected = get_selected_col(map, "O2", cols, ""))
      ))
    }
    
    if ("H2" %in% selected_vars){
      ui_list <- append(ui_list, list(
        selectInput("H2", "Select H2 column", choices = c("None" = "", as.character(cols)), selected = get_selected_col(map, "H2", cols, ""))
      ))
    }

    if ("Airflow" %in% selected_vars){
      ui_list <- append(ui_list, list(
        # selectInput("airflow_col", "Select Airflow column", choices = cols, selected = if ("FLUAIR" %in% cols) "FLUAIR")
        selectInput("airflow_col", "Select Airflow column", choices = c("None" = "", as.character(cols)), selected = get_selected_col(map, "airflow_col", cols, ""))
      ))
    }
    
    tagList(ui_list)
    
  })
  
  
  # Data reading
  raw_data_rv <- reactiveVal(NULL)
  
  observeEvent(input$Done_GF, {
    
    print("GF RAW DATA READING")
    
    req(input$animal, input$machine, input$startdt, input$enddt, input$duree_day, input$hour_day, input$CO2, input$CH4)
    req(data())
    
    cached_raw_data <<- NULL

    selected_vars <- input$available_data # H2, O2, airflow
    
    # all default dataset
    required_cols <- c(input$animal, input$machine, input$startdt, input$enddt, input$duree_day, input$hour_day, input$CO2, input$CH4)
    required_cols <- required_cols[required_cols != ""]
    
    # optional data column (H2, O2, airflow)
    optional_vars <- c()
    
    if ("H2" %in% selected_vars && !is.null(input$H2)){
      optional_vars <- c(optional_vars, input$H2)
    }
    if ("O2" %in% selected_vars && !is.null(input$O2)){
      optional_vars <- c(optional_vars, input$O2)
    }
    if ("Airflow" %in% selected_vars && !is.null(input$airflow_col)){
      optional_vars <- c(optional_vars, input$airflow_col)
    }
    
    all_cols <- c(required_cols, optional_vars)
    all_cols <- all_cols[all_cols != "" & !is.na(all_cols)] # Filtering, for just in case
    
    df.raw <- data()[, all_cols, drop = FALSE]
    
    ###### DATE ######
    parse_datetime_col <- function(x, tz = "UTC") {
      if (inherits(x, c("POSIXct", "POSIXlt"))){
        return(format(x, "%Y-%m-%d %H:%M:%S"))
      }
      
      if (inherits(x, "Date")){
        return(format(as.POSIXct(x, tz = tz), "%Y-%m-%d %H:%M:%S"))
      }
      x_chr <- trimws(as.character(x))
      out <- rep(NA_character_, length(x_chr))
      
      suppressWarnings(x_num <- as.numeric(x_chr))
      
      is_numeric_datetime <- !is.na(x_num) & grepl("^\\d+(\\.\\d+)?$", x_chr)
      
      # Numeric Unix timestamp
      out[is_numeric_datetime] <- format(
        as.POSIXct(
          x_num[is_numeric_datetime],
          origin = "1970-01-01",
          tz = tz
        ),
        "%Y-%m-%d %H:%M:%S"
      )
      
      # Text datetime
      out[!is_numeric_datetime] <- format(
        as.POSIXct(
          x_chr[!is_numeric_datetime],
          format = "%Y-%m-%d %H:%M:%S",
          tz = tz
        ),
        "%Y-%m-%d %H:%M:%S"
      )
      
      return(out)
    }
    
    df.raw[[input$startdt]] <- parse_datetime_col(df.raw[[input$startdt]], tz = "UTC")
    df.raw[[input$enddt]] <- parse_datetime_col(df.raw[[input$enddt]], tz = "UTC")
    
    ###### DURATION ######

    df.raw[[input$duree_day]] <- format(as.POSIXct(as.numeric(df.raw[[input$duree_day]]), origin = "1970-01-01", tz = "UTC"), "%H:%M:%S")
    
    good.time <- sapply(df.raw[[input$duree_day]], function(x) {
      if (!is.na(x)) {
        hms <- as.numeric(strsplit(x, ":")[[1]])
        hms[1] * 3600 + hms[2] * 60 + hms[3]
      } else {
        0
      }
    })

    format_duree <- function(total_second) {

      if (is.na(total_second) || total_second == 0) {
        return("0:00:00")
      }

      h <- floor(total_second / 3600)
      m <- floor((total_second %% 3600) / 60)
      s <- floor(total_second %% 60)

      sprintf("%d:%02d:%02d", h, m, s)
    }
    
    date.only <- format(as.Date(df.raw[[input$startdt]]), "%Y-%m-%d")
    
    df.treat <- df.raw %>% mutate(Date_rcd = date.only,
                              Good_time = good.time,
                              Duree_hms = sapply(good.time, format_duree),
                              Time_zone = floor(df.raw[[input$hour_day]]))
    
    
    rename_dict <- setNames(
      c("Farm_name", "GF_unit", "Start_time", "End_time", "Duration", "Hours_day", "CO2_gd", "CH4_gd", "Date_rcd", "Duration_sec", "Time_zone"),
      c(input$animal, input$machine, input$startdt, input$enddt, "Duree_hms", input$hour_day, input$CO2, input$CH4, "Date_rcd", "Good_time", "Time_zone")
    )
    
    optional_renames <- c()
    
    if ("H2" %in% selected_vars && !is.null(input$H2)) optional_renames[[input$H2]] <- "H2_gd"
    if ("O2" %in% selected_vars && !is.null(input$O2)) optional_renames[[input$O2]] <- "O2_gd"
    if ("Airflow" %in% selected_vars && !is.null(input$airflow_col)) optional_renames[[input$airflow_col]] <- "Airflow"
    
    full_rename_dict <- c(rename_dict, optional_renames)
    
    common_colnames <- intersect(names(full_rename_dict), colnames(df.treat))
    colnames(df.treat)[match(common_colnames, colnames(df.treat))] <- full_rename_dict[common_colnames]
    
    keep_cols <- c("Farm_name", "GF_unit", "Start_time", "End_time", "Duration", "Duration_sec", "Hours_day", "CO2_gd", "CH4_gd", "H2_gd", "O2_gd", "Airflow", 
                   "Date_rcd", "Time_zone")
    
    keep_cols <- unique(keep_cols[keep_cols %in% colnames(df.treat)])
    df.treat <- df.treat[, keep_cols, drop = FALSE]

    # Caching cleaned dataset for follow procedure
    cached_raw_data <<- df.treat
    raw_data_rv(df.treat)
    
  })
  
  raw_data <- reactive({
    req(raw_data_rv())
    raw_data_rv()
  })
  
  output$step2_ready_text <- renderUI({
    req(raw_data())
    tags$span(
      style = "color:black; font-style:italic; font-weight:600;",
      "LET'S GO TO STEP2 !!"
    )
  })
  
  ########################################################################################################################
  # Treatment datafile manipulate #  

  sheets_trt <- reactive({
    inFile <- if (isTRUE(input$same_file)) input$file else input$file_trt
    req(inFile)
    
    if (grepl("\\.xlsx$|\\.xlsm$|\\.xltm$", inFile$name, ignore.case = TRUE)) {
      excel_sheets(inFile$datapath)
    } else if (grepl("\\.csv$", inFile$name, ignore.case = TRUE)) {
      "CSV"
    } else {
      NULL
    }
  })

  output$sheet_ui_trt <- renderUI({
    req(sheets_trt())
    selectInput("sheet_trt", "Select the sheet with factor info", choices = sheets_trt())
  })

  data_trt <- reactive({
    req(input$sheet_trt)
    inFile <- if (input$same_file) input$file else input$file_trt
    req(inFile)

    if (grepl("\\.xlsx$|\\.xlsm$|\\.xltm$", inFile$name)){
      read_excel(inFile$datapath, sheet = input$sheet_trt)
    } else if (grepl("\\.csv$", inFile$name, ignore.case = TRUE)){
      read.csv(inFile$datapath)
    } else {
      NULL
    }
  })

  # select target column for treatment file
  output$column_ui_trt <- renderUI({
    req(data_trt())
    cols_trt <- colnames(data_trt())
    
    n_treat_default <- 1
    
    tagList(
      selectInput("animal_trt", "Select Animal ID column (It should be same as GF file)", choices = cols_trt),
      uiOutput("trt_alert_msg"),
      
      numericInput("n_treat", "Number of factor columns (max=5)", value = n_treat_default, min = 0, max = 5, step = 1),
      
      uiOutput("dynamic_treat_ui"),
    )
  })
  
  
  observe({
    choices_dist <- c("Entire")
    choices_kinetic <- c("Whole", "Animal")
    
    if (isTruthy(input$treat1)){
      choices_dist <- c(choices_dist, "Factor1")
      choices_kinetic <- c(choices_kinetic, "Factor1")
    }
    
    if (isTruthy(input$treat2)){
      choices_dist <- c(choices_dist, "Factor2")
      choices_kinetic <- c(choices_kinetic, "Factor2")
    }
    
    # distribution  
    sel_dist <- input$ch4_dist_group
    
    if (isTruthy(sel_dist) && (sel_dist %in% choices_dist)){
      updateSelectInput(session, "ch4_dist_group", choices = choices_dist, selected = sel_dist)  
    }
    
    # Kinetic_visit
    sel_visit <- input$visit_group
    if (isTruthy(sel_visit) && (sel_visit %in% choices_kinetic)){
      updateSelectInput(session, "visit_group", choices = choices_kinetic, selected = sel_visit)  
    }
    
    # Kinetic_ch4
    sel_ch4 <- input$ch4_group
    if (isTruthy(sel_ch4) && (sel_ch4 %in% choices_kinetic)){
      updateSelectInput(session, "ch4_group", choices = choices_kinetic, selected = sel_ch4)  
    }
    
    
  })
  
  output$dynamic_treat_ui <- renderUI({
    req(data_trt(), input$n_treat)
    cols_trt <- colnames(data_trt())
    
    treat_inputs <- lapply(seq_len(input$n_treat), function(i){
      selectInput(
        inputId = paste0("treat", i),
        label = paste("Select Factor", i),
        choices = c("None" = "", as.character(cols_trt)),
        selected = ""
      )
    })
    tagList(treat_inputs)
  })
  

  # Treatment list create
  treat_factor_names <- reactive({
    # req(input$treat_info, input$n_treat)
    
    if(!isTRUE(input$treat_info) || is.null(input$n_treat) || input$n_treat == 0){
      return(character(0))
    }
    
    trt_factors <- sapply(seq_len(input$n_treat), function(i){
      input[[paste0("treat", i)]]
    }, USE.NAMES = FALSE)
    
    trt_factors <- trt_factors[!is.null(trt_factors) & trt_factors != ""]
    unique(trt_factors)
  })
  

  ########################################################################################################################
  # MERGING GF DATA & TREATMENT INFO #
  ########################################################################################################################
  
  # Matching animal ID and treatments
    
  selected_treatments <- reactive({
    if(!isTRUE(input$treat_info) || is.null(input$n_treat) || input$n_treat == 0){
      return(character(0))
    } 
      vals <- sapply(seq_len(input$n_treat), function(i) input[[paste0("treat", i)]])
      vals <- vals[!is.na(vals) & nzchar(vals)]
      unique(vals)
  })
  
  n_selected_treat <- reactive({
    length(selected_treatments())
  })
  
  # Descriptive statistics UI
  
  output$whole_analysis_tabs <- renderUI({
    n_trt <- n_selected_treat()
    
    tab_list <- list(
      tabPanel("Per Animal",
               br(),
               actionButton("calc_whole_anim", "Calculate (Animal)", icon = icon("play")),
               downloadButton("download_whole_anim", "Download (Animal)", disabled = TRUE))
    )
    
    if (n_trt >= 1) {
      tab_list <- append(tab_list, list(
        tabPanel("Per Factor1", br(),
                 actionButton("calc_whole_trt1", "Calculate (Factor1)", icon = icon("play")),
                 downloadButton("download_whole_trt1", "Download (Factor1)", disabled = TRUE))
      ))
    }
    
    if (n_trt >= 2) {
      tab_list <- append(tab_list, list(
        tabPanel("Per Factor2", br(),
                 actionButton("calc_whole_trt2", "Calculate (Factor2)", icon = icon("play")),
                 downloadButton("download_whole_trt2", "Download (Factor2)", disabled = TRUE)),
        tabPanel("Per Factor1 X Factor2", br(),
                 actionButton("calc_whole_trt1xtrt2", "Calculate (Factor1 X Factor2)", icon = icon("play")),
                 downloadButton("download_whole_trt1xtrt2", "Download (Factor1 X Factor2)", disabled = TRUE))
      ))
    }
    
    do.call(tabsetPanel, tab_list)
  })
  
  
  output$daily_analysis_tabs <- renderUI({
    n_trt <- n_selected_treat()
    
    tab_list <- list(
      tabPanel("Entire dataset",
               br(),
               actionButton("calc_daily_entire", "Calculate (Entire)", icon = icon("play")),
               downloadButton("download_daily_entire", "Download (Entire)", disabled = TRUE)),
      
      tabPanel("Per Animal",
               br(),
               actionButton("calc_daily_anim", "Calculate (Animal)", icon = icon("play")),
               downloadButton("download_daily_anim", "Download (Animal)", disabled = TRUE))
    )
    
    if (n_trt >= 1) {
      tab_list <- append(tab_list, list(
        tabPanel("Per Factor1", br(),
                 actionButton("calc_daily_trt1", "Calculate (Factor1)", icon = icon("play")),
                 downloadButton("download_daily_trt1", "Download (Factor1)", disabled = TRUE))
      ))
    }
    
    if (n_trt >= 2) {
      tab_list <- append(tab_list, list(
        tabPanel("Per Factor2", br(),
                 actionButton("calc_daily_trt2", "Calculate (Factor2)", icon = icon("play")),
                 downloadButton("download_daily_trt2", "Download (Factor2)", disabled = TRUE)),
        
        tabPanel("Per Factor1 X Factor2", br(),
                 actionButton("calc_daily_trt1xtrt2", "Calculate (Factor1 X Factor2)", icon = icon("play")),
                 downloadButton("download_daily_trt1xtrt2", "Download (Factor1 X Factor2)", disabled = TRUE))
      ))
    }
    
    do.call(tabsetPanel, tab_list)
  })
  
  
  output$hourly_analysis_tabs <- renderUI({
    n_trt <- n_selected_treat()
    
    tab_list <- list(
      tabPanel("Entire dataset", br(),
               actionButton("calc_hourly_entire", "Calculate (Entire)", icon = icon("play")),
               downloadButton("download_hourly_entire", "Download (Entire)", disabled = TRUE)),
      
      tabPanel("Per Animal", br(),
               actionButton("calc_hourly_anim", "Calculate (Animal)", icon = icon("play")),
               downloadButton("download_hourly_anim", "Download (Animal)", disabled = TRUE))
    )
    
    if (n_trt >= 1) {
      tab_list <- append(tab_list, list(
        tabPanel("Per Factor1", br(),
                 actionButton("calc_hourly_trt1", "Calculate (Factor1)", icon = icon("play")),
                 downloadButton("download_hourly_trt1", "Download (Factor1)", disabled = TRUE))
      ))
    }
    
    if (n_trt >= 2) {
      tab_list <- append(tab_list, list(
        tabPanel("Per Factor2", br(),
                 actionButton("calc_hourly_trt2", "Calculate (Factor2)", icon = icon("play")),
                 downloadButton("download_hourly_trt2", "Download (Factor2)", disabled = TRUE)),
        tabPanel("Per Factor1 X Factor2", br(),
                 actionButton("calc_hourly_trt1xtrt2", "Calculate (Factor1 X Factor2)", icon = icon("play")),
                 downloadButton("download_hourly_trt1xtrt2", "Download (Factor1 X Factor2)", disabled = TRUE))
      ))
    }
    
    do.call(tabsetPanel, tab_list)
  })
  
  
  output$dhly_analysis_tabs <- renderUI({
    n_trt <- n_selected_treat()
    
    tab_list <- list(
      tabPanel("Entire dataset", br(),
               actionButton("calc_dhly_entire", "Calculate (Entire)", icon = icon("play")),
               downloadButton("download_dhly_entire", "Download (Entire)", disabled = TRUE)),
      
      tabPanel("Per Animal", br(),
               actionButton("calc_dhly_anim", "Calculate (Animal)", icon = icon("play")),
               downloadButton("download_dhly_anim", "Download (Animal)", disabled = TRUE))
    )
    
    if (n_trt >= 1) {
      tab_list <- append(tab_list, list(
        tabPanel("Per Factor1", br(),
                 actionButton("calc_dhly_trt1", "Calculate (Factor1)", icon = icon("play")),
                 downloadButton("download_dhly_trt1", "Download (Factor1)", disabled = TRUE))
      ))
    }
    
    if (n_trt >= 2) {
      tab_list <- append(tab_list, list(
        tabPanel("Per Factor2", br(),
                 actionButton("calc_dhly_trt2", "Calculate (Factor2)", icon = icon("play")),
                 downloadButton("download_dhly_trt2", "Download (Factor2)", disabled = TRUE)),
        tabPanel("Per Factor1 X Factor2", br(),
                 actionButton("calc_dhly_trt1xtrt2", "Calculate (Factor1 X Factor2)", icon = icon("play")),
                 downloadButton("download_dhly_trt1xtrt2", "Download (Factor1 X Factor2)", disabled = TRUE))
      ))
    }
    
    do.call(tabsetPanel, tab_list)
  })
  
  output$rep_analysis_tabs <- renderUI({
    n_trt <- n_selected_treat()
    
    tab_list <- list(
      tabPanel("Entire dataset", br(),
               actionButton("calc_rep_entire", "Calculate (Entire)", icon = icon("play")),
               downloadButton("download_rep_entire", "Download (Entire)", disabled = TRUE)),
      
      tabPanel("Per Animal", br(),
               actionButton("calc_rep_anim", "Calculate (Animal)", icon = icon("play")),
               downloadButton("download_rep_anim", "Download (Animal)", disabled = TRUE))
    )
    
    if (n_trt >= 1) {
      tab_list <- append(tab_list, list(
        tabPanel("Per Factor1", br(),
                 actionButton("calc_rep_trt1", "Calculate (Factor1)", icon = icon("play")),
                 downloadButton("download_rep_trt1", "Download (Factor1)", disabled = TRUE))
      ))
    }
    
    if (n_trt >= 2) {
      tab_list <- append(tab_list, list(
        tabPanel("Per Factor2", br(),
                 actionButton("calc_rep_trt2", "Calculate (Factor2)", icon = icon("play")),
                 downloadButton("download_rep_trt2", "Download (Factor2)", disabled = TRUE)),
        tabPanel("Per Factor1 X Factor2", br(),
                 actionButton("calc_rep_trt1xtrt2", "Calculate (Factor1 X Factor2)", icon = icon("play")),
                 downloadButton("download_rep_trt1xtrt2", "Download (Factor1 X Factor2)", disabled = TRUE))
      ))
    }
    
    do.call(tabsetPanel, tab_list)
  })
  
  
  visit_trt_data_rv <- reactiveVal(NULL)
  
  observeEvent(input$Done_trt,{
    req(raw_data()) # Generate visit db as NA even if there is no trt input
    
    print("== Merge factor & Animal process started ==")
    
    withProgress(message = "Merge dataset...", value = 0,{
      incProgress(0.2, detail = "Data reading...")
      
      df.raw <- raw_data()
      df.raw$Farm_name <- as.character(df.raw$Farm_name)
      
      treatment_cols <- selected_treatments()
      
      treatment_active <- length(treatment_cols) > 0 &&
        !is.null(input$animal_trt) && nzchar(input$animal_trt)
      
      incProgress(0.5, detail = "Data merging...")

      if (treatment_active){
        req(data_trt())
        
        animal_ids_clean <- unique(df.raw$Farm_name)
        
        trt_data <- data_trt() %>% 
          select(all_of(c(input$animal_trt, treatment_cols))) %>%
          rename(Animal_ID = all_of(input$animal_trt)) %>%
          mutate(Animal_ID = as.character(Animal_ID)) %>% 
          mutate(across(all_of(treatment_cols), as.factor)) %>% 
          filter(Animal_ID %in% animal_ids_clean)
        
        # Check duplicate
        duplicated_animals <- trt_data$Animal_ID[duplicated(trt_data$Animal_ID)]
        
        if (length(duplicated_animals) > 0){
          trt_dup_alert(
            paste0("WARNING: THERE IS DUPLICATED ANIMAL ID: ",
                   paste(unique(duplicated_animals), collapse = ", "))
          )
          return(NULL)
        } else{
          trt_dup_alert(NULL)
        }
        
        df.raw <- df.raw %>% filter(Farm_name %in% trt_data$Animal_ID)
        
        trt_data <- as.data.frame(trt_data)
        
        # Merge based on "Animal_ID"
        df.trt <- df.raw %>% left_join(trt_data, by = c("Farm_name" = "Animal_ID"))
        
        incProgress(0.8, detail = "Data ready...")
        
        if (nrow(df.trt) > nrow(df.raw)){
          warning(sprintf("Join increased rows: %d -> %d", nrow(df.raw), nrow(df.trt)))
        }
        
        # print(head(df.trt))
        # return(df.trt)
        visit_trt_data_rv(df.trt)
        
      } else{
        trt_dup_alert(NULL)
        # return(df.raw)
        incProgress(0.8, detail = "Data ready...")
        visit_trt_data_rv(df.raw)
        
      }
      
    })
  })
  
  visit_trt_data <- reactive({
    req(visit_trt_data_rv())
    visit_trt_data_rv()
  })
  
  output$trt_alert_msg <- renderUI({
    msg <- trt_dup_alert()
    if (!is.null(msg)) {
      div(style = "color: red; font-weight: bold; margin-bottom: 10px;", msg)
    }
  })
  
  output$step3_ready_text <- renderUI({
    req(visit_trt_data())
    tags$span(
      # style = "margin-left:10px; font-weight:600; color:black; font-style:italic;",
      style = "color:black; font-style:italic; font-weight:600;",
      "LET'S GO TO STEP3 !!"
    )
  })
  
  outputOptions(output, "step3_ready_text", suspendWhenHidden = FALSE)
  
  ########################################################################################################################
  # 3. DATA SCREENING #
  ########################################################################################################################

  output$machine_scr_ui <- renderUI({
    req(visit_trt_data(), input$machine)
    req(input$machine != "")
    
    r.db <- tryCatch({ visit_trt_data() }, error = function(e) NULL)
    
    if (is.null(r.db) || nrow(r.db) == 0){
      return(pickerInput(
        inputId = "machine_scr",
        label = "Include GF units (default: All)",
        choices = NULL,
        multiple = TRUE,
        options = list(
          'action-box' = TRUE,
          'live-search' = TRUE,
          'selected-text-format' = "count > 3"
        )
      ))
    }
    
    GF_list <- sort(unique(as.character(r.db$GF_unit)))
    
    pickerInput(
      inputId = "machine_scr",
      label = "Include GF units (default: All)",
      choices = c("<All>" = "All", GF_list),
      selected = GF_list,
      multiple = TRUE,
      options = list(
        'actions-box' = TRUE,
        'live-search' = TRUE,
        'selected-text-format' = "count > 3"
      )
    )
  })
  
  output$dynamic_treat_scr_ui <- renderUI({
    req(input$treat_info)
    req(visit_trt_data())
    
    r.db <- tryCatch({ visit_trt_data() }, error = function(e) NULL)
    req(r.db)
    
    treatment_cols <- selected_treatments()
    req(length(treatment_cols) > 0)
    
    valid_treatments <- treatment_cols[treatment_cols %in% colnames(r.db)]
    req(length(valid_treatments) > 0)
    
    tagList(
      lapply(seq_along(valid_treatments), function(i){
        trt_col <- valid_treatments[i]
        
        trt_list <- sort(unique(as.character(r.db[[trt_col]])))
        trt_list <- trt_list[!is.na(trt_list) & nzchar(trt_list)]
        
        pickerInput(
          inputId = paste0("treat", i, "_scr"),
          label = paste("Select ", trt_col, " (default: All)"),
          choices = trt_list,
          selected = trt_list,
          multiple = TRUE,
          options = list(
            'actions-box' = TRUE,
            'live-search' = TRUE,
            'selected-text-format' = "count > 3"
          )
        )   
      })
    )
  })
  
 
  output$animal_scr_ui <- renderUI({
    req(visit_trt_data())
    
    r.db <- tryCatch({ visit_trt_data() }, error = function(e) NULL)
    
    if (is.null(r.db) || nrow(r.db) == 0) {
      return(
        pickerInput(
          inputId = "animal_scr",
          label = "Include animals (default: All)",
          choices = NULL,
          multiple = TRUE,
          options = list(
            'actions-box' = TRUE,
            'live-search' = TRUE,
            'selected-text-format' = "count > 3"
          )
        )
      )
    }
    
    anim_list <- sort(unique(as.character(r.db$Farm_name)))
    
    pickerInput(
      inputId = "animal_scr",
      label = "Include animals (default: All)",
      choices = c("<All>" = "All", anim_list),
      selected = anim_list,
      multiple = TRUE,
      options = list(
        'actions-box' = TRUE,
        'live-search' = TRUE,
        'selected-text-format' = "count > 3"
      )
    )
  })
  
  output$date_scr_ui <- renderUI({
    req(visit_trt_data())
    
    r.db <- tryCatch({ visit_trt_data() }, error = function(e) NULL)
    
    if (is.null(r.db) || nrow(r.db) == 0 || !("Date_rcd" %in% colnames(r.db))) {
      return(
        dateRangeInput(
          inputId = "date_scr_range",
          label = "Select date range:",
          start = NULL, end = NULL,
          min = NULL, max = NULL
        )
      )
    }
    
    dates_rdb <- sort(unique(as.Date(r.db$Date_rcd)))
    validate(need(length(dates_rdb) > 0, "No dates found."))
    
    dateRangeInput(
      inputId = "date_scr_range",
      label = "Select date range:",
      start = dates_rdb[1],
      end = dates_rdb[length(dates_rdb)],
      min = dates_rdb[1],
      max = dates_rdb[length(dates_rdb)]
    )
  })
  
  # Machine & Treatment & Animal & Date list filtering
  
  scr_data_rv <- reactiveVal(NULL)
  
  observeEvent(input$data_scr_done,{
    req(visit_trt_data())
    
    print("== 3.Data screening start ==")
    
    r.db <- tryCatch({ visit_trt_data() }, error = function(e) NULL)
    req(!is.null(r.db), nrow(r.db) > 0)
    
    df <- r.db
    
    # Machine
    if (!is.null(input$machine_scr) && length(input$machine_scr) > 0){
      if(!("All" %in% input$machine_scr)){
        df <- df[df$GF_unit %in% input$machine_scr, , drop = FALSE]
      }
    }
    
    # Treatment
    if (isTRUE(input$treat_info)) {
      treatment_cols <- selected_treatments()
      
      if (length(treatment_cols) > 0) {
        for (i in seq_along(treatment_cols)) {
          trt_col <- treatment_cols[i]
          trt_scr_input <- input[[paste0("treat", i, "_scr")]]
          
          if (!is.null(trt_col) &&
              nzchar(trt_col) &&
              trt_col %in% colnames(df) &&
              !is.null(trt_scr_input) &&
              length(trt_scr_input) > 0) {
            
            if (!("All" %in% trt_scr_input)) {
              df <- df[as.character(df[[trt_col]]) %in% trt_scr_input, , drop = FALSE]
            }
          }
        }
      }
    }

    # Animal list
    if (!is.null(input$animal_scr) && length(input$animal_scr) > 0){
      if(!("All" %in% input$animal_scr)){
        df <- df[df$Farm_name %in% input$animal_scr, , drop = FALSE]
      }
    }
    
    # Date range
    if (!is.null(input$date_scr_range) && length(input$date_scr_range) == 2 &&
        "Date_rcd" %in% colnames(df)){
      date_vec <- as.Date(df$Date_rcd)
      
      df <- df[
        !is.na(date_vec) &
          date_vec >= as.Date(input$date_scr_range[1]) &
          date_vec <= as.Date(input$date_scr_range[2]), , drop = FALSE
      ]
    }
    
    scr_data_rv(df)
    # print(head(df))
  })
  
  scr_data <- reactive({
    req(scr_data_rv())
    scr_data_rv()
  })
  
  output$step4_ready_text <- renderUI({
    req(scr_data_rv())
    tags$span(
      style = "color:black; font-style:italic; font-weight:600;",
      "LET'S GO TO STEP4!!"
    )
  })

  ########################################################################################################################
  # 4. OUTLIER ELIMINATE #
  ########################################################################################################################
  
  cleaning_rate <- reactiveVal(NULL)
  cleaning_skip <- reactiveVal(NULL)
  
  ##################################### CLEANING #####################################
  `%||%` <- function(a, b) if (!is.null(a)) a else b
  
  add_reason <- function(x, idx, label){
    if (length(idx) == 0) return(x)
    x[idx] <- ifelse(is.na(x[idx]), label, paste(x[idx], label, sep = ","))
    x
  }
  
  has_o2h2 <- reactive({
    available <- input$available_data %||% character(0)
    any(c("O2", "H2") %in% input$available_data)
  })
  
  treatment_cols_available <- reactive({
    req(scr_data())
    cols <- selected_treatments() %||% character(0)
    cols[cols %in% names(scr_data())]
  })
  
  output$ch4co2_split_vars_ui <- renderUI({
    req(treatment_cols_available())
    selectizeInput(
      "ch4co2_split_vars",
      "Split cleaning by factor column(s)",
      choices = treatment_cols_available(),
      selected = treatment_cols_available()[1],
      multiple = TRUE,
      options = list(
        placeholder = "Select factor column(s)"
      )
    )
  })
  
  
  output$o2_split_vars_ui <- renderUI({
    req(treatment_cols_available())
    selectizeInput(
      "o2_split_vars",
      "Split O2 cleaning by factor column(s)",
      choices = treatment_cols_available(),
      selected = if (length(treatment_cols_available()) > 0) treatment_cols_available()[1] else NULL,
      multiple = TRUE,
      options = list(
        placeholder = "Select factor column(s)"
      )
    )
  })
  
  output$h2_split_vars_ui <- renderUI({
    req(treatment_cols_available())
    selectizeInput(
      "h2_split_vars",
      "Split H2 cleaning by factor column(s)",
      choices = treatment_cols_available(),
      selected = if (length(treatment_cols_available()) > 0) treatment_cols_available()[1] else NULL,
      multiple = TRUE,
      options = list(
        placeholder = "Select factor column(s)"
      )
    )
  })
  
  outputOptions(output, "ch4co2_split_vars_ui", suspendWhenHidden = FALSE)
  outputOptions(output, "o2_split_vars_ui", suspendWhenHidden = FALSE)
  outputOptions(output, "h2_split_vars_ui", suspendWhenHidden = FALSE)
  
  
  valid_split_vars <- function(df, vars){
    vars <- vars %||% character(0)
    vars <- vars[vars %in% names(df)]
    vars
  }
  
  sanitize_gas_values <- function(df){
    gas_cols <- intersect(c("CH4_gd", "CO2_gd", "O2_gd", "H2_gd"), names(df))
    
    for (col in gas_cols) {
      df[[col]][df[[col]] == -999] <- NA
      df[[col]][df[[col]] < 0] <- NA
    }
    
    df
  }
  
  safe_wilcox <- function(x, y){
    x <- x[is.finite(x)]
    y <- y[is.finite(y)]
    
    if (length(x) < 2 || length(y) < 2) {
      return(list(
        p.value = NA_real_,
        median_diff = NA_real_,
        effect_r = NA_real_
      ))
    }
    
    wt <- tryCatch(
      suppressWarnings(wilcox.test(x, y, exact = FALSE)),
      error = function(e) NULL
    )
    
    pval <- if (is.null(wt)) NA_real_ else wt$p.value
    med_diff <- median(x, na.rm = TRUE) - median(y, na.rm = TRUE)
    
    pooled_sd <- sd(c(x, y), na.rm = TRUE)
    eff <- if (is.na(pooled_sd) || pooled_sd == 0) NA_real_ else med_diff / pooled_sd
    
    list(
      p.value = pval,
      median_diff = med_diff,
      effect_r = eff
    )
  }
  
  format_p_value <- function(p){
    if (is.na(p)) return(NA_character_)
    if (p < 2.22e-16) return("< 2.22e-16")
    formatC(p, format = "e", digits = 3)
  }
  
  
  get_sd_outlier_idx_all <- function(df, sd_multiplier){
    if (nrow(df) <= 1 || !all(c("CH4_gd", "CO2_gd") %in% names(df))) return(integer(0))
    
    ch4_mean <- mean(df$CH4_gd, na.rm = TRUE)
    ch4_sd   <- sd(df$CH4_gd, na.rm = TRUE)
    co2_mean <- mean(df$CO2_gd, na.rm = TRUE)
    co2_sd   <- sd(df$CO2_gd, na.rm = TRUE)
    
    if (is.na(ch4_sd) || is.na(co2_sd) || ch4_sd == 0 || co2_sd == 0) return(integer(0))
    
    ch4_upper <- ch4_mean + sd_multiplier * ch4_sd
    ch4_lower <- ch4_mean - sd_multiplier * ch4_sd
    co2_upper <- co2_mean + sd_multiplier * co2_sd
    co2_lower <- co2_mean - sd_multiplier * co2_sd
    
    unique(c(
      which(df$CH4_gd > ch4_upper),
      which(df$CH4_gd < ch4_lower),
      which(df$CO2_gd > co2_upper),
      which(df$CO2_gd < co2_lower)
    ))
  }
  
  get_sd_outlier_idx_split <- function(df, sd_multiplier, split_vars = NULL){
    split_vars <- valid_split_vars(df, split_vars)
    
    df2 <- df %>% mutate(.row_id_tmp = seq_len(n()))
    
    if (length(split_vars) == 0) {
      return(get_sd_outlier_idx_all(df2, sd_multiplier))
    }
    
    grp_list <- df2 %>%
      group_by(across(all_of(split_vars))) %>%
      group_split(.keep = TRUE)
    
    out_global <- lapply(grp_list, function(g){
      idx_local <- get_sd_outlier_idx_all(g, sd_multiplier)
      if (length(idx_local) == 0) return(integer(0))
      g$.row_id_tmp[idx_local]
    })
    
    sort(unique(unlist(out_global)))
  }
  
  
  apply_single_gas_sd <- function(df, gas_col, sd_multiplier){
    x <- df[[gas_col]]
    
    valid_pos <- which(!is.na(x) & x > 0)
    x_valid <- x[valid_pos]
    
    out_local <- which(!is.na(x) & x < 0)
    
    if (length(x_valid) > 1) {
      x_mean <- mean(x_valid, na.rm = TRUE)
      x_sd   <- sd(x_valid, na.rm = TRUE)
      
      if (!is.na(x_sd) && x_sd > 0) {
        upper <- x_mean + sd_multiplier * x_sd
        lower <- x_mean - sd_multiplier * x_sd
        
        out_local <- unique(c(
          out_local,
          valid_pos[which(x_valid > upper)],
          valid_pos[which(x_valid < lower)]
        ))
      }
    }
    
    out_local
  }
  
  apply_single_gas_sd_split <- function(df, gas_col, sd_multiplier, split_vars = NULL){
    split_vars <- valid_split_vars(df, split_vars)
    
    df2 <- df %>% mutate(.row_id_tmp = seq_len(n()))
    
    if (length(split_vars) == 0) {
      idx_local <- apply_single_gas_sd(df2, gas_col, sd_multiplier)
      return(list(
        row_ids = idx_local,
        df_out = {
          out <- df2
          if (length(idx_local) > 0) out[[gas_col]][idx_local] <- NA
          out
        }
      ))
    }
    
    grp_list <- df2 %>%
      group_by(across(all_of(split_vars))) %>%
      group_split(.keep = TRUE)
    
    grp_res <- lapply(grp_list, function(g){
      idx_local <- apply_single_gas_sd(g, gas_col, sd_multiplier)
      if (length(idx_local) > 0) {
        g[[gas_col]][idx_local] <- NA
      }
      list(
        row_ids = g$.row_id_tmp[idx_local],
        df = g
      )
    })
    
    df_out <- bind_rows(lapply(grp_res, `[[`, "df")) %>%
      arrange(.row_id_tmp)
    
    row_ids <- sort(unique(unlist(lapply(grp_res, `[[`, "row_ids"))))
    
    list(
      row_ids = row_ids,
      df_out = df_out
    )
  }
  
  apply_single_gas_iqr <- function(df, gas_col, iqr_multiplier = 1.5){
    x <- df[[gas_col]]
    
    valid_pos <- which(!is.na(x) & is.finite(x) & x > 0)
    x_valid <- x[valid_pos]
    
    out_local <- which(!is.na(x) & !is.finite(x))
    
    if (length(x_valid) > 1) {
      q1 <- quantile(x_valid, 0.25, na.rm = TRUE, type = 7)
      q3 <- quantile(x_valid, 0.75, na.rm = TRUE, type = 7)
      iqr_val <- q3 - q1
      
      if (!is.na(iqr_val) && iqr_val > 0) {
        lower <- q1 - iqr_multiplier * iqr_val
        upper <- q3 + iqr_multiplier * iqr_val
        
        out_local <- unique(c(
          out_local,
          valid_pos[which(x_valid < lower)],
          valid_pos[which(x_valid > upper)]
        ))
      }
    }
    
    out_local
  }
  
  apply_single_gas_iqr_split <- function(df, gas_col, iqr_multiplier = 1.5, split_vars = NULL){
    split_vars <- valid_split_vars(df, split_vars)
    
    df2 <- df %>% mutate(.row_id_tmp = seq_len(n()))
    
    if (length(split_vars) == 0) {
      idx_local <- apply_single_gas_iqr(df2, gas_col, iqr_multiplier)
      return(list(
        row_ids = idx_local,
        df_out = {
          out <- df2
          if (length(idx_local) > 0) out[[gas_col]][idx_local] <- NA
          out
        }
      ))
    }
    
    grp_list <- df2 %>%
      group_by(across(all_of(split_vars))) %>%
      group_split(.keep = TRUE)
    
    grp_res <- lapply(grp_list, function(g){
      idx_local <- apply_single_gas_iqr(g, gas_col, iqr_multiplier)
      if (length(idx_local) > 0) {
        g[[gas_col]][idx_local] <- NA
      }
      list(
        row_ids = g$.row_id_tmp[idx_local],
        df = g
      )
    })
    
    df_out <- bind_rows(lapply(grp_res, `[[`, "df")) %>%
      arrange(.row_id_tmp)
    
    row_ids <- sort(unique(unlist(lapply(grp_res, `[[`, "row_ids"))))
    
    list(
      row_ids = row_ids,
      df_out = df_out
    )
  }
  
  show_gas_distribution_modal <- function(gas_name, gas_col, data_reactive, treatment_choices){
    dat <- data_reactive()
    treat_cols <- treatment_choices() %||% character(0)
    
    req(gas_col %in% names(dat))
    
    prefix <- tolower(gas_name)
    
    has_treatment <- length(treat_cols) > 0
    default_var <- if (has_treatment) treat_cols[1] else NULL
    default_levels <- if (has_treatment) {
      sort(unique(as.character(dat[[default_var]])))
    } else {
      character(0)
    }
    
    showModal(
      modalDialog(
        title = tagList(icon("chart-bar"), paste(gas_name, "Distribution check")),
        size = "l",
        easyClose = TRUE,
        footer = modalButton("Close"),
        
        fluidRow(
          column(
            if (has_treatment) 4 else 12,
            checkboxInput(
              paste0(prefix, "_show_all"),
              paste("Show overall", gas_name, "distribution"),
              value = TRUE
            )
          ),
          
          if (has_treatment) {
            column(
              4,
              selectInput(
                paste0(prefix, "_modal_treatment_var"),
                "Factor column",
                choices = treat_cols,
                selected = default_var
              )
            )
          },
          
          if (has_treatment) {
            column(
              4,
              selectizeInput(
                paste0(prefix, "_modal_treatment_levels"),
                "Factor levels",
                choices = default_levels,
                selected = default_levels,
                multiple = TRUE,
                options = list(
                  placeholder = "Select factor levels"
                )
              )
            )
          }
        ),
        
        tags$hr(),
        plotOutput(paste0(prefix, "_density_plot"), height = "420px"),
        
        if (has_treatment) {
          tagList(
            br(),
            h4("Pairwise distinguish test among selected treatment levels"),
            uiOutput(paste0(prefix, "_distinguish_summary")),
            div(style = "overflow-x:auto;", tableOutput(paste0(prefix, "_pairwise_table")))
          )
        } else {
          div(
            style = "margin-top:12px; font-style:italic; color:#666;",
            "No factor file was uploaded, so only the overall distribution is available."
          )
        }
      )
    )
  }
  
  register_gas_outputs <- function(gas_name, gas_col, data_reactive){
    prefix <- tolower(gas_name)
    
    observeEvent(input[[paste0(prefix, "_modal_treatment_var")]], {
      req(data_reactive())
      req(input[[paste0(prefix, "_modal_treatment_var")]])
      
      dat <- data_reactive()
      treat_var <- input[[paste0(prefix, "_modal_treatment_var")]]
      
      req(treat_var %in% names(dat))
      
      lvls <- sort(unique(as.character(dat[[treat_var]])))
      lvls <- lvls[!is.na(lvls) & nzchar(lvls)]
      
      updateSelectizeInput(
        session,
        paste0(prefix, "_modal_treatment_levels"),
        choices = lvls,
        selected = lvls
      )
    }, ignoreInit = TRUE)
    
    observeEvent(input[[paste0(prefix, "_modal_treatment_levels")]], {
      cur <- input[[paste0(prefix, "_modal_treatment_levels")]] %||% character(0)
      if (length(cur) > 5) {
        updateSelectizeInput(
          session,
          paste0(prefix, "_modal_treatment_levels"),
          selected = cur[1:5]
        )
        showNotification("Factor levels can be selected up to 5.", type = "warning")
      }
    }, ignoreInit = TRUE)
    
    output[[paste0(prefix, "_density_plot")]] <- renderPlot({
      req(data_reactive())
      
      dat <- data_reactive() %>%
        filter(!is.na(.data[[gas_col]]))
      
      p <- ggplot()
      
      if (isTRUE(input[[paste0(prefix, "_show_all")]])) {
        p <- p +
          geom_density(
            data = dat,
            aes(x = .data[[gas_col]]),
            fill = "grey70",
            alpha = 0.25,
            color = "black",
            linewidth = 1
          )
      }
      
      treat_var <- input[[paste0(prefix, "_modal_treatment_var")]] %||% NULL
      sel_levels <- input[[paste0(prefix, "_modal_treatment_levels")]] %||% character(0)
      
      if (!is.null(treat_var) &&
          treat_var %in% names(dat) &&
          length(sel_levels) > 0) {
        
        dat2 <- dat %>%
          mutate(.grp = as.character(.data[[treat_var]])) %>%
          filter(.grp %in% sel_levels) %>%
          mutate(.grp = factor(.grp, levels = sel_levels))
        
        if (nrow(dat2) > 0) {
          p <- p +
            geom_density(
              data = dat2,
              aes(x = .data[[gas_col]], color = .grp, fill = .grp),
              alpha = 0.20,
              linewidth = 1
            )
        }
      }
      
      p +
        labs(
          x = gas_col,
          y = "Density",
          color = "Treatment",
          fill = "Treatment"
        ) +
        theme_bw(base_size = 13)
    })
    
    output[[paste0(prefix, "_pairwise_table")]] <- renderTable({
      req(data_reactive())
      
      dat <- data_reactive() %>%
        filter(!is.na(.data[[gas_col]]))
      
      treat_var <- input[[paste0(prefix, "_modal_treatment_var")]] %||% NULL
      sel_levels <- input[[paste0(prefix, "_modal_treatment_levels")]] %||% character(0)
      
      if (is.null(treat_var) || !(treat_var %in% names(dat)) || length(sel_levels) < 2) {
        return(NULL)
      }
      
      dat <- dat %>%
        mutate(.grp = as.character(.data[[treat_var]])) %>%
        filter(.grp %in% sel_levels)
      
      grp_levels <- unique(dat$.grp)
      if (length(grp_levels) < 2) {
        return(NULL)
      }
      
      combs <- combn(grp_levels, 2, simplify = FALSE)
      
      res <- lapply(combs, function(x){
        g1 <- dat %>% filter(.grp == x[1]) %>% pull(!!sym(gas_col))
        g2 <- dat %>% filter(.grp == x[2]) %>% pull(!!sym(gas_col))
        
        tst <- safe_wilcox(g1, g2)
        
        data.frame(
          Group1 = x[1],
          Group2 = x[2],
          N1 = length(g1),
          N2 = length(g2),
          Median1 = median(g1, na.rm = TRUE),
          Median2 = median(g2, na.rm = TRUE),
          Median_diff = tst$median_diff,
          Effect_r = tst$effect_r,
          P_value = format_p_value(tst$p.value),
          Significant = ifelse(
            !is.na(tst$p.value) && tst$p.value < 0.05 &&
              !is.na(tst$effect_r) && abs(tst$effect_r) >= 0.5,
            "Yes", "No"
          )
        )
      })
      
      bind_rows(res)
    })
    
    output[[paste0(prefix, "_distinguish_summary")]] <- renderUI({
      req(data_reactive())
      
      dat <- data_reactive() %>%
        filter(!is.na(.data[[gas_col]]))
      
      treat_var <- input[[paste0(prefix, "_modal_treatment_var")]] %||% NULL
      sel_levels <- input[[paste0(prefix, "_modal_treatment_levels")]] %||% character(0)
      
      if (is.null(treat_var) || !(treat_var %in% names(dat)) || length(sel_levels) < 2) {
        return(tags$span(style = "color:#666;", "Select at least 2 factor levels to run pairwise tests."))
      }
      
      dat <- dat %>%
        mutate(.grp = as.character(.data[[treat_var]])) %>%
        filter(.grp %in% sel_levels)
      
      grp_levels <- unique(dat$.grp)
      if (length(grp_levels) < 2) {
        return(tags$span(style = "color:#666;", "Not enough valid groups for comparison."))
      }
      
      combs <- combn(grp_levels, 2, simplify = FALSE)
      
      test_res <- lapply(combs, function(x){
        g1 <- dat %>% filter(.grp == x[1]) %>% pull(!!sym(gas_col))
        g2 <- dat %>% filter(.grp == x[2]) %>% pull(!!sym(gas_col))
        safe_wilcox(g1, g2)
      })
      
      sig_vec <- sapply(test_res, function(tst){
        !is.na(tst$p.value) &&
          tst$p.value < 0.05 &&
          !is.na(tst$effect_r) &&
          abs(tst$effect_r) >= 0.5
      })
      
      sig_n <- sum(sig_vec)
      total_n <- length(sig_vec)
      
      tags$span(
        style = "font-style:italic;",
        paste0("Distinguishable pair(s): ", sig_n, " / ", total_n)
      )
    })
  }
  
  run_stage1 <- function(df, input, selected_vars){
    df.treat <- df %>% mutate(Remove_reason = NA_character_,
                              H2_outlier = NA_character_,
                              O2_outlier = NA_character_)
    
    #######################
    # 1. AIRFLOW & DURATE #
    #######################
    if (input$airflow_outlier == 'Yes' && "Airflow" %in% selected_vars &&
        "Airflow" %in% names(df.treat) && !is.null(input$airflow_lim)){
      airflow_idx <- which(df.treat$Airflow < input$airflow_lim)
      df.treat$Remove_reason <- add_reason(df.treat$Remove_reason, airflow_idx, "Airflow")
    }
    
    if (input$duration_outlier == "Yes" && "Duration_sec" %in% names(df.treat) &&
        !is.null(input$duration_lim)){
      duration_idx <- which(df.treat$Duration_sec < input$duration_lim)
      df.treat$Remove_reason <- add_reason(df.treat$Remove_reason, duration_idx, "Duration")
    }
    
    idx_after_stage1 <- which(is.na(df.treat$Remove_reason))
    
    ###################
    # 2. VISIT NUMBER #
    ###################
    
    if(
      input$no_visit_outlier == "Yes" && !is.null(input$no_visit_lim) && "Farm_name" %in% names(df.treat)
    ){
      df.stage1 <- df.treat[idx_after_stage1, , drop = FALSE]
      
      visit.stat <- df.stage1 %>% group_by(Farm_name) %>% summarise(n_visit = n(), .groups = "drop")
      
      farm_remove <- visit.stat %>% filter(n_visit < input$no_visit_lim) %>% pull(Farm_name)
      
      if(length(farm_remove) > 0){
        visit_idx_local <- which(df.stage1$Farm_name %in% farm_remove)
        visit_idx_real <- idx_after_stage1[visit_idx_local]
        df.treat$Remove_reason <- add_reason(df.treat$Remove_reason, visit_idx_real, "No_visit")
      }
    }
    
    idx_after_stage2 <- which(is.na(df.treat$Remove_reason))
    df.clean <- df.treat[idx_after_stage2, , drop = FALSE]
    
    list(
      df_treat = df.treat,
      df_clean = df.clean
    )
  }
  
  # Calculate Coefficient correlation between CO2 & CH4
  
  ch4co2_corr_result <- eventReactive(input$ch4co2_r2_calc, {
    req(stage1_result())
    req(input$analysis_method %in% c("Arbre_2016", "Coppa_2021"))
    
    get_ch4co2_corr_summary(stage1_result()$df_clean)
  })
  
  output$ch4co2_corr_summary <- renderTable({
    req(ch4co2_corr_result())
    ch4co2_corr_result()
  })
  
  get_ch4co2_corr_summary <- function(df){
    if (!all(c("CH4_gd", "CO2_gd") %in% names(df))) {
      return(NULL)
    }
    
    dat <- df %>%
      filter(
        is.finite(CH4_gd),
        is.finite(CO2_gd)
      )
    
    if (nrow(dat) < 3) {
      return(data.frame(
        Items = c("n", "R", "R²", "P-value", "Significant"),
        Value = c(nrow(dat), NA, NA, NA, "No")
      ))
    }
    
    ct <- tryCatch(
      cor.test(dat$CH4_gd, dat$CO2_gd, method = "pearson"),
      error = function(e) NULL
    )
    
    if (is.null(ct)) {
      return(data.frame(Items = c("n", "R", "R²", "P-value", "Significant"),
                        Value = c(nrow(dat), NA, NA, NA, "No")
      ))
    }
    
    r_val <- unname(ct$estimate)
    
    data.frame(
      Items = c("n", "R", "R²", "P-value", "Significant"),
      Value = c(
        nrow(dat),
        round(r_val, 2),
        round(r_val^2, 2),
        format_p_value(ct$p.value),
        ifelse(!is.na(ct$p.value) && ct$p.value < 0.05, "Yes", "No")
      )
    )
  }
  
  run_stage2 <- function(stage1_res, input){
    df.treat <- stage1_res$df_treat
    df.base.clean <- stage1_res$df_clean
    
    method_outlier_idx <- numeric(0)
    idx_after_stage2 <- which(is.na(df.treat$Remove_reason))
    
    if(input$analysis_method != "No" && nrow(df.base.clean) > 1 && all(c("CH4_gd", "CO2_gd") %in% names(df.base.clean))
    ) {
      if (input$analysis_method == "Arbre_2016") {
        norm.CH4 <- (df.base.clean$CH4_gd - mean(df.base.clean$CH4_gd, na.rm = TRUE)) /
          sd(df.base.clean$CH4_gd, na.rm = TRUE)
        norm.CO2 <- (df.base.clean$CO2_gd - mean(df.base.clean$CO2_gd, na.rm = TRUE)) /
          sd(df.base.clean$CO2_gd, na.rm = TRUE)
        
        dist.gas <- abs(norm.CH4 - norm.CO2) / sqrt(2)
        cor.coeff.gas <- cor(norm.CH4, norm.CO2, use = "complete.obs")
        
        method_outlier_idx <- which(dist.gas > 3 * sqrt(1 - cor.coeff.gas)) 
      } else if(input$analysis_method == "Coppa_2021"){
        CH4.lm <- lm(CH4_gd ~ CO2_gd, data = df.base.clean)
        CO2.lm <- lm(CO2_gd ~ CH4_gd, data = df.base.clean)
        
        CH4.pred <- predict(CH4.lm, df.base.clean)
        CO2.pred <- predict(CO2.lm, df.base.clean)
        
        CH4.residue <- CH4.pred - df.base.clean$CH4_gd
        CO2.residue <- CO2.pred - df.base.clean$CO2_gd
        
        CH4.SD <- 3 * sd(CH4.residue, na.rm = TRUE)
        CO2.SD <- 3 * sd(CO2.residue, na.rm = TRUE)
        
        CH4.outlier <- which(abs(CH4.residue) > CH4.SD)
        CO2.outlier <- which(abs(CO2.residue) > CO2.SD)
        
        method_outlier_idx <- union(CH4.outlier, CO2.outlier)
      } else if (input$analysis_method == "Standard_deviation") {
        req(input$sd_multiplier)
        
        sd_multiplier <- input$sd_multiplier
        
        split_vars <- NULL
        if (identical(input$ch4co2_split_cleaning, "Yes")) {
          split_vars <- input$ch4co2_split_vars
        }
        
        method_outlier_idx <- get_sd_outlier_idx_split(
          df = df.base.clean,
          sd_multiplier = sd_multiplier,
          split_vars = split_vars
        )
      }
    }
    
    if (length(method_outlier_idx) > 0) {
      method_real_idx <- idx_after_stage2[method_outlier_idx]
      df.treat$Remove_reason <- add_reason(
        df.treat$Remove_reason,
        method_real_idx,
        input$analysis_method
      )
    }
    
    idx_after_stage3 <- which(is.na(df.treat$Remove_reason))
    df.clean <- df.treat[idx_after_stage3, , drop = FALSE]
    
    list(
      df_treat = df.treat,
      df_clean = df.clean
    )
  }
  
  
  run_stage3 <- function(stage2_res, input, selected_vars){
    df.treat <- stage2_res$df_treat
    df.clean <- stage2_res$df_clean
    
    idx_after_stage3 <- which(is.na(df.treat$Remove_reason))
    
    ## O2 ##
    if (input$O2_outlier == "Yes" &&
        "O2" %in% selected_vars &&
        "O2_gd" %in% names(df.clean) &&
        !is.null(input$O2_lim)) {
      
      o2_split_vars <- NULL
      if (identical(input$o2_split_cleaning, "Yes")) {
        o2_split_vars <- input$o2_split_vars
      }
      
      o2_res <- apply_single_gas_sd_split(
        df = df.clean,
        gas_col = "O2_gd",
        sd_multiplier = input$O2_lim,
        split_vars = o2_split_vars
      )
      
      if (length(o2_res$row_ids) > 0) {
        O2.real.idx <- idx_after_stage3[o2_res$row_ids]
        df.treat$O2_outlier[O2.real.idx] <- "O2"
      }
      
      df.clean <- o2_res$df_out %>% select(-any_of(".row_id_tmp"))
    }
    
    ## H2 ##
    if (input$H2_outlier == "Yes" &&
        "H2" %in% selected_vars &&
        "H2_gd" %in% names(df.clean)) {
      
      h2_split_vars <- NULL
      if (identical(input$h2_split_cleaning, "Yes")) {
        h2_split_vars <- input$h2_split_vars
      }
      
      h2_iqr_multiplier <- input$H2_lim %||% 1.5
      
      h2_res <- apply_single_gas_iqr_split(
        df = df.clean,
        gas_col = "H2_gd",
        iqr_multiplier = h2_iqr_multiplier,
        split_vars = h2_split_vars
      )
      
      if (length(h2_res$row_ids) > 0) {
        H2.real.idx <- idx_after_stage3[h2_res$row_ids]
        df.treat$H2_outlier[H2.real.idx] <- "H2"
      }
      
      df.clean <- h2_res$df_out %>% select(-any_of(".row_id_tmp"))
    }
    
    list(
      df_treat = df.treat,
      df_clean = df.clean
    )
  }
  
  ##################################### RESULTS #####################################
  
  stage1_result <- eventReactive(input$`1st_cln`, {
    req(scr_data())
    
    df <- sanitize_gas_values(scr_data())
    
    selected_vars <- input$available_data %||% character(0)
    run_stage1(df, input, selected_vars)
  })
  
  stage2_result <- eventReactive(input$`2nd_cln`, {
    req(stage1_result())
    req(input$analysis_method)
    run_stage2(stage1_result(), input)
  })
  
  stage3_result <- eventReactive(input$`3rd_cln`, {
    req(stage2_result())
    selected_vars <- input$available_data %||% character(0)
    run_stage3(stage2_result(), input, selected_vars)
  })
  
  
  observe({
    shinyjs::disable("data_cln_done")
  })
  
  observeEvent(input$`2nd_cln`, {
    req(stage2_result())
    
    if (!isTRUE(has_o2h2())) {
      shinyjs::enable("data_cln_done")
    } else {
      shinyjs::disable("data_cln_done")
    }
  })
  
  observeEvent(input$`3rd_cln`, {
    req(stage3_result())
    
    if (isTRUE(has_o2h2())) {
      shinyjs::enable("data_cln_done")
    }
  })
  
  ##################################### DISTRIBUTION CHECK #####################################
  data_for_ch4 <- reactive({
    req(stage1_result())
    stage1_result()$df_clean
  })
  
  
  data_for_o2h2 <- reactive({
    req(stage2_result())
    stage2_result()$df_clean
  })
  
  
  observeEvent(input$ch4_dist_check, {
    req(data_for_ch4())
    show_gas_distribution_modal(
      gas_name = "CH4",
      gas_col = "CH4_gd",
      data_reactive = data_for_ch4,
      treatment_choices = treatment_cols_available
    )
  })
  
  observeEvent(input$o2_dist_check, {
    req(data_for_o2h2())
    req("O2_gd" %in% names(data_for_o2h2()))
    show_gas_distribution_modal(
      gas_name = "O2",
      gas_col = "O2_gd",
      data_reactive = data_for_o2h2,
      treatment_choices = treatment_cols_available
    )
  })
  
  observeEvent(input$h2_dist_check, {
    req(data_for_o2h2())
    req("H2_gd" %in% names(data_for_o2h2()))
    show_gas_distribution_modal(
      gas_name = "H2",
      gas_col = "H2_gd",
      data_reactive = data_for_o2h2,
      treatment_choices = treatment_cols_available
    )
  })
  
  register_gas_outputs("CH4", "CH4_gd", data_for_ch4)
  register_gas_outputs("O2", "O2_gd", data_for_o2h2)
  register_gas_outputs("H2", "H2_gd", data_for_o2h2)
  
  ##################################### FINAL CLEANING #####################################
  
  final_cleaned_data <- reactiveVal(NULL)
  
  build_cleaned_result <- function(){
    req(scr_data())
    
    scr.data <- sanitize_gas_values(scr_data())
    selected_vars <- input$available_data %||% character(0)
    treatment_cols <- selected_treatments() %||% character(0)
    
    cached_cleaned_data <<- NULL
    annotated_data <<- NULL
    
    if (!isTRUE(input$outlier_info)) {
      df.annotated <- scr.data %>%
        mutate(
          Remove_reason = NA_character_,
          H2_outlier = NA_character_,
          O2_outlier = NA_character_
        )
      
      reorder <- c("Farm_name", "GF_unit", treatment_cols, "Start_time", "End_time", "Duration", "Duration_sec", "Hours_day", "CO2_gd", "CH4_gd",
                   "H2_gd", "O2_gd", "Airflow", "Date_rcd", "Time_zone", "Remove_reason", "H2_outlier", "O2_outlier"
      )
      reorder_cols <- reorder[reorder %in% names(df.annotated)]
      
      annotated_data <<- df.annotated[, reorder_cols, drop = FALSE]
      cached_cleaned_data <<- scr.data
      final_cleaned_data(scr.data)
      
      cleaning_rate(NULL)
      cleaning_skip("You can go to next step")
      
      shinyjs::enable("download_clean_data")
      return(invisible(scr.data))
    }
    
    req(stage1_result())
    req(stage2_result())
    
    final_res <- if (isTRUE(has_o2h2())) {
      req(stage3_result())
      stage3_result()
    } else {
      stage2_result()
    }
    
    df.treat <- final_res$df_treat
    df.clean <- final_res$df_clean %>%
      select(-any_of(c("Remove_reason", "H2_outlier", "O2_outlier")))
    
    total_rows <- nrow(scr.data)
    removed_rows <- sum(!is.na(df.treat$Remove_reason))
    removed_perc <- if (total_rows > 0) round(100 * removed_rows / total_rows, 1) else 0
    
    cleaning_rate(
      HTML(
        paste0(
          "Outlier removal: ", removed_perc,
          "% of rows removed (", removed_rows, " / ", total_rows, "); <b><i>LET'S GO TO STEP5 !!</i></b>"
        )
      )
    )
    
    # cleaning_rate(
    #   paste0(
    #     "Outlier removal: ", removed_perc,
    #     "% of rows removed (", removed_rows, " / ", total_rows, "); LET'S GO TO STEP5 !!"
    #   )
    # )
    cleaning_skip(NULL)
    
    reorder <- c("Farm_name", "GF_unit", treatment_cols, "Start_time", "End_time", "Duration", "Duration_sec", "Hours_day", "CO2_gd", "CH4_gd",
                 "H2_gd", "O2_gd", "Airflow", "Date_rcd", "Time_zone", "Remove_reason", "H2_outlier", "O2_outlier"
    )
    reorder_cols <- reorder[reorder %in% names(df.treat)]
    
    annotated_data <<- df.treat[, reorder_cols, drop = FALSE]
    cached_cleaned_data <<- df.clean
    final_cleaned_data(df.clean)
    
    shinyjs::enable("download_clean_data")
    invisible(df.clean)
  }
  
  observeEvent(input$data_cln_done, {
    build_cleaned_result()
  })
  
  observeEvent(input$skip_cln, {
    build_cleaned_result()
  })
  
  output$cleaning_summary <- renderUI({
    req(input$data_cln_done > 0)
    req(final_cleaned_data())
    
    txt <- cleaning_rate()
    
    styled_txt <- sub(
      "LET'S GO TO STEP4 !!",
      "<span style='font-style:italic; font-weight:bold;'>LET'S GO TO STEP4 !!</span>",
      txt
    )
    
    HTML(styled_txt)
  })
  
  output$step5_ready_text <- renderUI({
    req((input$skip_cln > 0) || (input$data_cln_done > 0))
    req(final_cleaned_data())
    tags$span(
      style = "color:black; font-style:italic; font-weight:600;",
      "LET'S GO TO STEP5 !!"
    )
    
  })
  
  ##################################### DOWNLOAD #####################################
  output$download_clean_data <- downloadHandler(
    filename = function() {
      paste0("Cleaned_data_", Sys.Date(), ".xlsx")
    },
    
    content = function(file){
      req(cached_cleaned_data)
      req(annotated_data)
      
      data_clean <- cached_cleaned_data
      data_annotated <- annotated_data
      
      if ("Remove_reason" %in% names(data_annotated)) {
        removed_data <- data_annotated[!is.na(data_annotated$Remove_reason), , drop = FALSE]
      } else {
        removed_data <- data_annotated[0, , drop = FALSE]
      }
      
      removed_data <- data_annotated[!is.na(data_annotated$Remove_reason), , drop = FALSE]
      removed_data <- removed_data %>% select(-any_of(c("H2_outlier", "O2_outlier")))
      
      force_dt_cols <- function(datatable) {
        if ("Start_time" %in% names(datatable)) {
          datatable$Start_time <- as.POSIXct(datatable$Start_time, tz = "UTC")
        }
        if ("End_time" %in% names(datatable)) {
          datatable$End_time <- as.POSIXct(datatable$End_time, tz = "UTC")
        }
        datatable
      }
      
      data_clean <- force_dt_cols(data_clean)
      data_annotated <- force_dt_cols(data_annotated)
      removed_data <- force_dt_cols(removed_data)
      
      wb <- createWorkbook()
      addWorksheet(wb, "Cleaned_Data")
      addWorksheet(wb, "Annotated_Original")
      addWorksheet(wb, "Removed_Lines")
      
      writeData(wb, "Cleaned_Data", data_clean)
      writeData(wb, "Annotated_Original", data_annotated)
      writeData(wb, "Removed_Lines", removed_data)
      
      dt_style <- createStyle(numFmt = "yyyy-mm-dd hh:mm:ss")
      
      style_dt <- function(sheet, datatable){
        
        n <- nrow(datatable)
        if (n == 0) return(invisible(NULL))
        
        dt_cols <- which(names(datatable) %in% c("Start_time", "End_time"))
        if (length(dt_cols) == 0) return(invisible(NULL))
        
        addStyle(
          wb, sheet, style = dt_style,
          rows = 2:(n + 1), cols = dt_cols,
          gridExpand = TRUE, stack = TRUE
        )
      }
      
      style_dt("Cleaned_Data", data_clean)
      style_dt("Annotated_Original", data_annotated)
      style_dt("Removed_Lines", removed_data)
      
      saveWorkbook(wb, file, overwrite = TRUE)
    }
  )
  
  
  ########################################################################################################################
  # 5. CH4 DATA DIURNAL ADJUSTMENT #
  ########################################################################################################################
  
  output$adj_factor_ui <- renderUI({
    if(!isTRUE(input$treat_info)) return(NULL)
    
    selectInput(
      "adj_factors",
      "Select factor(s) to consider for diurnal adjustment (default: No factor)",
      choices = treat_factor_names(),
      selected = character(0),
      multiple = TRUE
    )
  })
  
  adj_data <- reactiveVal(NULL)
   
  observeEvent(input$adj_ch4, {
    req(cached_cleaned_data)

    raw.df <- cached_cleaned_data
    
    print("== TIME SMOOTHING START ==")
    withProgress(message = "Time smoothing...", value = 0, { 
    
    incProgress(0.2, detail = "Data reading...")
      
    if (isTRUE(input$time_smooth)){
      raw.df$Farm_name <- as.factor(raw.df$Farm_name)
      raw.df$Time_zone <- as.factor(raw.df$Time_zone)

      adj_factors <- input$adj_factors
      
      if (length(adj_factors) > 0){
        for (trt.fact in adj_factors){
          if (trt.fact %in% colnames(raw.df)){
            raw.df[[trt.fact]] <- as.factor(raw.df[[trt.fact]])
          }
        }
      }

      # find reference time zone
      incProgress(0.5, detail = "Reference point finding...")
      
      mean_by_hr <- raw.df %>% group_by(Time_zone) %>% summarise(mean_CH4 = mean(CH4_gd, na.rm = TRUE), .groups = "drop")
      
      overall_mean_CH4 <- mean(raw.df$CH4_gd, na.rm = TRUE)

      closest_hr <- mean_by_hr %>% mutate(diff = abs(mean_CH4 - overall_mean_CH4)) %>% arrange(diff) %>% slice(1) %>% pull(Time_zone)
      
      print(paste("Closest Time zone: ", closest_hr, " ; Whole average: ", overall_mean_CH4, sep = ""))

      # Re-level
      df.copy <- raw.df
      df.copy$Time_zone <- relevel(df.copy$Time_zone, ref = as.character(closest_hr))

      # Mixed model
      fixed_terms <- c("Time_zone", adj_factors)
      
      adj.formula <- as.formula(paste("CH4_gd ~", paste(fixed_terms, collapse = " + "), "+ (1|Farm_name)"))
      
      adj.model <- lmer(adj.formula, data = df.copy)

      coeffs_hr <- fixef(adj.model)
      
      # extract Time_zone coeff
      tz_coef_idx <- grepl("^Time_zone", names(coeffs_hr))
      tz_coefs <- coeffs_hr[tz_coef_idx]

      coeffs_hr_day <- data.frame(
        coef_hr_day = as.numeric(tz_coefs),
        Time_zone = sub("^Time_zone", "", names(tz_coefs)),
        stringsAsFactors = FALSE
      )
      
      # Reference hour
      coeffs_hr_day <- rbind(
        coeffs_hr_day,
        data.frame(
          coef_hr_day = 0,
          Time_zone = as.character(closest_hr),
          stringsAsFactors = FALSE
        )
      )
      
      df.copy$Time_zone <- as.character(df.copy$Time_zone)
      
      incProgress(0.7, detail = "Time effect smoothing...")
      
      df.copy <- merge(df.copy, coeffs_hr_day, by = "Time_zone", all.x = TRUE)
      df.copy$coef_hr_day[is.na(df.copy$coef_hr_day)] <- 0
      df.copy$adjCH4_gd <- df.copy$CH4_gd - as.numeric(df.copy$coef_hr_day)

      # dataframe column sorting
      
      treatment_cols <- selected_treatments()
      
      base_order <- c("Farm_name", "GF_unit", treatment_cols, "Start_time", "End_time", "Duration", "Duration_sec", "Hours_day", "CO2_gd", "CH4_gd", "H2_gd", "O2_gd", "Airflow",
                      "Date_rcd", "Time_zone", "coef_hr_day", "adjCH4_gd")

      existing_base_cols <- base_order[base_order %in% colnames(df.copy)]
      df.copy <- df.copy[, existing_base_cols]

      # dataframe row sorting
      df.copy <- df.copy %>% arrange(as.Date(Date_rcd), Farm_name, Hours_day)
      df.copy <- df.copy %>% mutate(Time_zone = floor(Hours_day))
      adj.df <- df.copy

    } else {

      # df.copy <- add_column(raw.df, adjCH4_gd = raw.df$CH4_gd)
      df.copy <- raw.df
      # dataframe row sorting
      df.copy <- df.copy %>% arrange(as.Date(Date_rcd), Farm_name, Hours_day)
      adj.df <- df.copy

    }
    adj.df$Start_time <- as.POSIXct(adj.df$Start_time, tz = "UTC")
    adj.df$End_time <- as.POSIXct(adj.df$End_time, tz = "UTC")
    
    incProgress(0.9, detail = "File preparing...")
    
    })
    
    shinyjs::enable("download_adj_data")
    
    adj_data(adj.df)
  })
  
  output$download_adj_data <- downloadHandler(
    filename = function() {
      paste("Adjusted_data_", Sys.Date(), ".xlsx", sep = "")
    },
    content = function(file) {
      data_adj <- adj_data()

      force_dt_cols <-function(datatable){
        datatable$Start_time <- as.POSIXct(datatable$Start_time, tz = "UTC")
        datatable$End_time <- as.POSIXct(datatable$End_time, tz = "UTC")
        return(datatable)
      }

      data_adj <- force_dt_cols(data_adj)

      wb <- createWorkbook()
      addWorksheet(wb, "Adjusted_data")

      writeData(wb, "Adjusted_data", data_adj)

      dt_style <- createStyle(numFmt = "yyyy-mm-dd hh:mm:ss")

      style_dt <- function(sheet, datatable){
        n <- nrow(datatable); if(n == 0) return(invisible(NULL))
        dt_cols <- which(names(datatable) %in% c("Start_time", "End_time"))
        addStyle(wb, sheet, style = dt_style, rows = 2:(n+1), cols = dt_cols, gridExpand = TRUE, stack = TRUE)
      }

      style_dt("Adjusted_data", data_adj)

      saveWorkbook(wb, file, overwrite = TRUE)

    }
  )
  
  output$step6_ready_text <- renderUI({
    req(adj_data())
    tags$span(
      style = "color:black; font-style:italic; font-weight:600;",
      "LET'S GO TO STEP6 !!"
    )
  })
  
  ########################################################################################################################
  # 6. ANALYSIS RANGE SELECTION #
  ########################################################################################################################
  
  output$machine_filter_ui <- renderUI({
    req(adj_data(), input$machine)
    req(input$machine != "")
    
    adj.db <- tryCatch({ adj_data() }, error = function(e) NULL)
    
    if (is.null(adj.db) || nrow(adj.db) == 0){
      return(pickerInput(
        inputId = "machine_filt",
        label = "Include GF units (default: All)",
        choices = NULL,
        multiple = TRUE,
        options = list(
          'action-box' = TRUE,
          'live-search' = TRUE,
          'selected-text-format' = "count > 3"
        )
      ))
    }
    
    GF_list <- sort(unique(as.character(adj.db$GF_unit)))
    
    pickerInput(
      inputId = "machine_filt",
      label = "Include GF units (default: All)",
      choices = c("<All>" = "All", GF_list),
      selected = GF_list,
      multiple = TRUE,
      options = list(
        'actions-box' = TRUE,
        'live-search' = TRUE,
        'selected-text-format' = "count > 3"
      )
    )
  })
  
  output$dynamic_treat_filter_ui <- renderUI({
    req(input$treat_info)
    req(adj_data())
    
    adj.db <- tryCatch({ adj_data() }, error = function(e) NULL)
    req(adj.db)
    
    treatment_cols <- selected_treatments()
    req(length(treatment_cols) > 0)
    
    valid_treatments <- treatment_cols[treatment_cols %in% colnames(adj.db)]
    req(length(valid_treatments) > 0)
    
    tagList(
      lapply(seq_along(valid_treatments), function(i){
        trt_col <- valid_treatments[i]
        
        trt_list <- sort(unique(as.character(adj.db[[trt_col]])))
        trt_list <- trt_list[!is.na(trt_list) & nzchar(trt_list)]
        
        pickerInput(
          inputId = paste0("treat", i, "_filt"),
          label = paste("Select ", trt_col, " (default: All)"),
          choices = trt_list,
          selected = trt_list,
          multiple = TRUE,
          options = list(
            'actions-box' = TRUE,
            'live-search' = TRUE,
            'selected-text-format' = "count > 3"
          )
        )   
      })
    )
  })
  
  
  output$animal_filter_ui <- renderUI({
    req(adj_data())
    
    adj.db <- tryCatch({ adj_data() }, error = function(e) NULL)
    
    if (is.null(adj.db) || nrow(adj.db) == 0) {
      return(
        pickerInput(
          inputId = "animal_filt",
          label = "Include animals (default: All)",
          choices = NULL,
          multiple = TRUE,
          options = list(
            'actions-box' = TRUE,
            'live-search' = TRUE,
            'selected-text-format' = "count > 3"
          )
        )
      )
    }
    
    anim_list <- sort(unique(as.character(adj.db$Farm_name)))
    
    pickerInput(
      inputId = "animal_filt",
      label = "Include animals (default: All)",
      choices = c("<All>" = "All", anim_list),
      selected = anim_list,
      multiple = TRUE,
      options = list(
        'actions-box' = TRUE,
        'live-search' = TRUE,
        'selected-text-format' = "count > 3"
      )
    )
  })
  
  output$date_range_ui <- renderUI({
    req(visit_trt_data())
    
    adj.db <- tryCatch({ adj_data() }, error = function(e) NULL)
    
    if (is.null(adj.db) || nrow(adj.db) == 0 || !("Date_rcd" %in% colnames(adj.db))) {
      return(
        dateRangeInput(
          inputId = "date_filt_range",
          label = "Select date range:",
          start = NULL, end = NULL,
          min = NULL, max = NULL
        )
      )
    }
    
    dates_rdb <- sort(unique(as.Date(adj.db$Date_rcd)))
    validate(need(length(dates_rdb) > 0, "No dates found."))
    
    dateRangeInput(
      inputId = "date_filt_range",
      label = "Select date range:",
      start = dates_rdb[1],
      end = dates_rdb[length(dates_rdb)],
      min = dates_rdb[1],
      max = dates_rdb[length(dates_rdb)]
    )
  })
  
  # Machine & Treatment & Animal & Date list filtering
  
  filt_data_rv <- reactiveVal(NULL)
    
  observeEvent(input$range_sel_done,{
    req(visit_trt_data())
    
    print("== 6. Analysis range select ==")
    
    adj.db <- tryCatch({ adj_data() }, error = function(e) NULL)
    req(!is.null(adj.db), nrow(adj.db) > 0)
    
    if(!isTRUE(input$range_info) && "Date_rcd" %in% colnames(adj.db)){
      dates_rdb <- sort(unique(as.Date(adj.db$Date_rcd)))
      dates_rdb <- dates_rdb[!is.na(dates_rdb)]
      
      if (length(dates_rdb) > 0){
        updateDateRangeInput(session, inputId = "date_filt_range",
                             start = dates_rdb[1],
                             end = dates_rdb[length(dates_rdb)],
                             min = dates_rdb[1],
                             max = dates_rdb[length(dates_rdb)])
      }
    }
    
    df <- adj.db
    
    # Machine
    if (!is.null(input$machine_filt) && length(input$machine_filt) > 0){
      if(!("All" %in% input$machine_filt)){
        df <- df[df$GF_unit %in% input$machine_filt, , drop = FALSE]
      }
    }
    
    # Treatment
    if (isTRUE(input$treat_info)) {
      treatment_cols <- selected_treatments()
      
      if (length(treatment_cols) > 0) {
        for (i in seq_along(treatment_cols)) {
          trt_col <- treatment_cols[i]
          trt_filt_input <- input[[paste0("treat", i, "_filt")]]
          
          if (!is.null(trt_col) &&
              nzchar(trt_col) &&
              trt_col %in% colnames(df) &&
              !is.null(trt_filt_input) &&
              length(trt_filt_input) > 0) {
            
            if (!("All" %in% trt_filt_input)) {
              df <- df[as.character(df[[trt_col]]) %in% trt_filt_input, , drop = FALSE]
            }
          }
        }
      }
    }
    
    # Animal list
    if (!is.null(input$animal_filt) && length(input$animal_filt) > 0){
      if(!("All" %in% input$animal_filt)){
        df <- df[df$Farm_name %in% input$animal_filt, , drop = FALSE]
      }
    }
    
    # Date range
    if (!is.null(input$date_filt_range) && length(input$date_filt_range) == 2 &&
        "Date_rcd" %in% colnames(df)){
      date_vec <- as.Date(df$Date_rcd)
      
      df <- df[
        !is.na(date_vec) &
          date_vec >= as.Date(input$date_filt_range[1]) &
          date_vec <= as.Date(input$date_filt_range[2]), , drop = FALSE
      ]
    }
    
    filt_data_rv(df)
    
    shinyjs::enable("download_ready_data")
    
    })
  
  
  filt_data <- reactive({
    req(filt_data_rv())
    filt_data_rv()
  })
  
  output$analysis_ready_text <- renderUI({
    req(filt_data())
    tags$span(
      style = "color:black; font-style:italic; font-weight:600;",
      "WE ARE READY FOR ANALYSIS"
    )
  })
  
  output$download_ready_data <- downloadHandler(
    filename = function(){
      paste("Ready_dataset_", Sys.Date(), ".xlsx", sep = "")
    },
    content = function(file){
      data_to_save <- filt_data()
      openxlsx::write.xlsx(data_to_save, file)
    }
  )
  
 #  
 #  ########################################################################################################################
 #  # REPEATABILITY CALCULATION #
 #  ########################################################################################################################
 #  
 #  # Reactive repeatability data table for graph -> Arbre et al., 2016
  
  output$rep_date_range_ui <- renderUI({
    req(filt_data())
    
    df <- filt_data()
    req("Date_rcd" %in% names(df))
    
    dates <- sort(unique(as.Date(df$Date_rcd)))
    validate(need(length(dates) > 0, "No dates found."))
    
    dateRangeInput(
      inputId = "rep_date_range",
      label   = "Select date range for repeatability only",
      start   = min(dates, na.rm = TRUE),
      end     = max(dates, na.rm = TRUE),
      min     = min(dates, na.rm = TRUE),
      max     = max(dates, na.rm = TRUE)
    )
  })
  
  output$rep_fixed_effect_ui <- renderUI({
    req(isTRUE(input$treat_info))
    req(filt_data())
    
    trt_cols <- selected_treatments()
    trt_cols <- trt_cols[trt_cols %in% names(filt_data())]
    
    if (length(trt_cols) == 0) return(NULL)
    
    checkboxGroupInput(
      inputId = "rep_fixed_effects",
      label   = "Select fixed effect factor(s)",
      choices = trt_cols,
      selected = trt_cols
    )
  })
  
  outputOptions(output, "rep_date_range_ui", suspendWhenHidden = FALSE)
  outputOptions(output, "rep_fixed_effect_ui", suspendWhenHidden = FALSE)
  
  
  # ==============================
  # Helper functions
  # ==============================
  
  build_rep_dataset <- function(df, date_mode = "calendar", date_range = NULL){
    req(df)
    req("Farm_name" %in% names(df), "Date_rcd" %in% names(df))
    
    df2 <- df %>%
      mutate(
        Farm_name = as.character(Farm_name),
        Date_rcd  = as.Date(Date_rcd)
      )
    
    if (!is.null(date_range) && length(date_range) == 2) {
      start_date <- as.Date(unlist(date_range, use.names = FALSE)[1])
      end_date   <- as.Date(unlist(date_range, use.names = FALSE)[2])
      
      df2 <- df2 %>%
        filter(
          Date_rcd >= start_date,
          Date_rcd <= end_date
        )
    }
    
    validate(
      need(nrow(df2) > 0, "No data available in selected repeatability range.")
    )
    
    if (identical(date_mode, "calendar")) {
      
      date_list <- sort(unique(df2$Date_rcd))
      
      df2 <- df2 %>%
        mutate(rep_day_index = match(Date_rcd, date_list))
      
      total_days <- length(date_list)
      
    } else if (identical(date_mode, "ordinal")) {
      
      animal_day_map <- df2 %>%
        distinct(Farm_name, Date_rcd) %>%
        arrange(Farm_name, Date_rcd) %>%
        group_by(Farm_name) %>%
        mutate(rep_day_index = row_number()) %>%
        ungroup()
      
      df2 <- df2 %>%
        left_join(animal_day_map, by = c("Farm_name", "Date_rcd"))
      
      animal_lengths <- animal_day_map %>%
        group_by(Farm_name) %>%
        summarise(n_days = max(rep_day_index, na.rm = TRUE), .groups = "drop")
      
      total_days <- min(animal_lengths$n_days, na.rm = TRUE)
      
      validate(
        need(is.finite(total_days) && total_days >= 1,
             "No data remain after ordinal-day indexing.")
      )
      
    } else {
      validate(need(FALSE, "Invalid repeatability date mode."))
    }
    
    attr(df2, "rep_total_days") <- as.integer(total_days)
    df2
  }
  
  get_valid_rep_periods <- function(total_days){
    if (is.na(total_days) || total_days < 1) return(integer(0))
    
    vals <- which(total_days %% seq_len(total_days) == 0)
    
    vals <- vals[(total_days / vals) >= 2]
    
    vals
  }
  
  build_rep_formula <- function(response, fixed_effects = NULL){
    fixed_effects <- fixed_effects[!is.na(fixed_effects) & nzchar(fixed_effects)]
    
    rhs_parts <- c("1")
    
    if (length(fixed_effects) > 0) {
      rhs_parts <- c(rhs_parts, fixed_effects)
    }
    
    rhs_parts <- c(rhs_parts, "(1|Farm_name)", "(1|Period)")
    
    as.formula(
      paste(response, "~", paste(rhs_parts, collapse = " + "))
    )
  }
  
  safe_lmer <- function(formula, data){
    tryCatch(
      suppressWarnings(
        lmer(
          formula,
          data = data,
          REML = TRUE,
          control = lmerControl(
            optimizer = "bobyqa",
            optCtrl = list(maxfun = 2e5)
          )
        )
      ),
      error = function(e) NULL
    )
  }
  
  
  # ==============================
  # Trigger
  # ==============================
  
  rep_trigger <- reactiveVal(NULL)
  
  observeEvent(input$calc_repeatability, { rep_trigger(paste0("repeatability_", Sys.time())) })
  observeEvent(input$calc_rep_entire,    { rep_trigger(paste0("rep_entire_", Sys.time())) })
  observeEvent(input$calc_rep_anim,      { rep_trigger(paste0("rep_anim_", Sys.time())) })
  observeEvent(input$calc_rep_trt1,      { rep_trigger(paste0("rep_trt1_", Sys.time())) })
  observeEvent(input$calc_rep_trt2,      { rep_trigger(paste0("rep_trt2_", Sys.time())) })
  observeEvent(input$calc_rep_trt1xtrt2, { rep_trigger(paste0("rep_trt1xtrt2_", Sys.time())) })
  
  
  # ====================================
  # Preview dataset after "Select Done"
  # ====================================
  
  rep_preview_data <- eventReactive(input$rep_range_done, {
    req(filt_data())
    
    df.preview <- filt_data()
    
    df.preview <- build_rep_dataset(
      df         = df.preview,
      date_mode  = input$rep_date_mode,
      date_range = input$rep_date_range
    )
    
    total_days <- length(sort(unique(df.preview$rep_day_index)))
    dividers   <- get_valid_rep_periods(total_days)
    
    list(
      data       = df.preview,
      total_days = total_days,
      dividers   = dividers
    )
  })
  
  rep_preview_info <- reactiveVal(NULL)
  
  observeEvent(input$rep_range_done, {
    req(filt_data())
    
    tryCatch({
      df.preview <- filt_data()
      
      df.preview <- build_rep_dataset(
        df         = df.preview,
        date_mode  = input$rep_date_mode,
        date_range = input$rep_date_range
      )
      
      total_days <- attr(df.preview, "rep_total_days")
      total_days <- as.integer(total_days)
      
      if (is.na(total_days) || total_days < 1) {
        total_days <- length(sort(unique(df.preview$rep_day_index)))
      }
      
      dividers <- get_valid_rep_periods(total_days)
      
      msg <- if (length(dividers) == 0) {
        paste0(
          "Selected valid days: ", total_days, "\n",
          "Validated divider: none\n",
          "Please reset date range."
        )
      } else {
        paste0(
          "Selected valid days: ", total_days, "\n",
          "Validated divider(s): ", paste(dividers, collapse = ", ")
        )
      }
      
      rep_preview_info(list(
        data = df.preview,
        total_days = total_days,
        dividers = dividers,
        message = msg
      ))
      
    }, error = function(e) {
      rep_preview_info(list(
        data = NULL,
        total_days = NA_integer_,
        dividers = integer(0),
        message = paste("Preview failed:", e$message)
      ))
    })
  })
  
  output$divider_list <- renderText({
    info <- rep_preview_info()
    
    if (is.null(info)) {
      return("Click 'Select Done' to preview valid days and divider options.")
    }
    
    info$message
  })
  
  
  # ==============================
  # Main calculation
  # ==============================
  
  rep_result_data <- eventReactive(rep_trigger(), {
    req(rep_preview_info())
    
    preview_info <- rep_preview_info()
    
    df.clean.sel <- preview_info[["data"]]
    total_days   <- as.integer(preview_info[["total_days"]])
    dividers     <- as.integer(unlist(preview_info[["dividers"]], use.names = FALSE))
    
    withProgress(message = "Calculating repeatability...", value = 0, {
      
      incProgress(0.10, detail = "Preparing dataset...")
      
      validate(
        need(!is.null(df.clean.sel), "Repeatability preview data is missing. Click 'Select Done' first."),
        need(total_days >= 2, "At least 2 days are required."),
        need(length(dividers) > 0,
             "No valid divider found. Please change repeatability date range and click 'Select Done' again."),
        need("CH4_gd" %in% names(df.clean.sel), "CH4_gd column not found."),
        need("rep_day_index" %in% names(df.clean.sel), "rep_day_index column not found.")
      )
      
      if (isTRUE(input$time_smooth)) {
        validate(
          need("adjCH4_gd" %in% names(df.clean.sel), "adjCH4_gd column not found.")
        )
      }
      
      fixed_effects <- character(0)
      
      if (isTRUE(input$rep_use_fixed)) {
        validate(
          need(isTRUE(input$treat_info), "Factor file must be uploaded to use fixed effects."),
          need(!is.null(input$rep_fixed_effects) && length(input$rep_fixed_effects) > 0,
               "Select at least one fixed effect factor.")
        )
        
        fixed_effects <- as.character(unlist(input$rep_fixed_effects, use.names = FALSE))
        fixed_effects <- fixed_effects[fixed_effects %in% names(df.clean.sel)]
      }
      
      incProgress(0.25, detail = "Running mixed models...")
      
      res_list <- vector("list", length(dividers))
      
      for (i in seq_along(dividers)) {
        jour.per <- as.integer(dividers[[i]])
        
        tmp.df.input <- df.clean.sel %>%
          mutate(
            rep_day_index = as.integer(rep_day_index),
            Period = ((rep_day_index - 1L) %/% jour.per) + 1L
          )
        
        if (identical(input$rep_date_mode, "ordinal")) {
          period_check <- tmp.df.input %>%
            distinct(Farm_name, rep_day_index, Period) %>%
            count(Farm_name, Period, name = "n_days")
          
          valid_periods <- period_check %>%
            filter(n_days == jour.per) %>%
            select(Farm_name, Period)
          
          tmp.df.input <- tmp.df.input %>%
            semi_join(valid_periods, by = c("Farm_name", "Period"))
          
        } else {
          period_day_check <- tmp.df.input %>%
            distinct(rep_day_index, Period) %>%
            count(Period, name = "n_days")
          
          valid_period_ids <- period_day_check %>%
            filter(n_days == jour.per) %>%
            pull(Period)
          
          tmp.df.input <- tmp.df.input %>%
            filter(Period %in% valid_period_ids)
        }
        
        if (nrow(tmp.df.input) == 0) {
          res_list[[i]] <- NULL
          next
        }
        
        if (length(unique(tmp.df.input$Period)) < 2) {
          res_list[[i]] <- NULL
          next
        }
        
        tmp.df.input <- tmp.df.input %>%
          mutate(
            Farm_name = as.factor(Farm_name),
            Period    = as.factor(Period),
            CH4_gd    = as.numeric(CH4_gd)
          )
        
        has_adj <- "adjCH4_gd" %in% names(tmp.df.input)
        if (has_adj) {
          tmp.df.input$adjCH4_gd <- as.numeric(tmp.df.input$adjCH4_gd)
        }
        
        group_cols <- c("Farm_name", "Period", fixed_effects)
        group_cols <- group_cols[group_cols %in% names(tmp.df.input)]
        
        if (has_adj) {
          avg.df <- tmp.df.input %>%
            group_by(across(all_of(group_cols))) %>%
            summarise(
              CH4_avg    = mean(CH4_gd, na.rm = TRUE),
              adjCH4_avg = mean(adjCH4_gd, na.rm = TRUE),
              .groups    = "drop"
            )
        } else {
          avg.df <- tmp.df.input %>%
            group_by(across(all_of(group_cols))) %>%
            summarise(
              CH4_avg    = mean(CH4_gd, na.rm = TRUE),
              adjCH4_avg = NA_real_,
              .groups    = "drop"
            )
        }
        
        if (nrow(avg.df) == 0) {
          res_list[[i]] <- NULL
          next
        }
        
        valid_fixed_effects <- fixed_effects[
          vapply(fixed_effects, function(v) {
            v %in% names(avg.df) && dplyr::n_distinct(avg.df[[v]], na.rm = TRUE) > 1
          }, logical(1))
        ]
        
        for (v in valid_fixed_effects) {
          avg.df[[v]] <- as.factor(avg.df[[v]])
        }
        
        if (dplyr::n_distinct(avg.df$Farm_name) < 2 ||
            dplyr::n_distinct(avg.df$Period) < 2) {
          res_list[[i]] <- NULL
          next
        }
        
        raw_formula <- build_rep_formula("CH4_avg", valid_fixed_effects)
        mix.model   <- safe_lmer(raw_formula, avg.df)
        
        if (is.null(mix.model)) {
          res_list[[i]] <- NULL
          next
        }
        
        random.eff <- VarCorr(mix.model)
        
        var.anim   <- if ("Farm_name" %in% names(random.eff)) attr(random.eff$Farm_name, "stddev")^2 else NA_real_
        var.period <- if ("Period" %in% names(random.eff)) attr(random.eff$Period, "stddev")^2 else NA_real_
        var.resid  <- sigma(mix.model)^2
        
        rep.val <- ifelse(
          is.na(var.anim) || is.na(var.resid) || (var.anim + var.resid) == 0,
          NA_real_,
          var.anim / (var.anim + var.resid + var.period)
        )
        
        one_row <- data.frame(Days = jour.per,
                              N_subgroup = length(unique(avg.df$Period)),
                              N_animals = length(unique(avg.df$Farm_name)),
                              Fixed_effects = ifelse(length(valid_fixed_effects) == 0, "None", paste(valid_fixed_effects, collapse = " + ")),
                              Repeatability = round(rep.val, 3),
                              Variance_anim = round(var.anim, 3),
                              Variance_period = round(var.period, 3),
                              Residue = round(var.resid, 3),
                              stringsAsFactors = FALSE)
        
        if (isTRUE(input$time_smooth) &&
            "adjCH4_avg" %in% names(avg.df) &&
            any(!is.na(avg.df$adjCH4_avg))) {
          
          adj_formula   <- build_rep_formula("adjCH4_avg", valid_fixed_effects)
          mix.model.adj <- safe_lmer(adj_formula, avg.df)
          
          if (!is.null(mix.model.adj)) {
            random.eff.adj <- VarCorr(mix.model.adj)
            
            var.anim.adj   <- if ("Farm_name" %in% names(random.eff.adj)) attr(random.eff.adj$Farm_name, "stddev")^2 else NA_real_
            var.period.adj <- if ("Period" %in% names(random.eff.adj)) attr(random.eff.adj$Period, "stddev")^2 else NA_real_
            var.resid.adj  <- sigma(mix.model.adj)^2
            
            rep.val.adj <- ifelse(
              is.na(var.anim.adj) || is.na(var.resid.adj) || (var.anim.adj + var.resid.adj) == 0,
              NA_real_,
              var.anim.adj / (var.anim.adj + var.resid.adj + var.period.adj)
            )
            
            one_row$Repeatability_diurnal_adj <- round(rep.val.adj, 3)
            one_row$Variance_anim_diurnal_adj <- round(var.anim.adj, 3)
            one_row$Variance_period_diurnal_adj <- round(var.period.adj, 3)
            one_row$Residue_diurnal_adj <- round(var.resid.adj, 3)
          }
        }
        
        res_list[[i]] <- one_row
        incProgress(0.25 + 0.75 * (i / length(dividers)))
      }
      
      df.rep.result <- bind_rows(res_list)
      
      validate(
        need(nrow(df.rep.result) > 0, "No repeatability result could be estimated.")
      )
      
      shinyjs::enable("download_repeatability")
      df.rep.result
    })
  })
  
  
  # ==============================
  # Plot flags
  # ==============================
  
  observeEvent(input$calc_repeatability, {
    plot_state$rep_plot_ready    <- TRUE
    plot_state$adjrep_plot_ready <- TRUE
  })
  
  observeEvent(rep_result_data(), {
    plot_state$rep_plot_ready    <- TRUE
    plot_state$adjrep_plot_ready <- TRUE
  })
  
  
  # ==============================
  # Download
  # ==============================
  
  output$download_repeatability <- downloadHandler(
    filename = function() {
      paste0("Repeatability_Results_", Sys.Date(), ".xlsx")
    },
    content = function(file) {
      openxlsx::write.xlsx(rep_result_data(), file)
    }
  )
  
  
  # ==============================
  # Plot outputs
  # ==============================
  
  grp_theme_SJ <- theme_classic()+
    theme(text = element_text(family = "Arial Black"),
          axis.text.x = element_text(size = 12, face = 'bold', color = "black", margin = margin(t = 5)),
          axis.text.y = element_text(size = 12, face = 'bold', color = "black", margin = margin(t = 5)),
          axis.title.x = element_text(size = 14, face = 'bold', margin = margin(t = 20)),
          axis.title.y = element_text(size = 14, face = 'bold', margin = margin(t = 20)),
          plot.title = element_text(hjust = 0.5, size = 16, face = 'bold'))
  
  
  output$Rep_grp <- renderPlotly({
    req(rep_result_data())
    req(plot_state$rep_plot_ready)
    df.rep.result <- rep_result_data()
    
    rep.grp <- ggplot(df.rep.result, aes(x = Days, y = Repeatability))+
      geom_line()+
      geom_point()+
      ggtitle("Repeatability over measurement period")+
      ylab("Repeatability")+
      xlab("Days/Subgroup")+
      # scale_x_continuous(breaks = seq(0, nrow(df.rep.result), 5))+
      scale_y_continuous(breaks = seq(min(df.rep.result$Repeatability, df.rep.result$Repeatability_diurnal_adj)-0.1, 
                                      max(df.rep.result$Repeatability, df.rep.result$Repeatability_diurnal_adj)+0.1, 0.1),
                         labels = function(x) sprintf("%.2f", x)) +
      grp_theme_SJ
    
    if (isTRUE(input$show_hline)){
      rep.grp <- rep.grp + geom_hline(yintercept = input$hline_value, linetype = 2)
    }
    
    ggplotly(rep.grp)
  })
  
  # With diurnal adjustment
  output$adjRep_grp <- renderPlotly({
    req(rep_result_data())
    req(plot_state$adjrep_plot_ready)
    df.rep.result <- rep_result_data()
    
    rep.grp.adj <- ggplot(df.rep.result, aes(x = Days, y = Repeatability_diurnal_adj))+
      geom_line()+
      geom_point()+
      ggtitle("Repeatability over measurement period with Diurnal adjustment")+
      ylab("Adjusted Repeatability")+
      xlab("Days/Subgroup")+
      theme_classic()+
      # scale_x_continuous(breaks = seq(0, nrow(df.rep.result), 5))+
      scale_y_continuous(breaks = seq(min(df.rep.result$Repeatability, df.rep.result$Repeatability_diurnal_adj)-0.1, 
                                      max(df.rep.result$Repeatability, df.rep.result$Repeatability_diurnal_adj)+0.1, 0.1),
                         labels = function(x) sprintf("%.2f", x))+
      grp_theme_SJ
    
    if (isTRUE(input$show_hline)){
      rep.grp.adj <- rep.grp.adj + geom_hline(yintercept = 0.7, linetype = 2)
    }
    
    ggplotly(rep.grp.adj)
  })


  observeEvent(input$calc_repeatability, {
    plot_state$rep_plot_ready <- TRUE
    plot_state$adjrep_plot_ready <- TRUE
  })


  ########################################################################################################################
  # DESCRIPTIVE STATISTICS CALCULATION #
  ########################################################################################################################

  #======================================================================================================================#
  # WHOLE PERIOD STATISTICS #
  #======================================================================================================================#

  # Whole period per animal
  whole_anim <- eventReactive(input$calc_whole_anim, {
    req(filt_data())
    print("== Whole period statistics per Animal ==")

    trt_data <- filt_data()

    withProgress(message = "Calculating Whole statistics - Animal...", value = 0, {
      #===========================================#
      incProgress(0.2, detail = "Loading data...")

      # optional column check
      exist_H2 <- "H2_gd" %in% colnames(trt_data)
      exist_O2 <- "O2_gd" %in% colnames(trt_data)
      exist_airflow <- "Airflow" %in% colnames(trt_data)
      exist_adjCH4 <- "adjCH4_gd" %in% colnames(trt_data)

      # Check if treatment columns exists, add only if they are non-null
      treatment_cols <- c(input$treat1, input$treat2)
      treatment_cols <- treatment_cols[!is.null(treatment_cols) & nzchar(treatment_cols)]

      #===========================================#
      incProgress(0.5, detail = "Calculating...")

      if (isTRUE(input$time_smooth) && exist_adjCH4){

        df.whole.anim <- trt_data %>% group_by(Farm_name) %>% summarise(Farm_name = unique(Farm_name),
                                                                            across(all_of(treatment_cols), ~first(na.omit(.)), .names = "{.col}"),
                                                                            Nb_visit = n(),
                                                                            CO2_avg_gd = mean(CO2_gd, na.rm = TRUE),
                                                                            CO2_SD = sd(CO2_gd, na.rm = TRUE),
                                                                            CH4_avg_gd = mean(CH4_gd, na.rm = TRUE),
                                                                            CH4_SD = sd(CH4_gd, na.rm = TRUE),
                                                                            adjCH4_avg_gd = mean(adjCH4_gd, na.rm = TRUE),
                                                                            adjCH4_SD = sd(adjCH4_gd, na.rm = TRUE),

                                                                            H2_avg_gd = if (exist_H2) mean(H2_gd, na.rm = TRUE) else NA_real_,
                                                                            H2_SD = if (exist_H2) sd(H2_gd, na.rm = TRUE) else NA_real_,
                                                                            O2_avg_gd = if (exist_O2) mean(O2_gd, na.rm = TRUE) else NA_real_,
                                                                            O2_SD = if (exist_O2) sd(O2_gd, na.rm = TRUE) else NA_real_,
                                                                            Airflow_avg_ls = if (exist_airflow) mean(Airflow, na.rm = TRUE) else NA_real_,
                                                                            Airflow_SD = if (exist_airflow) sd(Airflow, na.rm = TRUE) else NA_real_,
                                                                            .groups = "drop")
      } else{
        df.whole.anim <- trt_data %>% group_by(Farm_name) %>% summarise(Farm_name = unique(Farm_name),
                                                                            across(all_of(treatment_cols), ~first(na.omit(.)), .names = "{.col}"),
                                                                            Nb_visit = n(),
                                                                            CO2_avg_gd = mean(CO2_gd, na.rm = TRUE),
                                                                            CO2_SD = sd(CO2_gd, na.rm = TRUE),
                                                                            CH4_avg_gd = mean(CH4_gd, na.rm = TRUE),
                                                                            CH4_SD = sd(CH4_gd, na.rm = TRUE),

                                                                            H2_avg_gd = if (exist_H2) mean(H2_gd, na.rm = TRUE) else NA_real_,
                                                                            H2_SD = if (exist_H2) sd(H2_gd, na.rm = TRUE) else NA_real_,
                                                                            O2_avg_gd = if (exist_O2) mean(O2_gd, na.rm = TRUE) else NA_real_,
                                                                            O2_SD = if (exist_O2) sd(O2_gd, na.rm = TRUE) else NA_real_,
                                                                            Airflow_avg_ls = if (exist_airflow) mean(Airflow, na.rm = TRUE) else NA_real_,
                                                                            Airflow_SD = if (exist_airflow) sd(Airflow, na.rm = TRUE) else NA_real_,
                                                                            .groups = "drop")

      }

      incProgress(0.7, detail = "Calculating...")
      #===========================================#
      incProgress(1, detail = "Finalizing...")

      # Empty column eliminate
      df.whole.anim <- df.whole.anim %>% select(where(~ !all(is.na(.))))

    })

    return(df.whole.anim)
  })


  # control download button for 'whole animal'
  shinyjs::disable("download_whole_anim")

  observe({
    if (!is.null(whole_anim())) {
      shinyjs::enable("download_whole_anim")
    } else{
      shinyjs::disable("download_whole_anim")
    }
  })

  output$download_whole_anim <- downloadHandler(
    filename = function() {
      paste("Whole_Animal_", Sys.Date(), ".xlsx", sep = "")
    },
    content = function(file) {
      data_to_save <- whole_anim()
      if (!is.null(data_to_save)){
        openxlsx::write.xlsx(data_to_save, file)

      } else{
        stop("No data available to downloads.")
      }
    }
  )

  ########################################################################################################################
  # Whole period average per Factor1
  whole_t1 <- eventReactive(input$calc_whole_trt1, {
    req(filt_data(), input$treat1)
    print("== Whole period statistics per Factor1 ==")

    trt_data <- filt_data()

    withProgress(message = "Calculating Whole period statistics - Factor1...", value = 0, {
      #===========================================#
      incProgress(0.2, detail = "Loading data...")

      # optional column check
      exist_H2 <- "H2_gd" %in% colnames(trt_data)
      exist_O2 <- "O2_gd" %in% colnames(trt_data)
      exist_airflow <- "Airflow" %in% colnames(trt_data)
      exist_adjCH4 <- "adjCH4_gd" %in% colnames(trt_data)

      # Check if treatment columns exists, add only if they are non-null
      treatment_cols <- input$treat1
      treatment_cols <- treatment_cols[!is.null(treatment_cols) & nzchar(treatment_cols)]
      #===========================================#
      incProgress(0.5, detail = "Calculating...")

      # Dynamically construct group_by columns
      group_by_cols <- c()
      if (!is.null(treatment_cols)) {
        group_by_cols <- c(treatment_cols)
      }

      # Group and summarise dynamically
      if (isTRUE(input$time_smooth) && exist_adjCH4){

        df.whole.trt1 <- trt_data %>% group_by(!!!syms(group_by_cols)) %>% summarise(Nb_visit = n(),
                                                                                    Nb_animal = n_distinct(Farm_name),
                                                                                    Nb_visit_per_animal = Nb_visit / Nb_animal,
                                                                                    CO2_avg_gd = mean(CO2_gd, na.rm = TRUE),
                                                                                    CO2_SD = sd(CO2_gd, na.rm = TRUE),
                                                                                    CH4_avg_gd = mean(CH4_gd, na.rm = TRUE),
                                                                                    CH4_SD = sd(CH4_gd, na.rm = TRUE),
                                                                                    adjCH4_avg_gd = mean(adjCH4_gd, na.rm = TRUE),
                                                                                    adjCH4_SD = sd(adjCH4_gd, na.rm = TRUE),

                                                                                    H2_avg_gd = if (exist_H2) mean(H2_gd, na.rm = TRUE) else NA_real_,
                                                                                    H2_SD = if (exist_H2) sd(H2_gd, na.rm = TRUE) else NA_real_,
                                                                                    O2_avg_gd = if (exist_O2) mean(O2_gd, na.rm = TRUE) else NA_real_,
                                                                                    O2_SD = if (exist_O2) sd(O2_gd, na.rm = TRUE) else NA_real_,
                                                                                    Airflow_avg_ls = if (exist_airflow) mean(Airflow, na.rm = TRUE) else NA_real_,
                                                                                    Airflow_SD = if (exist_airflow) sd(Airflow, na.rm = TRUE) else NA_real_,
                                                                                    .groups = "drop")
      } else{
        df.whole.trt1 <- trt_data %>% group_by(!!!syms(group_by_cols)) %>% summarise(Nb_visit = n(),
                                                                                    Nb_animal = n_distinct(Farm_name),
                                                                                    Nb_visit_per_animal = Nb_visit / Nb_animal,
                                                                                    CO2_avg_gd = mean(CO2_gd, na.rm = TRUE),
                                                                                    CO2_SD = sd(CO2_gd, na.rm = TRUE),
                                                                                    CH4_avg_gd = mean(CH4_gd, na.rm = TRUE),
                                                                                    CH4_SD = sd(CH4_gd, na.rm = TRUE),

                                                                                    H2_avg_gd = if (exist_H2) mean(H2_gd, na.rm = TRUE) else NA_real_,
                                                                                    H2_SD = if (exist_H2) sd(H2_gd, na.rm = TRUE) else NA_real_,
                                                                                    O2_avg_gd = if (exist_O2) mean(O2_gd, na.rm = TRUE) else NA_real_,
                                                                                    O2_SD = if (exist_O2) sd(O2_gd, na.rm = TRUE) else NA_real_,
                                                                                    Airflow_avg_ls = if (exist_airflow) mean(Airflow, na.rm = TRUE) else NA_real_,
                                                                                    Airflow_SD = if (exist_airflow) sd(Airflow, na.rm = TRUE) else NA_real_,
                                                                                    .groups = "drop")

      }
      incProgress(0.7, detail = "Calculating...")
      #===========================================#
      incProgress(1, detail = "Finalizing...")

      # Empty column eliminate
      df.whole.trt1 <- df.whole.trt1 %>% select(where(~ !all(is.na(.))))

    })

    return(df.whole.trt1)

  })

  # control download button for 'whole trt1'
  shinyjs::disable("download_whole_trt1")

  observe({
    if (!is.null(whole_t1())) {
      shinyjs::enable("download_whole_trt1")
    } else{
      shinyjs::disable("download_whole_trt1")
    }
  })

  output$download_whole_trt1 <- downloadHandler(
    filename = function() {
      paste("Whole_Factor1_", Sys.Date(), ".xlsx", sep = "")
    },
    content = function(file) {
      data_to_save <- whole_t1()
      if (!is.null(data_to_save)){
        openxlsx::write.xlsx(data_to_save, file)

      } else{
        stop("No data available to downloads.")
      }
    }
  )

  ########################################################################################################################
  # Whole period average per Factor2
  whole_t2 <- eventReactive(input$calc_whole_trt2, {
    req(filt_data(), input$treat2)
    print("== Whole period statistics per Factor2 ==")

    trt_data <- filt_data()

    withProgress(message = "Calculating Whole period statistics - Factor2...", value = 0, {
      #===========================================#
      incProgress(0.2, detail = "Loading data...")

      # optional column check
      exist_H2 <- "H2_gd" %in% colnames(trt_data)
      exist_O2 <- "O2_gd" %in% colnames(trt_data)
      exist_airflow <- "Airflow" %in% colnames(trt_data)
      exist_adjCH4 <- "adjCH4_gd" %in% colnames(trt_data)

      # Check if treatment columns exists, add only if they are non-null
      treatment_cols <- input$treat2
      treatment_cols <- treatment_cols[!is.null(treatment_cols) & nzchar(treatment_cols)]
      #===========================================#
      incProgress(0.5, detail = "Calculating...")

      # Dynamically construct group_by columns
      group_by_cols <- c()
      if (!is.null(treatment_cols)) {
        group_by_cols <- c(treatment_cols)
      }

      # Group and summarise dynamically
      if (isTRUE(input$time_smooth) && exist_adjCH4){

        df.whole.trt2 <- trt_data %>% group_by(!!!syms(group_by_cols)) %>% summarise(Nb_visit = n(),
                                                                                     Nb_animal = n_distinct(Farm_name),
                                                                                     Nb_visit_per_animal = Nb_visit / Nb_animal,
                                                                                     CO2_avg_gd = mean(CO2_gd, na.rm = TRUE),
                                                                                     CO2_SD = sd(CO2_gd, na.rm = TRUE),
                                                                                     CH4_avg_gd = mean(CH4_gd, na.rm = TRUE),
                                                                                     CH4_SD = sd(CH4_gd, na.rm = TRUE),
                                                                                     adjCH4_avg_gd = mean(adjCH4_gd, na.rm = TRUE),
                                                                                     adjCH4_SD = sd(adjCH4_gd, na.rm = TRUE),

                                                                                     H2_avg_gd = if (exist_H2) mean(H2_gd, na.rm = TRUE) else NA_real_,
                                                                                     H2_SD = if (exist_H2) sd(H2_gd, na.rm = TRUE) else NA_real_,
                                                                                     O2_avg_gd = if (exist_O2) mean(O2_gd, na.rm = TRUE) else NA_real_,
                                                                                     O2_SD = if (exist_O2) sd(O2_gd, na.rm = TRUE) else NA_real_,
                                                                                     Airflow_avg_ls = if (exist_airflow) mean(Airflow, na.rm = TRUE) else NA_real_,
                                                                                     Airflow_SD = if (exist_airflow) sd(Airflow, na.rm = TRUE) else NA_real_,
                                                                                     .groups = "drop")
      } else{
        df.whole.trt2 <- trt_data %>% group_by(!!!syms(group_by_cols)) %>% summarise(Nb_visit = n(),
                                                                                     Nb_animal = n_distinct(Farm_name),
                                                                                     Nb_visit_per_animal = Nb_visit / Nb_animal,
                                                                                     CO2_avg_gd = mean(CO2_gd, na.rm = TRUE),
                                                                                     CO2_SD = sd(CO2_gd, na.rm = TRUE),
                                                                                     CH4_avg_gd = mean(CH4_gd, na.rm = TRUE),
                                                                                     CH4_SD = sd(CH4_gd, na.rm = TRUE),

                                                                                     H2_avg_gd = if (exist_H2) mean(H2_gd, na.rm = TRUE) else NA_real_,
                                                                                     H2_SD = if (exist_H2) sd(H2_gd, na.rm = TRUE) else NA_real_,
                                                                                     O2_avg_gd = if (exist_O2) mean(O2_gd, na.rm = TRUE) else NA_real_,
                                                                                     O2_SD = if (exist_O2) sd(O2_gd, na.rm = TRUE) else NA_real_,
                                                                                     Airflow_avg_ls = if (exist_airflow) mean(Airflow, na.rm = TRUE) else NA_real_,
                                                                                     Airflow_SD = if (exist_airflow) sd(Airflow, na.rm = TRUE) else NA_real_,
                                                                                     .groups = "drop")

      }
      incProgress(0.7, detail = "Calculating...")
      #===========================================#
      incProgress(1, detail = "Finalizing...")

      # Empty column eliminate
      df.whole.trt2 <- df.whole.trt2 %>% select(where(~ !all(is.na(.))))

    })

    return(df.whole.trt2)

  })

  # control download button for 'whole trt2'
  shinyjs::disable("download_whole_trt2")

  observe({
    if (!is.null(whole_t2())) {
      shinyjs::enable("download_whole_trt2")
    } else{
      shinyjs::disable("download_whole_trt2")
    }
  })

  output$download_whole_trt2 <- downloadHandler(
    filename = function() {
      paste("Whole_Factor2_", Sys.Date(), ".xlsx", sep = "")
    },
    content = function(file) {
      data_to_save <- whole_t2()
      if (!is.null(data_to_save)){
        openxlsx::write.xlsx(data_to_save, file)

      } else{
        stop("No data available to downloads.")
      }
    }
  )

  ########################################################################################################################
  # Whole average per Factor1 X Factor2
  whole_t1xt2 <- eventReactive(input$calc_whole_trt1xtrt2, {
    req(filt_data(), input$treat1, input$treat2)
    print("== Whole period statistics per Factor1 X Factor2 ==")

    trt_data <- filt_data()

    withProgress(message = "Calculating Whole period statistics - Factor1 X Factor2...", value = 0, {
      #===========================================#
      incProgress(0.2, detail = "Loading data...")

      # optional column check
      exist_H2 <- "H2_gd" %in% colnames(trt_data)
      exist_O2 <- "O2_gd" %in% colnames(trt_data)
      exist_airflow <- "Airflow" %in% colnames(trt_data)
      exist_adjCH4 <- "adjCH4_gd" %in% colnames(trt_data)

      # Check if treatment columns exists, add only if they are non-null
      treatment_cols <- c(input$treat1, input$treat2)
      treatment_cols <- treatment_cols[!is.null(treatment_cols) & nzchar(treatment_cols)]

      #===========================================#
      incProgress(0.5, detail = "Calculating...")

      # Dynamically construct group_by columns
      group_by_cols <- c()

      if (!is.null(treatment_cols)) {
        group_by_cols <- c(treatment_cols)
      }

      # Group and summarise dynamically
      if (isTRUE(input$time_smooth) && exist_adjCH4){
        df.whole.trt1xtrt2 <- trt_data %>% group_by(!!!syms(group_by_cols)) %>% summarise(Nb_visit = n(),
                                                                                         Nb_animal = n_distinct(Farm_name),
                                                                                         Nb_visit_per_animal = Nb_visit / Nb_animal,
                                                                                         CO2_avg_gd = mean(CO2_gd, na.rm = TRUE),
                                                                                         CO2_SD = sd(CO2_gd, na.rm = TRUE),
                                                                                         CH4_avg_gd = mean(CH4_gd, na.rm = TRUE),
                                                                                         CH4_SD = sd(CH4_gd, na.rm = TRUE),
                                                                                         adjCH4_avg_gd = mean(adjCH4_gd, na.rm = TRUE),
                                                                                         adjCH4_SD = sd(adjCH4_gd, na.rm = TRUE),

                                                                                         H2_avg_gd = if (exist_H2) mean(H2_gd, na.rm = TRUE) else NA_real_,
                                                                                         H2_SD = if (exist_H2) sd(H2_gd, na.rm = TRUE) else NA_real_,
                                                                                         O2_avg_gd = if (exist_O2) mean(O2_gd, na.rm = TRUE) else NA_real_,
                                                                                         O2_SD = if (exist_O2) sd(O2_gd, na.rm = TRUE) else NA_real_,
                                                                                         Airflow_avg_ls = if (exist_airflow) mean(Airflow, na.rm = TRUE) else NA_real_,
                                                                                         Airflow_SD = if (exist_airflow) sd(Airflow, na.rm = TRUE) else NA_real_,
                                                                                         .groups = "drop")
      } else{
        df.whole.trt1xtrt2 <- trt_data %>% group_by(!!!syms(group_by_cols)) %>% summarise(Nb_visit = n(),
                                                                                         Nb_animal = n_distinct(Farm_name),
                                                                                         Nb_visit_per_animal = Nb_visit / Nb_animal,
                                                                                         CO2_avg_gd = mean(CO2_gd, na.rm = TRUE),
                                                                                         CO2_SD = sd(CO2_gd, na.rm = TRUE),
                                                                                         CH4_avg_gd = mean(CH4_gd, na.rm = TRUE),
                                                                                         CH4_SD = sd(CH4_gd, na.rm = TRUE),

                                                                                         H2_avg_gd = if (exist_H2) mean(H2_gd, na.rm = TRUE) else NA_real_,
                                                                                         H2_SD = if (exist_H2) sd(H2_gd, na.rm = TRUE) else NA_real_,
                                                                                         O2_avg_gd = if (exist_O2) mean(O2_gd, na.rm = TRUE) else NA_real_,
                                                                                         O2_SD = if (exist_O2) sd(O2_gd, na.rm = TRUE) else NA_real_,
                                                                                         Airflow_avg_ls = if (exist_airflow) mean(Airflow, na.rm = TRUE) else NA_real_,
                                                                                         Airflow_SD = if (exist_airflow) sd(Airflow, na.rm = TRUE) else NA_real_,
                                                                                         .groups = "drop")

      }
      incProgress(0.7, detail = "Calculating...")
      #===========================================#
      incProgress(1, detail = "Finalizing...")

      # Empty column eliminate
      df.whole.trt1xtrt2 <- df.whole.trt1xtrt2 %>% select(where(~ !all(is.na(.))))

    })

    return(df.whole.trt1xtrt2)

  })

  # control download button for 'daily trt2'
  shinyjs::disable("download_whole_trt1xtrt2")

  observe({
    if (!is.null(whole_t1xt2())) {
      shinyjs::enable("download_whole_trt1xtrt2")
    } else{
      shinyjs::disable("download_whole_trt1xtrt2")
    }
  })

  output$download_whole_trt1xtrt2 <- downloadHandler(
    filename = function() {
      paste("Whole_Factor1xFactor2_", Sys.Date(), ".xlsx", sep = "")
    },
    content = function(file) {
      data_to_save <- whole_t1xt2()
      if (!is.null(data_to_save)){
        openxlsx::write.xlsx(data_to_save, file)

      } else{
        stop("No data available to downloads.")
      }
    }
  )

  ########################################################################################################################
  # Running all the Daily analysis
  observeEvent(input$calc_all_whole, {
    shinyjs::click("calc_whole_anim")

    if (!is.null(input$treat1) && nzchar(input$treat1)){
      shinyjs::click("calc_whole_trt1")
    }

    if (!is.null(input$treat2) && nzchar(input$treat2)){
      shinyjs::click("calc_whole_trt2")
    }

    if(!is.null(input$treat1) && nzchar(input$treat1) && !is.null(input$treat2) && nzchar(input$treat2)){
      shinyjs::click("calc_whole_trt1xtrt2")
    }
  })



  #======================================================================================================================#
  # DAILY STATISTICS #
  #======================================================================================================================#

  # Daily average for Entire dataset
  daily_total <- eventReactive(input$calc_daily_entire, {
    # req(filt_data(), input$date_filt_range)
    req(filt_data())
    print("== Daily statistics for entire dataset ==")

    trt_data <- filt_data()
    
    withProgress(message = "Calculating Daily statistics - Entire dataset...", value = 0, {
      #===========================================#
      incProgress(0.2, detail = "Loading data...")

      # date difference calculation from start date: 1st day of sampling = 0
      # date_diff <- abs(as.numeric(as.Date(trt_data$Date_rcd) - as.Date(input$date_filt_range[1])))
      start_date <- min(as.Date(trt_data$Date_rcd), na.rm = TRUE)
      date_diff <- as.numeric(as.Date(trt_data$Date_rcd) - start_date)

      trt_data_jour <- add_column(trt_data, date_diff = date_diff)

      # optional column check
      exist_H2 <- "H2_gd" %in% colnames(trt_data)
      exist_O2 <- "O2_gd" %in% colnames(trt_data)
      exist_airflow <- "Airflow" %in% colnames(trt_data)

      #===========================================#
      incProgress(0.5, detail = "Calculating...")

      if (isTRUE(input$time_smooth)){

      df.daily.total <- trt_data_jour %>% group_by(date_diff) %>% summarise(Date = first(Date_rcd),
                                                                            Nb_visit = n(),
                                                                            Nb_animal = n_distinct(Farm_name),
                                                                            Nb_visit_per_animal = Nb_visit / Nb_animal,
                                                                            CO2_avg_gd = mean(CO2_gd, na.rm = TRUE),
                                                                            CO2_SD = sd(CO2_gd, na.rm = TRUE),
                                                                            CH4_avg_gd = mean(CH4_gd, na.rm = TRUE),
                                                                            CH4_SD = sd(CH4_gd, na.rm = TRUE),
                                                                            adjCH4_avg_gd = mean(adjCH4_gd, na.rm = TRUE),
                                                                            adjCH4_SD = sd(adjCH4_gd, na.rm = TRUE),

                                                                            H2_avg_gd = if (exist_H2) mean(H2_gd, na.rm = TRUE) else NA_real_,
                                                                            H2_SD = if (exist_H2) sd(H2_gd, na.rm = TRUE) else NA_real_,
                                                                            O2_avg_gd = if (exist_O2) mean(O2_gd, na.rm = TRUE) else NA_real_,
                                                                            O2_SD = if (exist_O2) sd(O2_gd, na.rm = TRUE) else NA_real_,
                                                                            Airflow_avg_ls = if (exist_airflow) mean(Airflow, na.rm = TRUE) else NA_real_,
                                                                            Airflow_SD = if (exist_airflow) sd(Airflow, na.rm = TRUE) else NA_real_,
                                                                            .groups = "drop")
      } else{
        df.daily.total <- trt_data_jour %>% group_by(date_diff) %>% summarise(Date = first(Date_rcd),
                                                                              Nb_visit = n(),
                                                                              Nb_animal = n_distinct(Farm_name),
                                                                              Nb_visit_per_animal = Nb_visit / Nb_animal,
                                                                              CO2_avg_gd = mean(CO2_gd, na.rm = TRUE),
                                                                              CO2_SD = sd(CO2_gd, na.rm = TRUE),
                                                                              CH4_avg_gd = mean(CH4_gd, na.rm = TRUE),
                                                                              CH4_SD = sd(CH4_gd, na.rm = TRUE),

                                                                              H2_avg_gd = if (exist_H2) mean(H2_gd, na.rm = TRUE) else NA_real_,
                                                                              H2_SD = if (exist_H2) sd(H2_gd, na.rm = TRUE) else NA_real_,
                                                                              O2_avg_gd = if (exist_O2) mean(O2_gd, na.rm = TRUE) else NA_real_,
                                                                              O2_SD = if (exist_O2) sd(O2_gd, na.rm = TRUE) else NA_real_,
                                                                              Airflow_avg_ls = if (exist_airflow) mean(Airflow, na.rm = TRUE) else NA_real_,
                                                                              Airflow_SD = if (exist_airflow) sd(Airflow, na.rm = TRUE) else NA_real_,
                                                                              .groups = "drop")

      }

      incProgress(0.7, detail = "Calculating...")
      #===========================================#
      incProgress(1, detail = "Finalizing...")
      # Empty column eliminate
      df.daily.total <- df.daily.total %>% select(where(~ !all(is.na(.))))

    })

    return(df.daily.total)
  })

  # control download button for 'daily entire'
  shinyjs::disable("download_daily_entire")

  observe({
    if (!is.null(daily_total())) {
      shinyjs::enable("download_daily_entire")
    } else{
      shinyjs::disable("download_daily_entire")
    }
  })

  output$download_daily_entire <- downloadHandler(
    filename = function() {
      paste("Daily_Entire_", Sys.Date(), ".xlsx", sep = "")
    },
    content = function(file) {
      data_to_save <- daily_total()
      if (!is.null(data_to_save)){
        openxlsx::write.xlsx(data_to_save, file)

      } else{
        stop("No data available to downloads.")
      }
    }
  )

  ########################################################################################################################
  # Daily average per Animal
  daily_anim <- eventReactive(input$calc_daily_anim, {
    # req(filt_data(), input$date_filt_range)
    req(filt_data())
    print("== Daily statistics per Animal ==")

    trt_data <- filt_data()
    
    print(head(trt_data))

    withProgress(message = "Calculating Daily statistics - Animal...", value = 0, {
      #===========================================#
      incProgress(0.2, detail = "Loading data...")

      # date difference calculation from start date: 1st day of sampling = 0
      # date_diff <- abs(as.numeric(as.Date(trt_data$Date_rcd) - as.Date(input$date_filt_range[1])))
      start_date <- min(as.Date(trt_data$Date_rcd), na.rm = TRUE)
      date_diff <- as.numeric(as.Date(trt_data$Date_rcd) - start_date)
      trt_data_jour <- add_column(trt_data, date_diff = date_diff)

      # optional column check
      exist_H2 <- "H2_gd" %in% colnames(trt_data)
      exist_O2 <- "O2_gd" %in% colnames(trt_data)
      exist_airflow <- "Airflow" %in% colnames(trt_data)
      exist_adjCH4 <- "adjCH4_gd" %in% colnames(trt_data)

      # Check if treatment columns exists, add only if they are non-null
      treatment_cols <- c(input$treat1, input$treat2)
      treatment_cols <- treatment_cols[!is.null(treatment_cols) & nzchar(treatment_cols)]

      #===========================================#
      incProgress(0.5, detail = "Calculating...")

      if (isTRUE(input$time_smooth) && exist_adjCH4){

      df.daily.anim <- trt_data_jour %>% group_by(Farm_name, date_diff) %>% summarise(Farm_name = unique(Farm_name),
                                                                                      across(all_of(treatment_cols), ~first(na.omit(.)), .names = "{.col}"),
                                                                                      Date = first(Date_rcd),
                                                                                      Nb_visit = n(),
                                                                                      CO2_avg_gd = mean(CO2_gd, na.rm = TRUE),
                                                                                      CO2_SD = sd(CO2_gd, na.rm = TRUE),
                                                                                      CH4_avg_gd = mean(CH4_gd, na.rm = TRUE),
                                                                                      CH4_SD = sd(CH4_gd, na.rm = TRUE),
                                                                                      adjCH4_avg_gd = mean(adjCH4_gd, na.rm = TRUE),
                                                                                      adjCH4_SD = sd(adjCH4_gd, na.rm = TRUE),

                                                                                      H2_avg_gd = if (exist_H2) mean(H2_gd, na.rm = TRUE) else NA_real_,
                                                                                      H2_SD = if (exist_H2) sd(H2_gd, na.rm = TRUE) else NA_real_,
                                                                                      O2_avg_gd = if (exist_O2) mean(O2_gd, na.rm = TRUE) else NA_real_,
                                                                                      O2_SD = if (exist_O2) sd(O2_gd, na.rm = TRUE) else NA_real_,
                                                                                      Airflow_avg_ls = if (exist_airflow) mean(Airflow, na.rm = TRUE) else NA_real_,
                                                                                      Airflow_SD = if (exist_airflow) sd(Airflow, na.rm = TRUE) else NA_real_,
                                                                                      .groups = "drop")
      } else{
        df.daily.anim <- trt_data_jour %>% group_by(Farm_name, date_diff) %>% summarise(Farm_name = unique(Farm_name),
                                                                                        across(all_of(treatment_cols), ~first(na.omit(.)), .names = "{.col}"),
                                                                                        Date = first(Date_rcd),
                                                                                        Nb_visit = n(),
                                                                                        CO2_avg_gd = mean(CO2_gd, na.rm = TRUE),
                                                                                        CO2_SD = sd(CO2_gd, na.rm = TRUE),
                                                                                        CH4_avg_gd = mean(CH4_gd, na.rm = TRUE),
                                                                                        CH4_SD = sd(CH4_gd, na.rm = TRUE),

                                                                                        H2_avg_gd = if (exist_H2) mean(H2_gd, na.rm = TRUE) else NA_real_,
                                                                                        H2_SD = if (exist_H2) sd(H2_gd, na.rm = TRUE) else NA_real_,
                                                                                        O2_avg_gd = if (exist_O2) mean(O2_gd, na.rm = TRUE) else NA_real_,
                                                                                        O2_SD = if (exist_O2) sd(O2_gd, na.rm = TRUE) else NA_real_,
                                                                                        Airflow_avg_ls = if (exist_airflow) mean(Airflow, na.rm = TRUE) else NA_real_,
                                                                                        Airflow_SD = if (exist_airflow) sd(Airflow, na.rm = TRUE) else NA_real_,
                                                                                        .groups = "drop")

      }

      incProgress(0.7, detail = "Calculating...")
      #===========================================#
      incProgress(1, detail = "Finalizing...")

      # Empty column eliminate
      df.daily.anim <- df.daily.anim %>% select(where(~ !all(is.na(.))))

      })

    return(df.daily.anim)
  })


  # control download button for 'daily animal'
  shinyjs::disable("download_daily_anim")

  observe({
    if (!is.null(daily_anim())) {
      shinyjs::enable("download_daily_anim")
    } else{
      shinyjs::disable("download_daily_anim")
    }
  })

  output$download_daily_anim <- downloadHandler(
    filename = function() {
      paste("Daily_Animal_", Sys.Date(), ".xlsx", sep = "")
    },
    content = function(file) {
      data_to_save <- daily_anim()
      if (!is.null(data_to_save)){
        openxlsx::write.xlsx(data_to_save, file)

      } else{
        stop("No data available to downloads.")
      }
    }
  )

  ########################################################################################################################
  # Daily average per Factor1
  daily_t1 <- eventReactive(input$calc_daily_trt1, {
    # req(filt_data(), input$date_filt_range, input$treat1)
    req(filt_data(), input$treat1)
    print("== Daily statistics per Factor1 ==")

    trt_data <- filt_data()

    withProgress(message = "Calculating Daily statistics - Factor1...", value = 0, {
      #===========================================#
      incProgress(0.2, detail = "Loading data...")
      # Date difference calculation from start date: 1st day of sampling = 0
      # date_diff <- abs(as.numeric(as.Date(input$date_filt_range[1]) - as.Date(trt_data$Date_rcd)))
      start_date <- min(as.Date(trt_data$Date_rcd), na.rm = TRUE)
      date_diff <- as.numeric(as.Date(trt_data$Date_rcd) - start_date)
      trt_data_jour <- add_column(trt_data, date_diff = date_diff)

      # optional column check
      exist_H2 <- "H2_gd" %in% colnames(trt_data)
      exist_O2 <- "O2_gd" %in% colnames(trt_data)
      exist_airflow <- "Airflow" %in% colnames(trt_data)
      exist_adjCH4 <- "adjCH4_gd" %in% colnames(trt_data)

      # Check if treatment columns exists, add only if they are non-null
      treatment_cols <- input$treat1
      treatment_cols <- treatment_cols[!is.null(treatment_cols) & nzchar(treatment_cols)]
      #===========================================#
      incProgress(0.5, detail = "Calculating...")

      # Dynamically construct group_by columns
      group_by_cols <- c("date_diff")
      if (!is.null(treatment_cols)) {
        group_by_cols <- c(group_by_cols, treatment_cols)
      }

      # Group and summarise dynamically
      if (isTRUE(input$time_smooth) && exist_adjCH4){

      df.daily.trt1 <- trt_data_jour %>% group_by(!!!syms(group_by_cols)) %>% summarise(Date = first(Date_rcd),
                                                                                        Nb_visit = n(),
                                                                                        Nb_animal = n_distinct(Farm_name),
                                                                                        Nb_visit_per_animal = Nb_visit / Nb_animal,
                                                                                        CO2_avg_gd = mean(CO2_gd, na.rm = TRUE),
                                                                                        CO2_SD = sd(CO2_gd, na.rm = TRUE),
                                                                                        CH4_avg_gd = mean(CH4_gd, na.rm = TRUE),
                                                                                        CH4_SD = sd(CH4_gd, na.rm = TRUE),
                                                                                        adjCH4_avg_gd = mean(adjCH4_gd, na.rm = TRUE),
                                                                                        adjCH4_SD = sd(adjCH4_gd, na.rm = TRUE),

                                                                                        H2_avg_gd = if (exist_H2) mean(H2_gd, na.rm = TRUE) else NA_real_,
                                                                                        H2_SD = if (exist_H2) sd(H2_gd, na.rm = TRUE) else NA_real_,
                                                                                        O2_avg_gd = if (exist_O2) mean(O2_gd, na.rm = TRUE) else NA_real_,
                                                                                        O2_SD = if (exist_O2) sd(O2_gd, na.rm = TRUE) else NA_real_,
                                                                                        Airflow_avg_ls = if (exist_airflow) mean(Airflow, na.rm = TRUE) else NA_real_,
                                                                                        Airflow_SD = if (exist_airflow) sd(Airflow, na.rm = TRUE) else NA_real_,
                                                                                        .groups = "drop")
      } else{
        df.daily.trt1 <- trt_data_jour %>% group_by(!!!syms(group_by_cols)) %>% summarise(Date = first(Date_rcd),
                                                                                          Nb_visit = n(),
                                                                                          Nb_animal = n_distinct(Farm_name),
                                                                                          Nb_visit_per_animal = Nb_visit / Nb_animal,
                                                                                          CO2_avg_gd = mean(CO2_gd, na.rm = TRUE),
                                                                                          CO2_SD = sd(CO2_gd, na.rm = TRUE),
                                                                                          CH4_avg_gd = mean(CH4_gd, na.rm = TRUE),
                                                                                          CH4_SD = sd(CH4_gd, na.rm = TRUE),

                                                                                          H2_avg_gd = if (exist_H2) mean(H2_gd, na.rm = TRUE) else NA_real_,
                                                                                          H2_SD = if (exist_H2) sd(H2_gd, na.rm = TRUE) else NA_real_,
                                                                                          O2_avg_gd = if (exist_O2) mean(O2_gd, na.rm = TRUE) else NA_real_,
                                                                                          O2_SD = if (exist_O2) sd(O2_gd, na.rm = TRUE) else NA_real_,
                                                                                          Airflow_avg_ls = if (exist_airflow) mean(Airflow, na.rm = TRUE) else NA_real_,
                                                                                          Airflow_SD = if (exist_airflow) sd(Airflow, na.rm = TRUE) else NA_real_,
                                                                                          .groups = "drop")

      }
      incProgress(0.7, detail = "Calculating...")
      #===========================================#
      incProgress(1, detail = "Finalizing...")

      # Empty column eliminate
      df.daily.trt1 <- df.daily.trt1 %>% select(where(~ !all(is.na(.))))

    })

    return(df.daily.trt1)

  })

  # control download button for 'daily trt1'
  shinyjs::disable("download_daily_trt1")

  observe({
    if (!is.null(daily_t1())) {
      shinyjs::enable("download_daily_trt1")
    } else{
      shinyjs::disable("download_daily_trt1")
    }
  })

  output$download_daily_trt1 <- downloadHandler(
    filename = function() {
      paste("Daily_Factor1_", Sys.Date(), ".xlsx", sep = "")
    },
    content = function(file) {
      data_to_save <- daily_t1()
      if (!is.null(data_to_save)){
        openxlsx::write.xlsx(data_to_save, file)

      } else{
        stop("No data available to downloads.")
      }
    }
  )

  ########################################################################################################################
  # Daily average per Factor2
  daily_t2 <- eventReactive(input$calc_daily_trt2,{
    # req(filt_data(), input$date_filt_range, input$treat2)
    req(filt_data(), input$treat2)
    print("== Daily statistics per Factor2 ==")

    trt_data <- filt_data()

    withProgress(message = "Calculating Daily statistics - Factor2...", value = 0, {
      #===========================================#
      incProgress(0.2, detail = "Loading data...")

      # Date difference calculation from start date: 1st day of sampling = 0
      # date_diff <- abs(as.numeric(as.Date(input$date_filt_range[1]) - as.Date(trt_data$Date_rcd)))
      start_date <- min(as.Date(trt_data$Date_rcd), na.rm = TRUE)
      date_diff <- as.numeric(as.Date(trt_data$Date_rcd) - start_date)
      trt_data_jour <- add_column(trt_data, date_diff = date_diff)

      # optional column check
      exist_H2 <- "H2_gd" %in% colnames(trt_data)
      exist_O2 <- "O2_gd" %in% colnames(trt_data)
      exist_airflow <- "Airflow" %in% colnames(trt_data)
      exist_adjCH4 <- "adjCH4_gd" %in% colnames(trt_data)

      # Check if treatment columns exists, add only if they are non-null
      treatment_cols <- input$treat2
      treatment_cols <- treatment_cols[!is.null(treatment_cols) & nzchar(treatment_cols)]

      #===========================================#
      incProgress(0.5, detail = "Calculating...")

      # Dynamically construct group_by columns
      group_by_cols <- c("date_diff")
      if (!is.null(treatment_cols)) {
        group_by_cols <- c(group_by_cols, treatment_cols)
      }

      if (isTRUE(input$time_smooth) && exist_adjCH4){
      # Group and summarise dynamically
      df.daily.trt2 <- trt_data_jour %>% group_by(!!!syms(group_by_cols)) %>% summarise(Date = first(Date_rcd),
                                                                                        Nb_visit = n(),
                                                                                        Nb_animal = n_distinct(Farm_name),
                                                                                        Nb_visit_per_animal = Nb_visit / Nb_animal,
                                                                                        CO2_avg_gd = mean(CO2_gd, na.rm = TRUE),
                                                                                        CO2_SD = sd(CO2_gd, na.rm = TRUE),
                                                                                        CH4_avg_gd = mean(CH4_gd, na.rm = TRUE),
                                                                                        CH4_SD = sd(CH4_gd, na.rm = TRUE),
                                                                                        adjCH4_avg_gd = mean(adjCH4_gd, na.rm = TRUE),
                                                                                        adjCH4_SD = sd(adjCH4_gd, na.rm = TRUE),

                                                                                        H2_avg_gd = if (exist_H2) mean(H2_gd, na.rm = TRUE) else NA_real_,
                                                                                        H2_SD = if (exist_H2) sd(H2_gd, na.rm = TRUE) else NA_real_,
                                                                                        O2_avg_gd = if (exist_O2) mean(O2_gd, na.rm = TRUE) else NA_real_,
                                                                                        O2_SD = if (exist_O2) sd(O2_gd, na.rm = TRUE) else NA_real_,
                                                                                        Airflow_avg_ls = if (exist_airflow) mean(Airflow, na.rm = TRUE) else NA_real_,
                                                                                        Airflow_SD = if (exist_airflow) sd(Airflow, na.rm = TRUE) else NA_real_,
                                                                                        .groups = "drop")
      } else{
        df.daily.trt2 <- trt_data_jour %>% group_by(!!!syms(group_by_cols)) %>% summarise(Date = first(Date_rcd),
                                                                                          Nb_visit = n(),
                                                                                          Nb_animal = n_distinct(Farm_name),
                                                                                          Nb_visit_per_animal = Nb_visit / Nb_animal,
                                                                                          CO2_avg_gd = mean(CO2_gd, na.rm = TRUE),
                                                                                          CO2_SD = sd(CO2_gd, na.rm = TRUE),
                                                                                          CH4_avg_gd = mean(CH4_gd, na.rm = TRUE),
                                                                                          CH4_SD = sd(CH4_gd, na.rm = TRUE),

                                                                                          H2_avg_gd = if (exist_H2) mean(H2_gd, na.rm = TRUE) else NA_real_,
                                                                                          H2_SD = if (exist_H2) sd(H2_gd, na.rm = TRUE) else NA_real_,
                                                                                          O2_avg_gd = if (exist_O2) mean(O2_gd, na.rm = TRUE) else NA_real_,
                                                                                          O2_SD = if (exist_O2) sd(O2_gd, na.rm = TRUE) else NA_real_,
                                                                                          Airflow_avg_ls = if (exist_airflow) mean(Airflow, na.rm = TRUE) else NA_real_,
                                                                                          Airflow_SD = if (exist_airflow) sd(Airflow, na.rm = TRUE) else NA_real_,
                                                                                          .groups = "drop")
      }
      incProgress(0.7, detail = "Calculating...")
      #===========================================#
      incProgress(1, detail = "Finalizing...")

      # Empty column eliminate
      df.daily.trt2 <- df.daily.trt2 %>% select(where(~ !all(is.na(.))))

    })

    return(df.daily.trt2)

  })

  # control download button for 'daily trt2'
  shinyjs::disable("download_daily_trt2")

  observe({
    if (!is.null(daily_t2())) {
      shinyjs::enable("download_daily_trt2")
    } else{
      shinyjs::disable("download_daily_trt2")
    }
  })

  output$download_daily_trt2 <- downloadHandler(
    filename = function() {
      paste("Daily_Factor2_", Sys.Date(), ".xlsx", sep = "")
    },
    content = function(file) {
      data_to_save <- daily_t2()
      if (!is.null(data_to_save)){
        openxlsx::write.xlsx(data_to_save, file)

      } else{
        stop("No data available to downloads.")
      }
    }
  )

  ########################################################################################################################
  # Daily average per Factor1 X Factor2
  daily_t1xt2 <- eventReactive(input$calc_daily_trt1xtrt2, {
    # req(filt_data(), input$date_filt_range, input$treat1, input$treat2)
    req(filt_data(), input$treat1, input$treat2)
    print("== Daily statistics per Factor1 X Factor2 ==")

    trt_data <- filt_data()

    withProgress(message = "Calculating Daily statistics - Factor1 X Factor2...", value = 0, {
      #===========================================#
      incProgress(0.2, detail = "Loading data...")
      # Date difference calculation from start date: 1st day of sampling = 0
      # date_diff <- abs(as.numeric(as.Date(input$date_filt_range[1]) - as.Date(trt_data$Date_rcd)))
      start_date <- min(as.Date(trt_data$Date_rcd), na.rm = TRUE)
      date_diff <- as.numeric(as.Date(trt_data$Date_rcd) - start_date)
      trt_data_jour <- add_column(trt_data, date_diff = date_diff)

      # optional column check
      exist_H2 <- "H2_gd" %in% colnames(trt_data)
      exist_O2 <- "O2_gd" %in% colnames(trt_data)
      exist_airflow <- "Airflow" %in% colnames(trt_data)
      exist_adjCH4 <- "adjCH4_gd" %in% colnames(trt_data)

      # Check if treatment columns exists, add only if they are non-null
      treatment_cols <- c(input$treat1, input$treat2)
      treatment_cols <- treatment_cols[!is.null(treatment_cols) & nzchar(treatment_cols)]

      #===========================================#
      incProgress(0.5, detail = "Calculating...")

      # Dynamically construct group_by columns
      group_by_cols <- c("date_diff")

      if (!is.null(treatment_cols)) {
        group_by_cols <- c(group_by_cols, treatment_cols)
      }

      # Group and summarise dynamically
      if (isTRUE(input$time_smooth) && exist_adjCH4){
      df.daily.trt1xtrt2 <- trt_data_jour %>% group_by(!!!syms(group_by_cols)) %>% summarise(Date = first(Date_rcd),
                                                                                             Nb_visit = n(),
                                                                                             Nb_animal = n_distinct(Farm_name),
                                                                                             Nb_visit_per_animal = Nb_visit / Nb_animal,
                                                                                             CO2_avg_gd = mean(CO2_gd, na.rm = TRUE),
                                                                                             CO2_SD = sd(CO2_gd, na.rm = TRUE),
                                                                                             CH4_avg_gd = mean(CH4_gd, na.rm = TRUE),
                                                                                             CH4_SD = sd(CH4_gd, na.rm = TRUE),
                                                                                             adjCH4_avg_gd = mean(adjCH4_gd, na.rm = TRUE),
                                                                                             adjCH4_SD = sd(adjCH4_gd, na.rm = TRUE),

                                                                                             H2_avg_gd = if (exist_H2) mean(H2_gd, na.rm = TRUE) else NA_real_,
                                                                                             H2_SD = if (exist_H2) sd(H2_gd, na.rm = TRUE) else NA_real_,
                                                                                             O2_avg_gd = if (exist_O2) mean(O2_gd, na.rm = TRUE) else NA_real_,
                                                                                             O2_SD = if (exist_O2) sd(O2_gd, na.rm = TRUE) else NA_real_,
                                                                                             Airflow_avg_ls = if (exist_airflow) mean(Airflow, na.rm = TRUE) else NA_real_,
                                                                                             Airflow_SD = if (exist_airflow) sd(Airflow, na.rm = TRUE) else NA_real_,
                                                                                             .groups = "drop")
      } else{
        df.daily.trt1xtrt2 <- trt_data_jour %>% group_by(!!!syms(group_by_cols)) %>% summarise(Date = first(Date_rcd),
                                                                                               Nb_visit = n(),
                                                                                               Nb_animal = n_distinct(Farm_name),
                                                                                               Nb_visit_per_animal = Nb_visit / Nb_animal,
                                                                                               CO2_avg_gd = mean(CO2_gd, na.rm = TRUE),
                                                                                               CO2_SD = sd(CO2_gd, na.rm = TRUE),
                                                                                               CH4_avg_gd = mean(CH4_gd, na.rm = TRUE),
                                                                                               CH4_SD = sd(CH4_gd, na.rm = TRUE),

                                                                                               H2_avg_gd = if (exist_H2) mean(H2_gd, na.rm = TRUE) else NA_real_,
                                                                                               H2_SD = if (exist_H2) sd(H2_gd, na.rm = TRUE) else NA_real_,
                                                                                               O2_avg_gd = if (exist_O2) mean(O2_gd, na.rm = TRUE) else NA_real_,
                                                                                               O2_SD = if (exist_O2) sd(O2_gd, na.rm = TRUE) else NA_real_,
                                                                                               Airflow_avg_ls = if (exist_airflow) mean(Airflow, na.rm = TRUE) else NA_real_,
                                                                                               Airflow_SD = if (exist_airflow) sd(Airflow, na.rm = TRUE) else NA_real_,
                                                                                               .groups = "drop")

      }
      incProgress(0.7, detail = "Calculating...")
      #===========================================#
      incProgress(1, detail = "Finalizing...")

      # Empty column eliminate
      df.daily.trt1xtrt2 <- df.daily.trt1xtrt2 %>% select(where(~ !all(is.na(.))))

    })

    return(df.daily.trt1xtrt2)

  })

  # control download button for 'daily trt2'
  shinyjs::disable("download_daily_trt1xtrt2")

  observe({
    if (!is.null(daily_t1xt2())) {
      shinyjs::enable("download_daily_trt1xtrt2")
    } else{
      shinyjs::disable("download_daily_trt1xtrt2")
    }
  })

  output$download_daily_trt1xtrt2 <- downloadHandler(
    filename = function() {
      paste("Daily_Factor1xFactor2_", Sys.Date(), ".xlsx", sep = "")
    },
    content = function(file) {
      data_to_save <- daily_t1xt2()
      if (!is.null(data_to_save)){
        openxlsx::write.xlsx(data_to_save, file)

      } else{
        stop("No data available to downloads.")
      }
    }
  )

  ########################################################################################################################
  # Running all the Daily analysis
  observeEvent(input$calc_all_daily, {
    shinyjs::click("calc_daily_entire")
    shinyjs::click("calc_daily_anim")

    if (!is.null(input$treat1) && nzchar(input$treat1)){
      shinyjs::click("calc_daily_trt1")
    }

    if (!is.null(input$treat2) && nzchar(input$treat2)){
      shinyjs::click("calc_daily_trt2")
    }

    if(!is.null(input$treat1) && nzchar(input$treat1) && !is.null(input$treat2) && nzchar(input$treat2)){
      shinyjs::click("calc_daily_trt1xtrt2")
    }
  })


  #======================================================================================================================#
  # HOURLY STATISTICS #
  #======================================================================================================================#

  # Hourly average for Entire dataset
  hourly_total <- eventReactive(input$calc_hourly_entire, {
    req(filt_data())
    print("== Hourly statistics for Entire dataset ==")

    trt_data <- filt_data()

    withProgress(message = "Calculating Hourly statistics - Entire dataset...", value = 0, {
      #===========================================#
      incProgress(0.2, detail = "Loading data...")
      # optional column check
      exist_H2 <- "H2_gd" %in% colnames(trt_data)
      exist_O2 <- "O2_gd" %in% colnames(trt_data)
      exist_airflow <- "Airflow" %in% colnames(trt_data)
      exist_adjCH4 <- "adjCH4_gd" %in% colnames(trt_data)

      #===========================================#
      incProgress(0.5, detail = "Calculating...")

      trt_data <- trt_data %>% mutate(Time_zone = as.numeric(Time_zone))

      if (isTRUE(input$time_smooth) && exist_adjCH4){
      df.hourly.total <- trt_data %>% group_by(Time_zone) %>% summarise(Duration_minute = sum(Duration_sec, na.rm = TRUE)/60,
                                                                            Duration = sprintf("%d:%02d:%02d", sum(Duration_sec, na.rm = TRUE) %/% 3600,
                                                                                               (sum(Duration_sec, na.rm = TRUE) %% 3600) %/% 60,
                                                                                               sum(Duration_sec, na.rm = TRUE) %% 60),
                                                                            Nb_visit = n(),
                                                                            Nb_animal = n_distinct(Farm_name),
                                                                            Nb_visit_per_animal = Nb_visit / Nb_animal,
                                                                            CO2_avg_gd = mean(CO2_gd, na.rm = TRUE),
                                                                            CO2_SD = sd(CO2_gd, na.rm = TRUE),
                                                                            CH4_avg_gd = mean(CH4_gd, na.rm = TRUE),
                                                                            CH4_SD = sd(CH4_gd, na.rm = TRUE),
                                                                            adjCH4_avg_gd = mean(adjCH4_gd, na.rm = TRUE),
                                                                            adjCH4_SD = sd(adjCH4_gd, na.rm = TRUE),

                                                                            H2_avg_gd = if (exist_H2) mean(H2_gd, na.rm = TRUE) else NA_real_,
                                                                            H2_SD = if (exist_H2) sd(H2_gd, na.rm = TRUE) else NA_real_,
                                                                            O2_avg_gd = if (exist_O2) mean(O2_gd, na.rm = TRUE) else NA_real_,
                                                                            O2_SD = if (exist_O2) sd(O2_gd, na.rm = TRUE) else NA_real_,
                                                                            Airflow_avg_ls = if (exist_airflow) mean(Airflow, na.rm = TRUE) else NA_real_,
                                                                            Airflow_SD = if (exist_airflow) sd(Airflow, na.rm = TRUE) else NA_real_,
                                                                            .groups = "drop")
      } else{
        df.hourly.total <- trt_data %>% group_by(Time_zone) %>% summarise(Duration_minute = sum(Duration_sec, na.rm = TRUE)/60,
                                                                          Duration = sprintf("%d:%02d:%02d", sum(Duration_sec, na.rm = TRUE) %/% 3600,
                                                                                             (sum(Duration_sec, na.rm = TRUE) %% 3600) %/% 60,
                                                                                             sum(Duration_sec, na.rm = TRUE) %% 60),
                                                                          Nb_visit = n(),
                                                                          Nb_animal = n_distinct(Farm_name),
                                                                          Nb_visit_per_animal = Nb_visit / Nb_animal,
                                                                          CO2_avg_gd = mean(CO2_gd, na.rm = TRUE),
                                                                          CO2_SD = sd(CO2_gd, na.rm = TRUE),
                                                                          CH4_avg_gd = mean(CH4_gd, na.rm = TRUE),
                                                                          CH4_SD = sd(CH4_gd, na.rm = TRUE),

                                                                          H2_avg_gd = if (exist_H2) mean(H2_gd, na.rm = TRUE) else NA_real_,
                                                                          H2_SD = if (exist_H2) sd(H2_gd, na.rm = TRUE) else NA_real_,
                                                                          O2_avg_gd = if (exist_O2) mean(O2_gd, na.rm = TRUE) else NA_real_,
                                                                          O2_SD = if (exist_O2) sd(O2_gd, na.rm = TRUE) else NA_real_,
                                                                          Airflow_avg_ls = if (exist_airflow) mean(Airflow, na.rm = TRUE) else NA_real_,
                                                                          Airflow_SD = if (exist_airflow) sd(Airflow, na.rm = TRUE) else NA_real_,
                                                                          .groups = "drop")
      }
      incProgress(0.7, detail = "Calculating...")
      #===========================================#
      incProgress(1, detail = "Finalizing...")
      # Empty column eliminate
      df.hourly.total <- df.hourly.total %>% select(where(~ !all(is.na(.))))

    })

    return(df.hourly.total)

  })

  # control download button for 'hourly total'
  shinyjs::disable("download_hourly_entire")

  observe({
    if (!is.null(hourly_total())) {
      shinyjs::enable("download_hourly_entire")
    } else{
      shinyjs::disable("download_hourly_entire")
    }
  })

  output$download_hourly_entire <- downloadHandler(
    filename = function() {
      paste("Hourly_Entire_", Sys.Date(), ".xlsx", sep = "")
    },
    content = function(file) {
      data_to_save <- hourly_total()
      if (!is.null(data_to_save)){
        openxlsx::write.xlsx(data_to_save, file)

      } else{
        stop("No data available to downloads.")
      }
    }
  )

  ########################################################################################################################
  # Hourly average per Animal
  hourly_anim <- eventReactive(input$calc_hourly_anim,{
    req(filt_data())
    print("== Hourly statistics per Animal ==")

    trt_data <- filt_data()

    withProgress(message = "Calculating Hourly statistics - Animal...", value = 0, {
      #===========================================#
      incProgress(0.2, detail = "Loading data...")
      # optional column check
      exist_H2 <- "H2_gd" %in% colnames(trt_data)
      exist_O2 <- "O2_gd" %in% colnames(trt_data)
      exist_airflow <- "Airflow" %in% colnames(trt_data)
      exist_adjCH4 <- "adjCH4_gd" %in% colnames(trt_data)

      # Check if treatment columns exists, add only if they are non-null
      treatment_cols <- c(input$treat1, input$treat2)
      treatment_cols <- treatment_cols[!is.null(treatment_cols) & treatment_cols != ""]

      #===========================================#
      incProgress(0.5, detail = "Calculating...")
      # Group by Farm_name and Time_zone, without treatment_cols

      if (isTRUE(input$time_smooth) && exist_adjCH4){
      df.hourly.anim <- trt_data %>% group_by(Farm_name, Time_zone) %>% summarise(across(all_of(treatment_cols), ~first(na.omit(.)), .names = "{.col}"),
                                                                                  Duration_minute = sum(Duration_sec, na.rm = TRUE)/60,
                                                                                  Duration = sprintf("%d:%02d:%02d", sum(Duration_sec, na.rm = TRUE) %/% 3600,
                                                                                                     (sum(Duration_sec, na.rm = TRUE) %% 3600) %/% 60,
                                                                                                     sum(Duration_sec, na.rm = TRUE) %% 60),
                                                                                  Nb_visit = n(),
                                                                                  CO2_avg_gd = mean(CO2_gd, na.rm = TRUE),
                                                                                  CO2_SD = sd(CO2_gd, na.rm = TRUE),
                                                                                  CH4_avg_gd = mean(CH4_gd, na.rm = TRUE),
                                                                                  CH4_SD = sd(CH4_gd, na.rm = TRUE),
                                                                                  adjCH4_avg_gd = mean(adjCH4_gd, na.rm = TRUE),
                                                                                  adjCH4_SD = sd(adjCH4_gd, na.rm = TRUE),

                                                                                  H2_avg_gd = if (exist_H2) mean(H2_gd, na.rm = TRUE) else NA_real_,
                                                                                  H2_SD = if (exist_H2) sd(H2_gd, na.rm = TRUE) else NA_real_,
                                                                                  O2_avg_gd = if (exist_O2) mean(O2_gd, na.rm = TRUE) else NA_real_,
                                                                                  O2_SD = if (exist_O2) sd(O2_gd, na.rm = TRUE) else NA_real_,
                                                                                  Airflow_avg_ls = if (exist_airflow) mean(Airflow, na.rm = TRUE) else NA_real_,
                                                                                  Airflow_SD = if (exist_airflow) sd(Airflow, na.rm = TRUE) else NA_real_,
                                                                                  .groups = "drop")
      } else{
        df.hourly.anim <- trt_data %>% group_by(Farm_name, Time_zone) %>% summarise(across(all_of(treatment_cols), ~first(na.omit(.)), .names = "{.col}"),
                                                                                    Duration_minute = sum(Duration_sec, na.rm = TRUE)/60,
                                                                                    Duration = sprintf("%d:%02d:%02d", sum(Duration_sec, na.rm = TRUE) %/% 3600,
                                                                                                       (sum(Duration_sec, na.rm = TRUE) %% 3600) %/% 60,
                                                                                                       sum(Duration_sec, na.rm = TRUE) %% 60),
                                                                                    Nb_visit = n(),
                                                                                    CO2_avg_gd = mean(CO2_gd, na.rm = TRUE),
                                                                                    CO2_SD = sd(CO2_gd, na.rm = TRUE),
                                                                                    CH4_avg_gd = mean(CH4_gd, na.rm = TRUE),
                                                                                    CH4_SD = sd(CH4_gd, na.rm = TRUE),

                                                                                    H2_avg_gd = if (exist_H2) mean(H2_gd, na.rm = TRUE) else NA_real_,
                                                                                    H2_SD = if (exist_H2) sd(H2_gd, na.rm = TRUE) else NA_real_,
                                                                                    O2_avg_gd = if (exist_O2) mean(O2_gd, na.rm = TRUE) else NA_real_,
                                                                                    O2_SD = if (exist_O2) sd(O2_gd, na.rm = TRUE) else NA_real_,
                                                                                    Airflow_avg_ls = if (exist_airflow) mean(Airflow, na.rm = TRUE) else NA_real_,
                                                                                    Airflow_SD = if (exist_airflow) sd(Airflow, na.rm = TRUE) else NA_real_,
                                                                                    .groups = "drop")

      }

      incProgress(0.7, detail = "Calculating...")
      #===========================================#
      incProgress(1, detail = "Finalizing...")
      # Empty column eliminate
      df.hourly.anim <- df.hourly.anim %>% select(where(~ !all(is.na(.))))

    })

    return(df.hourly.anim)

  })

  # control download button for 'hourly animal'
  shinyjs::disable("download_hourly_anim")

  observe({
    if (!is.null(hourly_anim())) {
      shinyjs::enable("download_hourly_anim")
    } else{
      shinyjs::disable("download_hourly_anim")
    }
  })

  output$download_hourly_anim <- downloadHandler(
    filename = function() {
      paste("Hourly_Animal_", Sys.Date(), ".xlsx", sep = "")
    },
    content = function(file) {
      data_to_save <- hourly_anim()
      if (!is.null(data_to_save)){
        openxlsx::write.xlsx(data_to_save, file)

      } else{
        stop("No data available to downloads.")
      }
    }
  )

  ########################################################################################################################
  # Hourly average per Factor1
  hourly_t1 <- eventReactive(input$calc_hourly_trt1,{
    req(filt_data(), input$treat1)
    print("== Hourly statistics per Factor1 ==")

    trt_data <- filt_data()

    withProgress(message = "Calculating Hourly statistics - Factor1...", value = 0, {

      #===========================================#
      incProgress(0.2, detail = "Loading data...")
      # Check if treatment column exists, add only if they are non-null
      treatment_col <- input$treat1

      # optional column check
      exist_H2 <- "H2_gd" %in% colnames(trt_data)
      exist_O2 <- "O2_gd" %in% colnames(trt_data)
      exist_airflow <- "Airflow" %in% colnames(trt_data)
      exist_adjCH4 <- "adjCH4_gd" %in% colnames(trt_data)

      #===========================================#
      incProgress(0.5, detail = "Calculating...")
      # Dynamically construct group_by columns
      group_by_cols <- c("Time_zone")

      if (!is.null(treatment_col)) {
        group_by_cols <- c(group_by_cols, treatment_col)
      }

      group_by_syms <- syms(group_by_cols)

      # Group and summarise dynamically
      if (isTRUE(input$time_smooth) && exist_adjCH4){
        df.hourly.trt1 <- trt_data %>% group_by(!!!group_by_syms) %>% summarise(Duration_minute = sum(Duration_sec, na.rm = TRUE)/60,
                                                                                Duration = sprintf("%d:%02d:%02d", sum(Duration_sec, na.rm = TRUE) %/% 3600,
                                                                                                   (sum(Duration_sec, na.rm = TRUE) %% 3600) %/% 60,
                                                                                                   sum(Duration_sec, na.rm = TRUE) %% 60),
                                                                                Nb_visit = n(),
                                                                                Nb_animal = n_distinct(Farm_name),
                                                                                Nb_visit_per_animal = Nb_visit / Nb_animal,
                                                                                CO2_avg_gd = mean(CO2_gd, na.rm = TRUE),
                                                                                CO2_SD = sd(CO2_gd, na.rm = TRUE),
                                                                                CH4_avg_gd = mean(CH4_gd, na.rm = TRUE),
                                                                                CH4_SD = sd(CH4_gd, na.rm = TRUE),
                                                                                adjCH4_avg_gd = mean(adjCH4_gd, na.rm = TRUE),
                                                                                adjCH4_SD = sd(adjCH4_gd, na.rm = TRUE),

                                                                                H2_avg_gd = if (exist_H2) mean(H2_gd, na.rm = TRUE) else NA_real_,
                                                                                H2_SD = if (exist_H2) sd(H2_gd, na.rm = TRUE) else NA_real_,
                                                                                O2_avg_gd = if (exist_O2) mean(O2_gd, na.rm = TRUE) else NA_real_,
                                                                                O2_SD = if (exist_O2) sd(O2_gd, na.rm = TRUE) else NA_real_,
                                                                                Airflow_avg_ls = if (exist_airflow) mean(Airflow, na.rm = TRUE) else NA_real_,
                                                                                Airflow_SD = if (exist_airflow) sd(Airflow, na.rm = TRUE) else NA_real_,
                                                                                .groups = "drop")

      } else{
        df.hourly.trt1 <- trt_data %>% group_by(!!!group_by_syms) %>% summarise(Duration_minute = sum(Duration_sec, na.rm = TRUE)/60,
                                                                                Duration = sprintf("%d:%02d:%02d", sum(Duration_sec, na.rm = TRUE) %/% 3600,
                                                                                                   (sum(Duration_sec, na.rm = TRUE) %% 3600) %/% 60,
                                                                                                   sum(Duration_sec, na.rm = TRUE) %% 60),
                                                                                Nb_visit = n(),
                                                                                Nb_animal = n_distinct(Farm_name),
                                                                                Nb_visit_per_animal = Nb_visit / Nb_animal,
                                                                                CO2_avg_gd = mean(CO2_gd, na.rm = TRUE),
                                                                                CO2_SD = sd(CO2_gd, na.rm = TRUE),
                                                                                CH4_avg_gd = mean(CH4_gd, na.rm = TRUE),
                                                                                CH4_SD = sd(CH4_gd, na.rm = TRUE),

                                                                                H2_avg_gd = if (exist_H2) mean(H2_gd, na.rm = TRUE) else NA_real_,
                                                                                H2_SD = if (exist_H2) sd(H2_gd, na.rm = TRUE) else NA_real_,
                                                                                O2_avg_gd = if (exist_O2) mean(O2_gd, na.rm = TRUE) else NA_real_,
                                                                                O2_SD = if (exist_O2) sd(O2_gd, na.rm = TRUE) else NA_real_,
                                                                                Airflow_avg_ls = if (exist_airflow) mean(Airflow, na.rm = TRUE) else NA_real_,
                                                                                Airflow_SD = if (exist_airflow) sd(Airflow, na.rm = TRUE) else NA_real_,
                                                                                .groups = "drop")

      }
      incProgress(0.7, detail = "Calculating...")
      #===========================================#
      incProgress(1, detail = "Finalizing...")

      # Empty column eliminate
      df.hourly.trt1 <- df.hourly.trt1 %>% select(where(~ !all(is.na(.))))
    })

    return(df.hourly.trt1)

  })

  # control download button for profile trt1
  shinyjs::disable("download_hourly_trt1")

  observe({
    if (!is.null(hourly_t1())) {
      shinyjs::enable("download_hourly_trt1")
    } else{
      shinyjs::disable("download_hourly_trt1")
    }
  })

  output$download_hourly_trt1 <- downloadHandler(
    filename = function() {
      paste("Hourly_Factor1_", Sys.Date(), ".xlsx", sep = "")
    },
    content = function(file) {
      data_to_save <- hourly_t1()
      if (!is.null(data_to_save)){
        openxlsx::write.xlsx(data_to_save, file)

      } else{
        stop("No data available to downloads.")
      }
    }
  )

  ########################################################################################################################
  # Hourly average per Factor2
  hourly_t2 <- eventReactive(input$calc_hourly_trt2,{
    req(filt_data(), input$treat2)
    print("== Hourly statistics per Factor2 ==")

    trt_data <- filt_data()

    withProgress(message = "Calculating Hourly statistics - Factor2...", value = 0, {

      #===========================================#
      incProgress(0.2, detail = "Loading data...")
      # Check if treatment column exists, add only if they are non-null
      treatment_cols <- input$treat2
      treatment_cols <- if (!is.null(treatment_cols) && treatment_cols != "") treatment_cols else NULL

      # optional column check
      exist_H2 <- "H2_gd" %in% colnames(trt_data)
      exist_O2 <- "O2_gd" %in% colnames(trt_data)
      exist_airflow <- "Airflow" %in% colnames(trt_data)
      exist_adjCH4 <- "adjCH4_gd" %in% colnames(trt_data)

      #===========================================#
      incProgress(0.5, detail = "Calculating...")
      # Dynamically construct group_by columns
      group_by_cols <- c("Time_zone")

      if (!is.null(treatment_cols)) {
        group_by_cols <- c(group_by_cols, treatment_cols)
      }

      group_by_syms <- syms(group_by_cols)

      # Group and summarise dynamically

      if (isTRUE(input$time_smooth) && exist_adjCH4){
      df.hourly.trt2 <- trt_data %>% group_by(!!!group_by_syms) %>% summarise(Duration_minute = sum(Duration_sec, na.rm = TRUE)/60,
                                                                              Duration = sprintf("%d:%02d:%02d", sum(Duration_sec, na.rm = TRUE) %/% 3600,
                                                                                                 (sum(Duration_sec, na.rm = TRUE) %% 3600) %/% 60,
                                                                                                 sum(Duration_sec, na.rm = TRUE) %% 60),
                                                                              Nb_visit = n(),
                                                                              Nb_animal = n_distinct(Farm_name),
                                                                              Nb_visit_per_animal = Nb_visit / Nb_animal,
                                                                              CO2_avg_gd = mean(CO2_gd, na.rm = TRUE),
                                                                              CO2_SD = sd(CO2_gd, na.rm = TRUE),
                                                                              CH4_avg_gd = mean(CH4_gd, na.rm = TRUE),
                                                                              CH4_SD = sd(CH4_gd, na.rm = TRUE),
                                                                              adjCH4_avg_gd = mean(adjCH4_gd, na.rm = TRUE),
                                                                              adjCH4_SD = sd(adjCH4_gd, na.rm = TRUE),

                                                                              H2_avg_gd = if (exist_H2) mean(H2_gd, na.rm = TRUE) else NA_real_,
                                                                              H2_SD = if (exist_H2) sd(H2_gd, na.rm = TRUE) else NA_real_,
                                                                              O2_avg_gd = if (exist_O2) mean(O2_gd, na.rm = TRUE) else NA_real_,
                                                                              O2_SD = if (exist_O2) sd(O2_gd, na.rm = TRUE) else NA_real_,
                                                                              Airflow_avg_ls = if (exist_airflow) mean(Airflow, na.rm = TRUE) else NA_real_,
                                                                              Airflow_SD = if (exist_airflow) sd(Airflow, na.rm = TRUE) else NA_real_,
                                                                              .groups = "drop")
      } else{
        df.hourly.trt2 <- trt_data %>% group_by(!!!group_by_syms) %>% summarise(Duration_minute = sum(Duration_sec, na.rm = TRUE)/60,
                                                                                Duration = sprintf("%d:%02d:%02d", sum(Duration_sec, na.rm = TRUE) %/% 3600,
                                                                                                   (sum(Duration_sec, na.rm = TRUE) %% 3600) %/% 60,
                                                                                                   sum(Duration_sec, na.rm = TRUE) %% 60),
                                                                                Nb_visit = n(),
                                                                                Nb_animal = n_distinct(Farm_name),
                                                                                Nb_visit_per_animal = Nb_visit / Nb_animal,
                                                                                CO2_avg_gd = mean(CO2_gd, na.rm = TRUE),
                                                                                CO2_SD = sd(CO2_gd, na.rm = TRUE),
                                                                                CH4_avg_gd = mean(CH4_gd, na.rm = TRUE),
                                                                                CH4_SD = sd(CH4_gd, na.rm = TRUE),

                                                                                H2_avg_gd = if (exist_H2) mean(H2_gd, na.rm = TRUE) else NA_real_,
                                                                                H2_SD = if (exist_H2) sd(H2_gd, na.rm = TRUE) else NA_real_,
                                                                                O2_avg_gd = if (exist_O2) mean(O2_gd, na.rm = TRUE) else NA_real_,
                                                                                O2_SD = if (exist_O2) sd(O2_gd, na.rm = TRUE) else NA_real_,
                                                                                Airflow_avg_ls = if (exist_airflow) mean(Airflow, na.rm = TRUE) else NA_real_,
                                                                                Airflow_SD = if (exist_airflow) sd(Airflow, na.rm = TRUE) else NA_real_,
                                                                                .groups = "drop")

      }
      incProgress(0.7, detail = "Calculating...")
      #===========================================#
      incProgress(1, detail = "Finalizing...")
      # Empty column eliminate
      df.hourly.trt2 <- df.hourly.trt2 %>% select(where(~ !all(is.na(.))))

    })

    return(df.hourly.trt2)

  })

  # control download button for profile trt2
  shinyjs::disable("download_hourly_trt2")

  observe({
    if (!is.null(hourly_t2())) {
      shinyjs::enable("download_hourly_trt2")
    } else{
      shinyjs::disable("download_hourly_trt2")
    }
  })

  output$download_hourly_trt2 <- downloadHandler(
    filename = function() {
      paste("Hourly_Factor2_", Sys.Date(), ".xlsx", sep = "")
    },
    content = function(file) {
      data_to_save <- hourly_t2()
      if (!is.null(data_to_save)){
        openxlsx::write.xlsx(data_to_save, file)

      } else{
        stop("No data available to downloads.")
      }
    }
  )

  ########################################################################################################################
  # Hourly average per Factor1 X Factor2
  hourly_t1xt2 <- eventReactive(input$calc_hourly_trt1xtrt2,{
    req(filt_data(), input$treat1, input$treat2)

    print("== Hourly statistics per Factor1 X Factor2 ==")

    trt_data <- filt_data()

    withProgress(message = "Calculating Hourly statistics - Factor1 X Factor2...", value = 0, {
      #===========================================#
      incProgress(0.2, detail = "Loading data...")
      # Check if treatment column exists, add only if they are non-null
      treatment_cols <- c(input$treat1, input$treat2)
      treatment_cols <- if(length(treatment_cols) > 0 && all(treatment_cols != "")){
        treatment_cols
      } else { NULL }

      # optional column check
      exist_H2 <- "H2_gd" %in% colnames(trt_data)
      exist_O2 <- "O2_gd" %in% colnames(trt_data)
      exist_airflow <- "Airflow" %in% colnames(trt_data)
      exist_adjCH4 <- "adjCH4_gd" %in% colnames(trt_data)

      #===========================================#
      incProgress(0.5, detail = "Calculating...")
      # Dynamically construct group_by columns
      group_by_cols <- c("Time_zone")

      if (!is.null(treatment_cols)) {
        group_by_cols <- c(group_by_cols, treatment_cols)
      }

      group_by_syms <- syms(group_by_cols)

      # Group and summarise dynamically
      if (isTRUE(input$time_smooth) && exist_adjCH4){

      df.hourly.trt1by2 <- trt_data %>% group_by(!!!syms(group_by_syms)) %>% summarise(Duration_minute = sum(Duration_sec, na.rm = TRUE)/60,
                                                                                       Duration = sprintf("%d:%02d:%02d", sum(Duration_sec, na.rm = TRUE) %/% 3600,
                                                                                                          (sum(Duration_sec, na.rm = TRUE) %% 3600) %/% 60,
                                                                                                          sum(Duration_sec, na.rm = TRUE) %% 60),
                                                                                       Nb_visit = n(),
                                                                                       Nb_animal = n_distinct(Farm_name),
                                                                                       Nb_visit_per_animal = Nb_visit / Nb_animal,
                                                                                       CO2_avg_gd = mean(CO2_gd, na.rm = TRUE),
                                                                                       CO2_SD = sd(CO2_gd, na.rm = TRUE),
                                                                                       CH4_avg_gd = mean(CH4_gd, na.rm = TRUE),
                                                                                       CH4_SD = sd(CH4_gd, na.rm = TRUE),
                                                                                       adjCH4_avg_gd = mean(adjCH4_gd, na.rm = TRUE),
                                                                                       adjCH4_SD = sd(adjCH4_gd, na.rm = TRUE),

                                                                                       H2_avg_gd = if (exist_H2) mean(H2_gd, na.rm = TRUE) else NA_real_,
                                                                                       H2_SD = if (exist_H2) sd(H2_gd, na.rm = TRUE) else NA_real_,
                                                                                       O2_avg_gd = if (exist_O2) mean(O2_gd, na.rm = TRUE) else NA_real_,
                                                                                       O2_SD = if (exist_O2) sd(O2_gd, na.rm = TRUE) else NA_real_,
                                                                                       Airflow_avg_ls = if (exist_airflow) mean(Airflow, na.rm = TRUE) else NA_real_,
                                                                                       Airflow_SD = if (exist_airflow) sd(Airflow, na.rm = TRUE) else NA_real_,
                                                                                       .groups = "drop")
      } else{
        df.hourly.trt1by2 <- trt_data %>% group_by(!!!syms(group_by_syms)) %>% summarise(Duration_minute = sum(Duration_sec, na.rm = TRUE)/60,
                                                                                         Duration = sprintf("%d:%02d:%02d", sum(Duration_sec, na.rm = TRUE) %/% 3600,
                                                                                                            (sum(Duration_sec, na.rm = TRUE) %% 3600) %/% 60,
                                                                                                            sum(Duration_sec, na.rm = TRUE) %% 60),
                                                                                         Nb_visit = n(),
                                                                                         Nb_animal = n_distinct(Farm_name),
                                                                                         Nb_visit_per_animal = Nb_visit / Nb_animal,
                                                                                         CO2_avg_gd = mean(CO2_gd, na.rm = TRUE),
                                                                                         CO2_SD = sd(CO2_gd, na.rm = TRUE),
                                                                                         CH4_avg_gd = mean(CH4_gd, na.rm = TRUE),
                                                                                         CH4_SD = sd(CH4_gd, na.rm = TRUE),

                                                                                         H2_avg_gd = if (exist_H2) mean(H2_gd, na.rm = TRUE) else NA_real_,
                                                                                         H2_SD = if (exist_H2) sd(H2_gd, na.rm = TRUE) else NA_real_,
                                                                                         O2_avg_gd = if (exist_O2) mean(O2_gd, na.rm = TRUE) else NA_real_,
                                                                                         O2_SD = if (exist_O2) sd(O2_gd, na.rm = TRUE) else NA_real_,
                                                                                         Airflow_avg_ls = if (exist_airflow) mean(Airflow, na.rm = TRUE) else NA_real_,
                                                                                         Airflow_SD = if (exist_airflow) sd(Airflow, na.rm = TRUE) else NA_real_,
                                                                                         .groups = "drop")

      }
      incProgress(0.7, detail = "Calculating...")
      #===========================================#
      incProgress(1, detail = "Finalizing...")

      # Empty column eliminate
      df.hourly.trt1by2 <- df.hourly.trt1by2 %>% select(where(~ !all(is.na(.))))

    })

    return(df.hourly.trt1by2)

  })

  # control download button for profile trt1xtrt2
  shinyjs::disable("download_hourly_trt1xtrt2")

  observe({
    if (!is.null(hourly_t1xt2())) {
      shinyjs::enable("download_hourly_trt1xtrt2")
    } else{
      shinyjs::disable("download_hourly_trt1xtrt2")
    }
  })

  output$download_hourly_trt1xtrt2 <- downloadHandler(
    filename = function() {
      paste("Hourly_Factor1xFactor2_", Sys.Date(), ".xlsx", sep = "")
    },
    content = function(file) {
      data_to_save <- hourly_t1xt2()
      if (!is.null(data_to_save)){
        openxlsx::write.xlsx(data_to_save, file)

      } else{
        stop("No data available to downloads.")
      }
    }
  )

  ########################################################################################################################
  # Running all the Hourly analysis
  observeEvent(input$calc_all_hourly, {
    shinyjs::click("calc_hourly_entire")
    shinyjs::click("calc_hourly_anim")

    if (!is.null(input$treat1) && nzchar(input$treat1)){
      shinyjs::click("calc_hourly_trt1")
    }

    if (!is.null(input$treat2) && nzchar(input$treat2)){
      shinyjs::click("calc_hourly_trt2")
    }

    if(!is.null(input$treat1) && nzchar(input$treat1) && !is.null(input$treat2) && nzchar(input$treat2)){
      shinyjs::click("calc_hourly_trt1xtrt2")
    }
  })

  #======================================================================================================================#
  # DAILY & HOURLY STATISTICS #
  #======================================================================================================================#

  # Daily & Hourly average for Entire dataset
  dhly_total <- eventReactive(input$calc_dhly_entire, {
    # req(filt_data(), input$date_filt_range)
    req(filt_data())
    
    print("== Daily & Hourly statistics for entire dataset")

    trt_data <- filt_data()

    withProgress(message = "Calculate Daily & Hourly statistics - Entire dataset...", value = 0, {
      #===========================================#
      incProgress(0.2, detail = "Loading data...")

      # date difference calculation from start date: 1st day of sampling = 0
      # date_diff <- abs(as.numeric(as.Date(trt_data$Date_rcd) - as.Date(input$date_filt_range[1])))
      start_date <- min(as.Date(trt_data$Date_rcd), na.rm = TRUE)
      date_diff <- as.numeric(as.Date(trt_data$Date_rcd) - start_date)
      trt_data_jour <- add_column(trt_data, date_diff = date_diff)

      # optional column check
      exist_H2 <- "H2_gd" %in% colnames(trt_data)
      exist_O2 <- "O2_gd" %in% colnames(trt_data)
      exist_airflow <- "Airflow" %in% colnames(trt_data)
      exist_adjCH4 <- "adjCH4_gd" %in% colnames(trt_data)

      #===========================================#
      incProgress(0.5, detail = "Calculating...")

      trt_data_jour <- trt_data_jour %>% mutate(Time_zone = as.numeric(Time_zone))

      if (isTRUE(input$time_smooth) && exist_adjCH4){
      df.dhly.total <- trt_data_jour %>% group_by(date_diff, Time_zone) %>% summarise(Date = first(Date_rcd),
                                                                                      Duration_minute = sum(Duration_sec, na.rm = TRUE)/60,
                                                                                      Duration = sprintf("%d:%02d:%02d", sum(Duration_sec, na.rm = TRUE) %/% 3600,
                                                                                                         (sum(Duration_sec, na.rm = TRUE) %% 3600) %/% 60,
                                                                                                         sum(Duration_sec, na.rm = TRUE) %% 60),
                                                                                      Nb_visit = n(),
                                                                                      Nb_animal = n_distinct(Farm_name),
                                                                                      Nb_visit_per_animal = Nb_visit / Nb_animal,
                                                                                      CO2_avg_gd = mean(CO2_gd, na.rm = TRUE),
                                                                                      CO2_SD = sd(CO2_gd, na.rm = TRUE),
                                                                                      CH4_avg_gd = mean(CH4_gd, na.rm = TRUE),
                                                                                      CH4_SD = sd(CH4_gd, na.rm = TRUE),
                                                                                      adjCH4_avg_gd = mean(adjCH4_gd, na.rm = TRUE),
                                                                                      adjCH4_SD = sd(adjCH4_gd, na.rm = TRUE),

                                                                                      H2_avg_gd = if (exist_H2) mean(H2_gd, na.rm = TRUE) else NA_real_,
                                                                                      H2_SD = if (exist_H2) sd(H2_gd, na.rm = TRUE) else NA_real_,
                                                                                      O2_avg_gd = if (exist_O2) mean(O2_gd, na.rm = TRUE) else NA_real_,
                                                                                      O2_SD = if (exist_O2) sd(O2_gd, na.rm = TRUE) else NA_real_,
                                                                                      Airflow_avg_ls = if (exist_airflow) mean(Airflow, na.rm = TRUE) else NA_real_,
                                                                                      Airflow_SD = if (exist_airflow) sd(Airflow, na.rm = TRUE) else NA_real_,
                                                                                      .groups = "drop")
      } else{
        df.dhly.total <- trt_data_jour %>% group_by(date_diff, Time_zone) %>% summarise(Date = first(Date_rcd),
                                                                                        Duration_minute = sum(Duration_sec, na.rm = TRUE)/60,
                                                                                        Duration = sprintf("%d:%02d:%02d", sum(Duration_sec, na.rm = TRUE) %/% 3600,
                                                                                                           (sum(Duration_sec, na.rm = TRUE) %% 3600) %/% 60,
                                                                                                           sum(Duration_sec, na.rm = TRUE) %% 60),
                                                                                        Nb_visit = n(),
                                                                                        Nb_animal = n_distinct(Farm_name),
                                                                                        Nb_visit_per_animal = Nb_visit / Nb_animal,
                                                                                        CO2_avg_gd = mean(CO2_gd, na.rm = TRUE),
                                                                                        CO2_SD = sd(CO2_gd, na.rm = TRUE),
                                                                                        CH4_avg_gd = mean(CH4_gd, na.rm = TRUE),
                                                                                        CH4_SD = sd(CH4_gd, na.rm = TRUE),

                                                                                        H2_avg_gd = if (exist_H2) mean(H2_gd, na.rm = TRUE) else NA_real_,
                                                                                        H2_SD = if (exist_H2) sd(H2_gd, na.rm = TRUE) else NA_real_,
                                                                                        O2_avg_gd = if (exist_O2) mean(O2_gd, na.rm = TRUE) else NA_real_,
                                                                                        O2_SD = if (exist_O2) sd(O2_gd, na.rm = TRUE) else NA_real_,
                                                                                        Airflow_avg_ls = if (exist_airflow) mean(Airflow, na.rm = TRUE) else NA_real_,
                                                                                        Airflow_SD = if (exist_airflow) sd(Airflow, na.rm = TRUE) else NA_real_,
                                                                                        .groups = "drop")

      }
      incProgress(0.7, detail = "Calculating...")
      #===========================================#
      incProgress(1, detail = "Finalizing...")
      # Empty column eliminate
      df.dhly.total <- df.dhly.total %>% select(where(~ !all(is.na(.))))

    })

    return(df.dhly.total)

  })

  # control download button for 'dhly entire'
  shinyjs::disable("download_dhly_entire")

  observe({
    if (!is.null(dhly_total())) {
      shinyjs::enable("download_dhly_entire")
    } else{
      shinyjs::disable("download_dhly_entire")
    }
  })

  output$download_dhly_entire <- downloadHandler(
    filename = function() {
      paste("Daily_Hourly_Entire_", Sys.Date(), ".xlsx", sep = "")
    },
    content = function(file) {
      data_to_save <- dhly_total()
      if (!is.null(data_to_save)){
        openxlsx::write.xlsx(data_to_save, file)

      } else{
        stop("No data available to downloads.")
      }
    }
  )

  ########################################################################################################################
  # Daily & Hourly average per Animal
  dhly_anim <- eventReactive(input$calc_dhly_anim,{
    # req(filt_data(), input$date_filt_range)
    req(filt_data())
    
    print("== Daily & Hourly statistics per Animal ==")

    trt_data <- filt_data()

    withProgress(message = "Calculating Daily & Hourly statistics - Animal...", value = 0, {
      #===========================================#
      incProgress(0.2, detail = "Loading data...")

      # date difference calculation from start date: 1st day of sampling = 0
      # date_diff <- abs(as.numeric(as.Date(trt_data$Date_rcd) - as.Date(input$date_filt_range[1])))
      start_date <- min(as.Date(trt_data$Date_rcd), na.rm = TRUE)
      date_diff <- as.numeric(as.Date(trt_data$Date_rcd) - start_date)
      
      trt_data_jour <- add_column(trt_data, date_diff = date_diff)

      # optional column check
      exist_H2 <- "H2_gd" %in% colnames(trt_data)
      exist_O2 <- "O2_gd" %in% colnames(trt_data)
      exist_airflow <- "Airflow" %in% colnames(trt_data)
      exist_adjCH4 <- "adjCH4_gd" %in% colnames(trt_data)

      # Check if treatment columns exists, add only if they are non-null
      treatment_cols <- c(input$treat1, input$treat2)
      treatment_cols <- treatment_cols[!is.null(treatment_cols) & nzchar(treatment_cols)]

      #===========================================#
      incProgress(0.5, detail = "Calculating...")

      trt_data_jour <- trt_data_jour %>% mutate(Time_zone = as.numeric(Time_zone))

      if (isTRUE(input$time_smooth) && exist_adjCH4){
      df.dhly.anim <- trt_data_jour %>% group_by(Farm_name, date_diff, Time_zone) %>% summarise(Farm_name = unique(Farm_name),
                                                                                      across(all_of(treatment_cols), ~first(na.omit(.)), .names = "{.col}"),
                                                                                      Date = first(Date_rcd),
                                                                                     Duration_minute = sum(Duration_sec, na.rm = TRUE)/60,
                                                                                     Duration = sprintf("%d:%02d:%02d", sum(Duration_sec, na.rm = TRUE) %/% 3600,
                                                                                                        (sum(Duration_sec, na.rm = TRUE) %% 3600) %/% 60,
                                                                                                        sum(Duration_sec, na.rm = TRUE) %% 60),
                                                                                      Nb_visit = n(),
                                                                                      CO2_avg_gd = mean(CO2_gd, na.rm = TRUE),
                                                                                      CO2_SD = sd(CO2_gd, na.rm = TRUE),
                                                                                      CH4_avg_gd = mean(CH4_gd, na.rm = TRUE),
                                                                                      CH4_SD = sd(CH4_gd, na.rm = TRUE),
                                                                                      adjCH4_avg_gd = mean(adjCH4_gd, na.rm = TRUE),
                                                                                      adjCH4_SD = sd(adjCH4_gd, na.rm = TRUE),

                                                                                      H2_avg_gd = if (exist_H2) mean(H2_gd, na.rm = TRUE) else NA_real_,
                                                                                      H2_SD = if (exist_H2) sd(H2_gd, na.rm = TRUE) else NA_real_,
                                                                                      O2_avg_gd = if (exist_O2) mean(O2_gd, na.rm = TRUE) else NA_real_,
                                                                                      O2_SD = if (exist_O2) sd(O2_gd, na.rm = TRUE) else NA_real_,
                                                                                      Airflow_avg_ls = if (exist_airflow) mean(Airflow, na.rm = TRUE) else NA_real_,
                                                                                      Airflow_SD = if (exist_airflow) sd(Airflow, na.rm = TRUE) else NA_real_,
                                                                                      .groups = "drop")
      } else{
        df.dhly.anim <- trt_data_jour %>% group_by(Farm_name, date_diff, Time_zone) %>% summarise(Farm_name = unique(Farm_name),
                                                                                                  across(all_of(treatment_cols), ~first(na.omit(.)), .names = "{.col}"),
                                                                                                  Date = first(Date_rcd),
                                                                                                  Duration_minute = sum(Duration_sec, na.rm = TRUE)/60,
                                                                                                  Duration = sprintf("%d:%02d:%02d", sum(Duration_sec, na.rm = TRUE) %/% 3600,
                                                                                                                     (sum(Duration_sec, na.rm = TRUE) %% 3600) %/% 60,
                                                                                                                     sum(Duration_sec, na.rm = TRUE) %% 60),
                                                                                                  Nb_visit = n(),
                                                                                                  CO2_avg_gd = mean(CO2_gd, na.rm = TRUE),
                                                                                                  CO2_SD = sd(CO2_gd, na.rm = TRUE),
                                                                                                  CH4_avg_gd = mean(CH4_gd, na.rm = TRUE),
                                                                                                  CH4_SD = sd(CH4_gd, na.rm = TRUE),

                                                                                                  H2_avg_gd = if (exist_H2) mean(H2_gd, na.rm = TRUE) else NA_real_,
                                                                                                  H2_SD = if (exist_H2) sd(H2_gd, na.rm = TRUE) else NA_real_,
                                                                                                  O2_avg_gd = if (exist_O2) mean(O2_gd, na.rm = TRUE) else NA_real_,
                                                                                                  O2_SD = if (exist_O2) sd(O2_gd, na.rm = TRUE) else NA_real_,
                                                                                                  Airflow_avg_ls = if (exist_airflow) mean(Airflow, na.rm = TRUE) else NA_real_,
                                                                                                  Airflow_SD = if (exist_airflow) sd(Airflow, na.rm = TRUE) else NA_real_,
                                                                                                  .groups = "drop")

      }
      incProgress(0.7, detail = "Calculating...")
      #===========================================#
      incProgress(1, detail = "Finalizing...")

      # Empty column eliminate
      df.dhly.anim <- df.dhly.anim %>% select(where(~ !all(is.na(.))))

    })

    return(df.dhly.anim)

  })


  # control download button for 'dhly anim'
  shinyjs::disable("download_dhly_anim")

  observe({
    if (!is.null(dhly_anim())) {
      shinyjs::enable("download_dhly_anim")
    } else{
      shinyjs::disable("download_dhly_anim")
    }
  })

  output$download_dhly_anim <- downloadHandler(
    filename = function() {
      paste("Daily_Hourly_Animal_", Sys.Date(), ".xlsx", sep = "")
    },
    content = function(file) {
      data_to_save <- dhly_anim()
      if (!is.null(data_to_save)){
        openxlsx::write.xlsx(data_to_save, file)

      } else{
        stop("No data available to downloads.")
      }
    }
  )

  ########################################################################################################################
  # Daily & Hourly average per Factor1
  dhly_t1 <- eventReactive(input$calc_dhly_trt1, {
    # req(filt_data(), input$date_filt_range, input$treat1)
    req(filt_data(), input$treat1)
    print("== Daily & Hourly statistics per Factor1 ==")

    trt_data <- filt_data()

    withProgress(message = "Calculating Daily & Hourly statistics - Factor1...", value = 0, {
      #===========================================#
      incProgress(0.2, detail = "Loading data...")
      # Date difference calculation from start date: 1st day of sampling = 0
      # date_diff <- abs(as.numeric(as.Date(input$date_filt_range[1]) - as.Date(trt_data$Date_rcd)))
      start_date <- min(as.Date(trt_data$Date_rcd), na.rm = TRUE)
      date_diff <- as.numeric(as.Date(trt_data$Date_rcd) - start_date)
      
      trt_data_jour <- add_column(trt_data, date_diff = date_diff)

      # optional column check
      exist_H2 <- "H2_gd" %in% colnames(trt_data)
      exist_O2 <- "O2_gd" %in% colnames(trt_data)
      exist_airflow <- "Airflow" %in% colnames(trt_data)
      exist_adjCH4 <- "adjCH4_gd" %in% colnames(trt_data)

      # Check if treatment columns exists, add only if they are non-null
      treatment_cols <- input$treat1
      treatment_cols <- treatment_cols[!is.null(treatment_cols) & nzchar(treatment_cols)]
      #===========================================#
      incProgress(0.5, detail = "Calculating...")

      trt_data_jour <- trt_data_jour %>% mutate(Time_zone = as.numeric(Time_zone))

      # Dynamically construct group_by columns
      group_by_cols <- c("date_diff")
      if (!is.null(treatment_cols)) {
        group_by_cols <- c(group_by_cols, treatment_cols, "Time_zone")
      }

      # Group and summarise dynamically
      if (isTRUE(input$time_smooth) && exist_adjCH4){
        df.dhly.trt1 <- trt_data_jour %>% group_by(!!!syms(group_by_cols)) %>% summarise(Date = first(Date_rcd),
                                                                                         Duration_minute = sum(Duration_sec, na.rm = TRUE)/60,
                                                                                         Duration = sprintf("%d:%02d:%02d", sum(Duration_sec, na.rm = TRUE) %/% 3600,
                                                                                                            (sum(Duration_sec, na.rm = TRUE) %% 3600) %/% 60,
                                                                                                            sum(Duration_sec, na.rm = TRUE) %% 60),
                                                                                         Nb_visit = n(),
                                                                                         Nb_animal = n_distinct(Farm_name),
                                                                                         Nb_visit_per_animal = Nb_visit / Nb_animal,
                                                                                         CO2_avg_gd = mean(CO2_gd, na.rm = TRUE),
                                                                                         CO2_SD = sd(CO2_gd, na.rm = TRUE),
                                                                                         CH4_avg_gd = mean(CH4_gd, na.rm = TRUE),
                                                                                         CH4_SD = sd(CH4_gd, na.rm = TRUE),
                                                                                         adjCH4_avg_gd = mean(adjCH4_gd, na.rm = TRUE),
                                                                                         adjCH4_SD = sd(adjCH4_gd, na.rm = TRUE),

                                                                                         H2_avg_gd = if (exist_H2) mean(H2_gd, na.rm = TRUE) else NA_real_,
                                                                                         H2_SD = if (exist_H2) sd(H2_gd, na.rm = TRUE) else NA_real_,
                                                                                         O2_avg_gd = if (exist_O2) mean(O2_gd, na.rm = TRUE) else NA_real_,
                                                                                         O2_SD = if (exist_O2) sd(O2_gd, na.rm = TRUE) else NA_real_,
                                                                                         Airflow_avg_ls = if (exist_airflow) mean(Airflow, na.rm = TRUE) else NA_real_,
                                                                                         Airflow_SD = if (exist_airflow) sd(Airflow, na.rm = TRUE) else NA_real_,
                                                                                         .groups = "drop")

      } else{
        df.dhly.trt1 <- trt_data_jour %>% group_by(!!!syms(group_by_cols)) %>% summarise(Date = first(Date_rcd),
                                                                                         Duration_minute = sum(Duration_sec, na.rm = TRUE)/60,
                                                                                         Duration = sprintf("%d:%02d:%02d", sum(Duration_sec, na.rm = TRUE) %/% 3600,
                                                                                                            (sum(Duration_sec, na.rm = TRUE) %% 3600) %/% 60,
                                                                                                            sum(Duration_sec, na.rm = TRUE) %% 60),
                                                                                         Nb_visit = n(),
                                                                                         Nb_animal = n_distinct(Farm_name),
                                                                                         Nb_visit_per_animal = Nb_visit / Nb_animal,
                                                                                         CO2_avg_gd = mean(CO2_gd, na.rm = TRUE),
                                                                                         CO2_SD = sd(CO2_gd, na.rm = TRUE),
                                                                                         CH4_avg_gd = mean(CH4_gd, na.rm = TRUE),
                                                                                         CH4_SD = sd(CH4_gd, na.rm = TRUE),

                                                                                         H2_avg_gd = if (exist_H2) mean(H2_gd, na.rm = TRUE) else NA_real_,
                                                                                         H2_SD = if (exist_H2) sd(H2_gd, na.rm = TRUE) else NA_real_,
                                                                                         O2_avg_gd = if (exist_O2) mean(O2_gd, na.rm = TRUE) else NA_real_,
                                                                                         O2_SD = if (exist_O2) sd(O2_gd, na.rm = TRUE) else NA_real_,
                                                                                         Airflow_avg_ls = if (exist_airflow) mean(Airflow, na.rm = TRUE) else NA_real_,
                                                                                         Airflow_SD = if (exist_airflow) sd(Airflow, na.rm = TRUE) else NA_real_,
                                                                                         .groups = "drop")

      }

      incProgress(0.7, detail = "Calculating...")
      #===========================================#
      incProgress(1, detail = "Finalizing...")

      # Empty column eliminate
      df.dhly.trt1 <- df.dhly.trt1 %>% select(where(~ !all(is.na(.))))

    })

    return(df.dhly.trt1)

  })

  # control download button for 'dhly trt1'
  shinyjs::disable("download_dhly_trt1")

  observe({
    if (!is.null(dhly_t1())) {
      shinyjs::enable("download_dhly_trt1")
    } else{
      shinyjs::disable("download_dhly_trt1")
    }
  })

  output$download_dhly_trt1 <- downloadHandler(
    filename = function() {
      paste("Daily_Hourly_Factor1_", Sys.Date(), ".xlsx", sep = "")
    },
    content = function(file) {
      data_to_save <- dhly_t1()
      if (!is.null(data_to_save)){
        openxlsx::write.xlsx(data_to_save, file)

      } else{
        stop("No data available to downloads.")
      }
    }
  )


  ########################################################################################################################
  # Daily & Hourly average per Factor2
  dhly_t2 <- eventReactive(input$calc_dhly_trt2, {
    # req(filt_data(), input$date_filt_range, input$treat2)
    req(filt_data(), input$treat2)
    print("== Daily & Hourly statistics per Factor2 ==")

    trt_data <- filt_data()

    withProgress(message = "Calculating Daily & Hourly statistics - Factor2...", value = 0, {
      #===========================================#
      incProgress(0.2, detail = "Loading data...")
      # Date difference calculation from start date: 1st day of sampling = 0
      # date_diff <- abs(as.numeric(as.Date(input$date_filt_range[1]) - as.Date(trt_data$Date_rcd)))
      start_date <- min(as.Date(trt_data$Date_rcd), na.rm = TRUE)
      date_diff <- as.numeric(as.Date(trt_data$Date_rcd) - start_date)
      
      trt_data_jour <- add_column(trt_data, date_diff = date_diff)

      # optional column check
      exist_H2 <- "H2_gd" %in% colnames(trt_data)
      exist_O2 <- "O2_gd" %in% colnames(trt_data)
      exist_airflow <- "Airflow" %in% colnames(trt_data)
      exist_adjCH4 <- "adjCH4_gd" %in% colnames(trt_data)

      # Check if treatment columns exists, add only if they are non-null
      treatment_cols <- input$treat2
      treatment_cols <- treatment_cols[!is.null(treatment_cols) & nzchar(treatment_cols)]
      #===========================================#
      incProgress(0.5, detail = "Calculating...")

      trt_data_jour <- trt_data_jour %>% mutate(Time_zone = as.numeric(Time_zone))

      # Dynamically construct group_by columns
      group_by_cols <- c("date_diff")
      if (!is.null(treatment_cols)) {
        group_by_cols <- c(group_by_cols, treatment_cols, "Time_zone")
      }

      # Group and summarise dynamically
      if (isTRUE(input$time_smooth) && exist_adjCH4){
        df.dhly.trt2 <- trt_data_jour %>% group_by(!!!syms(group_by_cols)) %>% summarise(Date = first(Date_rcd),
                                                                                         Duration_minute = sum(Duration_sec, na.rm = TRUE)/60,
                                                                                         Duration = sprintf("%d:%02d:%02d", sum(Duration_sec, na.rm = TRUE) %/% 3600,
                                                                                                            (sum(Duration_sec, na.rm = TRUE) %% 3600) %/% 60,
                                                                                                            sum(Duration_sec, na.rm = TRUE) %% 60),
                                                                                         Nb_visit = n(),
                                                                                         Nb_animal = n_distinct(Farm_name),
                                                                                         Nb_visit_per_animal = Nb_visit / Nb_animal,
                                                                                         CO2_avg_gd = mean(CO2_gd, na.rm = TRUE),
                                                                                         CO2_SD = sd(CO2_gd, na.rm = TRUE),
                                                                                         CH4_avg_gd = mean(CH4_gd, na.rm = TRUE),
                                                                                         CH4_SD = sd(CH4_gd, na.rm = TRUE),
                                                                                         adjCH4_avg_gd = mean(adjCH4_gd, na.rm = TRUE),
                                                                                         adjCH4_SD = sd(adjCH4_gd, na.rm = TRUE),

                                                                                         H2_avg_gd = if (exist_H2) mean(H2_gd, na.rm = TRUE) else NA_real_,
                                                                                         H2_SD = if (exist_H2) sd(H2_gd, na.rm = TRUE) else NA_real_,
                                                                                         O2_avg_gd = if (exist_O2) mean(O2_gd, na.rm = TRUE) else NA_real_,
                                                                                         O2_SD = if (exist_O2) sd(O2_gd, na.rm = TRUE) else NA_real_,
                                                                                         Airflow_avg_ls = if (exist_airflow) mean(Airflow, na.rm = TRUE) else NA_real_,
                                                                                         Airflow_SD = if (exist_airflow) sd(Airflow, na.rm = TRUE) else NA_real_,
                                                                                         .groups = "drop")

      } else{
        df.dhly.trt2 <- trt_data_jour %>% group_by(!!!syms(group_by_cols)) %>% summarise(Date = first(Date_rcd),
                                                                                         Duration_minute = sum(Duration_sec, na.rm = TRUE)/60,
                                                                                         Duration = sprintf("%d:%02d:%02d", sum(Duration_sec, na.rm = TRUE) %/% 3600,
                                                                                                            (sum(Duration_sec, na.rm = TRUE) %% 3600) %/% 60,
                                                                                                            sum(Duration_sec, na.rm = TRUE) %% 60),
                                                                                         Nb_visit = n(),
                                                                                         Nb_animal = n_distinct(Farm_name),
                                                                                         Nb_visit_per_animal = Nb_visit / Nb_animal,
                                                                                         CO2_avg_gd = mean(CO2_gd, na.rm = TRUE),
                                                                                         CO2_SD = sd(CO2_gd, na.rm = TRUE),
                                                                                         CH4_avg_gd = mean(CH4_gd, na.rm = TRUE),
                                                                                         CH4_SD = sd(CH4_gd, na.rm = TRUE),

                                                                                         H2_avg_gd = if (exist_H2) mean(H2_gd, na.rm = TRUE) else NA_real_,
                                                                                         H2_SD = if (exist_H2) sd(H2_gd, na.rm = TRUE) else NA_real_,
                                                                                         O2_avg_gd = if (exist_O2) mean(O2_gd, na.rm = TRUE) else NA_real_,
                                                                                         O2_SD = if (exist_O2) sd(O2_gd, na.rm = TRUE) else NA_real_,
                                                                                         Airflow_avg_ls = if (exist_airflow) mean(Airflow, na.rm = TRUE) else NA_real_,
                                                                                         Airflow_SD = if (exist_airflow) sd(Airflow, na.rm = TRUE) else NA_real_,
                                                                                         .groups = "drop")

      }
      incProgress(0.7, detail = "Calculating...")
      #===========================================#
      incProgress(1, detail = "Finalizing...")

      # Empty column eliminate
      df.dhly.trt2 <- df.dhly.trt2 %>% select(where(~ !all(is.na(.))))

    })

    return(df.dhly.trt2)

  })

  # control download button for 'dhly trt2'
  shinyjs::disable("download_dhly_trt2")

  observe({
    if (!is.null(dhly_t2())) {
      shinyjs::enable("download_dhly_trt2")
    } else{
      shinyjs::disable("download_dhly_trt2")
    }
  })

  output$download_dhly_trt2 <- downloadHandler(
    filename = function() {
      paste("Daily_Hourly_Factor2_", Sys.Date(), ".xlsx", sep = "")
    },
    content = function(file) {
      data_to_save <- dhly_t2()
      if (!is.null(data_to_save)){
        openxlsx::write.xlsx(data_to_save, file)

      } else{
        stop("No data available to downloads.")
      }
    }
  )

  ########################################################################################################################
  # Daily & Hourly average per Factor1 X Factor2
  dhly_t1xt2 <- eventReactive(input$calc_dhly_trt1xtrt2, {
    # req(filt_data(), input$date_filt_range, input$treat1, input$treat2)
    req(filt_data(), input$treat1, input$treat2)
    print("== Daily & Hourly statistics per Factor1 X Factor2 ==")

    trt_data <- filt_data()

    withProgress(message = "Calculating Daily & Hourly statistics - Factor1 X Factor2...", value = 0, {
      #===========================================#
      incProgress(0.2, detail = "Loading data...")
      # Date difference calculation from start date: 1st day of sampling = 0
      # date_diff <- abs(as.numeric(as.Date(input$date_filt_range[1]) - as.Date(trt_data$Date_rcd)))
      start_date <- min(as.Date(trt_data$Date_rcd), na.rm = TRUE)
      date_diff <- as.numeric(as.Date(trt_data$Date_rcd) - start_date)
      
      trt_data_jour <- add_column(trt_data, date_diff = date_diff)

      # optional column check
      exist_H2 <- "H2_gd" %in% colnames(trt_data)
      exist_O2 <- "O2_gd" %in% colnames(trt_data)
      exist_airflow <- "Airflow" %in% colnames(trt_data)
      exist_adjCH4 <- "adjCH4_gd" %in% colnames(trt_data)

      # Check if treatment columns exists, add only if they are non-null
      treatment_cols <- c(input$treat1, input$treat2)
      treatment_cols <- treatment_cols[!is.null(treatment_cols) & nzchar(treatment_cols)]
      #===========================================#
      incProgress(0.5, detail = "Calculating...")

      trt_data_jour <- trt_data_jour %>% mutate(Time_zone = as.numeric(Time_zone))

      # Dynamically construct group_by columns
      group_by_cols <- c("date_diff")
      if (!is.null(treatment_cols)) {
        group_by_cols <- c(group_by_cols, treatment_cols, "Time_zone")
      }

      # Group and summarise dynamically
      if (isTRUE(input$time_smooth) && exist_adjCH4){
        df.dhly.trt1xtrt2 <- trt_data_jour %>% group_by(!!!syms(group_by_cols)) %>% summarise(Date = first(Date_rcd),
                                                                                              Duration_minute = sum(Duration_sec, na.rm = TRUE)/60,
                                                                                              Duration = sprintf("%d:%02d:%02d", sum(Duration_sec, na.rm = TRUE) %/% 3600,
                                                                                                                 (sum(Duration_sec, na.rm = TRUE) %% 3600) %/% 60,
                                                                                                                 sum(Duration_sec, na.rm = TRUE) %% 60),
                                                                                              Nb_visit = n(),
                                                                                              Nb_animal = n_distinct(Farm_name),
                                                                                              Nb_visit_per_animal = Nb_visit / Nb_animal,
                                                                                              CO2_avg_gd = mean(CO2_gd, na.rm = TRUE),
                                                                                              CO2_SD = sd(CO2_gd, na.rm = TRUE),
                                                                                              CH4_avg_gd = mean(CH4_gd, na.rm = TRUE),
                                                                                              CH4_SD = sd(CH4_gd, na.rm = TRUE),
                                                                                              adjCH4_avg_gd = mean(adjCH4_gd, na.rm = TRUE),
                                                                                              adjCH4_SD = sd(adjCH4_gd, na.rm = TRUE),

                                                                                              H2_avg_gd = if (exist_H2) mean(H2_gd, na.rm = TRUE) else NA_real_,
                                                                                              H2_SD = if (exist_H2) sd(H2_gd, na.rm = TRUE) else NA_real_,
                                                                                              O2_avg_gd = if (exist_O2) mean(O2_gd, na.rm = TRUE) else NA_real_,
                                                                                              O2_SD = if (exist_O2) sd(O2_gd, na.rm = TRUE) else NA_real_,
                                                                                              Airflow_avg_ls = if (exist_airflow) mean(Airflow, na.rm = TRUE) else NA_real_,
                                                                                              Airflow_SD = if (exist_airflow) sd(Airflow, na.rm = TRUE) else NA_real_,
                                                                                              .groups = "drop")
      } else{
        df.dhly.trt1xtrt2 <- trt_data_jour %>% group_by(!!!syms(group_by_cols)) %>% summarise(Date = first(Date_rcd),
                                                                                              Duration_minute = sum(Duration_sec, na.rm = TRUE)/60,
                                                                                              Duration = sprintf("%d:%02d:%02d", sum(Duration_sec, na.rm = TRUE) %/% 3600,
                                                                                                                 (sum(Duration_sec, na.rm = TRUE) %% 3600) %/% 60,
                                                                                                                 sum(Duration_sec, na.rm = TRUE) %% 60),
                                                                                              Nb_visit = n(),
                                                                                              Nb_animal = n_distinct(Farm_name),
                                                                                              Nb_visit_per_animal = Nb_visit / Nb_animal,
                                                                                              CO2_avg_gd = mean(CO2_gd, na.rm = TRUE),
                                                                                              CO2_SD = sd(CO2_gd, na.rm = TRUE),
                                                                                              CH4_avg_gd = mean(CH4_gd, na.rm = TRUE),
                                                                                              CH4_SD = sd(CH4_gd, na.rm = TRUE),

                                                                                              H2_avg_gd = if (exist_H2) mean(H2_gd, na.rm = TRUE) else NA_real_,
                                                                                              H2_SD = if (exist_H2) sd(H2_gd, na.rm = TRUE) else NA_real_,
                                                                                              O2_avg_gd = if (exist_O2) mean(O2_gd, na.rm = TRUE) else NA_real_,
                                                                                              O2_SD = if (exist_O2) sd(O2_gd, na.rm = TRUE) else NA_real_,
                                                                                              Airflow_avg_ls = if (exist_airflow) mean(Airflow, na.rm = TRUE) else NA_real_,
                                                                                              Airflow_SD = if (exist_airflow) sd(Airflow, na.rm = TRUE) else NA_real_,
                                                                                              .groups = "drop")
      }

      incProgress(0.7, detail = "Calculating...")
      #===========================================#
      incProgress(1, detail = "Finalizing...")

      # Empty column eliminate
      df.dhly.trt1xtrt2 <- df.dhly.trt1xtrt2 %>% select(where(~ !all(is.na(.))))

    })

    return(df.dhly.trt1xtrt2)

  })

  # control download button for 'dhly trt1xtrt2'
  shinyjs::disable("download_dhly_trt1xtrt2")

  observe({
    if (!is.null(dhly_t1xt2())) {
      shinyjs::enable("download_dhly_trt1xtrt2")
    } else{
      shinyjs::disable("download_dhly_trt1xtrt2")
    }
  })

  output$download_dhly_trt1xtrt2 <- downloadHandler(
    filename = function() {
      paste("Daily_Hourly_Factor1xFactor2_", Sys.Date(), ".xlsx", sep = "")
    },
    content = function(file) {
      data_to_save <- dhly_t1xt2()
      if (!is.null(data_to_save)){
        openxlsx::write.xlsx(data_to_save, file)

      } else{
        stop("No data available to downloads.")
      }
    }
  )

  ########################################################################################################################
  # Running all the Daily analysis
  observeEvent(input$calc_all_dhly, {
    shinyjs::click("calc_dhly_entire")
    shinyjs::click("calc_dhly_anim")

    if (!is.null(input$treat1) && nzchar(input$treat1)){
      shinyjs::click("calc_dhly_trt1")
    }

    if (!is.null(input$treat2) && nzchar(input$treat2)){
      shinyjs::click("calc_dhly_trt2")
    }

    if(!is.null(input$treat1) && nzchar(input$treat1) && !is.null(input$treat2) && nzchar(input$treat2)){
      shinyjs::click("calc_dhly_trt1xtrt2")
    }
  })

  #======================================================================================================================#
  # REPEATABILITY PERIOD BASED ANALYSIS #
  #======================================================================================================================#

  # Period average for Entire dataset
  rep_total <- eventReactive(input$calc_rep_entire, {
    req(filt_data())
    req(rep_preview_info())

    print("== Period statistics for entire dataset ==")

    preview_info <- rep_preview_info()
    dividers <- as.integer(unlist(preview_info$dividers, use.names = FALSE))

    validate(
      need(length(dividers) > 0, "No valid divider found. Please click 'Select Done' first.")
    )

    trt_data <- build_rep_dataset(df = filt_data(),
                                  date_mode = input$rep_date_mode,
                                  date_range = input$rep_date_range)

    wb <- createWorkbook()

    withProgress(message = "Calculating Period statistics - Entire dataset...", value = 0, {

      incProgress(0.2, detail = "Loading data...")

      exist_H2 <- "H2_gd" %in% colnames(trt_data)
      exist_O2 <- "O2_gd" %in% colnames(trt_data)
      exist_airflow <- "Airflow" %in% colnames(trt_data)
      exist_adjCH4 <- "adjCH4_gd" %in% colnames(trt_data)

      addWorksheet(wb, "Repeatability_results")
      writeData(wb, sheet = "Repeatability_results", rep_result_data())

      incProgress(0.5, detail = "Calculating...")

      for (jour.per in dividers){

        tmp.trt.data.jour <- trt_data %>% mutate(rep_day_index = as.integer(rep_day_index),
                                                 Period = ((rep_day_index - 1L) %/% as.integer(jour.per)) + 1L)

        if (identical(input$rep_date_mode, "ordinal")) {

          period_check <- tmp.trt.data.jour %>% distinct(Farm_name, rep_day_index, Period) %>% count(Farm_name, Period, name = "n_days")

          valid_periods <- period_check %>% filter(n_days == jour.per) %>% select(Farm_name, Period)

          tmp.trt.data.jour <- tmp.trt.data.jour %>% semi_join(valid_periods, by = c("Farm_name", "Period"))

        } else {

          period_day_check <- tmp.trt.data.jour %>% distinct(rep_day_index, Period) %>% count(Period, name = "n_days")

          valid_period_ids <- period_day_check %>% filter(n_days == jour.per) %>% pull(Period)

          tmp.trt.data.jour <- tmp.trt.data.jour %>% filter(Period %in% valid_period_ids)
        }

        if (nrow(tmp.trt.data.jour) == 0) next

        if (isTRUE(input$time_smooth) && exist_adjCH4) {

          df.period.total <- tmp.trt.data.jour %>% group_by(Period) %>% summarise(Nb_visit = n(),
                                                                                  Nb_animal = n_distinct(Farm_name),
                                                                                  Nb_visit_per_animal = Nb_visit / Nb_animal,
                                                                                  CO2_avg_gd = mean(CO2_gd, na.rm = TRUE),
                                                                                  CO2_SD = sd(CO2_gd, na.rm = TRUE),
                                                                                  CH4_avg_gd = mean(CH4_gd, na.rm = TRUE),
                                                                                  CH4_SD = sd(CH4_gd, na.rm = TRUE),
                                                                                  adjCH4_avg_gd = mean(adjCH4_gd, na.rm = TRUE),
                                                                                  adjCH4_SD = sd(adjCH4_gd, na.rm = TRUE),

                                                                                  H2_avg_gd = if (exist_H2) mean(H2_gd, na.rm = TRUE) else NA_real_,
                                                                                  H2_SD = if (exist_H2) sd(H2_gd, na.rm = TRUE) else NA_real_,
                                                                                  O2_avg_gd = if (exist_O2) mean(O2_gd, na.rm = TRUE) else NA_real_,
                                                                                  O2_SD = if (exist_O2) sd(O2_gd, na.rm = TRUE) else NA_real_,
                                                                                  Airflow_avg_ls = if (exist_airflow) mean(Airflow, na.rm = TRUE) else NA_real_,
                                                                                  Airflow_SD = if (exist_airflow) sd(Airflow, na.rm = TRUE) else NA_real_,
                                                                                  .groups = "drop")
        } else {

          df.period.total <- tmp.trt.data.jour %>% group_by(Period) %>% summarise(Nb_visit = n(),
                                                                                  Nb_animal = n_distinct(Farm_name),
                                                                                  Nb_visit_per_animal = Nb_visit / Nb_animal,
                                                                                  CO2_avg_gd = mean(CO2_gd, na.rm = TRUE),
                                                                                  CO2_SD = sd(CO2_gd, na.rm = TRUE),
                                                                                  CH4_avg_gd = mean(CH4_gd, na.rm = TRUE),
                                                                                  CH4_SD = sd(CH4_gd, na.rm = TRUE),

                                                                                  H2_avg_gd = if (exist_H2) mean(H2_gd, na.rm = TRUE) else NA_real_,
                                                                                  H2_SD = if (exist_H2) sd(H2_gd, na.rm = TRUE) else NA_real_,
                                                                                  O2_avg_gd = if (exist_O2) mean(O2_gd, na.rm = TRUE) else NA_real_,
                                                                                  O2_SD = if (exist_O2) sd(O2_gd, na.rm = TRUE) else NA_real_,
                                                                                  Airflow_avg_ls = if (exist_airflow) mean(Airflow, na.rm = TRUE) else NA_real_,
                                                                                  Airflow_SD = if (exist_airflow) sd(Airflow, na.rm = TRUE) else NA_real_,
                                                                                  .groups = "drop"
                                                                                  )
        }

        df.period.total <- df.period.total %>%
          select(where(~ !all(is.na(.))))

        sheet_name <- paste0(jour.per, "DpSG")

        addWorksheet(wb, sheet_name)
        writeData(wb, sheet = sheet_name, df.period.total)
      }

      incProgress(0.7, detail = "Saving...")

      file_path <- file.path(tempdir(), paste0("Period_Based_Entire_", Sys.Date(), ".xlsx"))

      saveWorkbook(wb, file = file_path, overwrite = TRUE)

      incProgress(1, detail = "Finalizing...")

      return(file_path)
    })
  })

  observeEvent(input$calc_rep_entire,{
    rep_total()
    shinyjs::enable("download_rep_entire")
  })

  output$download_rep_entire <- downloadHandler(
    filename = function() {
      paste0("Period_Based_Entire_", Sys.Date(), ".xlsx")
    },
    content = function(file) {
      req(rep_total())
      file.copy(rep_total(), file)
    }
  )


  ########################################################################################################################
  # Period average per Animal
  rep_anim <- eventReactive(input$calc_rep_anim, {
    req(filt_data())
    req(rep_preview_info())

    print("== Period statistics for Animal ==")

    preview_info <- rep_preview_info()
    dividers <- as.integer(unlist(preview_info$dividers, use.names = FALSE))

    validate(
      need(length(dividers) > 0, "No valider found. Please click 'Select Done' first.")
    )

    trt_data <- build_rep_dataset(df = filt_data(),
                                  date_mode = input$rep_date_mode,
                                  date_range = input$rep_date_range)

    wb <- createWorkbook()

    withProgress(message = "Calculating Period statistics - Animal...", value = 0, {
      #===========================================#
      incProgress(0.2, detail = "Loading data...")

      # optional column check
      exist_H2 <- "H2_gd" %in% colnames(trt_data)
      exist_O2 <- "O2_gd" %in% colnames(trt_data)
      exist_airflow <- "Airflow" %in% colnames(trt_data)
      exist_adjCH4 <- "adjCH4_gd" %in% colnames(trt_data)
      
      # Check if treatment columns exists, add only if they are non-null
      treatment_cols <- input$treat1
      treatment_cols <- treatment_cols[!is.null(treatment_cols) & nzchar(treatment_cols)]
      
      #===========================================#
      incProgress(0.5, detail = "Calculating...")
      
      addWorksheet(wb, "Repeatability_results")
      writeData(wb, sheet = "Repeatability_results", rep_result_data())
      
      # Dynamically construct group_by column
      group_by_cols <- c("Period")
      
      if (!is.null(treatment_cols)) {
        group_by_cols <- c(group_by_cols, treatment_cols)
      }
      
      for (jour.per in dividers){
        tmp.trt.data.jour <- trt_data %>% mutate(rep_day_index = as.integer(rep_day_index),
                                                      Period = ((rep_day_index - 1L) %/% as.integer(jour.per)) + 1L)

        if (identical(input$rep_date_mode, "ordinal")){
          period_check <- tmp.trt.data.jour %>% distinct(Farm_name, rep_day_index, Period) %>% count(Farm_name, Period, name = "n_days")

          valid_periods <- period_check %>% filter(n_days == jour.per) %>% select(Farm_name, Period)

          tmp.trt.data.jour <- tmp.trt.data.jour %>% semi_join(valid_periods, by = c("Farm_name", "Period"))

        } else {
          period_day_check <- tmp.trt.data.jour %>% distinct(rep_day_index, Period) %>% count(Period, name = "n_days")

          valid_periods_ids <- period_day_check %>% filter(n_days == jour.per) %>% pull(Period)

          tmp.trt.data.jour <- tmp.trt.data.jour %>% filter(Period %in% valid_periods_ids)
        }

        if (nrow(tmp.trt.data.jour) == 0) next

        if (isTRUE(input$time_smooth) && exist_adjCH4){

          df.period.anim <- tmp.trt.data.jour %>% group_by(Farm_name, Period) %>% summarise(Farm_name = unique(Farm_name),
                                                                                            across(all_of(treatment_cols), ~first(na.omit(.)), .names = "{.col}"),
                                                                                            Nb_visit = n(),
                                                                                            CO2_avg_gd = mean(CO2_gd, na.rm = TRUE),
                                                                                            CO2_SD = sd(CO2_gd, na.rm = TRUE),
                                                                                            CH4_avg_gd = mean(CH4_gd, na.rm = TRUE),
                                                                                            CH4_SD = sd(CH4_gd, na.rm = TRUE),
                                                                                            adjCH4_avg_gd = mean(adjCH4_gd, na.rm = TRUE),
                                                                                            adjCH4_SD = sd(adjCH4_gd, na.rm = TRUE),

                                                                                            H2_avg_gd = if (exist_H2) mean(H2_gd, na.rm = TRUE) else NA_real_,
                                                                                            H2_SD = if (exist_H2) sd(H2_gd, na.rm = TRUE) else NA_real_,
                                                                                            O2_avg_gd = if (exist_O2) mean(O2_gd, na.rm = TRUE) else NA_real_,
                                                                                            O2_SD = if (exist_O2) sd(O2_gd, na.rm = TRUE) else NA_real_,
                                                                                            Airflow_avg_ls = if (exist_airflow) mean(Airflow, na.rm = TRUE) else NA_real_,
                                                                                            Airflow_SD = if (exist_airflow) sd(Airflow, na.rm = TRUE) else NA_real_,
                                                                                            .groups = "drop")

        } else {
          df.period.anim <- tmp.trt.data.jour %>% group_by(Farm_name, Period) %>% summarise(Farm_name = unique(Farm_name),
                                                                                            across(all_of(treatment_cols), ~first(na.omit(.)), .names = "{.col}"),
                                                                                            Nb_visit = n(),
                                                                                            CO2_avg_gd = mean(CO2_gd, na.rm = TRUE),
                                                                                            CO2_SD = sd(CO2_gd, na.rm = TRUE),
                                                                                            CH4_avg_gd = mean(CH4_gd, na.rm = TRUE),
                                                                                            CH4_SD = sd(CH4_gd, na.rm = TRUE),

                                                                                            H2_avg_gd = if (exist_H2) mean(H2_gd, na.rm = TRUE) else NA_real_,
                                                                                            H2_SD = if (exist_H2) sd(H2_gd, na.rm = TRUE) else NA_real_,
                                                                                            O2_avg_gd = if (exist_O2) mean(O2_gd, na.rm = TRUE) else NA_real_,
                                                                                            O2_SD = if (exist_O2) sd(O2_gd, na.rm = TRUE) else NA_real_,
                                                                                            Airflow_avg_ls = if (exist_airflow) mean(Airflow, na.rm = TRUE) else NA_real_,
                                                                                            Airflow_SD = if (exist_airflow) sd(Airflow, na.rm = TRUE) else NA_real_,
                                                                                            .groups = "drop")

        }


        # Empty column eliminate
        df.period.anim <- df.period.anim %>% select(where(~ !all(is.na(.))))

        sheet_name <- paste0(jour.per, "DpSG")
        addWorksheet(wb, sheet_name)
        writeData(wb, sheet = sheet_name, df.period.anim)
      }

      incProgress(0.7, detail = "Saving...")
      file_path <- file.path(tempdir(), paste("Period_Based_Anim_", Sys.Date(), ".xlsx", sep = ""))
      saveWorkbook(wb, file = file_path, overwrite = TRUE)
      #===========================================#
      incProgress(1, detail = "Finalizing...")

      return(file_path)

    })
  })

  observeEvent(input$calc_rep_anim,{
    rep_anim()
    shinyjs::enable("download_rep_anim")
  })

  output$download_rep_anim <- downloadHandler(
    filename = function() {
      paste0("Period_Based_Anim_", Sys.Date(), ".xlsx")
    },
    content = function(file) {
      req(rep_anim())
      file.copy(rep_anim(), file)
    }
  )


  ########################################################################################################################
  # Period average per Factor1
  rep_trt1 <- eventReactive(input$calc_rep_trt1, {
    req(filt_data(), input$treat1, rep_preview_info())
    print("== Period statistics for Factor1 ==")

    preview_info <- rep_preview_info()
    dividers <- as.integer(unlist(preview_info$dividers, use.names = FALSE))

    validate(
      need(length(dividers) > 0, "No valid divider found. Please click 'Select Done' first,")
    )

    trt_data <- build_rep_dataset(df = filt_data(),
                                  date_mode = input$rep_date_mode,
                                  date_range = input$rep_date_range)

    wb <- createWorkbook()

    withProgress(message = "Calculating Period statistics - Factor1...", value = 0, {
      #===========================================#
      incProgress(0.2, detail = "Loading data...")

      # optional column check
      exist_H2 <- "H2_gd" %in% colnames(trt_data)
      exist_O2 <- "O2_gd" %in% colnames(trt_data)
      exist_airflow <- "Airflow" %in% colnames(trt_data)
      exist_adjCH4 <- "adjCH4_gd" %in% colnames(trt_data)

      # Check if treatment columns exists, add only if they are non-null
      treatment_cols <- input$treat1
      treatment_cols <- treatment_cols[!is.null(treatment_cols) & nzchar(treatment_cols)]

      #===========================================#
      incProgress(0.5, detail = "Calculating...")

      addWorksheet(wb, "Repeatability_results")
      writeData(wb, sheet = "Repeatability_results", rep_result_data())

      # Dynamically construct group_by column
      group_by_cols <- c("Period")

      if (!is.null(treatment_cols)) {
        group_by_cols <- c(group_by_cols, treatment_cols)
      }

      for (jour.per in dividers){
        tmp.trt.data.jour <- trt_data %>% mutate(rep_day_index = as.integer(rep_day_index),
                                                 Period = ((rep_day_index - 1L) %/% as.integer(jour.per)) + 1L)

        if (identical(input$rep_date_mode,  "ordinal")){
          period_check <- tmp.trt.data.jour %>% distinct(Farm_name, rep_day_index, Period) %>% count(Farm_name, Period, name = "n_days")

          valid_periods <- period_check %>% filter(n_days == jour.per) %>% select(Farm_name, Period)

          tmp.trt.data.jour <- tmp.trt.data.jour %>% semi_join(valid_periods, by = c("Farm_name", "Period"))
        } else {
          period_day_check <- tmp.trt.data.jour %>% distinct(rep_day_index, Period) %>% count(Period, name = "n_days")

          valid_periods_ids <- period_day_check %>% filter(n_days == jour.per) %>% pull(Period)

          tmp.trt.data.jour <- tmp.trt.data.jour %>% filter(Period %in% valid_periods_ids)
        }

        if (nrow(tmp.trt.data.jour) == 0) next

        if (isTRUE(input$time_smooth) && exist_adjCH4){
          df.period.trt1 <- tmp.trt.data.jour %>% group_by(!!!syms(group_by_cols)) %>% summarise(Nb_visit = n(),
                                                                                                 Nb_animal = n_distinct(Farm_name),
                                                                                                 Nb_visit_per_animal = Nb_visit / Nb_animal,
                                                                                                 CO2_avg_gd = mean(CO2_gd, na.rm = TRUE),
                                                                                                 CO2_SD = sd(CO2_gd, na.rm = TRUE),
                                                                                                 CH4_avg_gd = mean(CH4_gd, na.rm = TRUE),
                                                                                                 CH4_SD = sd(CH4_gd, na.rm = TRUE),
                                                                                                 adjCH4_avg_gd = mean(adjCH4_gd, na.rm = TRUE),
                                                                                                 adjCH4_SD = sd(adjCH4_gd, na.rm = TRUE),

                                                                                                 H2_avg_gd = if (exist_H2) mean(H2_gd, na.rm = TRUE) else NA_real_,
                                                                                                 H2_SD = if (exist_H2) sd(H2_gd, na.rm = TRUE) else NA_real_,
                                                                                                 O2_avg_gd = if (exist_O2) mean(O2_gd, na.rm = TRUE) else NA_real_,
                                                                                                 O2_SD = if (exist_O2) sd(O2_gd, na.rm = TRUE) else NA_real_,
                                                                                                 Airflow_avg_ls = if (exist_airflow) mean(Airflow, na.rm = TRUE) else NA_real_,
                                                                                                 Airflow_SD = if (exist_airflow) sd(Airflow, na.rm = TRUE) else NA_real_,
                                                                                                 .groups = "drop")

        } else {
          df.period.trt1 <- tmp.trt.data.jour %>% group_by(!!!syms(group_by_cols)) %>% summarise(Nb_visit = n(),
                                                                                                 Nb_animal = n_distinct(Farm_name),
                                                                                                 Nb_visit_per_animal = Nb_visit / Nb_animal,
                                                                                                 CO2_avg_gd = mean(CO2_gd, na.rm = TRUE),
                                                                                                 CO2_SD = sd(CO2_gd, na.rm = TRUE),
                                                                                                 CH4_avg_gd = mean(CH4_gd, na.rm = TRUE),
                                                                                                 CH4_SD = sd(CH4_gd, na.rm = TRUE),

                                                                                                 H2_avg_gd = if (exist_H2) mean(H2_gd, na.rm = TRUE) else NA_real_,
                                                                                                 H2_SD = if (exist_H2) sd(H2_gd, na.rm = TRUE) else NA_real_,
                                                                                                 O2_avg_gd = if (exist_O2) mean(O2_gd, na.rm = TRUE) else NA_real_,
                                                                                                 O2_SD = if (exist_O2) sd(O2_gd, na.rm = TRUE) else NA_real_,
                                                                                                 Airflow_avg_ls = if (exist_airflow) mean(Airflow, na.rm = TRUE) else NA_real_,
                                                                                                 Airflow_SD = if (exist_airflow) sd(Airflow, na.rm = TRUE) else NA_real_,
                                                                                                 .groups = "drop")
        }

        # Empty column eliminate
        df.period.trt1 <- df.period.trt1 %>% select(where(~ !all(is.na(.))))

        sheet_name <- paste0(jour.per, "DpSG")
        addWorksheet(wb, sheet_name)
        writeData(wb, sheet = sheet_name, df.period.trt1)
      }

      incProgress(0.7, detail = "Saving...")
      file_path <- file.path(tempdir(), paste("Period_Based_Factor1_", Sys.Date(), ".xlsx", sep = ""))
      saveWorkbook(wb, file = file_path, overwrite = TRUE)
      #===========================================#
      incProgress(1, detail = "Finalizing...")

      return(file_path)

    })
  })

  observeEvent(input$calc_rep_trt1,{
    rep_trt1()
    shinyjs::enable("download_rep_trt1")
  })

  output$download_rep_trt1 <- downloadHandler(
    filename = function() {
      paste0("Period_Based_Factor1_", Sys.Date(), ".xlsx")
    },
    content = function(file) {
      req(rep_trt1())
      file.copy(rep_trt1(), file)
    }
  )

  ########################################################################################################################
  # Period average per Factor2
  rep_trt2 <- eventReactive(input$calc_rep_trt2, {
    req(filt_data(), input$treat2, rep_preview_info())

    print("== Period statistics for Factor2 ==")

    preview_info <- rep_preview_info()
    dividers <- as.integer(unlist(preview_info$dividers, use.names = FALSE))

    validate(
      need(length(dividers) > 0, "No valid divider found. Please click 'Select Done' first.")
    )

    trt_data <- build_rep_dataset(df = filt_data(),
                                  date_mode = input$rep_date_mode,
                                  date_range = input$rep_date_range)

    wb <- createWorkbook()

    withProgress(message = "Calculating Period statistics - Factor2...", value = 0, {
      #===========================================#
      incProgress(0.2, detail = "Loading data...")

      # optional column check
      exist_H2 <- "H2_gd" %in% colnames(trt_data)
      exist_O2 <- "O2_gd" %in% colnames(trt_data)
      exist_airflow <- "Airflow" %in% colnames(trt_data)
      exist_adjCH4 <- "adjCH4_gd" %in% colnames(trt_data)

      # Check if treatment columns exists, add only if they are non-null
      treatment_cols <- input$treat2
      treatment_cols <- treatment_cols[!is.null(treatment_cols) & nzchar(treatment_cols)]

      #===========================================#
      incProgress(0.5, detail = "Calculating...")

      addWorksheet(wb, "Repeatability_results")
      writeData(wb, sheet = "Repeatability_results", rep_result_data())

      # Dynamically construct group_by column
      group_by_cols <- c("Period")

      if (!is.null(treatment_cols)) {
        group_by_cols <- c(group_by_cols, treatment_cols)
      }

      for (jour.per in dividers){
        tmp.trt.data.jour <- trt_data %>% mutate(rep_day_index = as.integer(rep_day_index),
                                                 Period = ((rep_day_index - 1L) %/% as.integer(jour.per)) + 1L)

        if (identical(input$rep_date_mode, "ordinal")){
          period_check <- tmp.trt.data.jour %>% distinct(Farm_name, rep_day_index, Period) %>% count(Farm_name, Period, name = "n_days")

          valid_periods <- period_check %>% filter(n_days == jour.per) %>% select(Farm_name, Period)

          tmp.trt.data.jour <- tmp.trt.data.jour %>% semi_join(valid_periods, by = c("Farm_name", "Period"))
        } else {
          period_day_check <- tmp.trt.data.jour %>% distinct(rep_day_index, Period) %>% count(Period, name = "n_days")

          valid_period_ids <- period_day_check %>% filter(n_days == jour.per) %>% pull(Period)

          tmp.trt.data.jour <- tmp.trt.data.jour %>% filter(Period %in% valid_period_ids)
        }

        if (nrow(tmp.trt.data.jour) == 0) next

        if (isTRUE(input$time_smooth) && exist_adjCH4){
          df.period.trt2 <- tmp.trt.data.jour %>% group_by(!!!syms(group_by_cols)) %>% summarise(Nb_visit = n(),
                                                                                                 Nb_animal = n_distinct(Farm_name),
                                                                                                 Nb_visit_per_animal = Nb_visit / Nb_animal,
                                                                                                 CO2_avg_gd = mean(CO2_gd, na.rm = TRUE),
                                                                                                 CO2_SD = sd(CO2_gd, na.rm = TRUE),
                                                                                                 CH4_avg_gd = mean(CH4_gd, na.rm = TRUE),
                                                                                                 CH4_SD = sd(CH4_gd, na.rm = TRUE),
                                                                                                 adjCH4_avg_gd = mean(adjCH4_gd, na.rm = TRUE),
                                                                                                 adjCH4_SD = sd(adjCH4_gd, na.rm = TRUE),

                                                                                                 H2_avg_gd = if (exist_H2) mean(H2_gd, na.rm = TRUE) else NA_real_,
                                                                                                 H2_SD = if (exist_H2) sd(H2_gd, na.rm = TRUE) else NA_real_,
                                                                                                 O2_avg_gd = if (exist_O2) mean(O2_gd, na.rm = TRUE) else NA_real_,
                                                                                                 O2_SD = if (exist_O2) sd(O2_gd, na.rm = TRUE) else NA_real_,
                                                                                                 Airflow_avg_ls = if (exist_airflow) mean(Airflow, na.rm = TRUE) else NA_real_,
                                                                                                 Airflow_SD = if (exist_airflow) sd(Airflow, na.rm = TRUE) else NA_real_,
                                                                                                 .groups = "drop")
        } else {
          df.period.trt2 <- tmp.trt.data.jour %>% group_by(!!!syms(group_by_cols)) %>% summarise(Nb_visit = n(),
                                                                                                 Nb_animal = n_distinct(Farm_name),
                                                                                                 Nb_visit_per_animal = Nb_visit / Nb_animal,
                                                                                                 CO2_avg_gd = mean(CO2_gd, na.rm = TRUE),
                                                                                                 CO2_SD = sd(CO2_gd, na.rm = TRUE),
                                                                                                 CH4_avg_gd = mean(CH4_gd, na.rm = TRUE),
                                                                                                 CH4_SD = sd(CH4_gd, na.rm = TRUE),

                                                                                                 H2_avg_gd = if (exist_H2) mean(H2_gd, na.rm = TRUE) else NA_real_,
                                                                                                 H2_SD = if (exist_H2) sd(H2_gd, na.rm = TRUE) else NA_real_,
                                                                                                 O2_avg_gd = if (exist_O2) mean(O2_gd, na.rm = TRUE) else NA_real_,
                                                                                                 O2_SD = if (exist_O2) sd(O2_gd, na.rm = TRUE) else NA_real_,
                                                                                                 Airflow_avg_ls = if (exist_airflow) mean(Airflow, na.rm = TRUE) else NA_real_,
                                                                                                 Airflow_SD = if (exist_airflow) sd(Airflow, na.rm = TRUE) else NA_real_,
                                                                                                 .groups = "drop")
        }

        # Empty column eliminate
        df.period.trt2 <- df.period.trt2 %>% select(where(~ !all(is.na(.))))

        sheet_name <- paste0(jour.per, "DpSG")
        addWorksheet(wb, sheet_name)
        writeData(wb, sheet = sheet_name, df.period.trt2)
      }

      incProgress(0.7, detail = "Saving...")
      file_path <- file.path(tempdir(), paste("Period_Based_Factor2_", Sys.Date(), ".xlsx", sep = ""))
      saveWorkbook(wb, file = file_path, overwrite = TRUE)
      #===========================================#
      incProgress(1, detail = "Finalizing...")

      return(file_path)

    })
  })

  observeEvent(input$calc_rep_trt2,{
    rep_trt2()
    shinyjs::enable("download_rep_trt2")
  })

  output$download_rep_trt2 <- downloadHandler(
    filename = function() {
      paste0("Period_Based_Factor2_", Sys.Date(), ".xlsx")
    },
    content = function(file) {
      req(rep_trt2())
      file.copy(rep_trt2(), file)
    }
  )

  ########################################################################################################################
  # Period average per Factor1 X Factor2
  rep_trt1xtrt2 <- eventReactive(input$calc_rep_trt1xtrt2, {
    req(filt_data(), rep_preview_info())
    print("== Period statistics for Factor1 X Factor2")

    preview_info <- rep_preview_info()
    dividers <- as.integer(unlist(preview_info$dividers, use.names = FALSE))

    validate(
      need(length(dividers) > 0, "No valid divider found. Please click 'Select Done' first.")
    )

    trt_data <- build_rep_dataset(df = filt_data(),
                                  date_mode = input$rep_date_mode,
                                  date_range = input$rep_date_range)

    wb <- createWorkbook()

    withProgress(message = "Calculating Period statistics - Factor1 X Factor2...", value = 0, {
      #===========================================#
      incProgress(0.2, detail = "Loading data...")

      # optional column check
      exist_H2 <- "H2_gd" %in% colnames(trt_data)
      exist_O2 <- "O2_gd" %in% colnames(trt_data)
      exist_airflow <- "Airflow" %in% colnames(trt_data)
      exist_adjCH4 <- "adjCH4_gd" %in% colnames(trt_data)

      # Check if treatment columns exists, add only if they are non-null
      treatment_cols <- c(input$treat1, input$treat2)
      treatment_cols <- treatment_cols[!is.null(treatment_cols) & nzchar(treatment_cols)]

      #===========================================#
      incProgress(0.5, detail = "Calculating...")

      addWorksheet(wb, "Repeatability_results")
      writeData(wb, sheet = "Repeatability_results", rep_result_data())

      # Dynamically construct group_by column
      group_by_cols <- c("Period")

      if (!is.null(treatment_cols)) {
        group_by_cols <- c(group_by_cols, treatment_cols)
      }


      for (jour.per in dividers){

        tmp.trt.data.jour <- trt_data %>% mutate(rep_day_index = as.integer(rep_day_index),
                                                 Period = ((rep_day_index - 1L) %/% as.integer(jour.per)) + 1L)

        if (identical(input$rep_date_mode, "ordinal")) {

          period_check <- tmp.trt.data.jour %>% distinct(Farm_name, rep_day_index, Period) %>% count(Farm_name, Period, name = "n_days")

          valid_periods <- period_check %>% filter(n_days == jour.per) %>% select(Farm_name, Period)

          tmp.trt.data.jour <- tmp.trt.data.jour %>% semi_join(valid_periods, by = c("Farm_name", "Period"))

        } else {

          period_day_check <- tmp.trt.data.jour %>% distinct(rep_day_index, Period) %>% count(Period, name = "n_days")

          valid_period_ids <- period_day_check %>% filter(n_days == jour.per) %>% pull(Period)

          tmp.trt.data.jour <- tmp.trt.data.jour %>% filter(Period %in% valid_period_ids)
        }

        if (nrow(tmp.trt.data.jour) == 0) next

        if (isTRUE(input$time_smooth) && exist_adjCH4){

          df.period.trt1xtrt2 <- tmp.trt.data.jour %>% group_by(!!!syms(group_by_cols)) %>% summarise(Nb_visit = n(),
                                                                                                      Nb_animal = n_distinct(Farm_name),
                                                                                                      Nb_visit_per_animal = Nb_visit / Nb_animal,
                                                                                                      CO2_avg_gd = mean(CO2_gd, na.rm = TRUE),
                                                                                                      CO2_SD = sd(CO2_gd, na.rm = TRUE),
                                                                                                      CH4_avg_gd = mean(CH4_gd, na.rm = TRUE),
                                                                                                      CH4_SD = sd(CH4_gd, na.rm = TRUE),
                                                                                                      adjCH4_avg_gd = mean(adjCH4_gd, na.rm = TRUE),
                                                                                                      adjCH4_SD = sd(adjCH4_gd, na.rm = TRUE),

                                                                                                      H2_avg_gd = if (exist_H2) mean(H2_gd, na.rm = TRUE) else NA_real_,
                                                                                                      H2_SD = if (exist_H2) sd(H2_gd, na.rm = TRUE) else NA_real_,
                                                                                                      O2_avg_gd = if (exist_O2) mean(O2_gd, na.rm = TRUE) else NA_real_,
                                                                                                      O2_SD = if (exist_O2) sd(O2_gd, na.rm = TRUE) else NA_real_,
                                                                                                      Airflow_avg_ls = if (exist_airflow) mean(Airflow, na.rm = TRUE) else NA_real_,
                                                                                                      Airflow_SD = if (exist_airflow) sd(Airflow, na.rm = TRUE) else NA_real_,
                                                                                                      .groups = "drop")

        } else{
          df.period.trt1xtrt2 <- tmp.trt.data.jour %>% group_by(!!!syms(group_by_cols)) %>% summarise(Nb_visit = n(),
                                                                                                      Nb_animal = n_distinct(Farm_name),
                                                                                                      Nb_visit_per_animal = Nb_visit / Nb_animal,
                                                                                                      CO2_avg_gd = mean(CO2_gd, na.rm = TRUE),
                                                                                                      CO2_SD = sd(CO2_gd, na.rm = TRUE),
                                                                                                      CH4_avg_gd = mean(CH4_gd, na.rm = TRUE),
                                                                                                      CH4_SD = sd(CH4_gd, na.rm = TRUE),

                                                                                                      H2_avg_gd = if (exist_H2) mean(H2_gd, na.rm = TRUE) else NA_real_,
                                                                                                      H2_SD = if (exist_H2) sd(H2_gd, na.rm = TRUE) else NA_real_,
                                                                                                      O2_avg_gd = if (exist_O2) mean(O2_gd, na.rm = TRUE) else NA_real_,
                                                                                                      O2_SD = if (exist_O2) sd(O2_gd, na.rm = TRUE) else NA_real_,
                                                                                                      Airflow_avg_ls = if (exist_airflow) mean(Airflow, na.rm = TRUE) else NA_real_,
                                                                                                      Airflow_SD = if (exist_airflow) sd(Airflow, na.rm = TRUE) else NA_real_,
                                                                                                      .groups = "drop")

        }

        # Empty column eliminate
        df.period.trt1xtrt2 <- df.period.trt1xtrt2 %>% select(where(~ !all(is.na(.))))

        sheet_name <- paste0(jour.per, "DpSG")
        addWorksheet(wb, sheet_name)
        writeData(wb, sheet = sheet_name, df.period.trt1xtrt2)
      }

      incProgress(0.7, detail = "Saving...")
      file_path <- file.path(tempdir(), paste("Period_Based_Factor1XFactor2_", Sys.Date(), ".xlsx", sep = ""))
      saveWorkbook(wb, file = file_path, overwrite = TRUE)
      #===========================================#
      incProgress(1, detail = "Finalizing...")

      return(file_path)

    })
  })

  observeEvent(input$calc_rep_trt1xtrt2,{
    rep_trt1xtrt2()
    shinyjs::enable("download_rep_trt1xtrt2")
  })

  output$download_rep_trt1xtrt2 <- downloadHandler(
    filename = function() {
      paste0("Period_Based_Factor1XFactor2_", Sys.Date(), ".xlsx")
    },
    content = function(file) {
      req(rep_trt1xtrt2())
      file.copy(rep_trt1xtrt2(), file)
    }
  )

  ########################################################################################################################
  # Running all the period analysis
  observeEvent(input$calc_all_rep, {
    shinyjs::click("calc_rep_entire")
    shinyjs::click("calc_rep_anim")

    if (!is.null(input$treat1) && nzchar(input$treat1)){
      shinyjs::click("calc_rep_trt1")
    }

    if (!is.null(input$treat2) && nzchar(input$treat2)){
      shinyjs::click("calc_rep_trt2")
    }

    if(!is.null(input$treat1) && nzchar(input$treat1) && !is.null(input$treat2) && nzchar(input$treat2)){
      shinyjs::click("calc_rep_trt1xtrt2")
    }
  })

  ########################################################################################################################
  # RESULT GRAPHS MAKING #
  ########################################################################################################################
  ## CH4 distribution graph ##
  # Entire dataset #
  dist_plot <- eventReactive(input$plot_dist,{
    req(filt_data())

    data <- filt_data()

    if(input$ch4_dist_group == "Entire"){

      grp.dist.ch4 <- ggplot(data, aes(x = CH4_gd)) +
        geom_line(stat = "density", linewidth = 1) +
        labs(title = "CH<sub>4</sub> distribution (Entire dataset)",
             x = "CH<sub>4</sub> (g/day)", y = "Density") +
        grp_theme_SJ

    } else if (input$ch4_dist_group == "Factor1"){

      grp.dist.ch4 <- ggplot(data, aes(x = CH4_gd, fill = !!sym(input$treat1), color = !!sym(input$treat1))) +
        geom_line(stat = "density", linewidth = 1, aes(linetype = !!sym(input$treat1))) +
        labs(title = "CH<sub>4</sub> distribution (Factor1)",
             x = "CH<sub>4</sub> (g/day)", y = "Density") +
        grp_theme_SJ


    } else if (input$ch4_dist_group == "Factor2"){

      grp.dist.ch4 <- ggplot(data, aes(x = CH4_gd, fill = !!sym(input$treat2), color = !!sym(input$treat2))) +
        geom_line(stat = "density", linewidth = 1, aes(linetype = !!sym(input$treat2))) +
        labs(title = "CH<sub>4</sub> distribution (Factor2)",
             x = "CH<sub>4</sub> (g/day)", y = "Density") +
        grp_theme_SJ

    }

    ggplotly(grp.dist.ch4)
  })

  output$CH4_dist <- renderPlotly({
    dist_plot()
  })

  ########################################################################################################################
  ## CH4 plot ##
  # Without diurnal adjustment
  plot_ch4_data <- eventReactive(input$plot_ch4, {
    req(filt_data(), input$error_bar)
  
    ########## plot style ##########
    make_style <- function(lv){
      shape_vals <- c(16, 1, 15, 0, 17, 2)
      dash_vals <- c("solid","dashed","dotted","dotdash","longdash","twodash")
      nlv <- length(lv)
      list(
        shape_list = setNames(rep(shape_vals, length.out = nlv), lv),
        line_list = setNames(rep(dash_vals, length.out = nlv), lv)
      )
    }
    

    ########## Dataset preparing ##########
    if (input$ch4_group == "Whole") {
      df <- filt_data() %>% select(Time_zone, CH4_gd, any_of("adjCH4_gd"))
      group_vars <- "Time_zone"
      
    } else if (input$ch4_group == "Animal") {
      df <- filt_data() %>% select(Animal = Farm_name, Time_zone, CH4_gd, any_of("adjCH4_gd"))
      group_vars <- c("Animal", "Time_zone")
      
    } else if (input$ch4_group == "Factor1") {
      df <- filt_data() %>%
        select(.treat = !!sym(input$treat1), Time_zone, CH4_gd, any_of("adjCH4_gd"))
      group_vars <- c(".treat", "Time_zone")
      
    } else if (input$ch4_group == "Factor2") {
      df <- filt_data() %>%
        select(.treat = !!sym(input$treat2), Time_zone, CH4_gd, any_of("adjCH4_gd"))
      group_vars <- c(".treat", "Time_zone")
    }
    
    has_adj <- "adjCH4_gd" %in% names(df)
    
    ########## Data summary ##########
    
    gf.summary <- df %>%
      group_by(across(all_of(group_vars))) %>%
      summarise(
        CH4_avg = mean(CH4_gd, na.rm = TRUE),
        CH4_sd  = sd(CH4_gd, na.rm = TRUE),
        CH4_sd  = ifelse(is.na(CH4_sd), 0, CH4_sd),
        CH4_se  = CH4_sd/sqrt(n()),
        adjCH4_avg = if (has_adj) mean(adjCH4_gd, na.rm = TRUE) else NA_real_,
        adjCH4_sd  = if (has_adj) sd(adjCH4_gd, na.rm = TRUE) else NA_real_,
        adjCH4_se  = if (has_adj) adjCH4_sd / sqrt(n()) else NA_real_,
        .groups = "drop"
      )
    
    y.min <- min(gf.summary$CH4_avg, if (has_adj) gf.summary$adjCH4_avg else Inf, na.rm = TRUE)
    y.max <- max(gf.summary$CH4_avg, if (has_adj) gf.summary$adjCH4_avg else -Inf, na.rm = TRUE)
    
    if (input$error_bar == "SD") {
      lower <- pmin(gf.summary$CH4_avg - gf.summary$CH4_sd,
                    if (has_adj) gf.summary$adjCH4_avg - gf.summary$adjCH4_sd else Inf,
                    na.rm = TRUE)
      upper <- pmax(gf.summary$CH4_avg + gf.summary$CH4_sd,
                    if (has_adj) gf.summary$adjCH4_avg + gf.summary$adjCH4_sd else -Inf,
                    na.rm = TRUE)
      y.min <- min(lower, na.rm = TRUE)
      y.max <- max(upper, na.rm = TRUE)
    }
    
    
    if (input$ch4_group == "Whole") {
      aes_base <- aes(x = as.numeric(Time_zone), y = CH4_avg)
      
    } else {
      aes_base <- aes(
        x = as.numeric(Time_zone),
        y = CH4_avg,
        group = !!sym(group_vars[1]),
        color = !!sym(group_vars[1]),
        shape = !!sym(group_vars[1])
      )
    }
    
    ########## Plot ##########
    gplot.ch4 <- ggplot(gf.summary, aes_base)+ 
      geom_point(size = 2) + 
      geom_line(size = 1) +
      xlab("Time zone") +
      ylab("CH<sub>4</sub> (g/day)") +
      scale_x_continuous(breaks = 0:23) +
      scale_y_continuous(
        breaks = seq(floor(y.min) - 10, ceiling(y.max) + 10, 30),
        limits = c(y.min - 10, y.max + 10)) +
      grp_theme_SJ
    
    if (input$ch4_group %in% c("Factor1", "Factor2")) {
      gplot.ch4 <- gplot.ch4 +
        labs(
          color = if (input$ch4_group == "Factor1") input$treat1 else input$treat2,
          shape = if (input$ch4_group == "Factor1") input$treat1 else input$treat2
        )
    }
    
    if (input$error_bar == "SEM") {
      gplot.ch4 <- gplot.ch4 + geom_errorbar(aes(ymin = CH4_avg - CH4_se, ymax = CH4_avg + CH4_se))
      
    } else if (input$error_bar == "SD") {
      gplot.ch4 <- gplot.ch4 + geom_errorbar(aes(ymin = CH4_avg - CH4_sd, ymax = CH4_avg + CH4_sd))
    }
    
    return(gplot.ch4)

  })

  observeEvent(input$plot_ch4, {
    plot_state$CH4_plot_ready <- TRUE
    plot_state$adjCH4_plot_ready <- TRUE
  })

  output$CH4_time_grp <- renderPlotly({
    req(plot_state$CH4_plot_ready)
    req(plot_ch4_data())
    ggplotly(plot_ch4_data(), tooltip = "text") %>% style(line = list(connectgaps = TRUE))
  })


 # With diurnal adjustment
  
  plot_ch4_data_adj <- eventReactive(input$plot_ch4, {
    req(filt_data(), input$error_bar)
    req("adjCH4_gd" %in% names(filt_data()))
    
    ########## Data preparing ##########

    if (input$ch4_group == "Whole") {
      df <- filt_data() %>% select(Time_zone, CH4_gd, adjCH4_gd)
      group_vars <- "Time_zone"
      
    } else if (input$ch4_group == "Animal") {
      df <- filt_data() %>% select(Animal = Farm_name, Time_zone, CH4_gd, adjCH4_gd)
      group_vars <- c("Animal", "Time_zone")
      
    } else if (input$ch4_group == "Factor1") {
      df <- filt_data() %>% select(.treat = !!sym(input$treat1), Time_zone, CH4_gd, adjCH4_gd)
      group_vars <- c(".treat", "Time_zone")
      
    } else if (input$ch4_group == "Factor2") {
      df <- filt_data() %>% select(.treat = !!sym(input$treat2), Time_zone, CH4_gd, adjCH4_gd)
      group_vars <- c(".treat", "Time_zone")
    }
    
    gf.summary <- df %>%
      group_by(across(all_of(group_vars))) %>%
      summarise(adjCH4_avg = mean(adjCH4_gd, na.rm = TRUE),
                adjCH4_sd  = sd(adjCH4_gd, na.rm = TRUE),
                adjCH4_sd  = ifelse(is.na(adjCH4_sd), 0, adjCH4_sd),
                adjCH4_se  = adjCH4_sd / sqrt(n()),
                
                CH4_avg = mean(CH4_gd, na.rm = TRUE),
                CH4_sd  = sd(CH4_gd, na.rm = TRUE),
                CH4_sd  = ifelse(is.na(CH4_sd), 0, CH4_sd),
                CH4_se  = CH4_sd / sqrt(n()),
                
                .groups = "drop")

    ########## plot ##########
    y.min <- min(gf.summary$adjCH4_avg, gf.summary$CH4_avg, na.rm = TRUE)
    y.max <- max(gf.summary$adjCH4_avg, gf.summary$CH4_avg, na.rm = TRUE)
    
    if (input$error_bar == "SD") {
      lower <- pmin(gf.summary$adjCH4_avg - gf.summary$adjCH4_sd,
                    gf.summary$CH4_avg - gf.summary$CH4_sd, na.rm = TRUE)
      upper <- pmax(gf.summary$adjCH4_avg + gf.summary$adjCH4_sd,
                    gf.summary$CH4_avg + gf.summary$CH4_sd, na.rm = TRUE)
      y.min <- min(lower, na.rm = TRUE)
      y.max <- max(upper, na.rm = TRUE)
    }

    if (input$ch4_group == "Whole") {
      aes_base <- aes(x = as.numeric(Time_zone), y = adjCH4_avg)
      
    } else {
      aes_base <- aes(x = as.numeric(Time_zone),
                      y = adjCH4_avg,
                      group = !!sym(group_vars[1]),
                      color = !!sym(group_vars[1]),
                      shape = !!sym(group_vars[1]))
    }

    gplot.ch4.adj <- ggplot(gf.summary, aes_base) + geom_point(size = 2) +
      geom_line(size = 1) +
      xlab("Time zone") +
      ylab("Adjusted CH<sub>4</sub> (g/day)") +
      scale_x_continuous(breaks = 0:23) +
      scale_y_continuous(
        breaks = seq(floor(y.min) - 10, ceiling(y.max) + 10, 30),
        limits = c(y.min - 10, y.max + 10)) +
      grp_theme_SJ
    
    if (input$ch4_group %in% c("Factor1", "Factor2")) {
      gplot.ch4.adj <- gplot.ch4.adj + 
        labs(color = if (input$ch4_group == "Factor1") input$treat1 else input$treat2,
             shape = if (input$ch4_group == "Factor1") input$treat1 else input$treat2)
    }
    
    if (input$error_bar == "SEM") {
      gplot.ch4.adj <- gplot.ch4.adj + geom_errorbar(aes(ymin = adjCH4_avg - adjCH4_se,
                                                         ymax = adjCH4_avg + adjCH4_se))
      
    } else if (input$error_bar == "SD") {
      gplot.ch4.adj <- gplot.ch4.adj + geom_errorbar(aes(ymin = adjCH4_avg - adjCH4_sd,
                                                         ymax = adjCH4_avg + adjCH4_sd))
    }
    
    return(gplot.ch4.adj)
  })

  output$CH4_time_adj_grp <- renderPlotly({
    # print("CH4_time_adj_grp renderplotly called")
    # p <- plot_ch4_data_adj
    # ggplotly(p)
    req(plot_state$adjCH4_plot_ready)
    req(plot_ch4_data_adj())
    ggplotly(plot_ch4_data_adj(), tooltip = "text") %>% style(line = list(connectgaps = TRUE))
  })


  ########################################################################################################################
  ## Visit plot ##
  
  plot_visit_data <- eventReactive(input$plot_visit, {
    req(filt_data(), input$error_bar)

    ####### Data preparing #######
    if (input$visit_group == "Whole") {
      df <- filt_data() %>% select(Animal = Farm_name, Time_zone)
      group_vars <- "Time_zone"
      
    } else if (input$visit_group == "Animal") {
      df <- filt_data() %>% select(Animal = Farm_name, Time_zone)
      group_vars <- c("Animal", "Time_zone")
      
    } else if (input$visit_group == "Factor1") {
      df <- filt_data() %>% select(.treat = !!sym(input$treat1), Time_zone)
      group_vars <- c(".treat", "Time_zone")
      
    } else if (input$visit_group == "Factor2") {
      df <- filt_data() %>% select(.treat = !!sym(input$treat2), Time_zone)
      group_vars <- c(".treat", "Time_zone")
    }
    
    df.summary <- df %>%
      count(across(all_of(group_vars)), name = "visit")
    
    if (input$visit_ytype == "Percent") {
      
      if (input$visit_group == "Whole") {
        total_visit <- sum(df.summary$visit)
        df.summary <- df.summary %>% mutate(visit_pct = visit / total_visit * 100)
        
      } else {
        df.summary <- df.summary %>% group_by(across(all_of(group_vars[1]))) %>%
          mutate(visit_pct = visit / sum(visit) * 100) %>% ungroup()
      }
    }

    ####### Plot #######
        
    y_var <- if (input$visit_ytype == "Percent") "visit_pct" else "visit"
    y_label <- if (input$visit_ytype == "Percent") "Visit (%)" else "Total number of visits"
    
    if (input$visit_group == "Whole") {
      aes_base <- aes(x = as.numeric(Time_zone), y = .data[[y_var]])
      
    } else if (input$visit_group == "Animal"){
      aes_base <- aes(x = as.numeric(Time_zone),
                      y = .data[[y_var]],
                      group = Animal,
                      color = Animal,
                      shape = Animal,
                      text = paste("Animal: ", Animal))
    } else if (input$visit_group == "Factor1"){
      aes_base <- aes(x = as.numeric(Time_zone),
                      y = .data[[y_var]],
                      group = !!sym(group_vars[1]),
                      color = !!sym(group_vars[1]),
                      shape = !!sym(group_vars[1]),
                      text = paste(input$treat1, ": ", .treat))
    } else {
      aes_base <- aes(x = as.numeric(Time_zone),
                      y = .data[[y_var]],
                      group = !!sym(group_vars[1]),
                      color = !!sym(group_vars[1]),
                      shape = !!sym(group_vars[1]),
                      text = paste(input$treat2, ": ", .treat))
    }
    
    gplot.visit <- ggplot(df.summary, aes_base) +
      geom_point(size = 2) +
      geom_line(size = 1) +
      xlab("Time zone") +
      ylab(y_label) +
      scale_x_continuous(breaks = 0:23) +
      grp_theme_SJ
    
    if (input$visit_group %in% c("Factor1", "Factor2")) {
      gplot.visit <- gplot.visit +
        labs(color = if (input$visit_group == "Factor1") input$treat1 else input$treat2,
             shape = if (input$visit_group == "Factor1") input$treat1 else input$treat2)
    }

    return(list(
      plot = ggplotly(gplot.visit, tooltip = "text"),
      data = df.summary
    ))
  })
  

  observeEvent(input$plot_visit, {
    plot_state$visit_plot_ready <- TRUE
  })

  output$visit_time_grp <- renderPlotly({
    req(plot_state$visit_plot_ready)
    req(plot_visit_data())
    ggplotly(plot_visit_data()$plot, tooltip = "text")

  })

  session$onSessionEnded(function() {
    isolate({
      if(!isTRUE(session$userData$reloading)){
        stopApp()
      }
    })
  })

}

########################################################################################################################
# Run the application 
shinyApp(ui = ui, server = server)
