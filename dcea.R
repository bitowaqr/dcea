# Shiny app for aggregate distributional cost-effectiveness analysis tool
# 
# Created by: 
#   James Love-Koh
#   22/10/2020
# Adapted by: 
#   Paul Schneider
#   22/09/2022


# Loading packages
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinybusy)
library(rintrojs)
library(tidyverse)
library(ggrepel)
library(highcharter)
library(DT)
library(knitr)
library(markdown)
library(shinyjs)
library(shinycssloaders)

# Source functions
source("functions_revised.R")
source("ui_elements.R")
source("report_functions.R")

# Toggle DEV_MODE 
HIDE_LOADING_SCREEN = F

# rename highchart donwload btn
lang <- getOption("highcharter.lang")
lang$contextButtonTitle <- "Download"
options(highcharter.lang = lang)

# IMD 1-5 color pallette
imdCols = function(){
  viridis_pal(begin = 0.3, end = 0.8,option = "magma")(5)
}


# UI -------
ui = dashboardPage(
  
  # HEADER ----
  dashboardHeader(
    # left header title
    title = div(
      tags$img(src="york_mini.png",width="35px"), # Uni logo
      "Health Equity Impact Calculator",
    ),
    titleWidth = "320px",
    
    # right header menu
    tags$li(
      class = "dropdown pe-5 text-white fw-bold d-flex", 
      div(
        class = "me-3",
        icon("info-circle", verify_fa = FALSE),
        actionLink("showAbout","About",class="text-white")
      ),
      div(
        class = "me-3",
        icon("book"),
        actionLink("showReferences","References",class="text-white")
        ),
      tags$div(
        icon("question-circle", verify_fa = FALSE),
        actionLink("tutorial_start","Watch the tutorial",class="text-white")
      )
      )
    ),
  
  
  # SIDEBAR  ----
  dashboardSidebar(
    width = "300px",
    collapsed = F,
    
    sidebarMenu(
      id = "tabs",
      
      # *tab 1: Intervention -----
      menuItem(
        tabName = "intervention",
        "Intervention", 
        startExpanded=T,
          textInput("intName1", "Intervention",NULL,placeholder = "E.g. Smoking prevention"),
          textInput("compName1", "Comparator",NULL,placeholder = "E.g. No intervention"),
          selectInput(
            "intervention_type","Population type",
            c("Risk factor population","Disease population")
            ),
          conditionalPanel(
            condition="input.intervention_type=='Disease population'",
            selectizeInput(
              "intICD", "ICD code(s)", 
              choices = NULL, 
              multiple=TRUE
            ),                      
            sliderInput(
              "age_range","Recipient age range",
              value = c(0, 100), 
              step=1, min = 0, max = 100
              ),
          ),
          conditionalPanel(
            condition="input.intervention_type=='Risk factor population'",
            checkboxGroupInput(
              "intRF", 
              div(class="mb-2", "Risk factor(s)"),
              unique(distRF$risk_factor),
              selected = "Smoking"
              )
          ),
          
          shinyWidgets::autonumericInput(
            inputId = "intPop_c1", 
            label = div(tip("Target population size",tipId = "elig_pop_text"),class="mb-1"),
            value = 10, 
            decimalPlaces = 0,
          ),
          
          shinyWidgets::prettySwitch(
            "enable_intPop_c1",
            label = span(class ="small", "Set custom population size"),
            value = F,status = "warning"
            )
        
      ),
      
      
      
      
      # *tab 2: CEA inputs -----
      menuItem(
        tabName = "cea",
        id = "cea-tab",
        introBox("CEA inputs",
          data.step = 4, data.intro = "Fourth step of the intro",data.position = "right"
          ),
        autonumericInput("incQALYs_c1", "Inc. QALYs per recipient", 0.02,  currencySymbol = "",decimalPlaces = 2),
        autonumericInput("incCost_c1", "Inc. costs per recipient", 200,  currencySymbol = "£",decimalPlaces = 0),
        div(
          class = "px-3 mt-3", 
          introBox(
          div(
            class = "mb-1",
            "Resulting ICER:", 
            ),
          data.step = 5, data.intro = "Fifth step of the intro", data.position = "right"
          ),
          div(
            class = "text-dark w-100 card px-2 py-2 text-center fw-bold",
            style = "cursor: not-allowed; background-color: #aaaaaa",
            textOutput("icer_text",inline = T))
        )
      ),
      
      # *tab 3: Eligiblity -------
      menuItem(
        tabName = "eligiblity",
        id = "eligiblity-tab",
        "Eligiblity",
        introBox(
          data.step = 6, data.intro = "Sixth step of the intro", data.position = "right",
        div(
          class = "no-lab-sliders-group mb-3",
          div(class="d-flex align-items-end pe-3", sliderInput("prevQ1", "IMD 1 (Most deprived)",0.5,min=0,max=1,step=0.01,ticks = F),div(class="pb-1 fw-bold text-decoration-underline",textOutput("prevQ1Compt"))),
          div(class="d-flex align-items-end pe-3", sliderInput("prevQ2", "IMD2",0.5,min=0,max=1,step=0.01,ticks = F),div(class="pb-1 fw-bold text-decoration-underline",textOutput("prevQ2Compt"))),
          div(class="d-flex align-items-end pe-3", sliderInput("prevQ3", "IMD3",0.5,min=0,max=1,step=0.01,ticks = F),div(class="pb-1 fw-bold text-decoration-underline",textOutput("prevQ3Compt"))),
          div(class="d-flex align-items-end pe-3", sliderInput("prevQ4", "IMD4",0.5,min=0,max=1,step=0.01,ticks = F),div(class="pb-1 fw-bold text-decoration-underline",textOutput("prevQ4Compt"))),
          div(class="d-flex align-items-end pe-3", sliderInput("prevQ5", "IMD5  (Least deprived)",0.5,min=0,max=1,step=0.01,ticks = F),div(class="pb-1 fw-bold text-decoration-underline",textOutput("prevQ5Compt"))),
          shinyWidgets::prettySwitch("choiceRecPop",label = span(class ="small", "Set custom population shares"),value = F,status = "warning"),
          
        )
        )
      ),
      
      
      
      # *tab 4: Distributional inputs tab ----
      menuItem(
        tabName = "uptake",
        "Uptake", 
        div(
          id="util-sliders-group",
          class = "no-lab-sliders-group",
          div(
            class="d-flex align-items-end pe-1", 
            sliderInput("util1Q1", "IMD 1 (Most deprived)",100,min=0,step=0.1,max=100,ticks = F),
            div(class="no-padding w-50",autonumericInput("util1Q1_num",NULL,100,minimumValue = 0, maximumValue = 100,currencySymbol = "%",decimalPlaces = 1, currencySymbolPlacement = "s", width = "100%")),
          ),
          div(
            class="d-flex align-items-end pe-1", 
            sliderInput("util1Q2", "IMD 2",100,min=0,step=0.1,max=100,ticks = F),
            div(class="no-padding w-50",autonumericInput("util1Q2_num",NULL,100,minimumValue = 0, maximumValue = 100,currencySymbol = "%",decimalPlaces = 1, currencySymbolPlacement = "s", width = "100%")),
          ),
          div(
            class="d-flex align-items-end pe-1", 
            sliderInput("util1Q3", "IMD 3",100,min=0,step=0.1,max=100,ticks = F),
            div(class="no-padding w-50",autonumericInput("util1Q3_num",NULL,100,minimumValue = 0, maximumValue = 100,currencySymbol = "%",decimalPlaces = 1, currencySymbolPlacement = "s", width = "100%")),
          ),
          div(
            class="d-flex align-items-end pe-1", 
            sliderInput("util1Q4", "IMD 4",100,min=0,step=0.1,max=100,ticks = F),
            div(class="no-padding w-50",autonumericInput("util1Q4_num",NULL,100,minimumValue = 0, maximumValue = 100,currencySymbol = "%",decimalPlaces = 1, currencySymbolPlacement = "s", width = "100%")),
          ),
          div(
            class="d-flex align-items-end pe-1", 
            sliderInput("util1Q5", "IMD 5 (Least deprived)",100,min=0,step=0.1,max=100,ticks = F),
            div(class="no-padding w-50",autonumericInput("util1Q5_num",NULL,100,minimumValue = 0, maximumValue = 100,currencySymbol = "%",decimalPlaces = 1, currencySymbolPlacement = "s", width = "100%")),
          ),
          div(
            class = "px-3 mt-3", 
            div(
              class = "mb-1",
              "Total uptake:", ),
            div(
              class = "text-dark w-100 card px-2 py-2 text-center fw-bold",
              style = "cursor: not-allowed; background-color: #aaaaaa",
              textOutput("rec_pop_text1",inline = T))
          ),
          div(class="text-center pe-2 small text-secondary",
              textOutput("rec_pop_text2",inline = T)
          )
        )
      ),
      
      
      
      
      
      
      # *tab 5: health effect tab -----
      menuItem(
        tabName = "effectiveness",
        "Effectiveness", 
        div(
          class = "no-lab-sliders-group",
          div(
            class="d-flex align-items-end pe-1",
            sliderInput("qaly1Q1", "IMD 1 (Most deprived)",1,min=0,step=0.25,max=5,ticks = F),
            div(class="no-padding w-50",autonumericInput("qaly1Q1_num",NULL,1,minimumValue = 0, maximumValue = 5,currencySymbol = "x",decimalPlaces = 1, currencySymbolPlacement = "s", width = "100%")),
          ),
          div(
            class="d-flex align-items-end pe-1",
            sliderInput("qaly1Q2", "IMD 2",1,min=0,step=0.1,max=5,ticks = F),
            div(class="no-padding w-50",autonumericInput("qaly1Q2_num",NULL,1,minimumValue = 0, maximumValue = 5,currencySymbol = "x",decimalPlaces = 1, currencySymbolPlacement = "s", width = "100%")),
          ),
          div(
            class="d-flex align-items-end pe-1",
            sliderInput("qaly1Q3", "IMD 3",1,min=0,step=0.1,max=5,ticks = F),
            div(class="no-padding w-50",autonumericInput("qaly1Q3_num",NULL,1,minimumValue = 0, maximumValue = 5,currencySymbol = "x",decimalPlaces = 1, currencySymbolPlacement = "s", width = "100%")),
          ),
          div(
            class="d-flex align-items-end pe-1",
            sliderInput("qaly1Q4", "IMD 4",1,min=0,step=0.1,max=5,ticks = F),
            div(class="no-padding w-50",autonumericInput("qaly1Q4_num",NULL,1,minimumValue = 0, maximumValue = 5,currencySymbol = "x",decimalPlaces = 1, currencySymbolPlacement = "s", width = "100%")),
          ),
          div(
            class="d-flex align-items-end pe-1",
            sliderInput("qaly1Q5", "IMD 5 (Least deprived)",1,min=0,step=0.1,max=5,ticks = F),
            div(class="no-padding w-50",autonumericInput("qaly1Q5_num",NULL,1,minimumValue = 0, maximumValue = 5,currencySymbol = "x",decimalPlaces = 1, currencySymbolPlacement = "s", width = "100%")),
          ),

        ),
        div(
          class = "px-3 mt-3", 
          div(
            class = "mb-1",
            "Average QALY gain/recipient:", ),
          div(
            class = "text-dark w-100 card px-2 py-2 text-center fw-bold",
            style = "cursor: not-allowed; background-color: #aaaaaa",
            textOutput("wt_qaly_text1",inline = T))
        ),
        div(class="text-start pe-2 small text-secondary text-wrap lh-1 pt-2 ps-3 pe-4",
            textOutput("wt_qaly_text2",inline = T)
        )
      ),
      
      
      # *tab 6: opportunity costs tab -----
      menuItem(
        tabName = "opportunity",
        "Opportunity costs", 
        autonumericInput(
          inputId = "ratioHOC", 
          label = tip("Marginal productivity (&#163;/QALY)",tip = "Marginal cost of producing a QALY from alternative resource use"),
          value = 15000, 
          min=0,  
          currencySymbol = "£",
          decimalPlaces = 0
          ),
        selectInput(
          "choiceHOC", "Health opportunity gradient:",
          c("Flat" = "flat","Moderate gradient" = "moderate","Steep gradient" = "steep")
        ),
        div(class="pe-3", highchartOutput("disthoc_plot",height="200px",width="100%")),
        div(
          class= "pe-3 ps-2 pt-1 text-wrap text-secondary lh-1",
          style = "font-size: 80%",
          textOutput("hoc_user_warning",inline = T),)
      ),
      hr(),
      # *run btn ------
      actionButton("run","Run Scenario",class="btn btn-custom px-5 mx-auto mt-3 fs-5 "),
      
      # *download btn ------
      div(
        class = "px-5",
        # proxy triggers download (avoids some bugs)
        actionButton("report_download_proxy","Download Report",icon = shiny::icon("download"), class = "btn-custom mt-2 fs-6 py-1"),
      # hidden download button
        downloadButton("report_download","", icon = NULL,  style = "width: 0; height:1; position: absolute !important; z-index: -1 !important; top:-10px;"),
      )
    )
    
  ),
  
  
  
  # BODY  -----
  dashboardBody(
    
    # *misc ui and styling ----
    # use bootstrap 5 sintead of shiny default 4
    suppressDependencies("bootstrap"),
    tags$script(
      src="https://cdn.jsdelivr.net/npm/bootstrap@5.0.2/dist/js/bootstrap.bundle.min.js",
      integrity="sha384-MrcW6ZMFYlzcLA8Nl+NtUVF0sA7MsXsP1UyJoMp4YLEuNSfAP+JcXn/tWtIaxVXM",
      crossorigin="anonymous"
    ),
    tags$link(
      href="https://cdn.jsdelivr.net/npm/bootstrap@5.0.2/dist/css/bootstrap.min.css",
      rel="stylesheet",
      integrity="sha384-EVSTQN3/azprG1Anm3QDgpJLIm9Nao0Yz1ztcQTwFspd3yD65VohhpuuCOmLASjC",
      crossorigin="anonymous"
    ),
    tags$link(
      href="https://cdn.datatables.net/1.12.1/css/jquery.dataTables.min.css",
      rel="stylesheet",
      crossorigin="anonymous"
    ),
    
    # add york favicon
    tags$head(tags$link(rel="shortcut icon", href="york_mini.png")),
    
    # add custom styling css
    includeCSS("style.css"),
    
    # use shinyjs
    useShinyjs(),
    
    # loading spinner while report is being built
    use_busy_spinner(
      spin_id = "report_spinner",
      spin =  "self-building-square",
      color = "#cb3e72",
      position = "full-page",
      width = "250px"
    ),
    
    # loading screen 
    if(!HIDE_LOADING_SCREEN){
      loadingScreenUi()
    },
    
    
    # *Main ----
    fluidRow(
      
      
      column(
        width = 12,
        
        class = "d-flex flex-wrap flex-sm-wrap flex-md-wrap flex-lg-wrap flex-xl-nowrap justify-content-center align-items-start mx-auto px-3",
        style = "max-width: 1500px;",
        
        # not ran yet? -> hide cards, show prompts
        conditionalPanel(
          "input.run===0",
          id="click-prompt",
          class = "d-flex",
          style = "position: absolute; height: 75vh; width: calc(100vw - 300px);",
          div(
            class = "d-flex flex-column justify-content-center mx-auto my-auto",
            div(
              class="w-100 text-secondary fw-bold fs-4 px-5 mx-auto text-start my-auto pb-5",
              tags$p("1 Click 'Run Scenario' to assess the health equity impact."),
              tags$p("2 Update input parameters."),
              tags$p("3 Click on 'Run Scenario' again to see updated results.")
            )
          ),
                         ),
        
        # *card 1 -----
        
        div(
          id="card-1",
          class = "card shadow-lg mx-1 mx-sm-1 mx-md-1 mx-lg-3 my-2",
          style ="min-width: 400px; max-width: 750px; width: 100%;",
        
          div(
            class = "card-header text-white card-title fs-5",
            style = "background-color: var(--primary)",
            "Distributional health impact",
          ),
          
          
        div(
          class = "d-flex flex-column card-body",
          
          div(
            class = "d-flex flex-column",
            
            
            div(
              class ="d-flex mt-1 mb-1 px-2 pb-1  fs-5",
              div(
                class="fw-bold me-3 ms-1", 
                tip(
                  str = "Net health",
                  tip = "The total gain in healthy years across the whole English population."
                  ), 
                "benefit:"),
              div(
                class = "fw-bold text-center custom-col",
                textOutput("nhb",inline = T), "QALYs")
            ),
            
            
            div(
              class ="d-flex mt-1 mb-4 border-bottom px-2 pb-3  fs-5",
              div(
                class="fw-bold me-3 ms-1", 
                tip(
                  str = "Net health inequality",
                  tip = "The impact on the inequality gap in healthy years lived between the most and least 
                  deprived fifths of the English population. More specifically, the value represents the 
                  modelled difference in net QALY benefit between the most and least deprived IMD group 
                  at population level. The measure differs from the observed gap by 
                  incorporating information on the net QALY benefits of IMD2-IMD4 using 
                  a simple linear regression model. The current English health inequality gap is about 
                  130 million healthy years (based on an average gap in health-adjusted life expectancy of 
                  about 13 times a population of about 10 million)."), 
                "benefit:"),
              div(
                class = "fw-bold text-center custom-col",
                textOutput("sii",inline = T), "QALYs")
            ),
            
            div(
              class="d-flex",
              div(class="ms-auto pe-2 fw-bold flex-shrink-0 align-bottom","Select plot:"),
              selectInput(
                "netbenefit_plot_type",
                label = NULL,
                width = "100%",
                choices = c(
                  "Eligible population",
                  "Uptake rate",
                  "Incremental QALYs/recipient",
                  "Share of opportunity costs",
                  "Proportion of recipients",
                  "Number of recipients",
                  "Intervention benefit",
                  "Intervention opportunity costs",                     
                  "Net health benefit"
                ),
                selected = "Net health benefit"
              )
            ),
            shinycssloaders::withSpinner(
              type = 2,
              color.background = "white",
              color = "#cb3e72",
              highchartOutput("netbenefit_plot"),
            ),
          ),
          
          
          div(
              style="overflow-x: scroll;",
            shinycssloaders::withSpinner(
              type = 2,
              color.background = "white",
              color = "#cb3e72",
              dataTableOutput("netbenefit_table",)
            )
          ),
          
          
          
        ),
          
        
        ),
        
        
        
        
        # *card 2 -----
        
        div(
          id="card-2",
          class = "card shadow-lg mx-1 mx-sm-1 mx-md-1 mx-lg-3 my-2",
          style ="min-width: 400px; max-width: 750px; width: 100%;",
          
          div(
            class = "card-header text-white card-title fs-5",
            style = "background-color: var(--primary)",
            "Equity & efficiency",
          ),
          
          div(
            class = "d-flex flex-column card-body",
            style = "min-width: 350px; flex-start: 48%; max-width: 750px;",
            
            
            
            div(class="w-100 border-bottom mb-3 pb-3 d-flex",
            div(
              class="px-1 mx-auto w-100", style = "max-width: 600px;",
            tags$table(
              class = "equity-table ",
              
              tags$colgroup(
                tags$col(span=1, width = "70%"),
                tags$col(span=1, width = "30%"),
              ),
              
              
              tags$tr(
                tags$td(tip("Decision threshold","Threshold cost per QALY gained for considering the intervention cost-effective")),
                tags$td(
                  div(
                    class="d-flex px-3 mx-auto align-items-center justify-content-center",
                    actionButton("eip_threshold_minus","-",class="btn-custom-outline"),
                    autonumericInput(
                      "eip_threshold",label = NULL, 
                      value = 30000, min = 0, max = 500000,
                      step = 1000, width = "80px",
                      currencySymbol = "£", decimalPlaces = 0
                    ),
                    actionButton("eip_threshold_plus","+",class="btn-custom-outline"),
                  )
                )
              ),
              
              tags$tr(
                tags$td(tip("Atkinson inequality aversion", tipId = "implicit_weight")),
                tags$td(
                  div(
                    class="d-flex px-3 mx-auto align-items-center justify-content-center",
                    actionButton("eip_aversion_minus","-",class="btn-custom-outline"),
                    autonumericInput(
                      "eip_aversion",label = NULL, 
                      value = 10, min = 0, max = 20,
                      step = 0.5, width = "80px",
                      readOnly = T
                    ),
                    actionButton("eip_aversion_plus","+",class="btn-custom-outline"),
                  ),
                )),
              
              tags$tr(
                style = "height: 20px;",
                tags$td(class="fw-bold", "Naive estimates:")
                ),
              
              tags$tr(
                tags$td(class="ps-4",tip("ICER", "Incremental cost-effectiveness ratio")),
                tags$td(class = "text-center", textOutput("icer_text2",inline = T))
              ),
              tags$tr(
                tags$td(class="ps-4",tip("INMB:", "Incremental net monetary benefit per recipient")),
                tags$td(class = "text-center", textOutput("inmb_text",inline = T))
              ),
              
              tags$tr(
                style = "height: 20px;",
                tags$td(class="fw-bold", "Equity-weighted estimates:")
              ),
              
            
              tags$tr(
                tags$td(class="ps-4",tip("Weighted ICER", "Equity-weighted incremental cost-effectiveness ratio")),
                tags$td(class = "text-center", textOutput("weighted_icer",inline = T))
              ),
              tags$tr(
                tags$td(class="ps-4",tip("Weighted INMB:", "Equity-weighted incremental net monetary benefit per recipient")),
                tags$td(class = "text-center", textOutput("weighted_inmb", inline = T))
              ),
              
              
              
            ),
            ),
            ),
            
            
            
            
            div(
              class="d-flex",
              div(class="ms-auto pe-2 fw-bold flex-shrink-0 align-bottom","Select plot:"),
              selectInput(
                "icer_plane_type",
                label = NULL,width = "100%",
                choices = c(
                  "Equity - ICER trade-off " = "icer_equityimpact_plot",
                  "Equity - net health impact trade-off" = "equityimpact_plot",
                  "CE-plane " = "ce_plane"
                ),
                selected = "icer_equityimpact_plot"
              ),
            ),
            
            
            shinycssloaders::withSpinner(
              type = 2,
              color.background = "white",
              color = "#cb3e72",
              highchartOutput("plane_plot",height = "500px"),
            ),
            textOutput("raw_icer_text"),
            
            
          ), 
        ),
            
        
        
      ),
      
      
      
      
      
    ),
    
  ),
  title = "DCEA app"
)



# _ -----
server = function(input, output, session){ 
  
  # JS UI SETUP ----- 
  # *circle progress bar ----
  update_progress(shiny_id = "progress-circle",value = 1)
  runjs("setTimeout(()=>{
        let el = document.querySelector('#landing-div');
        if(el === null) return;
        el.style.opacity = 0;
        setTimeout(() => {
            el.parentNode.removeChild(el);
        }, 1000);
  },3200)")
  # *hide cards at start if no model is ran -----
  runjs("
  document.querySelector('#card-1').style.visibility = 'hidden';
  document.querySelector('#card-2').style.visibility = 'hidden';
  ")
  # *activate tooltips -----
  runjs("
  var tooltipTriggerList = [].slice.call(document.querySelectorAll('[data-bs-toggle=\"tooltip\"]'))
  var tooltipList = tooltipTriggerList.map(function (tooltipTriggerEl) {
    return new bootstrap.Tooltip(tooltipTriggerEl)
  })")
  
  # *download button for highcharter ----
  runjs("
  Highcharts.SVGRenderer.prototype.symbols.download = (x, y, w, h) => {
    return ['M', x + w * 0.5, y,'L', x + w * 0.5, y + h * 0.7,'M', x + w * 0.3, y + h * 0.5,'L', x + w * 0.5, y + h * 0.7,'L', x + w * 0.7, y + h * 0.5,'M', x, y + h * 0.9,'L', x, y + h,'L', x + w, y + h,'L', x + w, y + h * 0.9];
    };
  ")
  # *avoid line break in sidebar slider input label ----
  # for some reason css setting has no effect on label? 
  runjs("document.querySelector('#util1Q5-label').style.whiteSpace = 'nowrap'")
  runjs("document.querySelector('#qaly1Q5-label').style.whiteSpace = 'nowrap'")
  
  # *server side rendering of selectize ----
  updateSelectizeInput(
    session, 
    'intICD', 
    choices = icd10_input_labs, 
    server = TRUE,
  )

  
 
  
  
 
  
  observe({
    
    popR = sum(recipients_table_raw1()$recipients_util)
    popRfrmt = formatC(popR, digits = 0, format = "f", big.mark = ",")
    popE = sum(resCEA()$pop)
    popEfrmt = formatC(popE, digits = 0, format = "f", big.mark = ",")
    
    output$rec_pop_text1 <- renderText({ 
      paste0(round((popR/popE)*100,0) ,"%")
    })
    output$rec_pop_text2 <- renderText({ 
      paste0("(",popRfrmt," / ",popEfrmt,")")
    })
    
  })
  
  
  # Input summary table ------------------------------------------------------
  
  distIncQALY <- reactive({
    qaly_dist(resCEA(),distQALYeffect())
  })
  
  
  # Non-output version for Markdown report
  input_summary_table <- reactive({
    table_inputSummary(distPrev(),distUtil1(),distIncQALY(),distHOC())
  })
  
  
  
  # Recipient distribution tables and plots ---------------------------------
  
  
  # Raw tables for each uptake scenario
  recipients_table_raw1 = reactive({
    table_recipients_raw(distPrev(),distUtil1(),resCEA())
  })
  recipients_table_raw2 = reactive({
    table_recipients_raw(distPrev(),distUtil2(),resCEA())
  })
  
  
  
  
  # _-----
  #  RUN { -----
  
  netbenefit_table_raw1 = eventReactive(input$run, ignoreNULL = F, {
    isolate(table_netbenefit_raw(recipients_table_raw1(),distHOC(),input$ratioHOC,
                                 distQALYeffect(),resCEA()))
  })
  netbenefit_table_raw2 = eventReactive(input$run, ignoreNULL = F, {
    isolate(table_netbenefit_raw(recipients_table_raw2(),distHOC(),input$ratioHOC,
                                 distQALYeffect(),resCEA()))
  })
  weightedicer_raw = eventReactive(input$run, ignoreNULL = F, {
    table_weightedicer_raw(netbenefit_table_raw1(),netbenefit_table_raw2(),
                           input$ratioHOC,imp_AtWeights_full)
  })
  healthdistribution_table_raw1 = eventReactive(input$run, ignoreNULL = F, {
    table_healthdistribution_raw(netbenefit_table_raw1())
  })
  healthdistribution_table_raw2 = eventReactive(input$run, ignoreNULL = F, {
    table_healthdistribution_raw(netbenefit_table_raw2())
  })
  inequality_table_raw1 = eventReactive(input$run, ignoreNULL = F, {
    table_inequality_raw(healthdistribution_table_raw1(),
                         resCEA(),input$ratioHOC)
  })
  inequality_table_raw2 = eventReactive(input$run, ignoreNULL = F, {
    table_inequality_raw(healthdistribution_table_raw2(),
                         resCEA(),input$ratioHOC)
  })
  
  inequality_table1 = eventReactive(input$run, ignoreNULL = F, {
    table_inequality(inequality_table_raw1())
  })
  inequality_table2 = eventReactive(input$run, ignoreNULL = F, {
    table_inequality(inequality_table_raw2())
  })
  
  intName = eventReactive(input$run, ignoreNULL = F, {
    if(input$intName1 == ""){"Smoking Prevention"} else {input$intName1}
  })
  compName = eventReactive(input$run, ignoreNULL = F, {
    if(input$compName1 == ""){"No Intervention"} else {input$compName1} 
  })
  
  
  observeEvent(input$run, ignoreNULL = F, priority = 4,{
    
    # checks fun if running os allowed
    
    if(input$run>0){
      
    runjs("
        document.querySelector('#card-1').style.visibility = 'visible';
        document.querySelector('#card-2').style.visibility = 'visible';
        document.querySelector('#click-prompt').remove();
        ")
    }
    
    
    R$scenario = "" # isolate(input$scenario_counter)
    output$intName_txt = renderText(intName())
    
    
    output$noEIP_text <- renderText({ 
      if(indicatorICER()==1) { paste0("",sep="") }
      else { paste0("This plot is only produced when the incremental QALYs 
        and costs for an intervention are positive.")  }
    })
    
    
    # Raw ICER warning
    output$raw_icer_text <- renderText({ 
      if(baseICER()=="dominant") {
        paste0("Warning: Interpret the ICER with caution as the intervention dominates the comparator")
      } else if(baseICER()=="dominated") {
        paste0("Warning: Interpret the ICER with caution as the intervention is dominated by the comparator")
      } else {
        paste0("")
      }
    })
    
    
    output$nhb = renderText({
      nhb = isolate(table_netbenefit(netbenefit_table_raw1()))
      formatC(as.numeric(nhb[3,"Total"]),digits = 0,format = "f",big.mark = "," )
    })
    
    output$sii = renderText({
      formatC(as.numeric(isolate(inequality_table_nb_raw1()$Value[1])),digits = 0,format = "f",big.mark = "," )
    })
    
    # ICER for results 
    R$baseICER = isolate(baseICER())
    output$icer_text2 <- renderText({
      if(R$baseICER=="dominant") {
        paste0("Dominating")
      } else if(R$baseICER=="dominated") {
        paste0("Dominated")
      } else {
        paste0("£",R$baseICER,"/QALY")
      }
    })
    # for inmb
    R$incC =  input$incCost_c1
    R$incQ =  input$incQALYs_c1
    
    # prepare distributional health impact table
    table_inputs = isolate(table_inputSummary(distPrev(),distUtil1(),distIncQALY(),distHOC()))
    names(table_inputs)[1] = "x"
    table = cbind(table_inputs,"Total" = c(1,NA,NA,1))
    table_ext = isolate(table_recipients(recipients_table_raw1()))
    names(table_ext)[1] = "x"
    table = rbind(table, table_ext)      
    table_outcomes = isolate(table_netbenefit(netbenefit_table_raw1()))
    names(table_outcomes)[1] = "x"
    table = rbind(table, table_outcomes)
    names(table)[1] = ""
    R$dhi_table = table
    
    # Net benefit tab --
    output$netbenefit_table =renderDataTable(server = F, {
      
      if(input$run == 0){return(NULL)}
        
        # messy reformatting
      table = reformatNBT(table)
        
        R$final_dtbl = table
        # styling
        borderStyle <- "value == 'T' ? 'double black' : value != 'white' ? '' : 'white'"  
        class(borderStyle) <- "JS_EVAL"
        bgStyle = "value == 'T' ? 'bold' : value != '' ? '' : ''"  
        class(bgStyle) <- "JS_EVAL"
        
        datatable(
          table,
          extensions = 'Buttons', 
          style = 'bootstrap',
          class = "hover compact",
          rownames = FALSE,
          options = list(
            dom = 'frtipB',
            buttons = c('copy', 'csv', 'excel', 'pdf'),
            paging=F,searching=F,info=F,ordering=F,
            columnDefs = list(list(visible=FALSE, targets=c(7,8)))
          )
        ) %>%
          formatStyle("s1", target = 'row', border = borderStyle) %>%
          formatStyle("s2", target = 'row', fontWeight = bgStyle)
        
    })
    
    
    inequality_table = table_inequality_comb(
      isolate(inequality_table1()),
      isolate(inequality_table2()),
      F # input$choiceUptake2
    )
    
    output$siai_reduction = renderText(formatC(inequality_table$Value[1],digits=6,format = "f"))
    output$rii_reduction = renderText(formatC(inequality_table$Value[2],digits=6,format = "f"))
    output$ci_reduction = renderText(formatC(inequality_table$Value[3],digits=6,format = "f"))
    
    
    Sys.sleep(0.5)
    
    
    if(R$trigger_download){
      R$trigger_download = F
      runjs("document.getElementById('report_download').click();")
    }
    
  }) # } -----
  
  


  # _-----
  # REACTIVE VALS STORE ----
  
  
  R <- reactiveValues(
    old_atkinson=NULL, 
    old_atkinson_icer=NULL,
    run = 0, 
    scenario=NULL,
    trigger_download = F
    )
  
  
  # Recipient pop statements
  popRec1 <- reactive({
    format(sum(recipients_table_raw1()$recipients_util), big.mark=",")
  })
  
  
  # Risk or intervenion population?
  observeEvent(list(input$intRF,input$intICD, input$age_range, input$intervention_type), ignoreNULL = F, {
    if(input$intervention_type == "Risk factor population"){
      newVal = popRF()
    } else {
      newVal = popICD()
    }
    updateNumericInput(session,"intPop_c1",value=newVal)
  })
  
  # Eligible population statement (Dist inputs tab)
  popElig <- reactive({
    format(sum(resCEA()$pop), big.mark=",")
  })
  observe({
    if(input$enable_intPop_c1){
      txt_ = paste0("Set a custom target population size")
    } else {
      txt_ = paste0("The socioeconomic distribution of the eligible population (n=",popElig(),") has been automatically generated from disease / risk factor prevalence data. A custom distribution can be defined below (see \"Distributional inputs\").")
    }
    updateTip(id = "elig_pop_text",tip = txt_)
  })
  
  # ICER statement 
  baseICER <- reactive({
    icer_calc(resCEA())
  })
  
  # Raw ICER
  baseICER_raw <- reactive({
    icer_calc_raw(resCEA())
  })
  
  # Weighted incremental QALYs statement
  wtQALY <- reactive({
    wt_qaly_calc(resCEA(),distQALYeffect(),recipients_table_raw1())
  })
  
  
  # User defined prevalence distribution
  distPrev_user <- reactive({
    sumPrev = sum(c(input$prevQ1,input$prevQ2,input$prevQ3,input$prevQ4,input$prevQ5))
    table = data.frame(
      imd=1:5,
      prop_prev=c(input$prevQ1,input$prevQ2,input$prevQ3,input$prevQ4,input$prevQ5)/sumPrev)
  })
  
  # Auto-generated prevalence distribution (from ICD / RF selection)
  distPrev_auto <- reactive({ 
    table_prevalence_raw(input$intervention_type,input$intICD,input$intRF,input$age_range,resCEA())
  })
  
  # Live prevalence distribution (selected based on UI input)
  distPrev <- reactive({
    if(
      (
        (input$intervention_type == "Disease population" & is.null(input$intICD)) | 
        (input$intervention_type == "Risk factor population" & is.null(input$intRF))
      ) | input$choiceRecPop==1 ) {
      distPrev_user()
    } else {
      distPrev_auto()    
    }
  })
  
  # Utilisation full uptake
  distUtilFull <- reactive({
    table = data.frame(imd=1:5,util_rate=c(1,1,1,1,1))
  })
  
  distUtil1 <- reactive({
    table = data.frame(imd=1:5,util_rate=c(input$util1Q1,input$util1Q2,input$util1Q3,input$util1Q4,input$util1Q5)/100)
  })
  
  distUtil2 <- reactive({
    table = data.frame(
      imd=1:5,
      util_rate= rep(0,5) 
    )
  })
  
  # QALY effect multipliers
  distQALYeffect <- reactive({
    table = data.frame(
      imd=1:5,
      qaly_mod=c(input$qaly1Q1,input$qaly1Q2,input$qaly1Q3,input$qaly1Q4,input$qaly1Q5))
  })
  
  
  # Create a reactive risk factor / disease population estimate for the comparator input fields
  popRF <- reactive({
    risk_factor_pop(input$intRF)
  })
  popICD <- reactive({
    icd_pop(input$intICD,input$age_range)
  })
  
   
  # *Population outcomes ----
  resCEA <- reactive({
    data.frame(
      comparator=1,
      qalys=ifelse(is.null(input$incQALYs_c1),NA,input$incQALYs_c1),
      cost=ifelse(is.null(input$incCost_c1),NA,input$incCost_c1),
      pop=as.numeric(input$intPop_c1))
  })
  
  # *Live HOC distribution -----
  distHOC <- reactive({
    distHOC_scenarios %>% filter(scenario==input$choiceHOC) %>% select(-scenario)
  })
  
  # *Net benefit inequality table ----
  inequality_table_nb_raw1 = reactive({
    table_nb_inequality(netbenefit_table_raw1(),recipients_table_raw1())
  })
  # *Atkinson ----
  atkinson_table_raw = reactive({
    table_atkinson_raw(healthdistribution_table_raw1(),
                       healthdistribution_table_raw2())
  })
  
  # _-----
  # PLOTS -----
  
  
  # *opportunity cost distribution in Sidebar ----
  output$disthoc_plot <- renderHighchart({
    plot_hoc_dist(distHOC())
  })
  
  
  # *Net benefits plot --------
  output$netbenefit_plot = renderHighchart({
    
    
    if(input$run == 0){
      p_error = highchart() %>%
        hc_title(
          text = "Click 'Run Scenario'",
          align = "center",
          x=-10,
          verticalAlign = 'middle',
          floating = "true",
          style = list(
            fontSize = "16px",
            color = "#7cb5ec"
          )
        )
      return(p_error)
    }
    
    plot_df <- isolate(netbenefit_table_raw1())
    plot_df$imd_str = c("IMD 1<br>(Most deprived)","IMD 2","IMD 3", "IMD 4", "IMD 5<br>(Least deprived)")
    plot_df$cols = imdCols()
    plot_df$cols = substr(plot_df$cols,1,7)
    
    zeroLine = data.frame(var = 0, imd_str = plot_df$imd_str)
    
    table_vars = c(
      "Proportion" = "Eligible population",
      "Proportion" =  "Uptake rate",
      "Incremental QALYs/recipient" = "Incremental QALYs/recipient",
      "Proportion" =  "Share of opportunity costs",
      "Proportion" =  "Proportion of recipients",
      "Number of recipients" = "Number of recipients",
      "Quality-adjusted Life Years" = "Intervention benefit",
      "Quality-adjusted Life Years" = "Intervention opportunity costs",                     
      "Quality-adjusted Life Years" = "Net health benefit"
    )
    
    selected_var = table_vars == input$netbenefit_plot_type
    plot_df$var = t(R$dhi_table[selected_var, 2:6 ])
    lab = names(table_vars)[selected_var]
    
    highchart() %>%
      
      hc_add_series(
        data = plot_df, "column",
        pointPadding = 0, groupPadding= 0.1, borderRadius= 5,
        hcaes(
          name = imd_str,
          x = imd_str,
          y = var,
          color = cols
        ),
        showInLegend = F,
        name = "QALYs"
      ) %>%
      hc_plotOptions(
        bar = list(
          pointWidth=0,
          dataLabels = list(enabled = TRUE)
        )) %>%
      # hc_title(text = lab, align = "center", x=40,y=10,  verticalAlign = 'top', floating = "true", style = list(fontSize = "16px")) %>%
      hc_chart(
        style = list(
          fontFamily = "Inter"
        )
      ) %>%
      hc_tooltip(
        valueDecimals = 2
      ) %>%
      hc_xAxis(
        categories = plot_df$imd_str
      ) %>%
      hc_yAxis(
        title  = list(
          text = lab,
          style = "font-weight: 600; font-size: 16px"
        ),
        plotLines = list(
          list(
            value= 0,
            width= 2,
            color = "black",
            zIndex=5
          )
        )
      ) %>%
      hc_boost(enabled = FALSE) %>% 
      hc_exporting(
        enabled = TRUE,
        formAttributes = list(
          target = "_blank"
        ),
        chartOptions = list(
          chart = list(
            backgroundColor = "white"
          )
        ),
        buttons = list(
          contextButton = list(
            symbol = "download",
            verticalAlign = "top",
            horizontalAlign = "right",
            onclick = JS("function () {
                     this.exportChart();
                 }")
          )))
    
  })
  
  
  
  # *Equity impact plane -----------------------------------------------------
  
  output$plane_plot = renderHighchart({
    
    if(input$run == 0){
      p_error = highchart() %>%
        hc_title(
          text = "Click 'Run Scenario'",
          align = "center",
          x=-10,
          verticalAlign = 'middle',
          floating = "true",
          style = list(
            fontSize = "16px",
            color = "#7cb5ec"
          )
        )
      return(p_error)
    }
    
    
    if(input$icer_plane_type == "ce_plane"){
      
      cost = R$incC
      qalys = R$incQ
      thresh = input$eip_threshold
      
      table = table_atkinson(atkinson_table_raw(),imp_AtWeights,weightedicer_raw(),F)
      selected_eip = table[,1] ==   input$eip_aversion
      
      max_yval <- abs(cost)*1.5
      max_xval <- max(abs(qalys))*1.5
      
      eip = data.frame(qalys = c(qalys), cost = cost, name = paste0("ICER: £",formatC(cost/qalys, format = "f",digits = 0,big.mark = ","),"/QALY"), cols=c("#212529"))
      reg_line = data.frame(x=c(-1,1),y=c(-thresh,thresh))
      
      p1 = drawCePlane(eip,reg_line,max_yval,max_xval)
      return(p1)
    }
    
    
      if(input$icer_plane_type == "equityimpact_plot"){
        res = plot_equity_impact(
          isolate(inequality_table_raw1()),
          isolate(inequality_table_raw2()),
          isolate(atkinson_table_raw()),
          input$eip_aversion,
          F, # input$choiceUptake2,
          isolate(intName()),
          isolate(compName()),
          old_atkinsons = isolate(R$old_atkinson),
          scenario_name = isolate(R$scenario),
          internal_counter = isolate(as.numeric(input$run)),
          show_old = F # input$show_prev
        )
      }
      
      
      if(input$icer_plane_type == "icer_equityimpact_plot"){
        
          res = plot_icer_equity_impact(
            isolate(baseICER_raw()),
            isolate(atkinson_table_raw()),
            input$eip_aversion,
            F, 
            isolate(intName()),
            isolate(compName()),
            input$eip_threshold,
            old_atkinsons = isolate(R$old_atkinson_icer),
            scenario_name = isolate(R$scenario),
            internal_counter = isolate(as.numeric(input$run)),
            show_old = F # input$show_prev
          )
      }
      
      return(res$plot)
      
    
  })
  
  
  
  # _-----
  
  # SIMPLE VALUE UPDATING -----
  
  
  
  
  
  output$wt_qaly_text1 <- renderText({ 
    wtQALY()
  })
  output$wt_qaly_text2 <- renderText({ 
    paste0("This set of multipliers yield an average incremental QALY gain of ",wtQALY()," per person")
    
  })
  
  
  output$icer_text <- renderText({
    if(baseICER()=="dominant") {
      paste0("Dominating")
    } else if(baseICER()=="dominated") {
      paste0("Dominated")
    } else {
      paste0("£",baseICER(),"/QALY")
    }
  })
  
  
  # *User defined HOC dist total -----
  output$hoc_user_warning <- renderText({ 
    if(input$choiceHOC=="flat") {
      "Health opportunity costs are distributed equally across deprivation groups"
    } else {
      "Health opportunity costs fall disproportionately on the more deprived groups"
    }
  })
  
  # *eligiblity output text -----
  observeEvent(
    list(input$prevQ1,input$prevQ2,input$prevQ3,input$prevQ4,input$prevQ5),{
      sumPrev = sum(c(input$prevQ1,input$prevQ2,input$prevQ3,input$prevQ4,input$prevQ5))
      output$prevQ1Compt <- renderText( paste0(round((input$prevQ1/sumPrev)*100,0),"%") )
      output$prevQ2Compt <- renderText( paste0(round((input$prevQ2/sumPrev)*100,0),"%") )
      output$prevQ3Compt <- renderText( paste0(round((input$prevQ3/sumPrev)*100,0),"%") )
      output$prevQ4Compt <- renderText( paste0(round((input$prevQ4/sumPrev)*100,0),"%") )
      output$prevQ5Compt <- renderText( paste0(round((input$prevQ5/sumPrev)*100,0),"%") )
    })
  
  # *custom populaton shares in IMDs? ----
  observeEvent(input$choiceRecPop, {
    inputIds = c("prevQ1","prevQ2","prevQ3","prevQ4","prevQ5")
    if(input$choiceRecPop)
      lapply(inputIds, shinyjs::enable)
    else 
      lapply(inputIds, shinyjs::disable)
  })
  
  # *risk factor or disease interventio? -----
  observe({
    if(!input$choiceRecPop){
      inputIds = c("prevQ1","prevQ2","prevQ3","prevQ4","prevQ5")
      if(
        (input$intervention_type == "Disease population" & is.null(input$intICD)) | 
        (input$intervention_type == "Risk factor population" & is.null(input$intRF))
      ){
        newVals = rep(.2,5)
      } else {
        newVals = distPrev_auto()$prop_prev
      }
      lapply(seq_along(inputIds),\(i){
        updateSliderInput(session = session, inputId = inputIds[i],value = newVals[i])
      })
      
    }
  })
  
  
  # *set custom population size?  ----
  observeEvent(input$enable_intPop_c1, {
    if(input$enable_intPop_c1){
      shinyjs::enable("intPop_c1")
    } else {
      shinyjs::disable("intPop_c1")
    }
  },ignoreNULL = F)
  
  
  # *uptake 2-way reactive inputs: UPTAKE -----
  utils_input_ids = c("util1Q1","util1Q2","util1Q3","util1Q4","util1Q5")
  utils_num_input_ids = c("util1Q1_num","util1Q2_num","util1Q3_num","util1Q4_num","util1Q5_num")
  observeEvent(lapply(utils_input_ids, \(i) input[[i]]),{
    lapply(seq_along(utils_input_ids),\(i){
      str = utils_input_ids[i]
      val_slider = round(input[[utils_input_ids[i]]],1)
      if(is.null(input[[utils_num_input_ids[i]]])){
        val_num  = -1
      } else {
        val_num = round(input[[utils_num_input_ids[i]]],1)
      }
      if(val_num != val_slider)
        updateAutonumericInput(session,inputId = utils_num_input_ids[i], value = round(input[[utils_input_ids[i]]],1))
    })
  })
  observeEvent(lapply(utils_num_input_ids, \(i) input[[i]]),{
    lapply(seq_along(utils_input_ids),\(i){
      val_slider = round(input[[utils_input_ids[i]]],1)
      if(is.null(input[[utils_num_input_ids[i]]])){return(NULL)}
      val_num = round(input[[utils_num_input_ids[i]]],1)
      if(val_num != val_slider)
        updateSliderInput(session,inputId = utils_input_ids[i], value = round(input[[utils_num_input_ids[i]]],1))
    })
  })
  
  # *uptake 2-way reactive inputs: EFFECTIVENESS -----
  effect_input_ids = c("qaly1Q1","qaly1Q2","qaly1Q3","qaly1Q4","qaly1Q5")
  effect_num_input_ids = c("qaly1Q1_num","qaly1Q2_num","qaly1Q3_num","qaly1Q4_num","qaly1Q5_num")
  observeEvent(lapply(effect_input_ids, \(i) input[[i]]),{
    lapply(seq_along(effect_input_ids),\(i){
      str = effect_input_ids[i]
      val_slider = round(input[[effect_input_ids[i]]],1)
      if(is.null(input[[effect_num_input_ids[i]]])){
        val_num  = -1
      } else {
        val_num = round(input[[effect_num_input_ids[i]]],1)
      }
      if(val_num != val_slider)
        updateAutonumericInput(session,inputId = effect_num_input_ids[i], value = round(input[[effect_input_ids[i]]],1))
    })
  })
  observeEvent(lapply(effect_num_input_ids, \(i) input[[i]]),{
    lapply(seq_along(effect_input_ids),\(i){
      val_slider = round(input[[effect_input_ids[i]]],1)
      if(is.null(input[[effect_num_input_ids[i]]])){return(NULL)}
      val_num = round(input[[effect_num_input_ids[i]]],1)
      if(val_num != val_slider)
        updateSliderInput(session,inputId = effect_input_ids[i], value = round(input[[effect_num_input_ids[i]]],1))
    })
  })
  
  # * Implicit Atkinson weight warnings -----
  observe({
    if(input$eip_aversion==0) { 
      txt_ = paste("This parameter value of 0 places equal weight on health gains and losses for the worst-off (IMD1) compared with the best-off (IMD5)",sep="") 
    } else { 
      txt_ = paste0("A parameter of ",input$eip_aversion, " gives health gains for the worst-off (IMD1) a ",implicit_weight_raw(input$eip_aversion), "% additional weight compared with those for best-off (IMD5)")  
    }
    updateTip(id = "implicit_weight",tip = txt_)
  })
  
  output$implicit_weight_text <- renderText({ 
    if(input$eip_aversion==0) { 
      paste("This parameter value of 0 places equal weight on health gains and losses for the worst-off (IMD1) compared with the best-off (IMD5)",sep="") 
    } else { 
      paste0("A parameter of ",input$eip_aversion, " gives health gains for the worst-off (IMD1) a ",implicit_weight(), "% additional weight compared with those for best-off (IMD5)")  
    }
  })
  
  # * ICER display: value/dominated/dominant ----
  indicatorICER = reactive({
    indicator_icer(resCEA())
  })
  
  # * iNMB update  -----
  observeEvent(list(input$eip_threshold,input$run), {
    inmb = formatC(c(R$incQ * input$eip_threshold - R$incC),digits = 0, big.mark = ",",format = "f")
    output$inmb_text <- renderText(paste0("£",inmb))
  })
  
  # * update atkinson ------
  observeEvent(input$eip_aversion_plus,{
    val = input$eip_aversion
    if(val<20){val = val+0.5}
    updateAutonumericInput(session = session, inputId = "eip_aversion", value = val) 
  })
  observeEvent(input$eip_aversion_minus,{
    val = input$eip_aversion
    if(val>0){val = val-0.5}
    updateAutonumericInput(session = session, inputId = "eip_aversion", value = val) 
  })
  # * update threshold
  observeEvent(input$eip_threshold_plus,{
    val = input$eip_threshold
    updateAutonumericInput(session = session, inputId = "eip_threshold", value = val+5000) 
  })
  observeEvent(input$eip_threshold_minus,{
    val = input$eip_threshold
    updateAutonumericInput(session = session, inputId = "eip_threshold", value = val-5000) 
  })
  
  # * weighted icer update  -----
  observeEvent(list(input$eip_threshold,input$run, input$eip_aversion),ignoreNULL = T, {
    table = table_atkinson(atkinson_table_raw(),imp_AtWeights,weightedicer_raw(),F)
    selected_eip = table[,1] == input$eip_aversion
    weighted_icer = table[selected_eip,3]
    output$weighted_icer <- renderText({
      paste0("£",formatC(weighted_icer, digits = 0, format = "f", big.mark = ","),"/QALY")
    })
    output$weighted_inmb <- renderText({
      weighted_incQ = R$incC/weighted_icer
      weighted_inmb = weighted_incQ * input$eip_threshold - R$incC
      weighted_inmb = formatC(weighted_inmb,digits = 0, big.mark = ",",format = "f")
      paste0("£",weighted_inmb)
      })
  })
  
  
  # # total inqautlity info modal (planned)
  # observeEvent(input$total_inequality_modal, {
  #   showModal(modalDialog(
  #     title = "Total health inequality in England",
  #     "Tbc",
  #     easyClose = TRUE,
  #     footer = div(modalButton("Close"),class="border rounded-3")
  #   ))
  # })
  
  # _ -----
  # Header top right menu UI ----
  observeEvent(input$showAbout,{
    aboutUi()
  })
                            
  observeEvent(input$showReferences,{
    referencesUi()
  })
  
  observeEvent(input$tutorial_start,{
    tutorialUi()
  })
  
  
  # _ -----
  # Markdown report --------------------------
  
  # this proxy download button is needed to avoid inconsistencies
  # between the ui and the report (bc of updated input values)
  observeEvent(input$report_download_proxy,{
    R$trigger_download = T
    # show loading spinner
    show_spinner(spin_id = "report_spinner")
    runjs("document.getElementById('run').click();")
  })
  
  output$report_download <- downloadHandler(
    filename = "equity_report.docx",
    content = function(file) {
      
      tryCatch({
      
      input_names <-  c(
        "Scenario counter" = 'run',
        
        "Intervention name" = 'intName1' ,
        "Comparator name" = "compName1",
        
        "Incremental QALYs" = 'incQALYs_c1' , 
        "Incremental cost (£)" = "incCost_c1",
        
        "Intervention disease category (ICD-10)" = "intICD",
        "Intervention risk factor" = "intRF",
        "Intervention type" = 'intervention_type',
        "Age range" = 'age_range',
        "Eligible population" = "intPop_c1",
        
        
        "Uptake (%) in IMD1 (most deprived)" = 'util1Q1',
        "Uptake (%) in IMD2" = 'util1Q2',
        "Uptake (%) in IMD3" = 'util1Q3',
        "Uptake (%) in IMD4" = 'util1Q4',
        "Uptake (%) in IMD5 (least deprived)" = 'util1Q5',  
        
        "Effectiveness in IMD1 (most deprived)" = 'qaly1Q1',
        "Effectiveness in IMD2" = 'qaly1Q2',
        "Effectiveness in IMD3" = 'qaly1Q3',
        "Effectiveness in IMD4" = 'qaly1Q4',
        "Effectiveness in IMD5 (least deprived)" = 'qaly1Q5', 
        
        # 'choiceRecPop' , 
        
        "Share of eligible population IMD1 (most deprived)" = 'prevQ1',
        "Share of eligible population IMD2" = 'prevQ2',
        "Share of eligible population IMD3" = 'prevQ3',
        "Share of eligible population IMD4" = 'prevQ4',
        "Share of eligible population IMD5 (least deprived)" = 'prevQ5',
        
        "Health opportunity cost distribution" = 'choiceHOC',
        "Marginal productivity" = "ratioHOC",
        
        "Atkinson inequality aversion value" = 'eip_aversion',
        "Decision threshold (£)"  = 'eip_threshold'
      )
      
      
      
      
      
      input_values <- lapply(input_names,\(x) input[[x]])
      
      tempReport <- file.path(tempdir(), "template.Rmd")
      file.copy("template.Rmd", tempReport, overwrite = TRUE)
      
      tempTemplate <- file.path(tempdir(), "template.docx")
      file.copy("template.docx", tempTemplate, overwrite = TRUE)
      
      
      isolate({
        cost = R$incC
        qalys = R$incQ
        thresh = input$eip_threshold
        eip_aversion = input$eip_aversion
        table = table_atkinson(atkinson_table_raw(),imp_AtWeights,weightedicer_raw(),F)
        selected_eip = table[,1] == eip_aversion
        max_yval <- abs(cost)*1.5
        max_xval <- max(abs(qalys))*1.5
        eip = data.frame(qalys = c(qalys), cost = cost, name = c("ICER"), cols=c("#212529"))
        reg_line = data.frame(x=c(-1,1),y=c(-thresh,thresh))
        max_yval <- abs(cost)*1.5
        max_xval <- max(abs(qalys))*1.5
        eip = data.frame(qalys = c(qalys), cost = cost, name = paste0("ICER: £",formatC(cost/qalys, format = "f",digits = 0,big.mark = ","),"/QALY"), cols=c("#212529"))
        
        icer = formatC(cost/qalys,digits = 0, format = "f",big.mark = ",")
        inmb = formatC(c(qalys * thresh - cost),digits = 0, big.mark = ",",format = "f")
        weighted_icer_raw = table[selected_eip,3]
        weighted_icer = paste0("£",formatC(weighted_icer_raw, digits = 0, format = "f", big.mark = ","),"/QALY")
        weighted_incQ = cost/weighted_icer_raw
        weighted_inmb_raw = weighted_incQ * thresh - cost
        weighted_inmb = paste0("£",formatC(weighted_inmb_raw,digits = 0, big.mark = ",",format = "f"))
        
      })

      report_ce_plane = rep_draw_ce_plane(eip,reg_line,max_yval,max_xval)
      
      intName = isolate(intName())
      compName = isolate(compName())
      
      nhb = isolate(table_netbenefit(netbenefit_table_raw1()))
      nhb = formatC(as.numeric(nhb[3,"Total"]),digits = 0,format = "f",big.mark = "," )
      sii = formatC(as.numeric(isolate(inequality_table_nb_raw1()$Value[1])),digits = 0,format = "f",big.mark = "," )
      
      report_disthoc_plot = rep_plot_hoc_dist(distHOC()) 
      plot_df = isolate(netbenefit_table_raw1())
      dhi_table = isolate(R$dhi_table)
      report_distr_plots = rep_distr_plots(plot_df, dhi_table)
      
      
      report_equity_nhb_plot = rep_draw_equityimpact_plot(
        isolate(inequality_table_raw1()),
        isolate(inequality_table_raw2()),
        isolate(atkinson_table_raw()),
        input$eip_aversion,
        F, # input$choiceUptake2,
        isolate(intName()),
        isolate(compName()),
        old_atkinsons = isolate(R$old_atkinson),
        scenario_name = isolate(R$scenario),
        internal_counter = isolate(as.numeric(input$run)),
        show_old = F 
      )
      
      
      report_equity_icer_plot = rep_draw_icer_equity_plot(
        isolate(baseICER_raw()),
        isolate(atkinson_table_raw()),
        input$eip_aversion,
        F, # input$choiceUptake2,
        isolate(intName()),
        isolate(compName()),
        input$eip_threshold,
        old_atkinsons = isolate(R$old_atkinson_icer),
        scenario_name = isolate(R$scenario),
        internal_counter = isolate(as.numeric(input$run)),
        show_old = F #input$show_prev
      )
      
      final_dtbl <- R$final_dtbl
      
      report_params <- list(
        intName = intName,
        compName = compName,
        nhb = nhb,
        sii = sii,
        icer = icer,
        inmb = inmb,
        thresh = formatC(thresh, digits = 0, big.mark = ",", format = "f"),
        eip_aversion = eip_aversion,
        weighted_icer = weighted_icer,
        weighted_inmb = weighted_inmb,
        disthoc_plot = report_disthoc_plot, 
        report_distr_plots = report_distr_plots,
        final_dtbl = final_dtbl,
        report_ce_plane = report_ce_plane,
        report_equity_nhb_plot = report_equity_nhb_plot,
        report_equity_icer_plot = report_equity_icer_plot,
        input_names = input_names,
        input_values = input_values
        )
      
      # compile report
      report = rmarkdown::render(
        input = tempReport,
        output_file = file,
        params = report_params,
        envir = new.env(parent = globalenv())
      ) 
      
      # remove loading spinner
      hide_spinner(spin_id = "report_spinner")
      
      return(report)
      
      },
      error = function(e){
          hide_spinner(spin_id = "report_spinner")
          alert("Error: sorry, something went wrong. The report could not be created.")
          return(NULL)
      },
      warning = function(w){
        hide_spinner(spin_id = "report_spinner")
        alert("Error: sorry, something went wrong. The report could not be created.")
        return(NULL)
      }
    )
    }
  )
  
  
  
  
  
  # trigger run in debug mode
  if(HIDE_LOADING_SCREEN){
    runjs("document.querySelector('#run').click()")
  }
  
  
  
}


shinyApp(ui, server)