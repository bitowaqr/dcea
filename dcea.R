
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
source("functions_revised.R")
# source("inputs.R")


# DEV:
HIDE_LOADING_SCREEN = F

# rename highchart donwload btn
lang <- getOption("highcharter.lang")
lang$contextButtonTitle <- "Download"
options(highcharter.lang = lang)

imdCols = function(){
  viridis_pal(begin = 0.3, end = 0.8,option = "magma")(5)
}

tip = function(str = "Atkinson inequality aversion", tip = "", tipId = "x" ,sup = "<i class=\"fa fa-question-circle\"></i>", class = "tip-div" ){
  HTML(paste0("<span class=\"",class,"\" id='",tipId,"' data-bs-toggle=\"tooltip\" data-bs-placement=\"top\" title='",tip,"'>",str,"<sup class='sup-tip'>",sup,"</sup></span>"))
}
updateTip = function(id, tip){
  runjs(paste0("document.querySelector('#",id,"').setAttribute('data-bs-original-title', '",tip,"');")) 
}






ui = dashboardPage(
  
  dashboardHeader(
    title = div(
      tags$img(src="york_mini.png",width="35px"),
      "Health Equity Impact Calculator",
    ),
    titleWidth = "320px",
    tags$li(
      class = "dropdown pe-5 text-white fw-bold d-flex", 
      div(
        class = "me-3",
        icon("info-circle"),
        actionLink("showAbout","About",class="text-white")
      ),
      div(
        class = "me-3",
        icon("book"),
        actionLink("showReferences","References",class="text-white")
        ),
      tags$div(
        icon("question-circle"),
        actionLink("tutorial_start","Watch the tutorial",class="text-white")
      )
      )
    ),
  
  dashboardSidebar(
    width = "300px",
    collapsed = F,
    
    
    
    # SIDEBAR ******* ----
    sidebarMenu(
      id = "tabs",
      
      
      # Intervention tab -----
      menuItem(
        tabName = "intervention",
        "Intervention", # icon = icon("bar-chart-o",verify_fa=F), 
        startExpanded=T,
        introBox(
          data.step = 1, data.intro = "First step of the intro",data.position = "right",
          introBox(
            data.step = 2, data.intro = "Second step of the intro",data.position = "right",
          textInput("intName1", "Intervention",NULL,placeholder = "E.g. Smoking prevention"),
          textInput("compName1", "Comparator",NULL,placeholder = "E.g. No intervention")
          ),
          introBox(
            data.step = 3, data.intro = "Third step of the intro",data.position = "right",
          selectInput("intervention_type","Population type",c("Risk factor population","Disease population"))
          ),
          
          conditionalPanel(
            condition="input.intervention_type=='Disease population'",
            selectizeInput(
              "intICD", "ICD code(s)", 
              choices = NULL, 
              multiple=TRUE
            ),                      
            sliderInput("age_range","Recipient age range",min=0, max=100,value=c(0, 100),step=1),
          ),
          conditionalPanel(
            condition="input.intervention_type=='Risk factor population'",
            checkboxGroupInput("intRF",div(class="mb-2", "Risk factor(s)"),unique(distRF$risk_factor),selected = "Smoking")
          ),
          
          shinyWidgets::autonumericInput(
            inputId = "intPop_c1", 
            label = div(tip("Target population size",tipId = "elig_pop_text"),class="mb-1"),
            value = 10, 
            decimalPlaces = 0,
          ),
          
          
          # numericInput("intPop_c1", tip("Target population size",tipId = "elig_pop_text"),100),
          shinyWidgets::prettySwitch("enable_intPop_c1",label = span(class ="small", "Set custom population size"),value = F,status = "warning")
          
        )
      ),
      
      
      
      
      # CEA input tab -----
      menuItem(
        tabName = "cea",
        id = "cea-tab",
        introBox("CEA inputs",
          data.step = 4, data.intro = "Fourth step of the intro",data.position = "right"
          ),
      # icon = icon("bar-chart-o",verify_fa=F),
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
      
      # Eligiblity -------
      menuItem(
        tabName = "eligiblity",
        id = "eligiblity-tab",
        "Eligiblity", # icon = icon("bar-chart-o",verify_fa=F),
        introBox(
          data.step = 6, data.intro = "Sixth step of the intro", data.position = "right",
        div(
          class = "no-lab-sliders-group mb-3",
          div(class="d-flex align-items-end pe-3", sliderInput("prevQ1", "IMD 1 (Most deprived)",0.5,min=0,max=1,step=0.01,ticks = F),div(class="pb-3 fw-bold text-decoration-underline",textOutput("prevQ1Compt"))),
          div(class="d-flex align-items-end pe-3", sliderInput("prevQ2", "IMD2",0.5,min=0,max=1,step=0.01,ticks = F),div(class="pb-3 fw-bold text-decoration-underline",textOutput("prevQ2Compt"))),
          div(class="d-flex align-items-end pe-3", sliderInput("prevQ3", "IMD3",0.5,min=0,max=1,step=0.01,ticks = F),div(class="pb-3 fw-bold text-decoration-underline",textOutput("prevQ3Compt"))),
          div(class="d-flex align-items-end pe-3", sliderInput("prevQ4", "IMD4",0.5,min=0,max=1,step=0.01,ticks = F),div(class="pb-3 fw-bold text-decoration-underline",textOutput("prevQ4Compt"))),
          div(class="d-flex align-items-end pe-3", sliderInput("prevQ5", "IMD5  (Least deprived)",0.5,min=0,max=1,step=0.01,ticks = F),div(class="pb-3 fw-bold text-decoration-underline",textOutput("prevQ5Compt"))),
          shinyWidgets::prettySwitch("choiceRecPop",label = span(class ="small", "Set custom population shares"),value = F,status = "warning"),
          
        )
        )
      ),
      
      
      
      # Distributional inputs tab ----
      menuItem(
        tabName = "uptake",
        "Uptake", # icon = icon("bar-chart-o",verify_fa=F),
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
          # sliderInput("util1Q2", "IMD2",100,min=0,step=5,max=100,post = "%"),
          # sliderInput("util1Q3", "IMD3",100,min=0,step=5,max=100,post = "%"),
          # sliderInput("util1Q4", "IMD4",100,min=0,step=5,max=100,post = "%"),
          # sliderInput("util1Q5", "IMD5 (Least deprived)",100,min=0,step=5,max=100,post = "%"),
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
      
      
      
      
      
      
      # health effect tab -----
      menuItem(
        tabName = "effectiveness",
        "Effectiveness", # icon = icon("bar-chart-o",verify_fa=F),
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
        # sliderInput("qaly1Q1", "IMD1 (Most deprived)",1,min=0,max=10,step=0.25, post = "x"),
        # sliderInput("qaly1Q2", "IMD2",1,min=0,max=10,step=0.25, post = "x"),
        # sliderInput("qaly1Q3", "IMD3",1,min=0,max=10,step=0.25, post = "x"),
        # sliderInput("qaly1Q4", "IMD4",1,min=0,max=10,step=0.25, post = "x"),
        # sliderInput("qaly1Q5", "IMD5 (Least deprieved)",1,min=0,max=10,step=0.25, post = "x"),
        ),
        div(
          class = "px-3 mt-3", 
          div(
            class = "mb-1",
            "Average QALY gain/person:", ),
          div(
            class = "text-dark w-100 card px-2 py-2 text-center fw-bold",
            style = "cursor: not-allowed; background-color: #aaaaaa",
            textOutput("wt_qaly_text1",inline = T))
        ),
        div(class="text-start pe-2 small text-secondary text-wrap lh-1 pt-2 ps-3 pe-4",
            textOutput("wt_qaly_text2",inline = T)
        )
        # textOutput("wt_qaly_text")
      ),
      
      
      # opportunity costs tab -----
      menuItem(
        tabName = "opportunity",
        "Opportunity costs", # icon = icon("bar-chart-o",verify_fa=F),
        autonumericInput("ratioHOC", HTML("Marginal productivity (&#163;/QALY) of alternative resource use"), 15000, min=0,  currencySymbol = "£",decimalPlaces = 0),
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
      actionButton("run","Run Scenario",class="btn btn-custom px-5 mx-auto mt-3 fs-5 "),
      div(
        class = "px-5",
        textInput("scenario_counter","Scenario label",value = "#1",width = "75%")
      )
    ),
    
    
    # download btn ------
    div(
      class = "mt-auto mx-auto pb-3 pe-2 ",
      downloadButton("report_download","Download Report", class = "btn-custom")
    )
    
    
  ),
  
  
  
  # DS BODY ********* -----
  dashboardBody(
    
    
    
    
    # use bootstrap 5
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
    # york favioon
    tags$head(tags$link(rel="shortcut icon", href="york_mini.png")),
    # custom styling css
    includeCSS("style.css"),
    
    tags$head(
      # Note the wrapping of the string in HTML()
      tags$style(HTML("
      #eip_threshold, #eip_aversion {
      background-color: white !important;
      padding: 5px 0;
      border: none;
      }
      #eip_threshold, #eip_aversion {
      padding:0;
      }
      .btn-custom-outline {
      padding: 0px 0px !important;
      height: 25px;
      width: 25px;
      }
      table {
        border-collapse: collapse;
      };
      "))
      ),
    
    # use shinyjs
    useShinyjs(),
    # use introjs tutorial
    introjsUI(),
    
    # loading screen -----
    if(!HIDE_LOADING_SCREEN){
    div(
      id="landing-div",
      style = "position: absolute; top:0; bottom:0; left:0; right:0; z-index: 1900;",
      class= "bg-dark",
      div(
        style = "height:100vh; width: 100vw; position: relative;",
        class = "d-flex flex-column",
        progress_circle(
          value = 0, 
          shiny_id = "progress-circle",
          color = "var(--primary)",
          stroke_width = 5,
          trail_color = "transparent",
          duration = 3000,
          text_color = "white",
          width = "75vw",
          height = "65vh",
          easing = "easeOut",
          ),
      div(
        class="my-auto mx-auto text-center",
        style = "z-index: 2500;",
        div(
          class="text-white display-2",
          style ="text-shadow: 2px 2px #222d32;",
          "Health Equity Impact Calculator",
        ),
        div(
          class="text-white display-5 py-3",
          style ="text-shadow: 2px 2px #222d32;",
        "James Love-Koh & Richard Cookson",
        ),
        div(
          class="d-flex flex-row justify-content-center align-items-center",
          div(
            class="cell mx-auto",
            tags$img(class ="image", src="york_logo.png")
          ),
        ),
      ),
    )
    )
    },
    
    
    
    # BODY ----
    fluidRow(
      column(
        width = 12,
        
        class = "d-flex flex-wrap flex-sm-wrap flex-md-wrap flex-lg-wrap flex-xl-nowrap justify-content-center align-items-start mx-auto px-5",
        style = "max-width: 1500px;",
        
        # div(
        #   style = "
        #   position: absolute; 
        #   left: 50%;
        #   top:50%;
        #   transform: translate(-50%,0) !important;
        #   z-index: 9999;
        #   ;",
        #   class ="w-100 bg-dark",
        #   div(
        #     class="spinner-border", 
        #     style = "width: 10rem; height: 10rem; 
        #     border: 0.5em solid currentColor;
        #     border-right-color: transparent;",
        #     role="status"
        #   )
        # ),
        
        # no run?
        conditionalPanel(
          "input.run===0",
          id="click-prompt",
          class = "d-flex",
          style = "position: absolute; height: 75vh; width: calc(100vw - 300px);",
          div(
            class = "d-flex flex-column justify-content-center mx-auto my-auto",
            div(
              class="w-100 text-secondary fw-bold fs-4 px-5 mx-auto text-center my-auto pb-5",
            "Click 'Run Scenario' to assess the health equity impact"
            )
          ),
                         ),
        # card 1 -----
        
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
              class ="d-flex mt-1 mb-4 border-bottom px-2 pb-3  fs-5",
              div(class="fw-bold me-3 ms-1", tip("Net health inequality",tip = "This value represents the modelled difference in net QALY benefit between the most and least deprived IMD group at population level. The measure differs from the observed gap by incorporating information on the net QALY benefits of IMD2-IMD4 using a simple linear regression model."), "benefit:"),
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
                  "Incremental QALYs/person",
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
        
        
        
        
        # # card 2 -----
        
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
                tags$td("Decision threshold"),
                tags$td(
                  div(
                    class="d-flex px-3 mx-auto align-items-center justify-content-center",
                    actionButton("eip_threshold_minus","-",class="btn-custom-outline"),
                    autonumericInput(
                      "eip_threshold",label = NULL, 
                      20000, min = 0, max = 500000,
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
                      1, min = 0, max = 20,
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
                tags$td(class="ps-4",tip("iNMB:", "Incremental net monetary benefit")),
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
                tags$td(class="ps-4",tip("Weighted iNMB:", "Equity-weighted incremental net monetary benefit")),
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
                  "CE-plane " = "ce_plane",
                  "Equity - net health impact trade-off" = "equityimpact_plot",
                  "Equity - ICER trade-off " = "icer_equityimpact_plot"
                ),
                selected = "ce_plane"
              ),
            ),
            
            
            shinycssloaders::withSpinner(
              type = 2,
              color.background = "white",
              color = "#cb3e72",
              highchartOutput("plane_plot",height = "500px"),
            ),
            div(class = "ms-auto pb-1", prettySwitch("show_prev", "History", value = T, width = "100px")),
            textOutput("raw_icer_text"),
            
            
          ), 
        ),
            
        
        # div(
        #   id="card-2",
        #   class = "d-flex flex-column card shadow-lg mx-1 mx-sm-1 mx-md-1 mx-lg-3 my-2 ",
        #   style ="min-width: 400px; flex-start: 48%; flex-shrink: 0; flex-grow: 0; width: 50%;",
        #   tabBox(
        #     title = NULL,
        #     selected = "main-tab",
        #     # The id lets us use input$tabset1 on the server to find the current tab
        #     id = "tab_box1", 
        #     width = 12,
        #     
        #     tabPanel(
        #       title = "Net health impact", 
        #       value = "main-tab",
        #       
        #       "EMPTY"
        #     ),
        #     
        #     
        #     
        #     
        #     tabPanel(
        #       title = "Equity-efficiency trade-off", 
        #       value = "plane",
        #       div(
        #         class = "d-flex flex-column",
        #         div(class="d-flex justify-content-between align-items-end",
        #             selectInput(
        #               "icer_plane_type",
        #               label = NULL,
        #               choices = c(
        #                 "Equity - net health impact trade-off" = "equityimpact_plot",
        #                 "Equity - ICER trade-off " = "icer_equityimpact_plot"
        #               ),
        #               selected = "equityimpact_plot"
        #             ),
        #             div(class="pb-1",prettySwitch("show_prev", "History",value = T))
        #         ),
        #         highchartOutput("plane_plot"), 
        #         textOutput("raw_icer_text"),
        #         div(
        #           class="d-flex flex-row align-items-end justify-content-evenly",
        #           div(class="px-3",autonumericInput("eip_threshold", tip("Decision threshold","info"),20000,min=0,max=500000,step=1000, width = "180px",currencySymbol = "£",decimalPlaces = 0),),
        #           sliderInput("eip_aversion",tip("Atkinson inequality aversion",tipId = "implicit_weight"),min=0, max=20,value=1,step=0.5),
        #         )
        #       )
        #       # textOutput("implicit_weight_text"),
        #     )
        #   ) # tabbox end
        # )
        
        
        
        
        
      ),
      
      
      
      # tabPanel(
      #   title = "Extended", 
      #   value = "extended-tab",
      #   fluidRow(
      #     column(
      #       width = 12,
      #       class = "px-3 py-3",
      #   h3("Equity impact summary measures"),
      #   "x",
      #   "y",
      #   
      #   hr(),
      #   div(
      #     
      #     textOutput("noEIP_text"),
      #     
      #     
      #   ),
      #   div(
      #     h4("Equity-weighted QALYs"),
      #     plotOutput("atkinson_plot"), 
      #     div(dataTableOutput("atkinson_table"), style = "font-size:90%"),
      #   )
      #     )
      #   )
      #   ),
      
      
      
      
      
      
      
      
    ),
    
  ),
  title = "DCEA app"
)


server = function(input, output, session){
  
  # loading circle progress bar
  update_progress(shiny_id = "progress-circle",value = 1)
  runjs("setTimeout(()=>{
        let el = document.querySelector('#landing-div');
        if(el === null) return;
        el.style.opacity = 0;
        setTimeout(() => {
            el.parentNode.removeChild(el);
        }, 1000);
  },3200)")
  
  # hide cards at start if no model is ran
  runjs("
        document.querySelector('#card-1').style.visibility = 'hidden';
        document.querySelector('#card-2').style.visibility = 'hidden';
        ")
  
  # trigger run in debug mode
  if(HIDE_LOADING_SCREEN){
    runjs("document.querySelector('#run').click()")
  }
  
  # activate tooltips
  runjs("
  var tooltipTriggerList = [].slice.call(document.querySelectorAll('[data-bs-toggle=\"tooltip\"]'))
  var tooltipList = tooltipTriggerList.map(function (tooltipTriggerEl) {
    return new bootstrap.Tooltip(tooltipTriggerEl)
  })")
  
  # download button for highcharter
  runjs("
  Highcharts.SVGRenderer.prototype.symbols.download = (x, y, w, h) => {
    return ['M', x + w * 0.5, y,'L', x + w * 0.5, y + h * 0.7,'M', x + w * 0.3, y + h * 0.5,'L', x + w * 0.5, y + h * 0.7,'L', x + w * 0.7, y + h * 0.5,'M', x, y + h * 0.9,'L', x, y + h,'L', x + w, y + h,'L', x + w, y + h * 0.9];
    };
  ")
  # avoid line break in sidebar slider input label
  # for some reason css setting has no effect on label? 
  runjs("document.querySelector('#util1Q5-label').style.whiteSpace = 'nowrap'")
  runjs("document.querySelector('#qaly1Q5-label').style.whiteSpace = 'nowrap'")
  
  shinyjs::disable("report_download")
  
  # server side rendering of selectize
  updateSelectizeInput(
    session, 
    'intICD', 
    choices = icd10_input_labs, 
    server = TRUE,
    # options = list(render = I(
    # '{option: function(item, escape) {
    #    return "<div class =\'badge bg-secondary\'>" + escape(item.label) + "</div>"
    # }}'))
  )
  # icd10_input_labs, ## server side rendering
  
  observeEvent(
    list(input$prevQ1,input$prevQ2,input$prevQ3,input$prevQ4,input$prevQ5),{
      sumPrev = sum(c(input$prevQ1,input$prevQ2,input$prevQ3,input$prevQ4,input$prevQ5))
      output$prevQ1Compt <- renderText( paste0(round((input$prevQ1/sumPrev)*100,0),"%") )
      output$prevQ2Compt <- renderText( paste0(round((input$prevQ2/sumPrev)*100,0),"%") )
      output$prevQ3Compt <- renderText( paste0(round((input$prevQ3/sumPrev)*100,0),"%") )
      output$prevQ4Compt <- renderText( paste0(round((input$prevQ4/sumPrev)*100,0),"%") )
      output$prevQ5Compt <- renderText( paste0(round((input$prevQ5/sumPrev)*100,0),"%") )
    })
  
  observeEvent(input$choiceRecPop, {
    inputIds = c("prevQ1","prevQ2","prevQ3","prevQ4","prevQ5")
    if(input$choiceRecPop)
      lapply(inputIds, shinyjs::enable)
    else 
      lapply(inputIds, shinyjs::disable)
  })
  
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
  
  
  
  observeEvent(input$enable_intPop_c1, {
    if(input$enable_intPop_c1){
      shinyjs::enable("intPop_c1")
    } else {
      shinyjs::disable("intPop_c1")
    }
  },ignoreNULL = F)
  
  # Create tables from UI inputs --------------------------------------------
  
  # Population outcomes
  resCEA <- reactive({
    data.frame(
      comparator=1,
      qalys=ifelse(is.null(input$incQALYs_c1),NA,input$incQALYs_c1),
      cost=ifelse(is.null(input$incCost_c1),NA,input$incCost_c1),
      pop=as.numeric(input$intPop_c1))
  })
  
  # Live HOC distribution
  distHOC <- reactive({
    distHOC_scenarios %>% filter(scenario==input$choiceHOC) %>% select(-scenario)
  })
  
  # User defined HOC dist total
  output$hoc_user_warning <- renderText({ 
    if(input$choiceHOC=="flat") {
      "Health opportunity costs are distributed equally across deprivation groups"
    } else {
      "Health opportunity costs fall disproportionately on the more deprived groups"
    }
  })
  
  output$disthoc_plot <- renderHighchart({
    plot_hoc_dist(distHOC())
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
      util_rate= rep(0,5) # c(input$util1Q2,input$util2Q2,input$util2Q3,input$util2Q4,input$util2Q5))
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
  output$icer_text <- renderText({
    if(baseICER()=="dominant") {
      paste0("Dominating")
    } else if(baseICER()=="dominated") {
      paste0("Dominated")
    } else {
      paste0("£",baseICER(),"/QALY")
    }
  })
  
  # Raw ICER
  baseICER_raw <- reactive({
    icer_calc_raw(resCEA())
  })
  
  # Weighted incremental QALYs statement
  wtQALY <- reactive({
    wt_qaly_calc(resCEA(),distQALYeffect(),recipients_table_raw1())
  })
  
  # output$wt_qaly_text <- renderText({ 
  #   paste0("This set of multipliers yield an average incremental QALY gain of ",wtQALY()," per person")
  #   
  # })
  output$wt_qaly_text1 <- renderText({ 
    wtQALY()
  })
  output$wt_qaly_text2 <- renderText({ 
    paste0("This set of multipliers yield an average incremental QALY gain of ",wtQALY()," per person")
    
  })
  
  
  # Recipient pop statements
  popRec1 <- reactive({
    format(sum(recipients_table_raw1()$recipients_util), big.mark=",")
  })
  
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
  
  
  
  
  
  #  RUN -----
  
  
  
  
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
    ### ...
    if(input$run>0){
      
    runjs("
        document.querySelector('#card-1').style.visibility = 'visible';
        document.querySelector('#card-2').style.visibility = 'visible';
        document.querySelector('#click-prompt').remove();
        ")
    }
    
    
    
    
    R$scenario = isolate(input$scenario_counter)
    updateTextInput(session,"scenario_counter",value = paste0("#",as.numeric(input$run)+1))
    output$intName_txt = renderText(intName())
    output$scenario_txt = renderText(isolate(input$scenario_counter))
    
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
    
    # Net benefit tab ---------------------------------------------------------
    output$netbenefit_table =renderDataTable(server = F, {
      
      if(input$run == 0){return(NULL)}
        
        # elaborate reformatting - needs refactoring
        table[1,2:7] = paste0(round(as.numeric(table[1,2:7])*100,0),"%")
        table[2,2:6] = paste0(round(as.numeric(table[2,2:6])*100,0),"%")
        table[2,1] = "Uptake" 
        table[3,1] = "Inc. QALY/person"
        table[4,2:7] = paste0(round(as.numeric(table[4,2:7])*100,0),"%")
        table[5,2:7] = paste0(round(as.numeric(table[5,2:7])*100,0),"%")
        table[5,1] = "Recipients (share)"
        table[6,2:7] = formatC(round(as.numeric(table[6,2:7])/1000,0),digits = 0, big.mark = ",", format="f")
        table[6,1] = "Recipients (in 1,000s)"
        table = table[c(1:4,6,5,7:9),]
        table[7,2:7] = formatC(round(as.numeric(table[7,2:7]),0),digits = 0, big.mark = ",", format="f")
        table[7,1] = "Intervention benefits (QALYs)"
        table[8,2:7] = formatC(round(as.numeric(table[8,2:7]),0),digits = 0, big.mark = ",", format="f")
        table[8,1] = "Opportunity costs (QALYs)"
        table[9,2:7] = formatC(round(as.numeric(table[9,2:7]),0),digits = 0, big.mark = ",", format="f")
        table[9,1] = "Net health benefit (QALYs)"
        table = cbind(table,"s1" = c(rep("F",5),"T",rep("F",3)))
        table = cbind(table,"s2" = c(rep("F",6),rep("T",3)))
        colnames(table)[1] = ""
        
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
    
    
    Sys.sleep(0.75)
    
  }) # run close
  
  
  
  
  
  
  # Net benefits plot
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
      # add recipients
      # recipeints_df = isolate(recipients_table_raw1())
      # plot_df <- merge(plot_df,recipeints_df, by = "imd")
      plot_df$imd_str = c("IMD 1<br>(Most deprived)","IMD 2","IMD 3", "IMD 4", "IMD 5<br>(Least deprived)")
      plot_df$cols = imdCols()
      plot_df$cols = substr(plot_df$cols,1,7)
      
      zeroLine = data.frame(var = 0, imd_str = plot_df$imd_str)
      
      table_vars = c(
        "Proportion" = "Eligible population",
        "Proportion" =  "Uptake rate",
        "Incremental QALYs/person" = "Incremental QALYs/person",
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
  
  
  
  
  
  
  
  
  
  
  
  # Net benefit inequality table
  inequality_table_nb_raw1 = reactive({
    table_nb_inequality(netbenefit_table_raw1(),recipients_table_raw1())
  })
  
  
  
  
  
  
  # Atkinson ----------------------------------------------------------------
  
  atkinson_table_raw = reactive({
    table_atkinson_raw(healthdistribution_table_raw1(),
                       healthdistribution_table_raw2())
  })
  
  output$atkinson_table =renderDataTable({
    
      table = table_atkinson(
        atkinson_table_raw(),
        imp_AtWeights,
        weightedicer_raw(),
        F # input$choiceUptake2
      )
      datatable(table,style = 'bootstrap',rownames = FALSE,
                options = list(paging=FALSE,searching=FALSE,info=FALSE))
    
  })
  
  output$atkinson_plot = renderPlot({
      plot_atkinson(
        atkinson_table_raw(),
        F # input$choiceUptake2
      )
  })
  
  # uptake 2-way reactive inputs: UPTAKE -----
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
  # uptake 2-way reactive inputs: EFFECTIVENESS -----
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

  
  # Equity impact plane -----------------------------------------------------
  
  R <- reactiveValues(old_atkinson=NULL, old_atkinson_icer=NULL,run = 0, scenario=NULL)
  
  
  observeEvent(input$icer_plane_type,{
    if(input$icer_plane_type=="ce_plane"){
      shinyjs::disable("show_prev")
    } else {
      shinyjs::enable("show_prev")
    }
  })
  
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
      
       # isolate(R$old_atkinson)
      cost = R$incC
      qalys = R$incQ
      thresh = input$eip_threshold
      
      table = table_atkinson(atkinson_table_raw(),imp_AtWeights,weightedicer_raw(),F)
      selected_eip = table[,1] ==   input$eip_aversion
      weighted_qalys = round(R$incC/table[selected_eip,3],4)
      
      max_yval <- abs(cost)*1.5
      max_xval <- max(c(abs(qalys),abs(weighted_qalys)))*1.5
      
      
      eip = data.frame(qalys = c(qalys,weighted_qalys), cost = cost, name = c("raw","equity weighted"), cols=c("#212529","#cb3e72"))
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
          show_old = input$show_prev
        )
        if(input$show_prev){
          R$old_atkinson = isolate(res$data)
        }
      }
      
      
      if(input$icer_plane_type == "icer_equityimpact_plot"){
        if(isolate(indicatorICER()==1)){
          res = plot_icer_equity_impact(
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
            show_old = input$show_prev
          )
          if(input$show_prev){
            R$old_atkinson_icer = isolate(res$data)
          }
          
        } else { NULL }
      }
      
      return(res$plot)
      
    
  })
  
  # Implicit weight warnings
  observe({
    if(input$eip_aversion==0) { 
      txt_ = paste("This parameter value of 0 places equal weight on health gains and losses for the worst-off (IMD1) compared with the best-off (IMD5)",sep="") 
    } else { 
      txt_ = paste0("A parameter of ",input$eip_aversion, " gives health gains for the worst-off (IMD1) a ",implicit_weight_raw(input$eip_aversion), "% additional weight compared with those for best-off (IMD5)")  
    }
    updateTip(id = "implicit_weight",tip = txt_)
  })
  
  # implicit_weight = reactive({
  #   x = implicit_weight_raw(input$eip_aversion)
  #   print(x)
  #   
  #   runjs(paste0("document.querySelector('#test').setAttribute('data-bs-original-title', 'New Tooltip Title');"))
  #                # innerhtml = '",x,"';"
  #                # ))
  #   return(x)
  # })
  
  output$implicit_weight_text <- renderText({ 
    if(input$eip_aversion==0) { 
      paste("This parameter value of 0 places equal weight on health gains and losses for the worst-off (IMD1) compared with the best-off (IMD5)",sep="") 
    } else { 
      paste0("A parameter of ",input$eip_aversion, " gives health gains for the worst-off (IMD1) a ",implicit_weight(), "% additional weight compared with those for best-off (IMD5)")  
    }
  })
  
  
  indicatorICER = reactive({
    indicator_icer(resCEA())
  })
  
  
  # iNMB update  -----
  observeEvent(list(input$eip_threshold,input$run), {
    inmb = formatC(c(R$incQ * input$eip_threshold - R$incC),digits = 0, big.mark = ",",format = "f")
    output$inmb_text <- renderText(paste0("£",inmb))
  })
  
  # update atkinson ------
  observeEvent(input$eip_aversion_plus,{
    val = input$eip_aversion
    if(val<20){
      val = val+0.5
    }
    updateAutonumericInput(session = session, inputId = "eip_aversion", value = val) 
  })
  observeEvent(input$eip_aversion_minus,{
    val = input$eip_aversion
    if(val>0){
      val = val-0.5
    }
    updateAutonumericInput(session = session, inputId = "eip_aversion", value = val) 
  })
  # update threshold
  observeEvent(input$eip_threshold_plus,{
    val = input$eip_threshold
    updateAutonumericInput(session = session, inputId = "eip_threshold", value = val+5000) 
  })
  observeEvent(input$eip_threshold_minus,{
    val = input$eip_threshold
    updateAutonumericInput(session = session, inputId = "eip_threshold", value = val-5000) 
  })
  
    
    
  # weighted icer update  -----
  observeEvent(list(input$eip_threshold,input$run, input$eip_aversion),ignoreNULL = T, {
    table = table_atkinson(atkinson_table_raw(),imp_AtWeights,weightedicer_raw(),F)
    table222 <<- table
    selected_eip = table[,1] ==   input$eip_aversion
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
  
  
  # Kolm --------------------------------------------------------------------
  kolm_table_raw = reactive({
    table_kolm_raw(healthdistribution_table_raw1(),healthdistribution_table_raw2())
  })
  
  output$kolm_table = renderDataTable({
    
      table = table_kolm(
        kolm_table_raw(),
        imp_KmWeights,
        F # input$choiceUptake2
      )
      datatable(table,style = 'bootstrap',rownames = FALSE,
                options = list(paging=FALSE,searching=FALSE,info=FALSE))
    
  })
  
  output$kolm_plot = renderPlot(plot_kolm(kolm_table_raw(), F))
  
  
  
  # total inqautlity info modal
  observeEvent(input$total_inequality_modal, {
    showModal(modalDialog(
      title = "Total health inequality in England",
      "Tbc",
      easyClose = TRUE,
      footer = div(modalButton("Close"),class="border rounded-3")
    ))
  })
  
  
  observeEvent(input$showAbout,{
    showModal(modalDialog(size = "l",fade = T,
                          title = "About",
                          div(
                            
                            div(
                              h4("About this application"),
                              HTML("<p>This application was developed by James Love-Koh and Richard Cookson, Centre for
                  Health Economics, University of York, and Paul Schneider, University of Sheffield, with advisory input from Susan Griffin, Rita Faria 
                    and Fan Yang. The NICE Project Leads were Lesley Owen and Monica Desai.</p>"
                              ),
                              br(),
                            ),
                            div(
                              h4("Acknowledgements"),
                              HTML("<p>For their helpful and detailed feedback on the draft calculator we would like to 
                    thank Deborah O’Callaghan, James Lomas, Mike Paulden and the many NICE officials, advisers 
                    and committee members that we consulted during development.</p>"
                              ),
                              br(),
                            ),
                            
                            
                            
                            div(
                              h4("Funding statement"),
                              HTML("<p>Financial support for this project was provided by the National Institute for Health and Care Excellence (NICE). 
             All errors and opinions represented in the application are entirely those of development team and
                  do not reflect those of NICE or the University of York.</p>"
                                   ),
                              br(),
                              ),
                            
                            div(
                              h4("Legal disclaimer"),
                              HTML("<p>The authors make no representations or warranties of any kind with respect to the
                  information, graphics and outputs available on this site.</p>"
                                   ),
                              br()
                              )
                            
                          
                          ),
                          easyClose = TRUE,
                          footer = div(modalButton("Close"),class="border rounded-3")
    )
    )
  })
                            
  
  observeEvent(input$showReferences,{
    showModal(modalDialog(size = "l",fade = T,
      title = "Key references",
      div(
               tags$div(
                 HTML("<p>Below is a list of publications that detail some of the concepts
                  and methods that have been used to build this calculator.</p>")),br(),
               h4("Overview of economic evaluation and equity concepts"),
               HTML("<p>Cookson, R., Griffin, S., Norheim, O.F., Culyer, A.J., 
           Chalkidou, K., 2020. Distributional Cost-Effectiveness Analysis 
           Comes of Age. Value in Health, 24(1), 118-120.
                  "),     
               tags$a(href="https://doi.org/10.1016/j.jval.2020.10.001","[Link]",target="_blank"),
               HTML("</p>"),
               HTML("<p>Cookson, R., Griffin, S., Norheim O.F., Culyer, A.J. (Eds), 2021. 
           Distributional cost-effectiveness analysis: quantifying health equity 
           impacts and trade-offs.  Oxford University Press.
                  "),     
               tags$a(href="https://www.york.ac.uk/che/publications/books/handbook-dcea/","[Link]",target="_blank"),
               HTML("</p>"),
               h4("Distributional cost-effectiveness using aggregate data"),
               HTML("<p>Griffin, S., Love-Koh, J., Pennington, B., Owen, L., 2019. 
                  Evaluation of Intervention Impact on Health Inequality for 
                  Resource Allocation. Medical Decision Making, 39(3), 172–181. 
                  "),     
               tags$a(href="https://doi.org/10.1177/0272989X19829726","[Link]",target="_blank"),
               HTML("</p>"),
               HTML("<p>Love-Koh, J., Cookson, R., Gutacker, N., Patton, T., Griffin, S., 
           2019. Aggregate Distributional Cost-Effectiveness Analysis of Health 
           Technologies. Value in Health, 22(5), 518–526.  
                  "),     
               tags$a(href="https://doi.org/10.1016/j.jval.2019.03.006","[Link]",target="_blank"),
               HTML("</p>"),
               h4("Distribution of health opportunity costs of NHS expenditure"),
               HTML("<p>Love-Koh, J., Cookson, R., Claxton, K., Griffin, S., 2020. 
           Estimating Social Variation in the Health Effects of Changes in Health 
           Care Expenditure. Medical Decision Making, 40(2), 170–182.
                  "),     
               tags$a(href="https://doi.org/10.1177/0272989X20904360","[Link]",target="_blank"),
               HTML("</p>"),
               h4("Baseline levels of health inequality"),
               HTML("<p>Love-Koh, J., Asaria, M., Cookson, R., Griffin, S., 2015. 
           The Social Distribution of Health: Estimating Quality-Adjusted Life 
           Expectancy in England. Value in Health, 18(5), 655–662.
                  "),     
               tags$a(href="https://doi.org/10.1016/j.jval.2015.03.1784","[Link]",target="_blank"),
               HTML("</p>"),
               h4("The inequality staircase effects of health interventions"),
               HTML("<p>Tugwell, P., de Savigny, D., Hawker, G., Robinson, V., 2006. 
           Applying clinical epidemiological methods to health equity: the 
           equity effectiveness loop. BMJ 332, 358–61.
                  "),     
               tags$a(href="https://doi.org/10.1136/bmj.332.7537.358","[Link]",target="_blank"),
               HTML("</p>"),br(),br()
      ),
      easyClose = TRUE,
      footer = div(modalButton("Close"),class="border rounded-3")
             )
    )
  })
  
  
  
  # Markdown report ---------------------------------------------------------
  
  # Create outputs for Markdown report
  ceaResMD <- reactive({
    data.frame(`Incremental QALYs`= ifelse(is.null(input$incQALYs_c1),NA,input$incQALYs_c1),
               `Incremental costs`=paste0("£",format(input$incCost_c1,big.mark=",")),
               `Eligible population`=format(input$intPop_c1,big.mark=","))
  })
  
  output$report_download <- downloadHandler(
    filename = "equity_report.html",
    content = function(file) {
      alert("Error: not implemented")
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport <- file.path(tempdir(), "summary_report.Rmd")
      file.copy("summary_report.Rmd", tempReport, overwrite = TRUE)
      
      # Set up parameters to pass to Rmd document
      report_params <- list(intName1 = if(input$intName1 == ""){"Smoking Prevention"} else {input$intName1} ,
                            ceaResTable = ceaResMD,
                            rawICER = baseICER_raw,
                            indICER = indicatorICER,
                            ineqAv = input$eip_aversion,
                            
                            eipCET = input$eip_threshold,
                            impWeight = implicit_weight,
                            
                            ceaRes = resCEA,
                            ratioHOC = input$ratioHOC,
                            ratioType = input$ratioType,
                            inputTab = input_summary_table,
                            uptakeScen = F , #input$choiceUptake2,
                            recTab1 = recipients_table_raw1,
                            recTab2 = recipients_table_raw2,
                            nbTab1 = netbenefit_table_raw1,
                            nbTab2 = netbenefit_table_raw2,
                            weightedICER = weightedicer_raw,
                            hdistTab1 = healthdistribution_table_raw1,
                            hdistTab2 = healthdistribution_table_raw2,
                            ineqTabRaw1 = inequality_table_raw1,
                            ineqTabRaw2 = inequality_table_raw2,
                            ineqTab1 = inequality_table1,
                            ineqTab2 = inequality_table2,
                            ineqNBTab1 = inequality_table_nb_raw1,
                            
                            atTab = atkinson_table_raw,
                            kmTab = kolm_table_raw,
                            nComp = input$nComparator,
                            compName = if(input$compName1 == ""){"No Intervention"} else {input$compName1} ,
                            # indName = input$indicationName,
                            codeICD = input$intICD,
                            codeRF = input$intRF,
                            codeType = input$intervention_type,
                            impWeightsAt = imp_AtWeights
      )
      
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      rmarkdown::render(tempReport,output_file = file,
                        params=report_params,
                        envir = new.env(parent = globalenv())
      )
    }
  )
  
  
  
  
  observeEvent(input$tutorial_start,{
    showModal(modalDialog(
      title = "Tutorial",size = "l",
      tags$iframe(width="560", height="315",src="https://www.youtube.com/embed/ScMzIvxBSi4",title="YouTube video player",frameborder="0"),
      easyClose = TRUE,
      footer = div(modalButton("Close"),class="border rounded-3")
    ))
    
  })
  
  
  
  
  
}


shinyApp(ui, server)