
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
      class = "dropdown pe-5 text-white fw-bold", 
      actionLink("tutorial_start","? WATCH THE TUTORIAL")
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
          id="prev-sliders-group", class = "mb-3",
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
          sliderInput("util1Q1", "IMD1 (Most deprived)",100,min=0,step=5,max=100,post = "%"),
          sliderInput("util1Q2", "IMD2",100,min=0,step=5,max=100,post = "%"),
          sliderInput("util1Q3", "IMD3",100,min=0,step=5,max=100,post = "%"),
          sliderInput("util1Q4", "IMD4",100,min=0,step=5,max=100,post = "%"),
          sliderInput("util1Q5", "IMD5 (Least deprived)",100,min=0,step=5,max=100,post = "%"),
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
        sliderInput("qaly1Q1", "IMD1 (Most deprived)",1,min=0,max=10,step=0.25, post = "x"),
        sliderInput("qaly1Q2", "IMD2",1,min=0,max=10,step=0.25, post = "x"),
        sliderInput("qaly1Q3", "IMD3",1,min=0,max=10,step=0.25, post = "x"),
        sliderInput("qaly1Q4", "IMD4",1,min=0,max=10,step=0.25, post = "x"),
        sliderInput("qaly1Q5", "IMD5 (Least deprieved)",1,min=0,max=10,step=0.25, post = "x"),
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
        "Author 1, Author 2, Author 3",
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
        
        class = "d-flex justify-content-center align-items-start mx-auto",
        # style = "max-width: 1500px;",
        
        # no run?
        conditionalPanel(
          "input.run===0",
          div(
            class = "d-flex flex-column justify-content-center flex-fill",
            style = "position: absolute; height: 75vh; width: calc(100vw - 300px);",
            div(
              class="w-100 text-secondary fw-light fs-4 px-5 mx-auto text-center my-auto pb-5",
            "Click 'Run Scenario' to assess the health equity impact"
            )
          ),
                         ),
        # card 1 -----
        div(
          id="card-1",
          class = "d-flex flex-column card card-body shadow-lg mx-1 mx-sm-1 mx-md-1 mx-lg-3 my-2",
          style ="min-width: 350px; flex-start: 48%; max-width: 750px;",
          
          
          div(
            class="lh-sm mb-2",
            div("Intervention:",span(class="text-decoration-underline fw-bold",textOutput("intName_txt", inline = T)), class="fs-5"),
            div("Scenario:",textOutput("scenario_txt", inline = T),class="text-secondary")
          ),
          
          # sii
          div(
            class = "fs-5  fw-bold  d-flex justify-content-between align-items-center py-1",
            span(tip("Net health inequality",tip = "This value represents the modelled difference in net QALY benefit between the most and least deprived IMD group at population level. The measure differs from the observed gap by incorporating information on the net QALY benefits of IMD2-IMD4 using a simple linear regression model."), "reduction:"), 
            div(
              class = "custom-col fw-bold text-decoration-underline fw-bold card d-inline-block px-2 py-1 text-center",
              textOutput("sii",inline = T), "QALYs"
            ),
          ),
          
          # siai
          div(
            class = "fs-6 d-flex justify-content-between align-items-center py-1",
            span(tip("Slope Index of Absolute Inequality",tip="Slope index of inequality (SII): The modelled gap between the most and least deprived individuals calculated from a simple linear regression line. This is similar to the observed gap between most and least deprived groups, but also takes into account outcomes for the middle groups. A negative SII indicates the most deprived have worse health outcomes than the least deprived. A reduction in the SII indicates absolute health inequality has been reduced.:"),"reduction"), 
            div(
              class = "custom-col text-decoration-underline fw-bold card d-inline-block px-2 py-1",
              textOutput("siai_reduction",inline = T), 
            ),
          ),
          
          # rii
          div(
            class = "fs-6 d-flex justify-content-between align-items-center py-1",
            span(tip("Relative Index on Inequality", "Relative index of inequality (RII): The proportional gap between the most and least deprived individuals, derived from the SII. An RII of -1 means the estimated health outcomes for the most deprived are 100% greater than those for the least deprived. A reduction in the RII indicates relative health inequality has been reduced."),"reduction:"),
            div(
              class = "custom-col text-decoration-underline fw-bold card d-inline-block px-2 py-1",
              textOutput("rii_reduction",inline = T), 
            ),
          ),
          
          # ci
          div(
            class = "fs-6 d-flex justify-content-between align-items-center py-1",
            span(tip("Concentration Index", "Concentration index: Another index of relative inequality, similar to the Gini index of income inequality and based on the correlation between deprivation rank and relative 'shares' of population total health. Equals -1 or 1 when only the most or least deprived individual experiences any health at all, and 0 if health is equally distributed. A reduction in a positive concentration index (or an increase for a negative index) indicates health inequality has been reduced."),"reduction:"),
            div(
              class = "custom-col text-decoration-underline fw-bold card d-inline-block px-2 py-1",
              textOutput("ci_reduction",inline = T), 
            ),
          ),
          
          # total health ineq modal 
          div(
            class = "ps-1 mt-2",
            actionLink("total_inequality_modal",label = "What is the total health inequality in England?",
                       class = "text-secondary text-decoration-underline small tri-marker")
          ),
          
          hr(),
          
          tags$details(
            tags$summary(span(class="fw-bold custom-col", "Show detailed results")),
          div(
            style="overflow-x: scroll;",
            dataTableOutput("netbenefit_table")
            ),
          )
          
          
          
        ),
        
        
        
        
        # # card 2 -----
        div(
          id="card-2",
          class = "d-flex flex-column card shadow-lg mx-1 mx-sm-1 mx-md-1 mx-lg-3 my-2 ",
          style ="min-width: 400px; flex-start: 48%; flex-shrink: 0; flex-grow: 0; width: 50%;",
          tabBox(
            title = NULL,
            selected = "main-tab",
            # The id lets us use input$tabset1 on the server to find the current tab
            id = "tab_box1", 
            width = 12,
            
            tabPanel(
              title = "Net health impact", 
              value = "main-tab",
              
              div(
                div(
                  class="d-flex align-items-center",
                  div(class="pe-2 fw-bold mb-3","Select plot:"),
                  selectInput(
                    "netbenefit_plot_type",
                    label = NULL,
                    choices = c(
                      "Number of recipients" = "recipients",
                      "Intervention health benefits" = "benefits", 
                      "Health opportunity costs" = "costs",
                      "Net health benefit" = "net"
                    ),
                    selected = "net"
                  )
                ),
                highchartOutput("netbenefit_plot"),
              ),
            ),
            
            
            
            
            tabPanel(
              title = "Equity-efficiency trade-off", 
              value = "plane",
              div(
                class = "d-flex flex-column",
                div(class="d-flex justify-content-between align-items-end",
                    selectInput(
                      "icer_plane_type",
                      label = NULL,
                      choices = c(
                        "Equity - net health impact trade-off" = "equityimpact_plot",
                        "Equity - ICER trade-off " = "icer_equityimpact_plot"
                      ),
                      selected = "equityimpact_plot"
                    ),
                    div(class="pb-1",prettySwitch("show_prev", "History",value = T))
                ),
                highchartOutput("plane_plot"), 
                textOutput("raw_icer_text"),
                div(
                  class="d-flex flex-row align-items-end justify-content-evenly",
                  div(class="px-3",autonumericInput("eip_threshold", tip("Decision threshold","info"),20000,min=0,max=500000,step=1000, width = "180px",currencySymbol = "£",decimalPlaces = 0),),
                  sliderInput("eip_aversion",tip("Atkinson inequality aversion",tipId = "implicit_weight"),min=0, max=20,value=1,step=0.5),
                )
              )
              # textOutput("implicit_weight_text"),
            )
          ) # tabbox end
        )
        
        
        
        
        
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
  #lapply(c(0.1,0.5,0.75,1),\(x){
    update_progress(shiny_id = "progress-circle",value = 1)
  runjs("setTimeout(()=>{
        let el = document.querySelector('#landing-div');
        if(el === null) return;
        el.style.opacity = 0;
        setTimeout(() => {
            el.parentNode.removeChild(el);
        }, 1000);
  },3200)")
  # })
  
  runjs("
        document.querySelector('#card-1').style.visibility = 'hidden';
        document.querySelector('#card-2').style.visibility = 'hidden';
        ")
  
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
      util_rate= rep(0,5) # c(input$util2Q1,input$util2Q2,input$util2Q3,input$util2Q4,input$util2Q5))
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
  
  
  # output$input_summary <- renderDataTable({
  #   
  #   withProgress(message = 'Loading ...',{
  #     table = table_inputSummary(distPrev(),distUtil1(),distIncQALY(),distHOC())
  #     table = cbind(table,"Total" = 0)
  #     table_ext = table_recipients(recipients_table_raw1())
  #     names(table_ext)[1] = "Input"
  #     table = rbind(table, table_ext)
  #     
  #     # my_vals = unique(df$ID)
  #     # my_colors = ifelse(my_vals=='myID','orange','grey')
  #     
  #     datatable(
  #       table,
  #       style = 'bootstrap',
  #       rownames = FALSE,
  #       options = list(paging=FALSE,searching=FALSE,info=FALSE)
  #       ) 
  #     # %>% 
  #     #   formatStyle('ID',
  #     #     target = 'row',
  #     #     backgroundColor = styleEqual(my_vals,my_colors)
  #     #   )
  #   })
  # })
  
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
  
  # Output tables for each scenario
  # output$recipients_table1 = renderDataTable({
  #   withProgress(message = 'Loading ...',{
  #     table = table_recipients(recipients_table_raw1())
  #     datatable(table,
  #               style = 'bootstrap',
  #               rownames = FALSE,
  #               options = list(paging=FALSE,searching=FALSE,info=FALSE))
  #   })
  # })
  
  
  # Output plot w/ both scenarios (if selected)
  # output$recipients_plot = renderPlot({
  #   print(recipients_table_raw1())
  #   withProgress(message = 'Loading ...',{
  #     plot_recipients(
  #       recipients_table_raw1(),
  #       recipients_table_raw2(),
  #       F, #input$choiceUptake2,
  #       if(input$intName1 == ""){"Smoking Prevention"} else {input$intName1} 
  #       )
  #   })
  # })
  
  
  
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
    
    
    
    
    # Net benefit tab ---------------------------------------------------------
    output$netbenefit_table =renderDataTable(server = F, {
      withProgress(message = 'Loading ...',{
        
        
        # elaborate reformatting - needs refactoring
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
        # print(table)
        # my_vals = c(rep(F,6),T,rep(F,2))
        # my_style = c(rep("0",6),T,rep("solid 1px red",2))
        
        borderStyle <- "value == 'T' ? 'double black' : value != 'white' ? '' : 'white'"  
        class(borderStyle) <- "JS_EVAL"
        bgStyle = "value == 'T' ? 'bold' : value != '' ? '' : ''"  
        class(bgStyle) <- "JS_EVAL"
        
        datatable(
          table,
          style = 'bootstrap',
          class = "hover compact",
          rownames = FALSE,
          options = list(
            paging=F,searching=F,info=F,ordering=F,
            columnDefs = list(list(visible=FALSE, targets=c(7,8)))
          )
        ) %>%
          formatStyle(
            "s1",
            target = 'row',
            border = borderStyle
          ) %>%
          formatStyle(
            "s2",
            target = 'row',
            fontWeight = bgStyle
          )
        
        
        # datatable(table,
        #           style = 'bootstrap',
        #           rownames = FALSE,
        #           options = list(paging=FALSE,searching=FALSE,info=FALSE))
      })
    })
    
    
    inequality_table = table_inequality_comb(
      isolate(inequality_table1()),
      isolate(inequality_table2()),
      F # input$choiceUptake2
    )
    
    output$siai_reduction = renderText(formatC(inequality_table$Value[1],digits=6,format = "f"))
    output$rii_reduction = renderText(formatC(inequality_table$Value[2],digits=6,format = "f"))
    output$ci_reduction = renderText(formatC(inequality_table$Value[3],digits=6,format = "f"))
    
    
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
    
    
    
    
    withProgress(message = 'Loading...',{
      
      plot_df <- isolate(netbenefit_table_raw1())
      # add recipients
      recipeints_df = isolate(recipients_table_raw1())
      plot_df <- merge(plot_df,recipeints_df, by = "imd")
      plot_df$imd_str = c("IMD 1<br>(Most deprived)","IMD 2","IMD 3", "IMD 4", "IMD 5<br>(Least deprived)")
      plot_df$cols = imdCols()
      
      zeroLine = data.frame(var = 0, imd_str = plot_df$imd_str) 
      
      
      
      if(input$netbenefit_plot_type == "benefits"){
        plot_df$var = plot_df$qalys
        lab = "QALYs"
      } else if (input$netbenefit_plot_type == "costs"){
        plot_df$var = -plot_df$hoc
        lab = "QALYs"
      } else if (input$netbenefit_plot_type == "recipients"){ 
        plot_df$var = plot_df$recipients_util
        lab = "Number of recipients"
      } else {
        plot_df$var = plot_df$net_qalys
        lab = "QALYs"
      }
      
      
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
        hc_title(text = lab, align = "center", x=40,y=10,  verticalAlign = 'top', floating = "true", style = list(fontSize = "16px")) %>%
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
            text = "Quality-Adjusted Life Years",
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
      
      # return(p1)
      
      
      
      
      
      # print("netbenefit_table_raw1()")
      # print(netbenefit_table_raw1())
      
      # if(input$netbenefit_plot_type == 1){
      #   return(ggplot())
      # }
      # 
      # plot_netbenefit(netbenefit_table_raw1())
      # 
      
      
      
    })
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
    withProgress(message = 'Loading ...',{
      table = table_atkinson(
        atkinson_table_raw(),
        imp_AtWeights,
        weightedicer_raw(),
        F # input$choiceUptake2
      )
      datatable(table,style = 'bootstrap',rownames = FALSE,
                options = list(paging=FALSE,searching=FALSE,info=FALSE))
    })
  })
  
  output$atkinson_plot =renderPlot({
    withProgress(message = 'Loading ...',{
      plot_atkinson(
        atkinson_table_raw(),
        F # input$choiceUptake2
      )
    })
  })
  
  # Equity impact plane -----------------------------------------------------
  
  R <- reactiveValues(old_atkinson=NULL, old_atkinson_icer=NULL,run = 0, scenario=NULL)
  
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
    
    withProgress(message = 'Loading ...',{
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
  
  
  
  
  # Kolm --------------------------------------------------------------------
  kolm_table_raw = reactive({
    table_kolm_raw(healthdistribution_table_raw1(),healthdistribution_table_raw2())
  })
  
  output$kolm_table = renderDataTable({
    withProgress(message = 'Loading ...',{
      table = table_kolm(
        kolm_table_raw(),
        imp_KmWeights,
        F # input$choiceUptake2
      )
      datatable(table,style = 'bootstrap',rownames = FALSE,
                options = list(paging=FALSE,searching=FALSE,info=FALSE))
    })
  })
  
  output$kolm_plot =renderPlot({
    withProgress(message = 'Loading ...',{
      plot_kolm(
        kolm_table_raw(),
        F # input$choiceUptake2
      )
    })
  })
  
  
  
  # total inqautlity info modal
  observeEvent(input$total_inequality_modal, {
    showModal(modalDialog(
      title = "Total health inequality in England",
      "Tbc",
      easyClose = TRUE,
      footer = div(modalButton("Close"),class="border rounded-3")
    ))
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
  
  
  # observe what tabs users are accessing
  runjs(
  '$(document).on("click", "a", function() {
    let href = $(this).attr("href");
    if(href === undefined || href === null) return; 
    if(!href.includes("#shiny-tab-")) return;
    let sub = href.replace("#shiny-tab-","")
    Shiny.setInputValue("tabs", sub);
  });'
        )
  tutorial = reactiveVal(F)
  
  observeEvent(input$tutorial_start,{
    tutorial(T)
  
    tutorialObserver = observeEvent(input$tabs,{
      go = T
      if(!is.null(input$tutorialEnded)){
        if(input$tutorialEnded){
          tutorial(F)
          tutorialObserver$destroy()
          go = F
        }
      }
      if(go) {
        print(input$tabs)
      }
    })
    
    introjs(
      session,
      options = list(
        "nextLabel"="Next",
        "prevLabel"="Back",
        "skipLabel"="Skip"
        ),
      events = list(
        "oncomplete"=I('Shiny.setInputValue("tutorialEnded", true);'),
        "onexit"=I('Shiny.setInputValue("tutorialEnded", true);'),
        "onbeforechange" = I("
        if (this._currentStep==3) 
          document.querySelector('#cea-tab').parentElement.childNodes[1].click();
        else if (this._currentStep==5) 
          document.querySelector('#eligiblity-tab').parentElement.childNodes[1].click();
        
        ")
        )
    )
  })
  
  
  
  
  
}


shinyApp(ui, server)