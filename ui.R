# Shiny UI for aggregate distributional cost-effectiveness analysis tool
# 
# James Love-Koh
# 22/10/2020


library("shiny")
library("DT")

shinyUI(
  navbarPage("Prototype Health Equity Impact Calculator",fluid=T,
             
             tabPanel("Data inputs",
                      
                      # Intervention scenario UI ------------------------------------------------
                      
                      sidebarPanel(
                        h4("How to use this application"),
                        tags$div(
                          HTML("
                          <p>This prototype calculator is independent academic-in-confidence 
                          work-in-progress by the University of York being shared for purposes of 
                          academic comment and feedback. The calculator has not been validated or 
                          peer reviewed and is not for citation. </p>
                          <p>We offer no guarantees or warranties 
                          of any kind for users and the views expressed are those of 
                          the authors and not the Medical Technologies Evaluation Programme, NICE 
                          or any other NHS body.</p>
                          
                          <p>This calculator produces quick and simple 'what if' estimates of health 
                          inequality impacts. Analysts can model how the health impacts of a policy differ 
                          across five socioeconomic groups, based on the neighbourhood-level index of multiple 
                          deprivation (IMD).  It does not not look separately at different dimensions of social inequality 
                          (e.g. individual socioeconomic status, regional deprivation, ethnicity, gender, disability).</p>
                          
                          <p>Please enter the following data inputs in the three “Data Inputs” pages to the right.
                          <br></br>
                          1. <b>Intervention:</b> The intervention, comparator, indication and health condition(s)<br></br>
                          2. <b>CEA Inputs:</b> Key findings from standard cost-effectiveness analysis<br></br>
                          3. <b>Distributional Inputs:</b> Your assumptions about the distributions of intervention
                          eligibility, uptake, effectiveness and opportunity cost </p>
                          
                          <p>The tool will then conduct a simplified form of “distributional” cost-effectiveness analysis 
                          (DCEA), by combining your data inputs with built-in data. All the graphs and tables produced 
                          can then be exported. </p>
                          
                          <p>The calculator can account for socioeconomic differences at four key steps along the “staircase 
                               of inequality” that determine the health inequality impacts of an intervention: (1) eligibility, 
                               (2) uptake, (3) effectiveness and (4) opportunity cost. However, it does not undertake detailed 
                               measurement or modelling of these four steps. If the data inputs are incorrect then the findings 
                               may be misleading. We therefore encourage you to undertake extensive “what if” sensitivity analysis 
                               using alternative assumptions.</p>
                               
                               <small><p><br>This prototype calculator is copyright University of York</p></small>")
                        )   
                      ),
                      mainPanel(
                        tabsetPanel(id="tabset",
                                    
                                    
                                    # 'Intervention' panel ----------------------------------------------------
                                    
                                    
                                    tabPanel("Intervention",br(),
                                             fluidRow(
                                               column(6,
                                                      textInput("intName1", "Intervention name","Intervention A"),
                                               ),
                                               
                                               column(6,
                                                      textInput("compName1", "Comparator name","Comparator X"),
                                               )
                                               
                                             ),
                                             textInput("indicationName", "Intervention indication","E.g. Over-65's with high blood pressure"),
                                             br(),
                                             tags$div(
                                               HTML("<small>
                                               <p>Select whether the intervention population is defined by disease (ICD) code (e.g. C50, 
                                               breast cancer) or risk factor (e.g. smokers). 
                                               Multiple ICD codes and risk factors can be included.</p></small>")
                                             ),
                                             fluidRow(
                                               column(6,
                                                      selectInput("intervention_type","Population type",c("Risk factor population","Disease population"))
                                               ),
                                               column(6,
                                                      numericInput("intPop_c1", "Eligible population",100)
                                               )),
                                             
                                             
                                             
                                             conditionalPanel(
                                               condition="input.intervention_type=='Disease population'",
                                               HTML("<small><p>A list of ICD-10 code descriptions can be found"),
                                               tags$a(href="https://icd.who.int/browse10/2010/en","here",target="_blank"),
                                               HTML("</p></small>"),
                                               uiOutput("icd_codes"),
                                               
                                               br(),
                                               
                                               sliderInput("age_range","Recipient age range",min=0, 
                                                           max=100,value=c(0, 100),step=1),
                                               
                                               tags$div(
                                                 HTML("<small>
                               <p>Select within range of 1-100</p>
                                                    </small>")
                                               )
                                             ),
                                             
                                             conditionalPanel(
                                               condition="input.intervention_type=='Risk factor population'",
                                               uiOutput("risk_factors")
                                             ),br(),
                                             
                                             HTML("<i>Once complete, proceed to the <b>'CEA inputs'</b> 
                                                  tab</i>"),br()
                                    ),
                                    
                                    # 'CEA inputs' panel ------------------------------------------------------
                                    
                                    
                                    tabPanel("CEA inputs",br(),
                                             tags$div(
                                               HTML("<small>")),
                                             numericInput("ratioHOC", "Marginal productivity (cost per QALY) of alternative resource use", 15000, min=0),
                                             
                                             tags$div(
                                               HTML("</small>")),
                                             br(),
                                             tags$div(
                                               HTML("<p><b>Cost-effectiveness analysis results</b></p>")),
                                             tags$div(
                                               HTML("
                               <small><p>It is strongly recommended that users define their own recipient population sizes. 
                               The estimates are automatically derived from the disease/risk factor population data and do
                               not relate to specific interventions.</p>
                                                    </br>")
                                             ), 
                                             fluidRow(
                                               column(6,
                                                      numericInput("incQALYs_c1", "Inc. QALYs per recipient",0.02)
                                               ),
                                               column(6,
                                                      numericInput("incCost_c1", "Inc. costs per recipient",200)
                                               ),
                                             ),
                                             HTML("</small>"),
                                             
                                             tags$div(
                                               HTML("<small><b><i>")), 
                                             textOutput("icer_text"),br(),
                                             tags$div(
                                               HTML("</i></small></b>")), 
                                             br(),
                                             HTML("<i>Once complete, proceed to the <b>'Distributional inputs'</b> 
                                                  tab</i>"),
                                    ),
                                    
                                    # 'Distributional inputs' panel -------------------------------------------
                                    
                                    
                                    tabPanel("Distributional inputs", br(),
                                             tags$div(
                                               HTML("<b>The Index of Multiple Deprivation</b><small>
                                                    <p>Socioeconomic differences are analysed using
                                                    the Index of Multiple Deprivation (IMD), which computes an 
                                                    index score for over 30,000 neighbourhoods in England based on
                                                    information on education, training, employment, housing, 
                                                    health, crime and income in the area.</p>
                                                    <p><b>IMD1</b> contains people living in the <b>most deprived</b> 
                                                    20% of neighbourhoods in England. <b>IMD5</b> contains those living 
                                                    in the <b>least deprived</b> 20%.</p></small>")),br(),
                                             tags$div(
                                               HTML("<b>Eligible population</b><small>")),
                                             textOutput("elig_pop_text"),
                                             
                                             checkboxInput("choiceRecPop","Define custom eligible population distribution",0),
                                             conditionalPanel(
                                               condition="input.choiceRecPop==1",
                                               fluidRow(
                                                 column(2,
                                                        numericInput("prevQ1", "IMD1",0.2,min=0,max=1,step=0.05)
                                                 ),
                                                 column(2,
                                                        numericInput("prevQ2", "IMD2",0.2,min=0,max=1,step=0.05)
                                                 ),
                                                 column(2,
                                                        numericInput("prevQ3", "IMD3",0.2,min=0,max=1,step=0.05)
                                                 ),
                                                 column(2,
                                                        numericInput("prevQ4", "IMD4",0.2,min=0,max=1,step=0.05)
                                                 ),
                                                 column(2,
                                                        numericInput("prevQ5", "IMD5",0.2,min=0,max=1,step=0.05)
                                                 )
                                                 
                                               )
                                             ),
                                             conditionalPanel(
                                               condition="(input.prevQ1+input.prevQ2+input.prevQ3+
                                                   input.prevQ4+input.prevQ5)!=1",
                                               tags$div(
                                                 HTML("<p><b>")),
                                               textOutput("prev_user_warning"),
                                               tags$div(
                                                 HTML("</b></p></small></br>"))
                                             ),
                                             br(),
                                             tags$div(
                                               HTML("<b>Uptake</b><small><br>
                               <p>The uptake rate (a value between 0 and 1) defines the proportion of the eligible population
                               utilising the intervention in each IMD quintile group.</br></p>")
                                             ),
                                             checkboxInput("choiceUptake2","Add alternative uptake scenario",0),
                                             tags$div(
                                               HTML("</small>")
                                             ),
                                             conditionalPanel(
                                               condition="input.choiceUptake2==1",
                                               h5("Base case uptake")),
                                             HTML("<small>"),
                                             fluidRow(
                                               column(2,
                                                      numericInput("util1Q1", "IMD1",1,min=0,step=0.05)
                                               ),
                                               column(2,
                                                      numericInput("util1Q2", "IMD2",1,min=0,step=0.05)
                                               ),
                                               column(2,
                                                      numericInput("util1Q3", "IMD3",1,min=0,step=0.05)
                                               ),
                                               column(2,
                                                      numericInput("util1Q4", "IMD4",1,min=0,step=0.05)
                                               ),
                                               column(2,
                                                      numericInput("util1Q5", "IMD5",1,min=0,step=0.05)
                                               )
                                               
                                             ),
                                             HTML("<i>"),
                                             textOutput("rec_pop_text1"),
                                             HTML("</i></small>"),br(),
                                             
                                             conditionalPanel(
                                               condition="input.choiceUptake2==1",
                                               h5("Alternative uptake scenario"),
                                               HTML("<small>"),
                                               fluidRow(
                                                 column(2,
                                                        numericInput("util2Q1", "IMD1",1,min=0,step=0.05)
                                                 ),
                                                 column(2,
                                                        numericInput("util2Q2", "IMD2",1,min=0,step=0.05)
                                                 ),
                                                 column(2,
                                                        numericInput("util2Q3", "IMD3",1,min=0,step=0.05)
                                                 ),
                                                 column(2,
                                                        numericInput("util2Q4", "IMD4",1,min=0,step=0.05)
                                                 ),
                                                 column(2,
                                                        numericInput("util2Q5", "IMD5",1,min=0,step=0.05)
                                                 )
                                                 
                                               ),
                                               HTML("<i>"),
                                               textOutput("rec_pop_text2")
                                               
                                             ),
                                             HTML("</i></small>"),
                                             br(),
                                             HTML("<b>Health effects</b><small><br>
                               <p>The per person incremental QALY effect of an intervention can vary by IMD quintile group. 
                               Ticking the box below allows you to modify a set of multipliers for each IMD quintile group
                               that are applied to the average incremental QALY effect defined in 'CEA inputs' tab (a value of 1 
                                                    yields the average effect).</br></p>"),
                                             checkboxInput("choiceQALYMod","Define custom incremental QALY multipliers",0),
                                             conditionalPanel(
                                               condition="input.choiceQALYMod==1",
                                               fluidRow(
                                                 column(2,
                                                        numericInput("qaly1Q1", "IMD1",1,min=0,max=10,step=0.05)
                                                 ),
                                                 column(2,
                                                        numericInput("qaly1Q2", "IMD2",1,min=0,max=10,step=0.05)
                                                 ),
                                                 column(2,
                                                        numericInput("qaly1Q3", "IMD3",1,min=0,max=10,step=0.05)
                                                 ),
                                                 column(2,
                                                        numericInput("qaly1Q4", "IMD4",1,min=0,max=10,step=0.05)
                                                 ),
                                                 column(2,
                                                        numericInput("qaly1Q5", "IMD5",1,min=0,max=10,step=0.05)
                                                 )
                                               ),
                                               HTML("<b><i>"),
                                               textOutput("wt_qaly_text")
                                             ),
                                             
                                             br(),
                                             tags$div(
                                               HTML("</i></small>Health opportunity costs</b><small>
                               <p>The cost impact of the intervention(s) fall on those across the health service and not 
                               just the intervention population. By default, an empirical estimate of the distribution
                                                    of opportunity costs is used. A custom distribution can be defined by ticking
                                                    the box below.</p>")
                                             ),
                                             
                                             fluidRow(
                                               column(4,
                                                      selectInput("choiceHOC", "Health opportunity gradient:",
                                                                  c("Flat" = "flat",
                                                                    "Moderate gradient" = "moderate",
                                                                    "Steep gradient" = "steep")),
                                                      tags$div(
                                                        HTML("<p><i>")),
                                                      textOutput("hoc_user_warning"),
                                                      tags$div(
                                                        HTML("</p></i>"))
                                               ),
                                               
                                               column(8,plotOutput("disthoc_plot",height="150px",width="350px")
                                               )),
                                             br(),                                        
                                             
                                             HTML("</small><i><p>Results can now be viewed on the <b>'Equity impact analysis'</b> and 
                                               <b>'Equity trade-off analysis'</b> pages in the top menu</p></i>"),br()
                                             
                                    )
                                    
                        )
                      )
             ),
             
             
             
             # Inequalities and net benefit UI -----------------------------------------
             
             tabPanel("Equity impact analysis",
                      
                      sidebarPanel(
                        h4("Equity impact analysis"),
                        
                        tags$div(
                          HTML("<p>This section analyses impacts on social health inequality in health in 
                               the general population of England.</p>
                               <p>The tables and graphs can all be exported into an HTML document by pressing the 
                               “Download Summary Report” button below.</p>
                               <small><p><br>This prototype calculator is copyright University of York</p></small>")
                        )
                        
                      ),
                      mainPanel(
                        tabsetPanel(id="tabset_ineq",
                                    tabPanel("Input summary", br(),
                                             HTML("<b>Summary of socioeconomically varied parameters</b>"),br(),
                                             br(),
                                             div(dataTableOutput("input_summary"),style="font-size:90%"),
                                             tags$div(
                                               HTML("<small><b>Note: IMD1</b> is the <b>most deprived</b> 
                                                    group and <b>IMD5</b> is the <b>least deprived</b>.</small>")),br(),br(),
                                             HTML("<b>Downloadble report</b>"),br(),
                                             HTML("<small>A summary report can be downloaded by clicking the button
                                                  in the 'Input summary' tab. The report contains key figures and tables produced 
                                                  in the 'Equity impact analysis' and 'Equity trade-off analysis'
                                                  pages.</small>"),br(),br(),
                                             downloadButton("report_download","Download summary report")
                                    ),
                                    tabPanel("Uptake distribution", br(),
                                             HTML("<b>Socioeconomic distribution of intervention recipients</b>"),br(),
                                             plotOutput("recipients_plot"),
                                             br(),
                                             conditionalPanel(
                                               condition="input.choiceUptake2==1",
                                               HTML("<b>Base case uptake</b>")),
                                             div(dataTableOutput("recipients_table1"),style="font-size:90%"),
                                             conditionalPanel(
                                               condition="input.choiceUptake2==1",
                                               HTML("<b>Alternative uptake</b>"),
                                               div(dataTableOutput("recipients_table2"),style="font-size:90%")
                                             )
                                    ),
                                    tabPanel("Net health benefit distribution", br(),
                                             HTML("<b>Distribution of intervention health effects (population totals)</b>"),
                                             br(),br(),
                                             conditionalPanel(
                                               condition="input.choiceUptake2==1",
                                               tags$div(
                                                 HTML("<small>")),
                                               selectInput("netBen_uptake_choice","Uptake scenario",
                                                           list("Base case" = "base","Alternative" = "alt")), 
                                               tags$div(
                                                 HTML("</small>")),
                                             ),br(),
                                             plotOutput("netbenefit_plot"),br(),
                                             div(dataTableOutput("netbenefit_table"), style = "font-size:90%"),
                                             br()
                                             
                                    ),
                                    tabPanel("Equity impact summary measures", br(),
                                             HTML("<p><b>Inequality in net health benefits – do worse-off people gain more (-ve), less (+ve) or 
                                                  the same?</b></p>
                                                  <small><p>The SII value below represents the modelled difference in net QALY benefit between the most and least deprived IMD group at population level.
                                                  The measure differs from the observed gap by incorporating information on the net QALY benefits of IMD2-IMD4 using 
                                                   a simple linear regression model.</p></small>"),
                                             div(dataTableOutput("inequality_table_nb"), style = "font-size:90%"),br(),br(),
                                             HTML("<b><p>Net health equity benefit – is health inequality reduced (+ve), increased (-ve), or unchanged?</b></p>
                                                  <small><p>The SII value below represents how the intervention has changed the modelled difference in 
                                                  expected individual lifetime health between the most and least deprived IMD group. It uses a similar linear model to
                                                  the one described above, but is reported at individual level on a much smaller scale than the population level slope index for net benefits.</p></small>"),
                                             div(dataTableOutput("inequality_table"), style = "font-size:90%"),br(),
                                             HTML("<p><b>Interpreting the inequality measures</b></p><small>
                                             <p>The nature of the health inequality impact sometimes depends on the nature of the 
                                             health inequality concern.  For example, providing equal net health benefits to all 
                                             groups will keep absolute differences in health constant but might reduce relative 
                                             differences.  We therefore present an absolute measure of inequality (the slope index) 
                                             as well as two relative measures (the relative and concentration indices).  
                                             We do not present relative measures for inequality in net health benefit, however, 
                                             since net health benefit can take negative and zero values and relative inequality 
                                             is ill-defined and badly behaved in such cases – one cannot compare proportions 
                                             of zero.
                                             </p><br>
                                                  <p><b><i>
                                                  Slope index of inequality (SII)</b></i>: The modelled gap between the most 
                                                  and least deprived individuals calculated from a simple linear regression 
                                                  line. This is similar to the observed gap between most and least deprived 
                                                  groups, but also takes into account outcomes for the middle groups. 
                                                  A negative SII indicates the most deprived have worse health outcomes 
                                                  than the least deprived. A reduction in the SII indicates absolute health 
                                                  inequality has been reduced.
                                                  <br></p>
                                                  <p><b><i>
                                                  Relative index of inequality (RII)</b></i>: The proportional gap between 
                                                  the most and least deprived individuals, derived from the SII. An RII 
                                                  of -1 means the estimated health outcomes for the most deprived are 100% 
                                                  greater than those for the least deprived. A reduction in the RII indicates 
                                                  relative health inequality has been reduced.
                                                  <br></p>
                                                  <p><b><i>Concentration index</b></i>: Another index of 
                                                  relative inequality, similar to the Gini index of income 
                                                  inequality and based on the correlation between deprivation 
                                                  rank and relative 'shares' of population total health.
                                                  Equals -1 or 1 when only the most or least deprived individual 
                                                  experiences any health at all, and 0 if health is equally 
                                                  distributed. A reduction in a positive concentration index 
                                                  (or an increase for a negative index) indicates health inequality 
                                                  has been reduced.
                                                  <br></p>
                                                  </small>")
                                    )
                        )
                        
                      )
             ),
             ##### SWF UI ####
             tabPanel("Equity trade-off analysis",
                      
                      sidebarPanel(
                        h4("Equity-efficiency trade-off analysis"),
                        tags$div(
                          HTML("<p>This section analyses potential trade-offs between equity (reducing health inequality) and efficiency 
             (improving total population health).</p>
             <p>The “Equity-Efficiency Impact Plane” tab presents the health equity impact and total population health impact 
             in a single graph, so you can spot any potential trade-offs.</p>
             <p>The health equity impact is measured as the “equity-weighted” population health impact minus the
             unweighted population health impact.  You can explore how setting different “equity weights” alters the magnitude of 
             the health equity impact by altering the Atkinson health inequality aversion parameter value.  An Atkinson parameter 
             value of zero gives no weight to equity and hence by definition the equity impact is zero.  Positive parameter values 
             give more weight to the worse-off and increase the magnitude of the equity impact.  The direction of the equity impact 
             may be positive (if the intervention reduces inequality) or negative (if the intervention increases inequality).  
             In the (rare) case that inequality is unchanged the inequality impact will remain zero for all values of the inequality 
             aversion parameter.</p>
             <p>The “Atkinson Equity Weighting” tab analyses the trade-off between equity and efficiency, if there is one.  It shows the
             “equity weighted” population health impact for different values of the Atkinson inequality aversion parameter. This parameter gives 
             more weight to worse off groups depending on the degree of concern for relative inequality in lifetime health.  A positive
             equity-weighted impact means that the intervention is better than the comparator, after taking account of both efficiency 
             and equity.  A negative equity-weighted impact means that the comparator is better.  If there is no equity-efficiency 
             trade-off then the equity-weighted impact will be consistently positive or consistently negative for all values of the 
             inequality aversion parameter.  If there is a trade-off, then the equity weighted impact will hit zero at the trade-off 
             point – i.e. the level of health inequality aversion at which the intervention is considered only just worthwhile.  
             The trade-off point is a “switching point”: for values of health inequality aversion above this level the intervention 
             is considered better than the comparator, and for values below this level the intervention is considered worse.</p>
             
                  
                  <small><p><br>This prototype calculator is copyright University of York</p></small>")
                        )     
                        
                      ),
                      mainPanel(
                        tabsetPanel(id="tabset_swf",
                                    tabPanel("Equity-efficiency impact plane", br(), br(),
                                             plotOutput("equityimpact_plot"), 
                                             HTML("<small><p><b>"),
                                             textOutput("raw_icer_text"),
                                             HTML("</b></p></small>"),
                                             br(),
                                             sliderInput("eip_aversion","Atkinson inequality aversion",min=0, 
                                                         max=20,value=1,step=0.5),
                                             tags$div(
                                               HTML("<small><p><b>")),
                                             textOutput("implicit_weight_text"),
                                             HTML("</b></p><p><b>Note: </b>Altering the inequality aversion parameter 
                                                relates to the inequality impact and does not affect the 
                                                population health impact.</small>"), br()
                                    ),
                                    tabPanel("Equity-efficiency impact plane (ICER)", br(),
                                             tags$div(
                                               HTML("<small><p><b>")),
                                             textOutput("noEIP_text"),
                                             HTML("</b></p></small>"),
                                             plotOutput("icer_equityimpact_plot"), br(),
                                             fluidRow(
                                               column(6,
                                                      sliderInput("eip_icer_aversion","Atkinson inequality aversion",min=0, 
                                                                  max=20,value=1,step=0.5),
                                               ),
                                               column(6,
                                                      numericInput("eip_threshold", "Decision threshold",20000,min=0,max=500000,
                                                                   step=1000)
                                               )),
                                             tags$div(
                                               HTML("<small><p><b>")),
                                             textOutput("implicit_weight_text2"),
                                             HTML("</p></b><p><b>Note: </b>Altering the inequality aversion parameter 
                                                relates to the inequality impact and does not affect the 
                                                incremental cost-effectiveness ratio.</small>"), br()
                                    ),
                                    tabPanel("Equity-weighted QALYs", br(),
                                             plotOutput("atkinson_plot"), 
                                             br(),
                                             div(dataTableOutput("atkinson_table"), style = "font-size:90%"),
                                             br()
                                    )
                                    
                        )
                        
                      )
             ),
             
             
             # About -------------------------------------------------------------------
             
             tabPanel("About",
                      sidebarPanel(
                        h4("Funding statement"),
                        tags$div(
                          HTML("Financial support for this project was provided by the National Institute for Health and Care Excellence (NICE). 
             All errors and opinions represented in the application are entirely those of development team and
                  do not reflect those of NICE or the University of York.")),br(),br(),
                        h4("Legal disclaimer"),
                        tags$div(
                          HTML("The authors make no representations or warranties of any kind with respect to the
                  information, graphics and outputs available on this site."))
                      ),
                      mainPanel(
                        h4("About this application"),
                        tags$div(
                          HTML("<p>This application was developed by James Love-Koh and Richard Cookson, Centre for
                  Health Economics, University of York, with advisory input from Susan Griffin, Rita Faria 
                    and Fan Yang. The NICE Project Leads were Lesley Owen and Monica Desai.</p>")),br(),
                        h4("Acknowledgements"),
                        tags$div(
                          HTML("<p>For their helpful and detailed feedback on the draft calculator we would like to 
                    thank Deborah O’Callaghan, James Lomas, Mike Paulden and the many NICE officials, advisers 
                    and committee members that we consulted during development.</p>"))
                      )
             ),
             tabPanel("References",
                      mainPanel(
                        h3("Key references"),
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
                      )
             )
  )
)