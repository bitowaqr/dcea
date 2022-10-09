
# UI utility function to create and update tooltips -----
tip = function(str = "Atkinson inequality aversion", tip = "", tipId = "x" ,sup = "<i class=\"fa fa-question-circle\"></i>", class = "tip-div" ){
  HTML(paste0("<span class=\"",class,"\" id='",tipId,"' data-bs-toggle=\"tooltip\" data-bs-placement=\"top\" title='",tip,"'>",str,"<sup class='sup-tip'>",sup,"</sup></span>"))
}

updateTip = function(id, tip){
  runjs(paste0("document.querySelector('#",id,"').setAttribute('data-bs-original-title', '",tip,"');")) 
}



# about UI modal dialog window -----
aboutUi <- function() {
  showModal(modalDialog(
    size = "l",
    fade = T,
    title = "About",
    div(
      div(
        h4("About this application"),
        HTML(
          "<p>This application was developed by James Love-Koh and Richard Cookson, Centre for
                  Health Economics, University of York, and Paul Schneider, University of Sheffield, with advisory input from Susan Griffin, Rita Faria
                    and Fan Yang. The NICE Project Leads were Lesley Owen and Monica Desai.</p>"
        ),
        br(),
      ),
      div(
        h4("Acknowledgements"),
        HTML(
          "<p>For their helpful and detailed feedback on the draft calculator we would like to
                    thank Deborah O’Callaghan, James Lomas, Mike Paulden and the many NICE officials, advisers
                    and committee members that we consulted during development.</p>"
        ),
        br(),
      ),
      
      
      
      div(
        h4("Funding statement"),
        HTML(
          "<p>Financial support for this project was provided by the National Institute for Health and Care Excellence (NICE) and the Wellcome Trust.
             All errors and opinions represented in the application are entirely those of development team and
                  do not reflect those of NICE, the Wellcome Trust or the University of York.</p>"
        ),
        br(),
      ),
      
      div(
        h4("Legal disclaimer"),
        HTML(
          "<p>The authors make no representations or warranties of any kind with respect to the
                  information, graphics and outputs available on this site.</p>"
        ),
        br()
      )
      
      
    ),
    easyClose = TRUE,
    footer = div(modalButton("Close"), class = "border rounded-3")
  ))
}


referencesUi <- function() {
  showModal(
    modalDialog(
      size = "l",
      fade = T,
      title = "Key references",
      div(
        tags$div(
          HTML(
            "<p>Below is a list of publications that detail some of the concepts
                  and methods that have been used to build this calculator.</p>"
          )
        ),
        br(),
        h4("Overview of economic evaluation and equity concepts"),
        HTML(
          "<p>Cookson, R., Griffin, S., Norheim, O.F., Culyer, A.J.,
           Chalkidou, K., 2020. Distributional Cost-Effectiveness Analysis
           Comes of Age. Value in Health, 24(1), 118-120.
                  "
        ),
        tags$a(href = "https://doi.org/10.1016/j.jval.2020.10.001", "[Link]", target =
                 "_blank"),
        HTML("</p>"),
        HTML(
          "<p>Cookson, R., Griffin, S., Norheim O.F., Culyer, A.J. (Eds), 2021.
           Distributional cost-effectiveness analysis: quantifying health equity
           impacts and trade-offs.  Oxford University Press.
                  "
        ),
        tags$a(href = "https://www.york.ac.uk/che/publications/books/handbook-dcea/", "[Link]", target =
                 "_blank"),
        HTML("</p>"),
        h4("Distributional cost-effectiveness using aggregate data"),
        HTML(
          "<p>Griffin, S., Love-Koh, J., Pennington, B., Owen, L., 2019.
                  Evaluation of Intervention Impact on Health Inequality for
                  Resource Allocation. Medical Decision Making, 39(3), 172–181.
                  "
        ),
        tags$a(href = "https://doi.org/10.1177/0272989X19829726", "[Link]", target =
                 "_blank"),
        HTML("</p>"),
        HTML(
          "<p>Love-Koh, J., Cookson, R., Gutacker, N., Patton, T., Griffin, S.,
           2019. Aggregate Distributional Cost-Effectiveness Analysis of Health
           Technologies. Value in Health, 22(5), 518–526.
                  "
        ),
        tags$a(href = "https://doi.org/10.1016/j.jval.2019.03.006", "[Link]", target =
                 "_blank"),
        HTML("</p>"),
        h4("Distribution of health opportunity costs of NHS expenditure"),
        HTML(
          "<p>Love-Koh, J., Cookson, R., Claxton, K., Griffin, S., 2020.
           Estimating Social Variation in the Health Effects of Changes in Health
           Care Expenditure. Medical Decision Making, 40(2), 170–182.
                  "
        ),
        tags$a(href = "https://doi.org/10.1177/0272989X20904360", "[Link]", target =
                 "_blank"),
        HTML("</p>"),
        h4("Baseline levels of health inequality"),
        HTML(
          "<p>Love-Koh, J., Asaria, M., Cookson, R., Griffin, S., 2015.
           The Social Distribution of Health: Estimating Quality-Adjusted Life
           Expectancy in England. Value in Health, 18(5), 655–662.
                  "
        ),
        tags$a(href = "https://doi.org/10.1016/j.jval.2015.03.1784", "[Link]", target =
                 "_blank"),
        HTML("</p>"),
        h4("The inequality staircase effects of health interventions"),
        HTML(
          "<p>Tugwell, P., de Savigny, D., Hawker, G., Robinson, V., 2006.
           Applying clinical epidemiological methods to health equity: the
           equity effectiveness loop. BMJ 332, 358–61.
                  "
        ),
        tags$a(href = "https://doi.org/10.1136/bmj.332.7537.358", "[Link]", target =
                 "_blank"),
        HTML("</p>"),
        br(),
        br()
      ),
      easyClose = TRUE,
      footer = div(modalButton("Close"), class = "border rounded-3")
    )
  )
}


tutorialUi <- function(){
  showModal(modalDialog(
    title = "Tutorial",size = "l",
    tags$iframe(width="560", height="315",src="https://www.youtube.com/embed/ScMzIvxBSi4",title="YouTube video player",frameborder="0"),
    easyClose = TRUE,
    footer = div(modalButton("Close"),class="border rounded-3")
  ))
}



loadingScreenUi = function(){
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
          HTML("James Love-Koh, Paul Schneider <br>& Richard Cookson"),
        ),
        div(
          class="d-flex flex-row justify-content-center align-items-center",
          div(
            class="cell ms-auto",
            tags$img(class ="image", src="york_logo.png"),
          ),
          div(
            class="me-auto",
            tags$img(class ="", src="wellcome.png", width = "150px"),
          ),
        ),
      ),
    )
  )
}