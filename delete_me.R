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