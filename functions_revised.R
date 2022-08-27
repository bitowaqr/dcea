#setwd("~/Google Drive/GitHub/adcea_prototype")
#rm(list=ls())

library(tidyverse)
library(scales)
library(ggrepel)
library(gridExtra)
library(highcharter)

# Load data ---------------------------------------------------------------

# Risk factor distributions
distRF <- read.csv("data/rf_imd.csv",h=T)
# Age and ICD distributions
distICD <- read.csv("data/icd_imd.csv",h=T)
# icd 10 labels
icd10_labs = read.csv("data/icd10_labs.csv")
icd10_input_labs = icd10_labs$code
names(icd10_input_labs) = paste0(icd10_labs$code, ". ", icd10_labs$label)
# Baseline QALE
distQALE <- read.csv("data/qale_pop_imd.csv",h=T)
# Mapping of single year age to age groups in ICD data
icd_imd_agematch <- read.csv("data/icd_imd_agematch.csv",h=T)
# Health opportunity costs
distHOC_scenarios <- read.csv("data/hoc_distribution.csv",h=T)
# Atkinson implicit weights
imp_AtWeights_full <- read.csv("data/impweights_AtFull.csv",h=T)
# Kolm implicit weights
imp_KmWeights <- read.csv("data/impweights_km.csv",h=T)


# Create separate dataframe for implicit weights IMD1/IMD5 only
imp_AtWeights <- imp_AtWeights_full %>% filter(imd==1) %>%
  select(-imd)

# SHINY functions ---------------------------------------------------------
imdCols = function(){
  viridis_pal(begin = 0.3, end = 0.8,option = "magma")(5)
}

tip = function(str = "Atkinson inequality aversion", tip = "", tipId = "x" ,sup = "<i class=\"fa fa-question-circle\"></i>", class = "tip-div" ){
  HTML(paste0("<span class=\"",class,"\" id='",tipId,"' data-bs-toggle=\"tooltip\" data-bs-placement=\"top\" title='",tip,"'>",str,"<sup class='sup-tip'>",sup,"</sup></span>"))
}
updateTip = function(id, tip){
  runjs(paste0("document.querySelector('#",id,"').setAttribute('data-bs-original-title', '",tip,"');")) 
}



# RF pop calculator
risk_factor_pop <- function(risk_factors,comparators=1){
  recipientPop <- subset(distRF, risk_factor %in% risk_factors)
  recipientPop <- recipientPop %>% 
    left_join(distQALE,by="imd") %>%
    select(-qale) %>% rename(pop_gen=pop) %>%
    mutate(recipients_full=prop*pop_gen)
  rf_pop <- round(sum(recipientPop$recipients_full)/comparators,0)
  rf_pop <- if(rf_pop!=0) { rf_pop } else { (sum(distQALE$pop)) }
  
  return(rf_pop)
}

# ICD pop calculator
icd_pop <- function(icd_codes,ages,comparators=1){
  age_bands <- unique(subset(icd_imd_agematch$hes_age,icd_imd_agematch$age>=ages[1] & 
                               icd_imd_agematch$age<=ages[2]))
  recipientDist <- subset(distICD, age %in% age_bands & icd %in% icd_codes)
  n_epi <- round(sum(recipientDist$episodes)/comparators,0) # Total n episodes
  n_epi <- if(n_epi!=0) { n_epi } else { (sum(distQALE$pop)) }
  return(n_epi)
}

# Indicator for CE plane NE quadrant (raw)
indicator_icer <- function(cea_input) {
  qalys_pp <- sum(cea_input$qalys*(cea_input$pop/sum(cea_input$pop)))
  cost_pp <- sum(cea_input$cost*(cea_input$pop/sum(cea_input$pop)))
  quadrant_ne <- ifelse(qalys_pp>=0 & cost_pp>=0,1,0)
  return(quadrant_ne)
}

# ICER calculation (raw)
icer_calc_raw <- function(cea_input) {
  qalys_pp <- sum(cea_input$qalys*(cea_input$pop/sum(cea_input$pop)))
  cost_pp <- sum(cea_input$cost*(cea_input$pop/sum(cea_input$pop)))
  icer <- cost_pp/qalys_pp
  return(icer)
}

# ICER calculation (formatted)
icer_calc <- function(cea_input) {
  qalys_pp <- sum(cea_input$qalys*(cea_input$pop/sum(cea_input$pop)))
  cost_pp <- sum(cea_input$cost*(cea_input$pop/sum(cea_input$pop)))
  icer <- if(cost_pp<0 & qalys_pp>=0) {
    "dominant"
  } else if(cost_pp>=0 & qalys_pp<0) {
    "dominated"
  } else {
    formatC(cost_pp/qalys_pp,digits = 0, big.mark=",", format = "f")
  }
  return(icer)
}

# Weighted inc QALYs post-multiplier (for UI statement)
wt_qaly_calc <- function(cea_input,qaly_modifiers,util_raw) {
  npop <- sum(cea_input$pop)
  qalys_pp <- sum(cea_input$qalys*(cea_input$pop/npop))
  qaly_df <- qaly_modifiers %>% left_join(util_raw,by="imd") %>% 
    mutate(inc_qalys=qaly_mod*qalys_pp*recipients_util)
  qalys_pp_wt <- round(sum(qaly_df$inc_qalys)/sum(qaly_df$recipients_util),4)
  return(qalys_pp_wt)
}

# Inc QALY distribution
qaly_dist <- function(cea_input,qaly_modifiers) {
  qalys_pp <- sum(cea_input$qalys*(cea_input$pop/sum(cea_input$pop)))
  inc_qaly_dist <- qaly_modifiers %>% mutate(inc_qalys=qaly_mod*qalys_pp) %>%
  select(imd,inc_qalys)
  return(inc_qaly_dist)
}

# HOC distribution plot (Dist Inputs preview)
plot_hoc_dist <- function(dist_hoc, cols = imdCols()) {
  # print(dist_hoc)
  # dist_hoc$imd <- recode(dist_hoc$imd,`1`="IMD1",`2`="IMD2",`3`="IMD3",`4`="IMD4",`5`="IMD5")
  
  
  
  dist_hoc$imd_str = c("IMD 1","IMD 2","IMD 3", "IMD 4", "IMD 5")
  dist_hoc$cols = cols
  
  hoc_plot <- highchart() %>%
    hc_add_series(
      data = dist_hoc, "column",
      pointPadding = 0, groupPadding= 0.1,borderRadius= 5,
      hcaes(
        name = imd_str,
        x = imd_str,
        y = "hoc_prop",
        color = cols
      ),
      showInLegend = F,
      name = "QALYs"
    ) %>%
    hc_title(
      text = "Proportion",
      style = list(fontWeight=600,fontSize="12px",color ="white")
    ) %>%
    hc_chart(
      style = list(
        fontFamily = "Inter"
      )
    ) %>%
    hc_tooltip(
    ) %>%
    hc_xAxis(
      categories = dist_hoc$imd_str,
      labels = list(
        style = list(
          color="white"
        ) 
      )
    )  %>%
    hc_yAxis(
      labels = list(
        style = list(
          color="white"
        ) 
      )
    ) %>%
    hc_boost(enabled = FALSE) %>%
    hc_exporting(enabled = FALSE)
      
  
  return(hoc_plot)
}



# Prev distribution table
table_prevalence_raw <- function(type,icd_codes,risk_factors,ages,pop) {
  if(type=="Disease population") {
    age_bands <- unique(subset(icd_imd_agematch$hes_age,icd_imd_agematch$age>=ages[1] & 
                                 icd_imd_agematch$age<=ages[2]))
    prevDist <- subset(distICD, age %in% age_bands & icd %in% icd_codes)
    n_epi <- sum(prevDist$episodes) # Total n episodes
    n_rec <- sum(pop$pop) # Total n recipient population across comparators
    
    prevDist <- prevDist %>% group_by(imd) %>% 
      summarise(prop_prev=sum(episodes)/n_epi) %>% ungroup() 
  } else { 
    prevDist <- subset(distRF, risk_factor %in% risk_factors)
    prevDist <- prevDist %>% 
      left_join(distQALE,by="imd") %>% 
      select(-qale) %>% rename(pop_gen=pop) %>%
      mutate(pop_prev=prop*pop_gen) %>% group_by(imd) %>% 
      summarise(pop_prev=sum(pop_prev),.groups='keep') %>% ungroup %>% 
      mutate(prop_prev=pop_prev/sum(pop_prev)) %>% select(-pop_prev)   
  }
  return(prevDist)
}

# Raw utilisation table
table_recipients_raw <- function(prev_raw,uptake,pop) {
  n_rec <- sum(pop$pop) # Total n recipient population across comparators
  recipientDist <- prev_raw %>% left_join(uptake,by="imd") %>% 
    mutate(recipients_util=prop_prev*n_rec*util_rate,
           prop_util=recipients_util/sum(recipients_util)) %>%
    select(-util_rate,-prop_prev) 
  recipientDist <- recipientDist[,c(1,3,2)]
  recipientDist[,3] <- round(recipientDist[,3],0)
  return(recipientDist)
}

recipient_pop <- function(util_raw) {
  recip_pop <- sum(util_raw$recipients_util)
}

# Utilisation table
table_recipients <- function(util_raw) {
  util_tab <- t(as.matrix(util_raw[,2:3]))
  util_tab <- as.data.frame(cbind(util_tab,rowSums(util_tab)))
  util_tab[1,1:5] <- round(util_tab[1,1:5],3)
  util_tab[2,1:6] <- round(util_tab[2,1:6],0)
  rownames(util_tab) <- c()
  colnames(util_tab) <- c("IMD1","IMD2","IMD3","IMD4","IMD5","Total")
  util_tab$Scenario <- c("Proportion of recipients (share)",
                         "Number of recipients")
  util_tab <- util_tab[,c(7,1,2,3,4,5,6)]
  return(util_tab)
}

# Utilisation / RF graph
plot_recipients <- function(util_raw1,util_raw2,uptake_scenario,int_name) {
  util_raw1$Uptake <- "Base case scenario"
  util_raw2$Uptake <- "Alternative scenario"
  util_raw <- if(uptake_scenario==1) {
    rbind(util_raw1,util_raw2)
  } else {
    util_raw1
  }
  util_raw$imd <- as.factor(util_raw$imd)
  util_raw$imd <- recode(util_raw$imd,`1`="IMD1",`2`="IMD2",`3`="IMD3",`4`="IMD4",`5`="IMD5")
  util_plot <- util_raw %>% select(imd,recipients_util,Uptake)
  util_plot$Uptake <- factor(util_plot$Uptake,levels=c("Base case scenario","Alternative scenario"))
  
  if(uptake_scenario==1) {
    util_plot <- ggplot(util_plot, aes(x=imd,y=recipients_util,fill=Uptake)) + 
      geom_bar(aes(group=Uptake),position="dodge", stat="identity") + 
      theme_classic() + scale_fill_brewer() +
      ylab("Recipients") + xlab("Index of multiple deprivation quintile group") + 
      theme(legend.position="bottom",text=element_text(size=14)) + scale_y_continuous(labels=comma) 
  } else {
    util_plot <- ggplot(util_plot, aes(x=imd,y=recipients_util,fill=Uptake)) + 
      geom_bar(aes(group=Uptake),position="dodge", stat="identity") + 
      theme_classic() + scale_fill_brewer() +
      ylab("Recipients") + xlab("Index of multiple deprivation quintile group") + 
      theme(legend.position="none",text=element_text(size=14)) + scale_y_continuous(labels=comma)
  }
  return(util_plot)
}
# Input summary table function
table_inputSummary <- function(dist_prev,dist_util,dist_qaly,dist_hoc) {
  inputSummary <- dist_prev %>%
    left_join(dist_util,by="imd") %>% left_join(dist_qaly,by="imd") %>% left_join(dist_hoc,by="imd") 
  
  inputSummary <- t(as.matrix(inputSummary[,2:5]))
  inputSummary <- as.data.frame(inputSummary)
  inputSummary <- round(inputSummary,3)

  rownames(inputSummary) <- c()
  colnames(inputSummary) <- c("IMD1","IMD2","IMD3","IMD4","IMD5")
  inputSummary$Input <- c("Share of the eligible population","Uptake rate (base case scenario)",
                          "Average incremental QALYs per person","Share of health opportunity costs")
  inputSummary <- inputSummary[,c("Input","IMD1","IMD2","IMD3","IMD4","IMD5")]
  return(inputSummary)
}
# Net benefit function
table_netbenefit_raw <- function(util_raw,hoc_distribution,hoc_ratio,qaly_mltplr,cea_input) {
  pop_full <- sum(cea_input$pop); pop_util <- sum(util_raw$recipients_util)
  qalys_pp <- sum(cea_input$qalys*(cea_input$pop/pop_full))
  cost_pp <- sum(cea_input$cost*(cea_input$pop/pop_full))
  
  dist_ben <- util_raw %>% select(imd,recipients_util) %>% left_join(qaly_mltplr,by="imd") %>% 
    mutate(qalys=recipients_util*qalys_pp*qaly_mod) %>% 
    select(imd,qalys)
  dist_hoc <- hoc_distribution %>% 
    mutate(hoc=(hoc_prop*cost_pp*pop_util)/hoc_ratio) %>% 
    select(-hoc_prop)
  dist_netben <- dist_ben %>% left_join(dist_hoc,by=c("imd")) %>%
    mutate(net_qalys=qalys-hoc)
  return(dist_netben)
}

# Net benefit graph
plot_netbenefit <- function(netbenefit_raw) {
  plot_netben_table <- netbenefit_raw %>% mutate(hoc=hoc*-1) %>% 
    rename(`Health benefit`=qalys,`Health loss`=hoc,`Net benefit`=net_qalys) %>% 
    gather("Outcome","QALYs",2:4)
  plot_netben_table$imd <- as.factor(plot_netben_table$imd)
  plot_netben_table$imd <- recode(plot_netben_table$imd,`1`="IMD1",`2`="IMD2",`3`="IMD3",`4`="IMD4",`5`="IMD5")
  
  plot_netben <- ggplot(plot_netben_table, aes(x=imd,y=QALYs)) + 
    geom_bar(position="identity",stat="identity",width=0.7) + 
    theme_classic() + scale_fill_brewer() +
    ylab("Quality-adjusted life years") + 
    xlab("Index of multiple deprivation quintile group") + 
    theme(legend.position="bottom",legend.title=element_blank(),
          text=element_text(size=14),
          panel.border=element_rect(colour="black",fill=NA)) + 
    scale_y_continuous(labels=comma) + facet_wrap(~Outcome) 
  
  return(plot_netben)
}

# Net benefit table
table_netbenefit <- function(netbenefit_raw) {
  netben_matrix <- t(as.matrix(netbenefit_raw[,2:4]))
  netben_table <- as.data.frame(cbind(netben_matrix,rowSums(netben_matrix)))
  netben_table <- round(netben_table,0)
  rownames(netben_table) <- c()
  colnames(netben_table) <- c("IMD1","IMD2","IMD3","IMD4","IMD5","Total")
  netben_table$Outcome <- c("Health benefit","Health opp cost","Net benefit")
  netben_table <- netben_table[,c(7,1,2,3,4,5,6)]
  return(netben_table)
}

# Equity-weighted ICER table
table_weightedicer_raw <- function(netbenefit_raw1,netbenefit_raw2,
                                   hoc_ratio,imp_weightsFull) {
  netbenefit_raw1 <- netbenefit_raw1 %>% rename(qaly1=qalys,hoc1=hoc) %>% 
    mutate(cost1=hoc1*hoc_ratio) %>% 
    select(-net_qalys,-hoc1) 
  netbenefit_raw2 <- netbenefit_raw2 %>% rename(qaly2=qalys,hoc2=hoc) %>% 
    mutate(cost2=hoc2*hoc_ratio) %>% 
    select(-net_qalys,-hoc2)
  
  weightedICER <- imp_weightsFull %>% left_join(netbenefit_raw1,by="imd") %>%
    left_join(netbenefit_raw2,by="imd") %>%
    mutate(qaly1=qaly1*imp_weight,cost1=cost1*imp_weight,qaly2=qaly2*imp_weight,
           cost2=cost2*imp_weight) %>% group_by(ia) %>%
    summarise(qaly1=sum(qaly1),cost1=sum(cost1),qaly2=sum(qaly2),cost2=sum(cost2)) %>%
    ungroup() %>% 
    mutate(icer1=ifelse(cost1<=0 & qaly1>0,"Dominant", 
                        ifelse(cost1>0 & qaly1<=0,"Dominated",
                               round(cost1/qaly1,0))),
           icer2=ifelse(cost2<=0 & qaly2>0,"Dominant", 
                        ifelse(cost2>0 & qaly2<=0,"Dominated",
                               round(cost2/qaly2,0)))) %>%
    select(ia,icer1,icer2)
  return(weightedICER)
}


# Health distribution function
table_healthdistribution_raw <- function(netbenefit_raw) {
  healthdist <- netbenefit_raw %>% left_join(distQALE,by="imd") %>%
    mutate(qale_post=qale+(net_qalys/pop),diff=qale_post-qale) %>%
    arrange(qale) %>% 
    mutate(frac.pop=pop/sum(pop),cf.pop=cumsum(frac.pop),
           fr=lag(cf.pop)+(frac.pop/2),
           frac.rank=(ifelse(imd==1,frac.pop/2,fr))) %>% select(-fr) %>% ungroup
  return(healthdist)
}

# Health distribution plot
plot_healthdistribution <- function(healthdist_raw) {
  healthdist_plot <- healthdist_raw %>% 
    select(imd,qale,qale_post) %>% rename(Baseline=qale,`Post-intervention`=qale_post) %>%
    gather("Scenario","QALE",2:3)
  
  healthdist_plot <- ggplot(healthdist_plot, aes(x=imd,y=QALE,fill=Scenario)) + 
    geom_bar(aes(group=Scenario),position="dodge", stat="identity") + 
    theme_classic() + scale_fill_brewer() + 
    ylab("Quality-adjusted life expectancy") + xlab("Index of multiple deprivation quintile") + 
    theme(legend.position="bottom",text=element_text(size=14)) + 
    scale_y_continuous(labels=comma) 
  return(healthdist_plot)
}

# Inequality metrics function
table_inequality_raw <- function(healthdist_raw,cea_input,hoc_ratio) {
  netben_mean <- cea_input %>% 
    summarise((crossprod(qalys,pop)-crossprod(cost,pop)/hoc_ratio)/sum(pop))
  netben_mean <- pull(netben_mean)

  
  
  
  # print(1)
  # x <<- healthdist_raw
  # x$sii_base = cov(x$qale,x$frac.rank)/var(x$frac.rank)
  # print("sii_base")
  # print(x$sii_base)
  # x$rii_base = x$sii_base/c(crossprod(x$qale, x$frac.pop))
  # print(2)
  ineq_measure <- healthdist_raw %>%
    mutate(pop.cum=cumsum(frac.pop),qalys.cum=cumsum(qale*pop),
           qalys_post.cum=cumsum(qale_post*pop),
           sii_base=cov(qale,frac.rank)/var(frac.rank),
           sii_post=cov(qale_post,frac.rank)/var(frac.rank),
           rii_base=sii_base/c(crossprod(qale,frac.pop)),
           rii_post=sii_post/c(crossprod(qale_post,frac.pop)),
           gini_base=(2/crossprod(qale,frac.pop))*cov(qale,frac.rank),
           gini_post=(2/crossprod(qale_post,frac.pop))*cov(qale_post,frac.rank)) %>% 
    summarise(sii_base=mean(sii_base),sii_post=mean(sii_post),
              rii_base=mean(rii_base),rii_post=mean(rii_post),gini_base=mean(gini_base),
              gini_post=mean(gini_post),net_qalys=sum(net_qalys)) %>%
    mutate(sii_diff=sii_base-sii_post,rii_diff=rii_base-rii_post,
           gini_diff=gini_base-gini_post) %>% ungroup
  ineq_measure$net_qalys_pp <- netben_mean[1,1]
  
  # print("sii_base")
  # print(dim(ineq_measure$sii_base))
  # print("crossprod(qale,frac.pop)")
  # print(crossprod(ineq_measure$qale,ineq_measure$frac.pop))
  # print("frac.pop")
  # print(ineq_measure$frac.pop)
  # print("qale")
  # print(ineq_measure$qale)

  return(ineq_measure)
}

# QALE distribution inequality

table_inequality <- function(inequality_raw) {
  
  ineq_sii <- inequality_raw %>% select(sii_base,sii_diff) %>%
    rename(base=sii_base,diff=sii_diff)
  ineq_sii$Measure <- "Reduction in Slope Index of Absolute Inequality in Lifetime Health"
  
  ineq_rii <- inequality_raw %>% select(rii_base,rii_diff) %>%
    rename(base=rii_base,diff=rii_diff)
  ineq_rii$Measure <- "Reduction in Relative Index (= Slope Index / Mean Lifetime Health)"
  
  ineq_gini <- inequality_raw %>% select(gini_base,gini_diff) %>%
    rename(base=gini_base,diff=gini_diff)
  ineq_gini$Measure <- "Reduction in Concentration Index of Relative Inequality in Lifetime Health"
  
  ineq_metrics <- ineq_sii %>% bind_rows(ineq_rii,ineq_gini) %>% rename(Value=diff)
  
  ineq_metrics <- ineq_metrics[,c(3,2)]
  
  return(ineq_metrics)
}

# QALE distribution inequality combined scenarios
table_inequality_comb <- function(inequality_tab1,inequality_tab2,uptake_scenario) {
  
  inequality_tab <- inequality_tab1
  
  if(uptake_scenario==1) {
    inequality_tab$`Alternative uptake` <- inequality_tab2$Value
    inequality_tab <- rename(inequality_tab,`Base case uptake`=Value)
    inequality_tab[,2:3] <- round(inequality_tab[,2:3],7)
  } else {
    inequality_tab[,2] <- round(inequality_tab[,2],7)
  }
  return(inequality_tab)
}
  

# Net benefit distribution inequality
table_nb_inequality <- function(netbenefit_raw,util_raw) {
  util_pop <- util_raw %>% select(imd,recipients_util) %>%
    rename(util_pop=recipients_util)
  ineq_table <- netbenefit_raw %>% left_join(util_pop,by="imd") %>%
    mutate(frac.pop=util_pop/sum(util_pop),cf.pop=cumsum(frac.pop),fr=lag(cf.pop)+(frac.pop/2),
           frac.rank=ifelse(imd==1,frac.pop/2,fr))
  sii=cov(ineq_table$net_qalys,ineq_table$frac.rank)/var(ineq_table$frac.rank)
  sii_intercept=mean(ineq_table$net_qalys)-(sii*mean(ineq_table$frac.rank))
  rii=as.vector(sii/crossprod(ineq_table$net_qalys,ineq_table$frac.pop))
  gini=as.vector((2/crossprod(ineq_table$net_qalys,ineq_table$frac.pop))*cov(ineq_table$net_qalys,ineq_table$frac.rank))
  
  ineq_nb <- data.frame(Measure=c("sii","sii_intercept"),Value=c(round(-sii,0),round(sii_intercept,3)))
  # Old DF w/ RII + CI
  #ineq_nb <- data.frame(Measure=c("Slope index of inequality","Relative index of inequality",
  #                                "Concentration index"),Value=c(round(sii,0),round(rii,3),round(gini,3)))
  return(ineq_nb)
}

table_nb_inequality_comb <- function(inequality_nb_tab1,inequality_nb_tab2,uptake_scenario) {
  
  table1 <- inequality_nb_tab1 %>% filter(Measure=="sii") %>% 
    mutate(Measure="Slope Index (of Inequality in Net Health Benefits)")
  table2 <- inequality_nb_tab2 %>% filter(Measure=="sii") %>% 
    mutate(Measure="Slope Index (of Inequality in Net Health Benefits)")  
  
  if(uptake_scenario==1) {
    table1$`Alternative uptake` <- table2$Value
    table1 <- rename(table1,`Base case uptake`=Value)
    table1[,2:3] <- round(table1[,2:3],5)
  } else {
    table1[,2] <- round(table1[,2],5)
  }
  return(table1)
}

# Net ben plot with SII line drawn

plot_nb_inequality <- function(inequality_nb_tab1,inequality_nb_tab2,netbenefit_raw1,
                               netbenefit_raw2,uptake_scenario) {
  sii1 <- inequality_nb_tab1[1,2]/5
  sii2 <- inequality_nb_tab2[1,2]/5
  sii_intercept1 <- inequality_nb_tab1[2,2]
  sii_intercept2 <- inequality_nb_tab2[2,2]
  
  nb1 <- mutate(netbenefit_raw1,Uptake="Base case")
  nb2 <- mutate(netbenefit_raw2,Uptake="Alternative")
  
  
  plot_netben <- ggplot(nb1, aes(x=imd,y=net_qalys)) + 
    geom_bar(stat="identity",width=0.7) + 
    theme_classic() + scale_fill_brewer() + 
    ylab("Net health benefit (QALYs)") + xlab("Index of multiple deprivation quintile") + 
    theme(legend.position="bottom",legend.title=element_blank(),text=element_text(size=14)) + 
    scale_y_continuous(labels=comma) +
    geom_abline(slope=sii1,intercept=sii_intercept1)
}

# Atkinson and Kolm EDE functions

atkinson <- function(h,p,ia) {
  if(ia==1){
    utility <- (h^(p/sum(p)))
  } else {
    utility <- (h^(1-ia))*p
  }
  mu_h <- mean(h)
  util_mu <- sum(utility)/sum(p)
  ede_atkinson <- if(ia==1) {
    prod(utility)
  } else {
    ede_a <- util_mu^(1/(1-ia))
  }
  return(ede_atkinson)
}

kolm <- function(h,p,ia) {
  util <- exp(-1*ia*h)*p
  mu_h <- mean(h)
  util_mu <- sum(util)/sum(p)
  ede_k <- -1*(1/ia)*log(util_mu)
  return(ede_k)
}

# Implicit weight function 
implicit_weight_raw <- function(eip_aversion) {
  weight <- filter(imp_AtWeights,ia==eip_aversion)
  weight <- round((weight[1,2]-1)*100,0)
  return(weight)
}

# Atkinson EDE table

table_atkinson_raw <- function(healthdist_raw1,healthdist_raw2) {
  ede_table <- data.frame(ia=numeric(),
                          base=numeric(),ede1=numeric(),ede2=numeric())
  
  table_raw1 <- healthdist_raw1
  table_raw2 <- healthdist_raw2
  
  netqaly_tot1 <- sum(table_raw1$net_qalys)
  netqaly_tot2 <- sum(table_raw2$net_qalys)
  
  pop_tot <- sum(table_raw1$pop)
  aversion_range <- c(seq(0,20,0.5))
  
  for(i in 1:length(aversion_range)) {
    ede_base <- atkinson(table_raw1$qale,table_raw1$pop,aversion_range[i])
    ede_post1 <- atkinson(table_raw1$qale_post,table_raw1$pop,aversion_range[i])
    ede_post2 <- atkinson(table_raw2$qale_post,table_raw2$pop,aversion_range[i])
    
    ede_table_row <- data.frame(ia=aversion_range[i],
                                base=ede_base,ede1=ede_post1,ede2=ede_post2)
    ede_table <- rbind(ede_table,ede_table_row)
  }
  
  ede_table <- ede_table %>% 
    mutate(ede_change1=(ede1-base)*pop_tot,ede_change2=(ede2-base)*pop_tot,
           ineq_ede1=ede_change1-netqaly_tot1,ineq_ede2=ede_change2-netqaly_tot2)
  ede_table[1,c("ineq_ede1","ineq_ede2")] <- 0
  return(ede_table)
}

table_atkinson <- function(atkinson_raw,imp_weights,weightedicer_raw,uptake_scenario) {
  ede_table <- atkinson_raw %>% left_join(imp_weights,by="ia") %>% 
    left_join(weightedicer_raw,by="ia")
  if(uptake_scenario==0) { 
    ede_table <- ede_table %>% 
      select(ia,imp_weight,icer1,ede_change1,ineq_ede1) %>% 
      mutate(imp_weight=round(imp_weight,2),
             ede_change1=round(ede_change1,0),
             ineq_ede1=round(ineq_ede1,0)) %>%
      rename(`Equity-weighted QALYs`=ede_change1,
             `Equity impact`=ineq_ede1,
             `Inequality aversion`=ia,
             `Implicit weight`=imp_weight,
             `Equity-weighted ICER`=icer1)
        } else {
    ede_table <- ede_table %>% 
      select(ia,imp_weight,icer1,ede_change1,ede_change2,icer2,ineq_ede1,ineq_ede2) %>%
      mutate(imp_weight=round(imp_weight,2),
             ede_change1=round(ede_change1,0),ede_change2=round(ede_change2,0),
             ineq_ede1=round(ineq_ede1,0),ineq_ede2=round(ineq_ede2,0)) %>%
      rename(`Equity-weighted QALYs (base case uptake)`=ede_change1,
             `Equity-weighted QALYs (alternative uptake)`=ede_change2,
             `Equity impact (base case uptake)`=ineq_ede1,
             `Equity impact (alternative uptake)`=ineq_ede2,
             `Inequality aversion`=ia,
             `Implicit weight`=imp_weight,
             `Equity-weighted ICER (base case uptake)`=icer1,
             `Equity-weighted ICER (alternative uptake)`=icer2)
  }
  return(ede_table)
}


# Atkinson plot

plot_atkinson <- function(atkinson_raw,uptake_scenario) {
  ede_table <- atkinson_raw %>% select(ia,ede_change1,ede_change2) %>% 
    gather(Uptake,"ede",2:3)
  
  ede_table$Uptake <- recode(ede_table$Uptake,"ede_change1"="Base case",
                                        "ede_change2"="Alternative") 
  
  if(uptake_scenario==0) { 
    ede_table <- filter(ede_table,Uptake=="Base case")
    
    ede_plot <- ggplot(ede_table,aes(x=ia,y=ede,group=Uptake)) +
      geom_line(aes(color=Uptake),size=1.2) +
      ylab("Total equity-weighted QALY gain") + xlab("Inequality aversion") + 
      scale_y_continuous(labels=comma) +
      scale_x_continuous(labels=comma,limits=c(0,max(ede_table$ia))) +
      theme_classic() +
      theme(legend.position="none",text=element_text(size=14))
  } else {
    ede_plot <- ggplot(ede_table,aes(x=ia,y=ede,group=Uptake)) +
      geom_line(aes(color=Uptake),size=1.2) +
      ylab("Total equity-weighted QALY gain") + xlab("Inequality aversion") + 
      scale_y_continuous(labels=comma) +
      scale_x_continuous(labels=comma,limits=c(0,max(ede_table$ia))) +
      theme_classic() +
      theme(legend.position="bottom",text=element_text(size=14))
  }
  return(ede_plot)
}

# Equity impact plane 
plot_equity_impact <- function(
    inequality_raw1,
    inequality_raw2,
    atkinson_raw,
    eip_aversion,
    uptake_choice = F,
    int_name,
    comp_name,
    old_atkinsons = NULL,
    scenario_name = NULL,
    internal_counter = runif(1),
    show_old = T
    ) {
  # uptake_choice is ignored 
  
  int_name = paste0(int_name, "<br>", scenario_name)
  
  # nhb <- inequality_raw1 %>% bind_rows(inequality_raw2) %>% select(net_qalys) 
  nhb <- inequality_raw1 %>%  select(net_qalys) 
  
  atkinson_save = atkinson_raw %>% 
    select(ia,ineq_ede1) %>% 
    gather("Uptake","ede",2) %>%
    bind_cols(nhb)
  atkinson_save$Uptake <- recode(atkinson_save$Uptake,"ineq_ede1"=int_name) 
  atkinson_save$internal_counter = internal_counter
  
  if(!is.null(old_atkinsons) & show_old){
    old_atkinsons = old_atkinsons[!(old_atkinsons$internal_counter %in% atkinson_save$internal_counter),]
    atkinson_save = rbind(atkinson_save, old_atkinsons)
  }
  
  eip <- atkinson_save %>% 
    filter(ia==eip_aversion)
  
  eip_origin <<- data.frame(ia=eip_aversion,Uptake=comp_name,ede=0,net_qalys=0,internal_counter=NA)
  
  eip <- rbind(eip,eip_origin)
  
    eip$ede <- round(eip$ede)
    eip$net_qalys <- round(eip$net_qalys)
    max_yval <- max(abs(eip$net_qalys))*1.5
    max_xval <- max(abs(eip$ede))*1.5
    
    p1 = highchart() %>%
      hc_add_series(
        data = eip, "scatter",
        pointPadding = 0, groupPadding= 0.1,
        color="var(--primary)",
        marker = list(enabledThreshold=0,radius=8),
        hcaes(
          x=ede,
          y=net_qalys,
          name = Uptake,
          # lala = eip$Uptake
          # color = Uptake
        ),
        showInLegend = F,
        name = ""
      ) %>%
      hc_yAxis(
        max = max_yval, min= -max_yval,
        title  = list(
          text = "Net population health impact (QALYs)",
          style = list(fontWeight=600, fontSize=16)
        ),
        plotLines = list(
          list(
            value= 0,
            width= 2,
            color = "black",
            zIndex=1
          )
        )
        ) %>% 
      hc_xAxis(
        title  = list(
          text = "Net health equity benefit (Equity-weighted QALYs – QALYs)",
          style = list(fontWeight=600, fontSize=16)# font-weight: 600; font-size: 16px"
        ),
         max=max_xval,
         min=-max_xval,
        gridLineWidth = 1,
        plotLines = list(
          list(
            value= 0,
            width= 2,
            color = "black",
            zIndex=1
          )
        )
      ) %>%
      hc_chart(
        style = list(
          fontFamily = "Inter"
        )
      ) %>%
      hc_tooltip(
        headerFormat="",
          pointFormat = '<b>{point.name}</b><br>QALYs: {point.x}<br>Net impact: {point.y} QALYs'
      ) %>%
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
    
  return(list(plot=p1, data = atkinson_save))
}

plot_icer_equity_impact <- function(
    baseICER,
    atkinson_raw,
    eip_aversion,
    uptake_choice = F,
    int_name,
    comp_name,
    eip_threshold,
    old_atkinsons = NULL,
    scenario_name = NULL,
    internal_counter = runif(1),
    show_old = T
    ) {
  # uptake_choice is ignored

  int_name = paste0(int_name, "<br>", scenario_name)
  
  atkinson_save <- atkinson_raw %>% 
    select(ia,ineq_ede1) %>% 
    gather("Uptake","ede",2) %>%
    mutate(icer=baseICER)
  atkinson_save$Uptake <- recode(atkinson_save$Uptake,"ineq_ede1"=int_name) 
  atkinson_save$internal_counter = internal_counter
  
  if(!is.null(old_atkinsons) & show_old){
    old_atkinsons = old_atkinsons[!(old_atkinsons$internal_counter %in% atkinson_save$internal_counter),]
    atkinson_save = rbind(atkinson_save, old_atkinsons)
  }
  
  eip <-  atkinson_save %>% 
    filter(ia==eip_aversion) 
    
  eip_origin <- data.frame(ia=eip_aversion,Uptake=comp_name,ede=0,icer=eip_threshold,internal_counter=NA)
  
  eip <- rbind(eip,eip_origin)
  
  eip$ede = round(eip$ede)
  eip$icer = round(eip$icer)
  max_yval <- max(abs(eip$icer))+20000
  max_xval <- max(abs(eip$ede))*1.5
  
  p1 = highchart() %>%
    hc_add_series(
      data = eip, "scatter",
      color="var(--primary)",
      pointPadding = 0, groupPadding= 0.1,
      marker = list(enabledThreshold=0,radius=8),
      hcaes(
        x=ede,
        y=icer,
        name = Uptake,
      ),
      showInLegend = F,
      name = ""
    ) %>%
    hc_yAxis(
      max = max_yval, min= -max_yval,
      title  = list(
        text = "Incremental cost-effectiveness ratio",
        style = list(fontWeight=600, fontSize=16)
      ),
      plotLines = list(
        list(
          value= eip_threshold,
          width= 2,
          color = "black",
          zIndex=1
        )
      )
    ) %>% 
    hc_xAxis(
      title  = list(
        text = "Net health equity benefit (Equity-weighted QALYs – QALYs)",
        style = list(fontWeight=600, fontSize=16)# font-weight: 600; font-size: 16px"
      ),
      max=max_xval,
      min=-max_xval,
      gridLineWidth = 1,
      plotLines = list(
        list(
          value= 0,
          width= 2,
          color = "black",
          zIndex=1
        )
      )
    ) %>%
    hc_chart(
      style = list(
        fontFamily = "Inter"
      )
    ) %>%
    hc_tooltip(
      headerFormat="",
      pointFormat = '<b>{point.name}</b><br>Net health equity impact: {point.x}<br>ICER: {point.y}'
    ) %>%
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
  
  return(list(plot=p1, data = atkinson_save))
  
  
    
    # eip <- ggplot(eip,aes(x=ede,y=icer,group=Uptake)) +
    #   geom_point(aes(fill=Uptake),size=3,shape=21,colour="black") +
    #   geom_vline(xintercept=0,colour="black") +
    #   geom_hline(yintercept=eip_threshold,colour="black") +
    #   ylab("Incremental cost-effectiveness ratio") + 
    #   xlab("Net health equity benefit (Equity-weighted QALYs – QALYs)") + 
    #   scale_y_reverse(labels=dollar_format(prefix="£"),
    #                      limits=c(max_yval,0),
    #                      breaks=seq(-500000,500000,10000)) +
    #   scale_x_continuous(labels=comma,limits=c(-1*max_xval,max_xval)) +
    #   theme_classic() + 
    #   scale_fill_brewer() + scale_colour_brewer() +  
    #   theme(legend.position="none",axis.line=element_blank(),text=element_text(size=14)) +
    #   geom_label_repel(size=4.5,aes(label=Uptake,fill=Uptake),colour='black',
    #                    box.padding=unit(0.5,"lines"),
    #                    point.padding=unit(0.5,"lines"),show.legend=FALSE)
  
  
  # return(eip)
}



# Kolm EDE table

table_kolm_raw <- function(healthdist_raw1,healthdist_raw2) {
  ede_table <- data.frame(ia=numeric(),
                          base=numeric(),ede1=numeric(),ede2=numeric())
  
  table_raw1 <- healthdist_raw1
  table_raw2 <- healthdist_raw2
  
  netqaly_tot1 <- sum(table_raw1$net_qalys)
  netqaly_tot2 <- sum(table_raw2$net_qalys)
  
  pop_tot <- sum(table_raw1$pop)
  aversion_range <- seq(0.005,0.25,0.005)
  
  for(i in 1:length(aversion_range)) {
    ede_base <- kolm(table_raw1$qale,table_raw1$pop,aversion_range[i])
    ede_post1 <- kolm(table_raw1$qale_post,table_raw1$pop,aversion_range[i])
    ede_post2 <- kolm(table_raw2$qale_post,table_raw2$pop,aversion_range[i])
    
    ede_table_row <- data.frame(ia=aversion_range[i],
                                base=ede_base,ede1=ede_post1,ede2=ede_post2)
    ede_table <- rbind(ede_table,ede_table_row)
  }
  
  ede_table <- ede_table %>% 
    mutate(ede_change1=(ede1-base)*pop_tot,ede_change2=(ede2-base)*pop_tot,
           ineq_ede1=ede_change1-netqaly_tot1,ineq_ede2=ede_change2-netqaly_tot2)
  return(ede_table)
}

table_kolm <- function(kolm_raw,imp_weights,uptake_scenario) {
  ede_table <- kolm_raw 
  if(uptake_scenario==0) { 
    ede_table <- ede_table %>% mutate(imp_weight=imp_weights$imp_weight) %>%
      select(ia,imp_weight,ede_change1,ineq_ede1) %>% 
      mutate(imp_weight=round(imp_weight,2),
             ede_change1=round(ede_change1,0),
             ineq_ede1=round(ineq_ede1,0)) %>%
      rename(`Equity-weighted QALYs`=ede_change1,
             `Equity impact`=ineq_ede1,
             `Inequality aversion`=ia,
             `Implied weight`=imp_weight)
  } else {
    ede_table <- ede_table %>% mutate(imp_weight=imp_weights$imp_weight) %>% 
      select(ia,imp_weight,ede_change1,ede_change2,ineq_ede1,ineq_ede2) %>%
      mutate(imp_weight=round(imp_weight,2),
             ede_change1=round(ede_change1,0),ede_change2=round(ede_change2,0),
             ineq_ede1=round(ineq_ede1,0),ineq_ede2=round(ineq_ede2,0)) %>%
      rename(`Equity-weighted QALYs (base case uptake)`=ede_change1,
             `Equity-weighted QALYs (alternative uptake)`=ede_change2,
             `Equity impact (base case uptake)`=ineq_ede1,
             `Equity impact (alternative uptake)`=ineq_ede2,
             `Inequality aversion`=ia,
             `Implied weight`=imp_weight)
  }
  return(ede_table)
}


# Kolm plot

plot_kolm <- function(kolm_raw,uptake_scenario) {
  ede_table <- kolm_raw %>% select(ia,ede_change1,ede_change2) %>% 
    gather(Uptake,"ede",2:3)
  
  ede_table$Uptake <- recode(ede_table$Uptake,"ede_change1"="Base case",
                             "ede_change2"="Alternative") 
  
  if(uptake_scenario==0) { 
    ede_table <- filter(ede_table,Uptake=="Base case")
    
    ede_plot <- ggplot(ede_table,aes(x=ia,y=ede,group=Uptake)) +
      geom_line(aes(color=Uptake),size=1.2) +
      ylab("Equity-weighted QALYs (population total)") + xlab("Inequality aversion") + 
      scale_y_continuous(labels=comma) +
      scale_x_continuous(labels=comma,limits=c(0,NA)) +
      theme_classic() +
      theme(legend.position="none",text=element_text(size=14))
  } else {
    ede_plot <- ggplot(ede_table,aes(x=ia,y=ede,group=Uptake)) +
      geom_line(aes(color=Uptake),size=1.2) +
      ylab("Equity-weighted QALYs (population total)") + xlab("Inequality aversion") + 
      scale_y_continuous(labels=comma) +
      scale_x_continuous(labels=comma,limits=c(0,NA)) +
      theme_classic() +
      theme(legend.position="bottom",text=element_text(size=14))
  }
  return(ede_plot)
}






# draw simple CE plane
drawCePlane = function(eip,reg_line,max_yval,max_xval){

p1 = highchart() %>%
  hc_add_series(
    data = eip, "scatter",
    pointPadding = 0, groupPadding= 0.1,
    # color="var(--primary)",
    marker = list(enabledThreshold=0,radius=8),
    hcaes(
      x=qalys,
      y=cost,
      name = name,
      color = cols
      # lala = eip$Uptake
      # color = Uptake
    ),
    showInLegend = F,
    name = ""
  ) %>%
  hc_add_series(
    data = reg_line, "line",
    pointPadding = 0, groupPadding= 0.1,
    color="black",
    hcaes(
      x=x,
      y=y,
      # lala = eip$Uptake
      # color = Uptake
    ),
    showInLegend = F,
    name = ""
  ) %>%
  hc_yAxis(
    max = max_yval, min= -max_yval,
    title  = list(
      text = "Incremental costs",
      style = list(fontWeight=600, fontSize=16)
    ),
    plotLines = list(
      list(
        value= 0,
        width= 2,
        color = "black",
        zIndex=1
      )
    )
  ) %>% 
  hc_xAxis(
    title  = list(
      text = "Incremental QALYs",
      style = list(fontWeight=600, fontSize=16)# font-weight: 600; font-size: 16px"
    ),
    max=max_xval,
    min=-max_xval,
    gridLineWidth = 1,
    plotLines = list(
      list(
        value= 0,
        width= 2,
        color = "black",
        zIndex=1
      )
    )
  ) %>%
  hc_chart(
    style = list(
      fontFamily = "Inter"
    )
  ) %>%
  hc_tooltip(
    headerFormat="",
    pointFormat = '<b>{point.name}</b><br>QALYs: {point.x}<br>Cost: {point.y}'
  ) %>%
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
return(p1)
}