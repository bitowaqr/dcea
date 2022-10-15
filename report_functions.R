# report functions
# duplicate plotting functions for creating the report
# because highchart plots won't be displayed in word
# (without some hacks) 

rep_plot_hoc_dist <- function(dist_hoc, cols = imdCols()) {
  
  require(ggplot2)
  
  dist_hoc$imd_str = c("IMD 1","IMD 2","IMD 3", "IMD 4", "IMD 5")
  dist_hoc$cols = cols
  
  p1 = ggplot(data=dist_hoc, aes(x=imd_str, y=hoc_prop, fill = imd_str)) +
    geom_bar(stat="identity") +
    scale_fill_manual(values = dist_hoc$cols) +
    theme_minimal() +
    ylab("Proportion") +
    xlab("") +
    theme(legend.position = "none") 
  
  return(p1)
}


rep_distr_plots = function(
    plot_df = isolate(netbenefit_table_raw1()), 
    dhi_table=isolate(R$dhi_table)
    ){
  
  require(ggplot2)
  
  plot_df$imd_str = c("IMD 1\n(Most deprived)","IMD 2","IMD 3", "IMD 4", "IMD 5\n(Least deprived)")
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
  
  p_res = lapply(seq_along(table_vars), \(var_i){
    plot_df$var = t(dhi_table[var_i, 2:6 ])
    lab = names(table_vars)[var_i]
    
    p1 = ggplot(data=plot_df, aes(x=imd_str, y=var, fill = imd_str)) +
      geom_hline(yintercept = 0, col = "gray", linetype = "dotted") +
      geom_bar(stat="identity") +
      scale_fill_manual(values = plot_df$cols) +
      theme_minimal() +
      ylab(lab) +
      xlab("") +
      theme(legend.position = "none") +
      ggtitle(table_vars[var_i])
    p1
  })
  
  net_heath_plot_indices = (length(p_res)-2):length(p_res)
  net_health_plot_limits = unlist(lapply(net_heath_plot_indices,\(x){
    layer_scales(p_res[[x]])$y$range$range
  }))
  print("net_health_plot_limits")
  print(net_health_plot_limits)
  for(i in net_heath_plot_indices){
    p_res[[i]] = p_res[[i]] +  coord_cartesian(ylim =c(min(net_health_plot_limits),max(net_health_plot_limits)))
  }
  
  return(p_res)
}

rep_draw_ce_plane = function(eip,reg_line,max_yval,max_xval){
  
  require(ggplot2)
  
  p1 = ggplot() +
    geom_hline(yintercept = 0, col = "gray", size = 0.5) +
    geom_vline(xintercept = 0, col = "gray", size = 0.5) +
    geom_point(data = eip, aes(x=qalys, y=cost, col = cols)) +
    geom_line(data=reg_line, aes(x=x,y=y)) +
    scale_color_manual(
      values = eip$cols, 
      labels = c("ICER"),
      name = ""
    ) +
    ylab("Incremental costs") +
    xlab("Incremental QALYs") +
    theme_minimal() +
    theme(legend.position = "top") +
    coord_cartesian(xlim = c(-max_xval,max_xval), ylim= c(-max_yval,max_yval))
  
  return(p1)
}


rep_draw_equityimpact_plot = function(
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
){
  
  int_name = paste(int_name, scenario_name)
  
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
  
  require(ggplot2)
  
  p1 = ggplot(eip) +
    geom_hline(yintercept = 0, col = "gray", size = 0.5) +
    geom_vline(xintercept = 0, col = "gray", size = 0.5) +
    geom_point(aes(x=ede,y=net_qalys,col = c("A","B")), size = 2) +
    # geom_point(data = eip, aes(x=qalys, y=cost, col = cols)) +
    # geom_line(data=reg_line, aes(x=x,y=y)) +
    scale_color_manual(
      values = c("#cb3e72","cadetblue"), 
      labels = c(int_name,comp_name),
      name = ""
    ) +
    scale_y_continuous(
      name ="Net population health impact (QALYs)",
      labels = scales::label_comma(scale = 1/1000, suffix = "k")
      ) +
    scale_x_continuous(
      name ="Net health equity benefit (Equity-weighted QALYs – QALYs)",
      labels = scales::label_comma(scale = 1/1000, suffix = "k")
    ) +
    theme_minimal() +
    theme(legend.position = "top") +
    coord_cartesian(xlim = c(-max_xval,max_xval), ylim= c(-max_yval,max_yval))
  
  
  
    
  
  return(p1)
}

  

rep_draw_icer_equity_plot <- function(
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
  
  int_name = paste(int_name, scenario_name)
  
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
  
  
  
  
  require(ggplot2)
  
  p1 = ggplot(eip) +
    geom_hline(yintercept = eip_threshold, col = "gray", size = 0.5) +
    geom_vline(xintercept = 0, col = "gray", size = 0.5) +
    geom_point(aes(x=ede,y=icer,col = c("A","B")), size = 2) +
    # geom_point(data = eip, aes(x=qalys, y=cost, col = cols)) +
    # geom_line(data=reg_line, aes(x=x,y=y)) +
    scale_color_manual(
      values = c("#cb3e72","cadetblue"), 
      labels = c(int_name,comp_name),
      name = ""
    ) +
    scale_y_reverse(
      name ="Incremental cost-effectiveness ratio",
      labels = scales::label_comma(scale = 1/1000, suffix = "k")
    ) +
    scale_x_continuous(
      name ="Net health equity benefit (Equity-weighted QALYs – QALYs)",
      labels = scales::label_comma(scale = 1/1000, suffix = "k")
    ) +
    theme_minimal() +
    theme(legend.position = "top") +
    coord_cartesian(xlim = c(-max_xval,max_xval), ylim= c(max_yval,-max_yval))
  
  return(p1)
}