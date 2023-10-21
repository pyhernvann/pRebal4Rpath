run_pRebal <- function(model_out, tabinfo=NULL, dietmatrix=NULL, exclude_class="no_class",  varsel="Biomass"){
  
  pRebal_out <- c()
  
  if(varsel!="PreyPredRatio"){
  
  labels_plot <- data.frame(variable=c("B", "PB", "totP",
                                    "QB", "totQ", "PQ"),
                            units=c("log (Biomass) (t.km-2)", "P/B (y-1)", "Total production",
                   "Q/B (year-1)", "Total Consumption", "P/Q"))
  

  if(varsel=="Biomass"){
    varsel <- "B"
  }
  
  model_out %>%
    mutate(index=seq(1, n())) %>%
    mutate(PB=ifelse(!is.na(Z) & is.na(PB), Z, PB)) %>%
    rename("B"="Biomass") %>%
    mutate(logB=log10(B),
           logPB=log10(PB),
           logQB=log10(QB),
           Ptot=PB*B,
           logPtot=log10(PB*B),
           Qtot=QB*B,
           logQtot=log10(QB*B)
           ) -> model_out
  
  
  if(!is.null(tabinfo)){
    
    model_out %>%
      bind_cols(info[, c("abb", "class")]) -> model_out
    
  }
  
  
  regB <- lm(model_out$logB ~ model_out$TL)
  regPB <- lm(model_out$logPB ~ model_out$TL)
  regPtot <-lm(model_out$logPtot ~ model_out$TL)
  regQB <-lm(model_out$logQB ~ model_out$TL)
  regQtot <- lm(model_out$logQtot ~ model_out$TL)
  regPQ <- lm(model_out$ProdCons ~ model_out$TL)
  
  reg_out <- bind_rows(regB$coefficients,
                       regPB$coefficients,
                       regPtot$coefficients,
                       regQB$coefficients,
                       regQtot$coefficients,
                       regPQ$coefficients) %>%
    dplyr::rename(intercept="(Intercept)", slope="model_out$TL") %>%
    mutate(r2=c(summary(regB)$r.squared,
                summary(regPB)$r.squared,
                summary(regPtot)$r.squared,
                summary(regQB)$r.squared,
                summary(regQtot)$r.squared,
                summary(regPQ)$r.squared)) %>%
    mutate(regid=c("regB",
                   "regPB",
                   "regPtot",
                   "regQB",
                   "regQtot",
                   "regPQ"))
  
  loc <- which(colnames(model_out)==varsel)
  
  model_out %>%
    ungroup() %>%
    mutate(toreg = .[[loc]]) %>%
    group_by(index) %>%
    mutate(abb = ifelse("abb" %in% colnames(model_out), abb, index),
           class = ifelse("class" %in% colnames(model_out), class, "not_sp")) %>%
    ungroup() %>%
    mutate(to_reg_restrict=ifelse(class %in% exclude_class, NA, toreg)) -> plot_model_out
  
  
  if(!is.null(tabinfo)){
    
    regB_rest <- lm(model_out$logB[!model_out$class %in% exclude_class] ~ model_out$TL[!model_out$class %in% exclude_class])
    regPB_rest <- lm(model_out$logPB[!model_out$class %in% exclude_class] ~ model_out$TL[!model_out$class %in% exclude_class])
    regPtot_rest <-lm(model_out$logPtot[!model_out$class %in% exclude_class] ~ model_out$TL[!model_out$class %in% exclude_class])
    regQB_rest <-lm(model_out$logQB[!model_out$class %in% exclude_class] ~ model_out$TL[!model_out$class %in% exclude_class])
    regQtot_rest <- lm(model_out$logQtot[!model_out$class %in% exclude_class] ~ model_out$TL[!model_out$class %in% exclude_class])
    regPQ_rest <- lm(model_out$ProdCons[!model_out$class %in% exclude_class] ~ model_out$TL[!model_out$class %in% exclude_class])
    
    regrest_out <- bind_rows(regB_rest$coefficients,
                         regPB_rest$coefficients,
                         regPtot_rest$coefficients,
                         regQB_rest$coefficients,
                         regQtot_rest$coefficients,
                         regPQ_rest$coefficients) %>%
      dplyr::rename(intercept="(Intercept)", slope="model_out$TL[!model_out$class %in% exclude_class]") %>%
      mutate(r2=c(summary(regB_rest)$r.squared,
                  summary(regPB_rest)$r.squared,
                  summary(regPtot_rest)$r.squared,
                  summary(regQB_rest)$r.squared,
                  summary(regQtot_rest)$r.squared,
                  summary(regPQ_rest)$r.squared)) %>%
      mutate(regid=c("regB_rest",
                     "regPB_rest",
                     "regPtot_rest",
                     "regQB_rest",
                     "regQtot_rest",
                     "regPQ_rest"))
    
    reg_out %>%
      bind_rows(regrest_out) -> reg_out
    
  }
  

  if(varsel %in% c("PQ", "QB", "Qtot")){
    lowboundx <- 2
    }else{
      lowboundx <- 1
    }
  
  plot_model_out %>%
    ggplot(aes(x=TL, y=toreg))+
    geom_smooth(method=lm,  linetype="dashed",
                color="red", fill="red", alpha=0.05) +
    geom_smooth(data=plot_model_out, aes(x=TL, y=to_reg_restrict), method=lm,  linetype=4,
                color="blue", fill="blue", alpha=0.05) +
    annotate("text", x=Inf, y=Inf, label="all groups", hjust = 2, vjust = 3, color="red", size=5) +
    annotate("text", x=Inf, y=Inf , label="with groups excluded", hjust = 1, vjust = 6, color="blue", size=5) +
    geom_point()+
    scale_y_continuous(trans='log10') +
    geom_label_repel(aes(label = abb,  fill = class),
                     #fill=GroupType,
                     box.padding   = 0.15,
                     label.size = 0.4,
                     point.padding = 0.1,
                     size=4,
                     segment.color = 'grey') +
    scale_fill_manual(values = c("lightpink1", "grey50","orchid3", "burlywood2","skyblue3","palegreen4"))+
    xlab("Trophic level") + ylab(labels_plot$units[labels_plot$variable==varsel])+
    theme_bw()+
    xlim(xmin=lowboundx, xmax=5.2)+
    theme(panel.grid.major = element_line(colour = "grey"),panel.grid.minor = element_blank(),axis.title.y=element_text(size=16),axis.title.x=element_text(size=16),panel.border=element_blank(),
          axis.text.y = element_text(size=15), axis.text.x=element_text(margin=margin(0,0,0,0), size=15),axis.line=element_line(),# panel.border=element_blank(),
          legend.text = element_text(size=12,margin=margin(0,0,0,0)),legend.justification = "center", legend.key=element_rect(colour=c(rep("black",2),"white",rep("black",13))),legend.key.size = unit(0.8, 'lines')) -> plotpRebal
  
  pRebal_out$table <- reg_out
  
  
  }else{
  
    dietmatrix %>%
      filter(DIETS!="Import") %>%
      pivot_longer(2:ncol(dietmatrix)) %>%
      mutate(pred_id=rep(seq(1, (nrow(dietmatrix)-1)), (ncol(dietmatrix)-1))) %>%
      dplyr::rename("Prey"="DIETS", "Pred"="name") %>%
      left_join(model_out[,c("Group", "Biomass")], by=c("Prey"="Group")) %>%
      dplyr::rename("Bprey"="Biomass") %>%
      left_join(model_out[,c("Group", "Biomass")], by=c("Pred"="Group")) %>%
      dplyr::rename("Bpred"="Biomass") %>% 
      mutate(predprey=Bpred/Bprey) %>%
      mutate(predprey=ifelse(value==0, NA, predprey)) %>%
      mutate(bioratio=ifelse(Pred==Prey & !is.na(predprey), "Cannibalism", ifelse(predprey>=1,">1", ifelse(predprey>=0.009 & predprey < 1,"]0.01;1]",
                                             ifelse(predprey>0 & predprey<0.009, "]0;0.01]", NA))))) %>%
      dplyr::select(Prey, Pred, bioratio, pred_id) -> biomass_ratios
      
    biomass_ratios$bioratio <- factor(biomass_ratios$bioratio, levels = c("Cannibalism", ">1", "]0.01;1]","]0;0.01]"))
    biomass_ratios$Pred <- factor(biomass_ratios$Pred, levels = unique(biomass_ratios$Prey))
    biomass_ratios$Prey <- factor(biomass_ratios$Prey, levels = rev(unique(biomass_ratios$Prey)))
    
    
    ggplot(biomass_ratios, aes(Pred, Prey, fill = bioratio)) + 
      geom_raster() +
      scale_fill_manual(values=c("dodgerblue4","firebrick2","palegreen3","darkorange1","white"),"Pred/Prey B ratio", na.value = rgb(0,0,0,0)) + theme_bw() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            axis.text.y = element_text(angle = 45, hjust = 1)) -> plotpRebal
 
    
}
  
  print(plotpRebal)
  
  pRebal_out$plot <- plotpRebal
  
  return(pRebal_out)
  
}




