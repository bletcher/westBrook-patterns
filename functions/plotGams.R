
plotGamVarBySpecies <- function(var = 's(cohort)', xAxisLab = 'Cohort', bkt = modBKT, bnt = modBNT){
  require(gratia)
  
  BKT <- evaluate_smooth(modBKT, var) %>% mutate(species = 'bkt')
  if(var == 's(cohort)'){
    BKT <- BKT %>% filter(!(cohort < 2002 & riverFactor %in% c('wb jimmy','wb mitchell','wb obear')))
  } 
  BNT <- evaluate_smooth(modBNT, var) %>% mutate(species = 'bnt')
  if(var == 's(cohort)'){
    BNT <- BNT %>% filter(!(cohort < 2002 & riverFactor %in% c('wb jimmy','wb mitchell','wb obear')))
  }     
  
  both <- bind_rows(BKT,BNT) %>% mutate(upperCI = est + se * 1.96, lowerCI = est - se * 1.96)
  
  both$speciesGG <- factor(both$species, levels = c('bkt','bnt','ats'), labels = c("Brook Trout", "Brown Trout", "Atlantic Salmon"), ordered = T)
  both$riverFactor <- factor(both$riverFactor,
                                  levels=c('west brook', 'wb jimmy', 'wb mitchell',"wb obear"),
                                  labels = c("West Brook","Open Large","Open Small","Isolated Small"))  
  xVar <- substr(var,3,nchar(var)-1)
  byVar <- as.character(unique(both$by_variable))
  
  if(var == 's(cohortStrengthScaledBNT2p5)') both <- filter(both, riverFactor %in% c("West Brook","Open Large"))
  
  gg <- 
    ggplot(both, aes(x = eval(as.name(xVar)), group = speciesGG)) + 
      geom_ribbon(aes( ymin = lowerCI, ymax = upperCI), fill  = 'grey70') +
      geom_line(aes(y = est, linetype = speciesGG), size = 1.5) +
      labs(y = 'Effect size',
           x = xAxisLab,
           linetype = 'Species') +
      theme_publication() +
      theme(legend.position = "right", 
            legend.direction = "vertical",
            legend.title = element_text(face="plain")) +
      facet_wrap(~ eval(as.name(byVar)))
  gg
} 


  # 
  # # main spline over ydayCumul
  # plotBKT = plot.gam(modBKT, select = 1, scale = 0)
  # plotBNT = plot.gam(modBNT, select = 1, scale = 0)
  # 
  # plotBKTydayCumul <- data.frame(x = plotBKT[[1]]$x, y = plotBKT[[1]]$fit, se = plotBKT[[1]]$se) %>% mutate(species = 'bkt')
  # plotBNTydayCumul <- data.frame(x = plotBNT[[1]]$x, y = plotBNT[[1]]$fit, se = plotBNT[[1]]$se) %>% mutate(species = 'bnt')
  # plotBothydayCumul <- bind_rows(plotBKTydayCumul,plotBNTydayCumul) %>% mutate(upperCI = y + se * 1.96, lowerCI = y - se * 1.96)
  # 
  # plotBothydayCumul$speciesGG <- factor(plotBothydayCumul$species, levels = c('bkt','bnt','ats'), labels = c("Brook Trout", "Brown Trout", "Atlantic Salmon"), ordered = T)
  # 
  # gg <- 
  # ggplot(plotBothydayCumul, aes(x, group = speciesGG)) + 
  #   geom_ribbon(aes( ymin = lowerCI, ymax = upperCI, fill  = speciesGG), alpha = 0.5) +
  #   geom_line(aes(y = y, linetype = speciesGG), size = 1.5) +
  #   scale_fill_manual(values = c('grey30', 'grey70')) +
  #   labs(y = 'Predicted body size (mm)',
  #        x = 'Days since age-0 Jan 1',
  #        linetype = 'Species') +
  #   theme_publication() +
  #   theme(legend.position = "right", 
  #         legend.direction = "vertical",
  #         legend.title = element_text(face="plain")) 



