## make a summary csv and figures for with SPB/6 and shaded SD and , 1-SPR with shaded sd
## for 19 sensitivity runs, chunked by format
## maia kapur maia.kapur@noaa.gov spring 2018

require(dplyr)
require(r4ss)
require(ggplot2)
require(RColorBrewer)
require(Rmisc)
require(gridBase)

## loop into directory and get needed quantities
rootdir <- "G:/MAKO/sensitivityplots"
categories <-  c('Biology','Late_Index','Catch')

## OPTIONAL re run models using RawSPR ---# for(c in 1:length(categories)){ ## iterate over chunks
#   message(paste0("running mods in ",categories[c]))
#   ## extract base case and alternate models
#   mods <- list.dirs(rootdir, recursive = T, full.names = T) %>%
#     .[grepl(categories[c], .)] %>%   list.dirs(., recursive = F) %>%
#     .[grepl(paste('Base','Alternative',sep = "|"), .)]
#   for(m in 1:length(mods)){
#     Starter = SS_readstarter( file=paste0(mods[m],"/starter.ss"), verbose = F)
#     Starter[['SPR_basis']] <- 4
#     SS_writestarter( mylist=Starter, dir = mods[m], overwrite=TRUE, verbose = F, warn = F)
#
#     setwd(mods[m])
#     shell( "ss3.exe")
#     message(paste0(mods[m]," executed with rawSPR"))
#   } ## end models
# } ## end chunks



## Extract Data ----
df <- data.frame()
for(c in 1:length(categories)){ ## iterate over chunks


  ## extract base case and alternate models
  mods <- list.dirs(rootdir, recursive = T, full.names = T) %>%
    .[grepl(categories[c], .)] %>%   
    list.dirs(., recursive = F) %>%
    .[grepl(paste('Base','Alternative',sep = "|"), .)]
  
  for(m in 1:length(mods)){
    mtemp <- mods[m] %>%
      SS_output(.,
                covar = F,
                forecast = F,
                ncols = 1000)

    df0 <-
    bind_cols(
      mtemp$derived_quants[grep("SSB_",mtemp$derived_quants$Label ),] %>%
      mutate('YEAR' = substr(Label,5,8),
             SPB = Value/6,
             SPB_SD = StdDev/6,
             SPB_LCI = ifelse(SPB - SPB_SD*1.96 < 0 ,0, SPB - SPB_SD*1.96 ),
             SPB_UCI =  SPB + SPB_SD*1.96 ) %>%
      select(YEAR, SPB, SPB_LCI, SPB_UCI) %>%
      filter(YEAR %in% 1975:2016) ,


      mtemp$derived_quants[grep("SPRratio_",mtemp$derived_quants$Label ),] %>%
        mutate('YEAR' = substr(Label,10,15),
               ONE_SPR = Value,
               ONE_SPRCI = 1.96*StdDev,
               SPR_LCI = ifelse(ONE_SPR - ONE_SPRCI < 0 ,0, ONE_SPR - ONE_SPRCI ),
               SPR_UCI =  ONE_SPR + ONE_SPRCI) %>%
        filter(YEAR %in% 1975:2016) %>%
      select( ONE_SPR, SPR_LCI, SPR_UCI) )   %>%



      # data.frame(mtemp$sprseries) %>%
      # select(SPR, SPR_report) %>%
      # mutate(ONE_SPR = 1 - SPR,
      #        ONE_SPRCI = 1.96*SPR_report,
      #        SPR_LCI = ifelse(ONE_SPR - ONE_SPRCI  < 0 ,0, ONE_SPR - ONE_SPRCI  ),
      #        SPR_UCI = ifelse(ONE_SPR + ONE_SPRCI  < 0 ,0, ONE_SPR + ONE_SPRCI  ))  %>%

      mutate('MOD' = basename(mods[m]),
             'CATEGORY' = categories[c])

    df <- rbind(df,df0) ## bind model-specific to master
  } ## end mods

## Plotting ----
  
  ## make master list -- needed for density plots
  modlist <- SSgetoutput(dirvec = mods, forecast = F, ncols = 1000) %>% SSsummarize(.)
  
  # df <- read.csv(paste0(rootdir,"/all_sensitivities.csv"))
  dftemp <- subset(df, CATEGORY == categories[c])

  ## time series of SPB with shaded CIs
  sa <- ggplot(dftemp, aes(x = as.numeric(YEAR), y = SPB,  group = MOD, fill = MOD)) +
    theme_minimal() +
    theme(panel.grid = element_blank(), legend.position = 'none', 
          panel.border = element_rect(colour = "black", fill=NA, size=1)) +
    labs(fill = "", col = "", y = "Stock Abundance (1000s of sharks)", x = 'YEAR') +
    scale_color_manual(values = c(rainbow(n = length(unique(dftemp$MOD))-1),'black')) +
    scale_fill_manual(values = c(rainbow(n = length(unique(dftemp$MOD))-1),'black')) +
    scale_x_continuous(limits = c(1975,2016), breaks = c(seq(1980,2011,10),2016)) +
    geom_ribbon(aes(ymin = SPB_LCI, ymax = SPB_UCI, fill = MOD), alpha = 0.2) +
    geom_line(aes(col = MOD), lwd = 0.9)


  ## time series of 1-spr with shaded CIEs
  spr <- ggplot(dftemp, aes(x = as.numeric(YEAR), y = ONE_SPR,  group = MOD, fill = MOD)) +
    theme_minimal() +
    theme(panel.grid = element_blank(), legend.position = 'none', panel.border = element_rect(colour = "black", fill=NA, size=1)) +
    labs(fill = "", col = "", y = "1 - SPR", x = 'YEAR') +
    scale_color_manual(values = c(rainbow(n = length(unique(dftemp$MOD))-1),'black')) +
    scale_fill_manual(values = c(rainbow(n = length(unique(dftemp$MOD))-1),'black')) +
    # ylim(0,5) +
    scale_x_continuous(limits = c(1975,2016), breaks = c(seq(1980,2011,10),2016)) +
    geom_ribbon(aes(ymin = SPR_LCI, ymax =SPR_UCI), alpha = 0.2) +
    geom_line(aes(col = MOD), lwd = 0.9)

  tiff(file = paste0(rootdir,"/",categories[c],'.tiff'),
       height = 8,
       width = 10,
       units = 'in',
       res = 800)
  par(mfrow = c(2,2))
  SSplotComparisons(modlist, subplot = c(14), legend = T, plot = T, new = F, pch = NA,
                    densitynames = c("SR_LN(R0)"), 
                    col =  c(rainbow(n = length(mods)-1),'black'),
                    shadealpha=0.2, legendlabels = c(basename(mods)) )
  SSplotComparisons(modlist, plot = T, new = F, subplot = 14, legend = F,
                    densitynames = c("SSB_Virgin"),
                    col =  c(rainbow(n = length(mods)-1),'black'),
                    shadealpha=0.2,
                    pch = NA,
                    densitytails = F,
                    legendlabels = c(basename(mods)) )

  
  ## custom plotting stuff to combine base & ggplots
  ## https://stackoverflow.com/questions/14124373/combine-base-and-ggplot-graphics-in-r-figure-window?utm_medium=organic&utm_source=google_rich_qa&utm_campaign=google_rich_qa

  vp1.BottomLeft <-
    grid::viewport(
      height = unit(1, "npc"),
      width = unit(1, "npc"),
      just = c("right", "top"),
      y = 1,
      x = 1
    )
  vp1.BottomRight <-
    grid::viewport(
      height = unit(1, "npc"),
      width = unit(1, "npc"),
      just = c("left", "top"),
      y = 1,
      x = 1
    )

  plot.new()
  vps <- baseViewports()
  grid::pushViewport(vps$figure)

  print(sa,vp = vp1.BottomLeft) 
  print(spr,vp = vp1.BottomRight) 
  graphics.off()

  
} ## end categories



## write CSV
write.csv(df, paste0(rootdir,"/all_sensitivities.csv"),row.names = F)
