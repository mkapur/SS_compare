require(dplyr)
require(r4ss)
require(ggplot2)
require(RColorBrewer)
require(Rmisc)
## loop into directory and get needed quantities
rootdir <- "G:/MAKO/sensitivityplots"

## re run models using RawSPR ---
  ## extract base case and alternate models
  mods <- list.dirs(rootdir, recursive = T, full.names = T) %>% 
    .[grepl("Late_Index", .)] %>%   list.dirs(., recursive = F) %>%
    .[grepl(paste('Base','Alternative',sep = "|"), .)]
  for(m in 1:length(mods)){
    Starter = SS_readstarter( file=paste0(mods[m],"/starter.ss"), verbose = F)
    Starter[['SPR_basis']] <- 4 ## do NOT read from par - esimate instead
    SS_writestarter( mylist=Starter, dir = mods[m], overwrite=TRUE, verbose = F, warn = F)
    
    setwd(mods[m])
    shell( "ss3.exe")
    message(paste0(mods[m]," executed with rawSPR"))
  } ## end models