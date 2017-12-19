kobe_compare_fn <- function(rootdir,
                            pattern = NA,
                            subpattern = NA,
                            toMatch = c("SSB", "Fstd", "SPR"),
                            Fishery = "",
                            title = "")
{
  ## plotting defaults
  par(mfrow = c(1, 1), mar = c(4,4,2,1))
  # par(mfrow = c(1,1))
  # Kobe plot layout setting
  x_max = 2
  x_min = 0
  y_max = 3
  y_min = 0
  
  cols = c('deepskyblue', "black", "mediumspringgreen")
  ## iterate avail. runs
  mods <- list.dirs(rootdir, recursive = F) %>%
    .[grepl(pattern, .)]
  
  refList = data.frame(
    "MOD" = NA,
    "SPR_SPRMSY" = NA,
    "F_FMSY" = NA,
    "SPB_SSBMSY" = NA
  )
  
  plot(
    c(x_min, x_max),
    c(y_min, y_max),
    type = "n",
    ylab = "",
    xlab = "",
    xaxs="i", yaxs="i",
    bty = "n",
    main = paste(Fishery, title, sep = " "),
    cex.main = 0.75
  )
  mtext(
    side = 1,
    expression(SSB / SSB[MSY]),
    line = 2.5,
    cex = 1
  )
  mtext(
    side = 2,
    expression(F / F[MSY]),
    line = 2.5,
    cex = 1
  )
  
  polygon(c(x_min, 1, 1, x_min),
          c(1, 1, x_min, x_min),
          col = "gold",
          border = NA)
  polygon(c(1, x_max, x_max, 1),
          c(1, 1, 0, 0),
          col = "forestgreen",
          border = NA)
  polygon(c(0, 1, 1, 0),
          c(1, 1, y_max, y_max),
          col = "red",
          border = NA)
  polygon(c(1, x_max, x_max, 1),
          c(1, 1, y_max, y_max),
          col = "goldenrod",
          border = NA)
  
  for (m in 1:length(mods)) {
    ## loop into master file
   
    ## use SS_output function to extract quantities
    if (!is.na(subpattern)) { ## if subpattern provided loop once more
      subdirs <- mods[m] %>%
        list.dirs(., recursive = T) %>%
        .[grepl(subpattern, .)]
      for (s in 1:length(subdirs)) {
        mtemp <- subdirs[s] %>% 
          SS_output(.,
                    covar = F,
                    ncols = 1000)
        ## extract ref point estimates, in order of toMatch
        refPts <-
          mtemp$derived_quants[which(mtemp$derived_quants$LABEL %in% paste0(toMatch, "_MSY")), 2]
        mtemp$sprseries[nrow(mtemp$sprseries),
                        which(names(mtemp$sprseries) %in% c("SPB", "F_std", "SPR"))]
        ## extract actual ref points @ terminal year

        ## ! beta: this is manual because names in SPR series don't match derived quants
        ## SSB_MSY = SPB, Fstd_MSY = F_std, SPR_MSY = spr
        ## store the three values in a list
        # cat((s-1)*length(m)+s, "\n")
        ## fancy indexing for sublist
        idx <- (m-1)*length(subdirs) + s
        refList[idx, "MOD" ] <- sub('.*\\/', '', mods)[m]
        refList[idx, 2:4]  <-  mtemp$sprseries[nrow(mtemp$sprseries),
                                                           which(names(mtemp$sprseries) %in% c("SPB", "F_std", "SPR"))] /
                  rev(refPts)
      } ## end of subdir loop
      print(refList)
      ## add points colored by EM
      for (i in 1:length(unique(refList$MOD))) {
        subRef <- subset(refList, MOD == unique(refList$MOD)[i])
        with(
          subRef,
          points(
            SPR_SPRMSY,
            F_FMSY,
            type = "o",
            pch = 19,
            col = cols[i]
          )
        )
      } ## end points loop

    } ## end !is na subpattern  

    # else{
    #   mtemp <- mods[m] %>%
    #     list.dirs(., recursive = T) %>%
    #     .[grepl(subpattern, .)] %>%
    #     SS_output(.,
    #               covar = F,
    #               ncols = 1000)
    #   # year_vec = min(model$sprseries$Year):max(model$sprseries$Year)
    #   ## extract ref point estimates, in order of toMatch
    #   refPts <-
    #     mtemp$derived_quants[which(mtemp$derived_quants$LABEL %in% paste0(toMatch, "_MSY")), 2]
    #   mtemp$sprseries[nrow(mtemp$sprseries),
    #                   which(names(mtemp$sprseries) %in% c("SPB", "F_std", "SPR"))]
    #   ## extract actual ref points @ terminal year
    #   
    #   ## ! beta: this is manual because names in SPR series don't match derived quants
    #   ## SSB_MSY = SPB, Fstd_MSY = F_std, SPR_MSY = spr
    #   ## store the three values in a list
    #   refList[m, ] <-
    #     cbind(sub('.*\\/', '', mods)[m], mtemp$sprseries[nrow(mtemp$sprseries),
    #                                                      which(names(mtemp$sprseries) %in% c("SPB", "F_std", "SPR"))] /
    #             rev(refPts))
    # 
    # with(
    #   refList,
    #   points(
    #     SPR_SPRMSY,
    #     F_FMSY,
    #     type = "o",
    #     pch = 21,
    #     bg = "grey88",
    #     col = 'white'))
    # text(
    #   x = 0.1,
    #   y = 0.1,
    #   labels = paste0("n = ", nrow(refList))
    # )
    # } ## end just mods loop
  } ## end mods loop
  if (!is.na(subpattern)) {
  legend("topright",
         legend = unique(refList$MOD),
         pch = 19,
         col = cols)
  }
    # else{  legend("topright",
  #                 legend = NA,
  #                 pch = 19,
  #                 col = cols))}
  text(x = 0.1,
       y = 0.1,
       labels = paste0("n = ", nrow(refList)))
} ## end function

kobe_compare_fn(rootdir = "G:/SSBOOT/",
                pattern = "SM_EM",
                subpattern = "Rep",
                toMatch = c("SSB","Fstd","SPR"),
                Fishery = "Striped Marlin",
                title = " - Estimation Models with Recruitment Deviation")
