## Function to generate and save plots from SSComparePlots and compute by-fleet likelihoods.
## M Kapur maia.kapur@noaa.gov
## Dec 2017 


ss_compare <- function(rootdir, 
                       pattern = "00_", 
                       llabels = c('W 0.1','W 0.2','W 0.3','W 0.4','W 0.5','W 0.6','W 0.7','W 0.8','W 0.9','W 1','Survey05'),
                       likeCSV = T,
                       likeLabel = c('Surv_like','SizeFreq_like:_2'),
                       lambdaLabel = c('Surv_lambda','SizeFreq_lambda:_2'),
                       fishery = "Deep7",
                       fleetIdx = c(2,3)){
  
dir.create("plots") ## make storage for plots

## make summary object
mods = list.dirs(rootdir) %>%
  .[grepl(pattern, .)] 

summaryoutput = mods %>%
  SSgetoutput(dirvec = .,
              getcovar = F,
              ncols = 1000) %>%
  SSsummarize()

## dump plots to file
SSplotComparisons(summaryoutput, plot=T, pdf=F, png=F, print = T,plotdir = "plots", 
                  legendloc='bottomright', legendlabels=llabels)

## dump special fleets to file
for (ii in 1:length(fleetIdx)) {
  SSplotComparisons(summaryoutput, plot=F, pdf=F, png=T, plotdir="plots", 
                    legendloc='bottomright', legendlabels=llabels, indexfleets=fleetIdx[ii])
}

## Not working - doxx says "needs to be generalized"
# SSplotSummaryF(summaryoutput,plot=T,print = T,plotdir="plots")


message("rendered plots")
if(likeCSV) {
  dir.create("By-Fleet Likelihoods")
  data.frame(
    'model' = sub('.*/', '', mods),
    'totlike' = as.numeric(summaryoutput$likelihoods[summaryoutput$likelihoods$Label == 'TOTAL', 1:length(mods)]),
    summaryoutput$likelihoods_by_fleet %>%
      filter(Label == lambdaLabel[2]) %>%
      select(fishery) %>%
      head(length(mods)),
    summaryoutput$likelihoods_by_fleet %>%
      filter(Label == likeLabel[2]) %>%
      select(fishery) %>%
      head(length(mods))
  ) %>%
    plyr::rename(c(
      "Deep7" = paste0(fishery, lambdaLabel[2]),
      "Deep7.1" = paste0(fishery, likeLabel[2])
    )) %>%
    write.csv(
      .,
      file = paste(
        'By-Fleet Likelihoods/LBF',
        format(Sys.time(), "%Y%m%d_%H%M.csv"),
        sep = '_'
      ),
      row.names = F
    )
}
  message("created by-fleet likelihood CSV")
graphics.off()
}





