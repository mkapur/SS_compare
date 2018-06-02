---
title: "SS Compare"
author: "Maia Kapur, Felipe Carvalho"
date: "Dec 2017"
output: html_notebook
---

## Script to execute comparison plotting of sensitivty runs for SS3
![Four-Panel Sensitivity Comparison](https://raw.githubusercontent.com/mkapur/SS_compare/master/Biology.png)

The above uses code code from `sensitivity-plots.R`. The information below refers to some wrapper functions used with r4ss to generate comparitive sensitivity plots for estimation models with many replicates. It is somewhat outdated; please see kaputils https://github.com/mkapur/kaputils.

```{r "setup"}
require(knitr)
rootdir = "your_dir"
opts_knit$set(root.dir = rootdir)
```


```{r, eval = F}
library(r4ss)
library(colorRamps)
library(dplyr)
library(ggplot2)

setwd(rootdir)
source("ss_arrange_fn.R")
source("ss_compare_fn.R")
source("kobe_compare_fn.R")
```
Run function `ss_compare()` to generate all plots, each of which will include every sensitivity run in the directory. "pattern" should be an identifying label that matches the folder names
for all sensitivity runs you'd like to examine. "llabels" are the lables that will appear on all plot legends.
If `likeCSV = T`, a .csv file will be generated with per-fleet likelihoods according to  `likeLabel` and `lambdaLabel`, which should be available in `summaryoutput$likelihoods$Label`. 

```{r}
ss_compare(rootdir, 
           pattern = "Rep", 
           llabels = paste("Rep", 1:10),
           likeCSV = T,
           likeLabel = c('Surv_like','SizeFreq_like:_2'),
           lambdaLabel = c('Surv_lambda','SizeFreq_lambda:_2'),
           fishery = "Striped Marlin")
```
Run function `ss_arrange()` to create a compiled PDF with specified plots of interest. The plots are called via default names assigned from `SSplotComparisons()`
```{r}
ss_arrange(rootdir, 
              toMatch = c("spawnbio","Bratio","recruits","recdevs","indices_log"), 
              Fishery = 'Striped Marlin')
```

Run function `kobe_compare_fn` to create a Kobe plot with terminal year reference point estimates for a variety of sensitivity runs. There are currently two approaches; if you leave `subpattern` as NA, the function will look into all directories in `rootdir` that match `pattern` and, assuming that each directory has a single `Report.sso` file, will extract the related reference points and add them all to the plot. The legend naming will match whatever each folder is called. The second approach is designed for estimation models with internal replicates (or bootstraps). This is enacted by specificying a `subpattern`, which will match the directory names of the replicates within each estimation model/sensitivity run. For example, the function below looks in `G:/SSBOOT/` and finds folders matching the pattern `SM_EM`: `SM_EM_1`,`SM_EM_2`, etc. Within each of those `_EM_` directories, there are several directories that match the pattern `Rep`. The function then plots every terminal year (one for each rep) and includes a legend. Obviously, this approach leads to more points.

*Edits needed:*
1) Make # colors vary with # EMs discovered
2) Find generalization for reference point names - currently a headache because names in `SPR series` don't match `derived quants`: SSB_MSY = SPB, Fstd_MSY = F_std, SPR_MSY = spr or SPR
3) Save `refList` object to visualize ref points in other ways

```{r}
kobe_compare_fn(rootdir = "G:/SSBOOT/",
                pattern = "SM_EM",
                subpattern = "Rep",
                toMatch = c("SSB","Fstd","SPR"),
                Fishery = "Striped Marlin",
                title = "- Estimation Models 1 and 2")
```

