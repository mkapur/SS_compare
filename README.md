---
title: "SS Compare"
author: "Felipe Carvalho, Maia Kapur"
date: "Dec 2017"
output: html_notebook
---

## Script to execute comparison plotting of sensitivty runs for SS3

```{r, eval = F}
library(r4ss)
library(colorRamps)
library(dplyr)
library(ggplot2)
rootdir = "G:/SS_Compare/"
setwd(rootdir)
source("ss_arrange_fn.R")
source("ss_compare_fn.R")
```

Run function `ss_compare()` to generate all plots. "pattern" should be an identifying label that matches the folder names
for all sensitivity runs you'd like to examine. "llabels" are the lables that will appear on all plot legends.
If `CSV = T`, a .csv file will be generated with per-fleet likelihoods according to  `likeLabel` and `lambdaLabel`, which should be available in summaryoutput$likelihoods$Label. 

```{r}
ss_compare(rootdir, 
           pattern = "00_", 
           llabels = c('W 0.1','W 0.2','W 0.3','W 0.4','W 0.5','W 0.6','W 0.7','W 0.8','W 0.9','W 1','Survey05'),
           likeCSV = T,
           likeLabel = c('Surv_like','SizeFreq_like:_2'),
           lambdaLabel = c('Surv_lambda','SizeFreq_lambda:_2'),
           fishery = "Deep7")
```
Run function `ss_arrange()` to create a compiled PDF with specified plots of interest. The plots are called via default names assigned from `SSplotComparisons()`
```{r}
ss_arrange(rootdir, 
              toMatch = c("spawnbio","Bratio","recruits","recdevs","indices_log"), 
              Fishery = 'MHI Deep 7 Bottomfish')
```

