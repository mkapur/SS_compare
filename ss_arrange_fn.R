## Function to arrange pre-existing plots generating using SSComparePlots
## M Kapur maia.kapur@noaa.gov
## Dec 2017 

ss_arrange = function(rootdir,
                         toMatch = c("spawnbio","Bratio","recruits","recdevs","indices_log"),
                         Fishery = 'Name of Fishery'){
library(png)
library(grid)
library(ggplot2)
library(gridExtra)

# toMatch = paste(c("spawnbio","Bratio","recruits","recdevs","indices_log"), collapse =  "|")
toMatch = paste(c(toMatch,"_titlepage"), collapse =  "|")
mar0 <- par()$mar # current margins
par(mar=rep(0,4))
png(paste0(rootdir,"/plots/_titlepage.png"))
plot(0,type="n",xlab="",ylab="",axes=FALSE,xlim=c(0,1),ylim=c(0,1))
y <- 0.9
ystep <- -.05
text(0,y,"Plots created using the 'r4ss' package in R",pos=4)
# y <- y+ystep
# text(0,y,paste("Stock Synthesis version:","3.X",pos=4))
y <- y+ystep
text(0,y,Sys.Date(),pos=4)
y <- y+ystep
text(0,y,Fishery,pos=4)
dev.off()
par(mar=mar0) # replace margins
mar0 <- par()$mar # current inner margins
oma0 <- par()$oma # current outer margins


# prexplots = list.files(pattern = '.*[.]png', paste0(rootdir,"/plots")) %>%

## extract desired plots and fill PDF
prexplots = list.files(paste0(rootdir,"/plots")) %>%
  .[grepl(toMatch, .)] %>%
lapply( .,function(x){
  img <- as.raster(readPNG(paste0(rootdir,"/plots/",x)))
  rasterGrob(img, interpolate = FALSE)
})
dev.off()

ggsave(paste0(rootdir,'/plots/SSplotSelected.pdf'), marrangeGrob(grobs=prexplots, nrow=3, ncol=2, top = NULL))
}

# mk_ss_arrange(rootdir, toMatch = c("spawnbio","Bratio","recruits","recdevs","indices_log"))