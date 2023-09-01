library (yarrr)
library(psych)
library(plotrix)
library (yarrr)
library(psych)
library(plotrix)
library(segmented)
#library(geolight)
library(viridis)
library(plotrix)
library(plot3D)

DUdata <- export2r
SEdata <- export2r

#make the colour gradient
#colfunc <- colorRampPalette(c("midnightblue", "yellow"))
#colfunc(11) # blue to yellow

#colfunc <- colorRampPalette(c("darkgold4", "yellow"))
#colfunc(11) # gold to yellow

#colfunc <- colorRampPalette(c("black", "yellow"))
#colfunc(daylight) # gold to yellow

#plot(rep(1,12),col=colfunc(10),pch=19,cex=3)

#colour gradient from diurnal data

col <- viridis(12,option = "E", begin=0.15, end=1, direction=1) 

dayCOL<-c("#50576CFF", "#989278FF", "#ADA274FF", "#D8C563FF", "#EED654FF", "#FFEA46FF", "#EED654FF", "#D8C563FF", "#ADA274FF", "#858379FF", "#50576CFF", "#1B3B6DFF")
as.data.frame(col)


CathyPalblue <- c("#303064","#5E5E4E","#8C8C38","#BABA21","#E8E80B","#FFFF00","#E8E80B",
"#D1D116","#A3A32C","#5E5E4E","#303064","#191970")

CathyPalyellow <- c("#968200","#AD9E00","#C4BA00","#DCD500","#F3F100","#FFFF00","#F3F100","#E7E300", "#D0C700","#AD9E00", "#968200", "#8B7500")
CathyPalBlack <- c("#191900", "#4C4C00", "#7F7F00", "#B2B200", "#E5E500" , "#FFFF00", "#E5E500", "#CCCC00", "#999900","#4C4C00","#191900","#000000") 
CathyPalgold <- c("#191500","#4C4000","#7F6B00","#B29600","#E5C100","#FFD700","#E5C100","#CCAC00","#998100","#4C4000","#191500","#000000") 


names <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
namesshort <-c("J","F","M","A","M","J","J","A","S","O","N","D")

#get the data 
#remove diseased
exporttoR <- write2R
healthy <- exporttoR[exporttoR$diseased==0,]
healthy <- healthy[!is.na(healthy$eid), ]

dev.off()
dev.new()

tiff("Plot5.tiff", width = 7, height = 6, units = 'in', res = 300)

par(mfrow=c(2,2), mar= c(5, 5, 0, 0.5),mgp=c(2,1,0))
#pirate plot CRP
pirateplot(formula =  CRP ~ BS_month,
           data = healthy,
           #col = rgb(cr(daylen / max(daylen)), max=255)
           pal=dayCOL, #set to your own color palette created previously
           xlab = "",
           ylab = expression(bold("C-Reactive Protein (mg/l)")),
           yaxt="n",
           xaxt = "n",
           theme=0, #set theme to 0
           bty="n",
           #bean.f.o = .4, # Bean fill
           #point.o = .4, # Points
           inf.f.o = .8, # Inference fill
           inf.b.o = 1, # Inference border
           inf.f.col = dayCOL,
           inf.method = "ci",
           avg.line.o = 1, # Average line
           #bty=n,
           #point.cex = .5, # Points
           #quant = c(.1, .9), # Adjust quantiles
           ylim=c(2, 2.5),  # Adjust y axis limits
           #gl.col = gray(.2), # Gridline specifications
           #gl.lty = 1,
           #gl.lwd = c(0, .3))
)

axis(2,col.axis="black", lwd=2, las=2, cex.axis = 0.6)
axis(1, at=1:12, labels = namesshort, lwd=2, cex.axis = 0.8,font=2)
lines(1:12, SEdata$crp, lty=2,lwd=1, col="black")

#lines(DATAcos$MONTHcos, DATAcos$CRPcos, lty=2,lwd=1, col="black")


#pirate plot lymphocytes
pirateplot(formula =  LY_count_C ~ BS_month,
           data = healthy,
           #col = rgb(cr(daylen / max(daylen)), max=255)
           pal=dayCOL, #set to your own color palette created previously
           xlab = "",
           ylab = expression(bold(paste("Lymphocytes x 10"^"9","/litre"))),
           yaxt="n",
           xaxt = "n",
           theme=0, #set theme to 0
           bty="n",
           #bean.f.o = .4, # Bean fill
           #point.o = .4, # Points
           inf.f.o = .8, # Inference fill
           inf.b.o = 1, # Inference border
           inf.f.col = dayCOL,
           inf.method = "ci",
           avg.line.o = 1, # Average line
           #bty=n,
           #point.cex = .5, # Points
           #quant = c(.1, .9), # Adjust quantiles
           ylim=c(1.88,2.02),  # Adjust y axis limits
           #gl.col = gray(.2), # Gridline specifications
           #gl.lty = 1,
           #gl.lwd = c(0, .3))
)

axis(2,col.axis="black", lwd=2, las=2, cex.axis = 0.6)
axis(1, at=1:12, labels = namesshort, lwd=2, cex.axis = 0.8,font=2)
lines(1:12,SEdata$ly, lty=2,lwd=1, col="black")

#pirate plot monocytes
pirateplot(formula =  MO_count_C ~ BS_month,
           data = healthy,
           #col = rgb(cr(daylen / max(daylen)), max=255)
           pal=dayCOL, #set to your own color palette created previously
           xlab = "",
           ylab = expression(bold(paste("Monocytes x 10"^"9","/litre"))),
           yaxt="n",
           xaxt = "n",
           theme=0, #set theme to 0
           bty="n",
           #bean.f.o = .4, # Bean fill
           #point.o = .4, # Points
           inf.f.o = .8, # Inference fill
           inf.b.o = 1, # Inference border
           inf.f.col = dayCOL,
           inf.method = "ci",
           avg.line.o = 1, # Average line
           #bty=n,
           #point.cex = .5, # Points
           #quant = c(.1, .9), # Adjust quantiles
           ylim=c(0.44,0.48),  # Adjust y axis limits
           #gl.col = gray(.2), # Gridline specifications
           #gl.lty = 1,
           #gl.lwd = c(0, .3))
)

axis(2,col.axis="black", lwd=2, las=2, cex.axis = 0.6)
axis(1, at=1:12, labels = namesshort, lwd=2, cex.axis = 0.8,font=2)
#lines(DATAcos$MONTHcos, DATAcos$MONcos, lty=2,lwd=1, col="black")



#pirate plot neutrophils
pirateplot(formula =  NE_count_C ~ BS_month,
           data = healthy,
           #col = rgb(cr(daylen / max(daylen)), max=255)
           pal=dayCOL, #set to your own color palette created previously
           xlab = "",
           ylab = expression(bold(paste("Neutrophils x 10"^"9","/litre"))),
           yaxt="n",
           xaxt = "n",
           theme=0, #set theme to 0
           bty="n",
           #bean.f.o = .4, # Bean fill
           #point.o = .4, # Points
           inf.f.o = .8, # Inference fill
           inf.b.o = 1, # Inference border
           inf.f.col = dayCOL,
           inf.method = "ci",
           avg.line.o = 1, # Average line
           #bty=n,
           #point.cex = .5, # Points
           #quant = c(.1, .9), # Adjust quantiles
           ylim=c(3.95,4.25),  # Adjust y axis limits
           #gl.col = gray(.2), # Gridline specifications
           #gl.lty = 1,
           #gl.lwd = c(0, .3))
)

axis(2,col.axis="black", lwd=2, las=2, cex.axis = 0.6)
axis(1, at=1:12, labels = namesshort, lwd=2, cex.axis = 0.8,font=2)
lines(1:12, SEdata$ne, lty=2,lwd=1, col="black")

dev.off()

#pirate plot wbs
pirateplot(formula =  WBC_count_C ~ BS_month,
           data = healthy,
           #col = rgb(cr(daylen / max(daylen)), max=255)
           pal=dayCOL, #set to your own color palette created previously
           xlab = "",
           ylab = expression(bold(paste("White Blood Cells x 10"^"9","/litre"))),
           yaxt="n",
           xaxt = "n",
           #mgp=c(3, .3, 0),
           theme=0, #set theme to 0
           bty="n",
           #bean.f.o = .4, # Bean fill
           #point.o = .4, # Points
           inf.f.o = .8, # Inference fill
           inf.b.o = 1, # Inference border
           inf.f.col = dayCOL,
           inf.method = "ci",
           avg.line.o = 1, # Average line
           #bty=n,
           #point.cex = .5, # Points
           #quant = c(.1, .9), # Adjust quantiles
           ylim=c(6.6,7),  # Adjust y axis limits
           #gl.col = gray(.2), # Gridline specifications
           #gl.lty = 1,
           #gl.lwd = c(0, .3))
)

axis(2,col.axis="black", lwd=2, las=2, cex.axis = 0.6,font=2)
axis(1, at=1:12, labels = namesshort, lwd=2, cex.axis = 0.6,font=2)
lines(1:12, SEdata$wbc, lty=2,lwd=1, col="black")
dev.off()
summary(write2R$daylen)


plot.new()

plot.new()
season_cols <- viridis(50, option ="E")

tiff("legend_season1.tiff", width = 7, height = 6, units = 'in', res = 300)
plot.new()
colkey(clim = range(6:18),
       col = season_cols,
      dist = -0.2, 
       #shift = 0.5,
       side = 4, 
       add = TRUE, 
       clab = "Daylength", 
       line.clab=.5,#lower title
       cex.clab = 1,
       col.clab = "black",
       adj.clab=0,
       length = .5, 
       width = 1.5, 
       col.axis = "black", 
       col.ticks = "black",
       col.box = "white",
       at = c(6.5,17.5), 
       labels = c("6hr","18hr"),
       mgp=c(3, .3, 0),
       tck=0,
       cex.axis = 1,
       lwd.ticks=.2)
dev.off()



tiff("legend_diurnal.tiff", width = 7, height = 6, units = 'in', res = 300)
plot.new()
colkey(clim = range(9:20),
       col = col,
       dist = -0.2, 
       #shift = 0.5,
       side = 4, 
       add = TRUE, 
       clab = "Time of Day", 
       line.clab=.5,#lower title
       cex.clab = 0.8,
       col.clab = "black",
       adj.clab=0,
       length = 1, 
       width = 3, 
       col.axis = "black", 
       col.ticks = "black",
       col.box = "white",
       at = c(9,20), 
       labels = c("9am","8pm"),
       mgp=c(3, .3, 0),
       tck=0,
       cex.axis = 1,
       lwd.ticks=.2)
dev.off()















# get some extra room
par(mar=c(7,4,4,6))
testcol<-color.gradient("yellow","darkgold4",nslices=11)
col.labels<-c("16hours","12hours","7hours")

# this will put the labels at the intersections
# col.labels<-c("","Cold","","Warm","","Warmer","","Hot","")
color2D.matplot(matrix(rnorm(100),nrow=10),"yellow","darkgold4")

color.legend(10.2,2,11,5,col.labels,testcol,align="rb",gradient="y")
par(mar=c(5,4,4,2))

