#===============================================================================
# FINAL SCRIPT TO MAKE PLOTS AND STATS MODELS FOR DJ WATER + OG WELL DATA
#===============================================================================
library(scales); library(grDevices); library(maps); library(mapdata); library(png); library(RColorBrewer)
library(ellipse)
rm(list=ls())

#====================================================================================
#LOAD  QC'd DATA  (Note that Production data are only from LTE2007 report - no additional
#COGCC data b/c too laborious to check all production formations and depths)
#====================================================================================
setwd("/Users/owensherwood/Google Drive/COGCC data project/9_OAS/0_DJ Basin Study/2_Working Files")
WW1 <- read.csv("DJbasin QClevel3 Data (gas_isotopes_physical_ions_final).csv")
OG1 <- read.csv("Wattenberg_Production_Baseline_LTE2007.csv")
COAL <- read.csv("Huron H 1 COALS.csv")


#===============================================================================
#WW1 FacID count:
length(unique(WW1$FacID))

#===============================================================================
# ISOLATE OG1 PRODUCING FORMATIONS
#===============================================================================
unique(OG1$Prod_Int)
Sx      <- OG1[OG1$Prod_Int == "Sussex",]
Nb      <- OG1[OG1$Prod_Int == "Niobrara",]
CdNb    <- OG1[OG1$Prod_Int == "Codell/Niobrara",]
Cd      <- OG1[OG1$Prod_Int == "Codell",]
JsCdNb  <- OG1[OG1$Prod_Int == "J Sand/Codell/Niobrara",]
JsCd    <- OG1[OG1$Prod_Int == "J Sand/Codell",]
Js      <- OG1[OG1$Prod_Int == "J Sand",]

#===============================================================================
# SET UP PLOT VARIABLES
#===============================================================================
d13c.lab <-     expression(paste(~delta^13*C," (", "\u2030", ")"))       
d13c1.lab <-    expression(paste(~delta^13*C, " of Methane"," (", "\u2030", ")"))       
d13c2.lab <-    expression(paste(~delta^13*C, " of Ethane"," (", "\u2030", ")"))       
dDc1.lab <-     expression(paste(~delta^2*H, " of Methane"," (", "\u2030", ")"))       
dryness.lab <-  expression('C'[1] * ' / ' * '(C'[2] * ' + C'[3] * ')' )
wetness.lab <-  expression('C'[2-6] *  ' / C'[1-6] * ' (%)' )
fms.list.og <- c("Sussex", "Niobrara", "Codell/Niobrara", "J Sand/Codell/Niobrara", 
                  "Codell", "J Sand/Codell", "J Sand")
fm.cols.og <-   c("darkgreen", "green", "yellow", "grey50", "red", "magenta", "slateblue4"  )
aquifers <- c("quaternary alluvium", "high plains", "dakota-cheyenne", "dawson", "denver",  "arapahoe", "laramie-foxhills (unconfined)", "laramie-foxhills (confined)" )
aq.labs <- c("Alluvium", "High Plains", "Dakota-Cheyenne", "Dawson", "Denver",  "Arapahoe", "LFH (unconfined)", "LFH (confined)" )
aq.cols <- brewer.pal(6, "GnBu")
fm.cols <- brewer.pal(7,"YlOrRd")
comp.labs <- c(expression('C'[1]), expression('C'[2]), expression('C'[3]), expression('iC'[4]),
               expression('nC'[4]), expression('iC'[5]), expression('nC'[5]), expression('C'[6]))

#====================================================================================
#AGGREGATE WATER WELL DATA ON FACID (MAXIMUM VALUES)
#Note that you cannot compute thermogenic based on facility aggregated values because not 
#collected at same time.
#====================================================================================
metacols <- c(1: which(colnames(WW1) == "n2")-1)
datacols <- c(which(colnames(WW1) == "n2"):79)
WW2max <- aggregate(WW1[,c(1,datacols)], list(WW1$FacID), max, na.rm = TRUE)
WW2max <- as.matrix(WW2max); WW2max[WW2max=="-Inf"] <- NA; WW2max <- as.data.frame(WW2max)
WW2max <- WW2max[,c(2:56)]
WW2max <- merge(WW1[!duplicated(WW1$FacID),metacols], WW2max, by = "FacID", all.x = FALSE)

#====================================================================================
#WELL AND SAMPLE COUNTS BY WELL TYPE AND SAMPLE REASON
#====================================================================================
# Well Type
aggregate(WW1$FacID, list(WW1$Use1), "length")
aggregate(WW2max$FacID, list(WW2max$Use1), "length")

Reasons <- read.csv("SampleReasons.csv")
aggregate(Reasons$FacID, list(Reasons$ReasonShort), "length")

length(unique(Reasons$FacID[Reasons$ReasonShort == "reg pre"]))
length(unique(Reasons$FacID[Reasons$ReasonShort == "reg post"]))
length(unique(Reasons$FacID[Reasons$ReasonShort == "vol pre"]))
length(unique(Reasons$FacID[Reasons$ReasonShort == "vol post"]))
length(unique(Reasons$FacID[Reasons$ReasonShort == "complaint"]))
length(unique(Reasons$FacID[Reasons$ReasonShort == "unknown"]))



#====================================================================================
#WELL COUNTS BY MAX CH4 CONCENTRATION
#====================================================================================
n <- length(unique(WW2max$FacID)) #1062

#Sites not measured for c1aq
tmp <- WW2max[is.na(WW2max$c1aq) == TRUE,]
length(unique(tmp$FacID))  #138 (13.0%)
138/n

#Sites measured for c1aq
tmp <- WW2max[is.na(WW2max$c1aq) == FALSE,]
length(unique(tmp$FacID))  #924 (87.0%)

#Sites below CH4 detection limits
tmp <- WW2max[which(WW2max$c1aq == 0),]
length(unique(tmp$FacID))  #331 (31.2%) 
331/924  #35.8%
331/n

#Sites exceeding CH4 detection limits
tmp <- WW2max[which(WW2max$c1aq > 0),]
length(unique(tmp$FacID))  #593 (55.8%) 
593/924  #64.2%
593/n

#0-1 mg/l
tmp <- WW2max[which(WW2max$c1aq > 0.00000 & WW2max$c1aq <= 1.00000 ),]
length(unique(tmp$FacID))  #332 (31.3%)
332/924  #35.9%

#1-10 mg/l
tmp <- WW2max[which(WW2max$c1aq > 1.00000 & WW2max$c1aq <= 10.00000 ),]
length(unique(tmp$FacID))  #139 (13.1%) 
139/924  #15.0%

#10-28 mg/l
tmp <- WW2max[which(WW2max$c1aq > 10.0000000 & WW2max$c1aq <= 28.0000000 ),]
length(unique(tmp$FacID))  #117 (11.0%) 
117/924  #12.7%

#>28 mg/l
tmp <- WW2max[which(WW2max$c1aq > 28.0000000),]
length(unique(tmp$FacID))  #5 (0.5%) 
5/924  #.5%
5/n
#Sites exceeding 1 mg/l
tmp <- WW1[which(WW1$c1aq > 1),]
length(unique(tmp$FacID))  #261 (24.6%)
261/924  #28.2%
261/n

#Sites exceeding 10 mg/l
tmp <- WW1[which(WW1$c1aq > 10.000000),]
length(unique(tmp$FacID))  #122 (11.5%)
122/924  #13.2%
122/n


#====================================================================================
#WELL COUNTS BY METHANE SOURCES
#====================================================================================
#Number of wells with isotope measurements...
isotopes <- WW1[is.na(WW1$d13c1) == FALSE,]
with(isotopes, hist(c1aq))
summary(isotopes$c1aq)
tmp <- isotopes[order(isotopes$c1aq),]
View(tmp)
#Number of sites with isotope measurements...
  isotopes <- WW2max[is.na(WW2max$d13c1) == FALSE,]
  isotopes <- WW2max[is.na(WW2max$d13c1) == FALSE | is.na(WW2max$d13c2) == FALSE | is.na(WW2max$d13c3) == FALSE,]
  length(isotopes[,1])  # n=213 

#Number of sites with gas dryness measurements...
  gas <- WW2max[is.na(WW2max$GasDryness) == FALSE,]
  length(gas[,1])  # n=234 
  
#Number of sites with isotopes OR gas dryness measurements...
  tmp <- union(isotopes$FacID, gas$FacID)
  length(tmp)  #n= 234 
  
#Counts of thermogenic  (d13C *AND* dryness parameters)
  thermo <- which(WW1$d13c1 >= -55 & WW1$GasDryness <= 50)
  thermoFacs <- unique(WW1$FacID[thermo])
  length(thermoFacs) #******** n = 31 sites with thermogenic gas
  View(WW1[thermo,])
  
#Counts of mixed (d13C *AND* dryness parameters)
  mixed <- which(WW1$d13c1 < -55 & WW1$GasDryness <= 100)
  mixedFacs <- unique(WW1$FacID[mixed])
  mixedFacs <- setdiff(mixedFacs, thermoFacs)
  length(mixedFacs) #******** n = 13 sites with mixed gas
  View(WW1[mixed,])
  
#Counts of microbial gas
  micro <- which(WW1$d13c1 < -60 & WW1$GasDryness >100)
  microFacs <- unique(WW1$FacID[micro])
  microFacs <- setdiff(microFacs, c(thermoFacs, mixedFacs))
  length(microFacs) #******** n = 169 sites with microbial gas
  
#confirm sum
length(thermoFacs) + length(mixedFacs) + length(microFacs)


#====================================================================================
#DISSOLVED METHANE VS METHANE SOURCE
# ****PROBLEM: WHY ARE DISSOLVED C2 AND C3 HIGHER IN MIXED THAN IN THERMOGENIC GASES??***
#====================================================================================
#Order by MethaneSource...
WW1$MethaneSource <- factor(WW1$MethaneSource,c("unknown","microbial","thermogenic","mixed"))

#Assess normality...  (they all fail for c1aq. therefore use bootstats)
hist(WW1$c1aq[WW1$MethaneSource == "microbial"])
  shapiro.test(WW1$c1aq[WW1$MethaneSource == "microbial"])
hist(WW1$c1aq[WW1$MethaneSource == "unknown"])
  shapiro.test(WW1$c1aq[WW1$MethaneSource == "unknown"])
hist(WW1$c1aq[WW1$MethaneSource == "thermogenic"])
  shapiro.test(WW1$c1aq[WW1$MethaneSource == "thermogenic"])

#Boxplot - dissolve METHANE vs source
pdf("Boxplot-Dissolved Methane vs Source.pdf", encoding = "MacRoman")
par(mar = c(5,7,3,7))
boxplot(c(-10,-10,-10,-10)~c(1:4), xaxt = "n", 
        ylim = c(0,40), xlab = "Genetic Origin",
        ylab = "Dissolved Methane (mg/l)", cex.lab = 1.25)
grid();box()
with(WW1[is.na(WW1$MethaneSource)==FALSE,],boxplot(c1aq ~ MethaneSource, 
         col = "lightgrey", add=TRUE, xaxs=FALSE))
dev.off()

#Boxplot - dissolve ETHANE vs source
pdf("Boxplot-Dissolved Ethane vs Source.pdf", encoding = "MacRoman")
par(mar = c(5,7,3,7))
boxplot(c(-10,-10,-10,-10)~c(1:4), xaxt = "n", 
        ylim = c(0,10), xlab = "Genetic Origin",
        ylab = "Dissolved Ethane (mg/l)", cex.lab = 1.25)
grid();box()
with(WW1[is.na(WW1$MethaneSource)==FALSE,],boxplot(c2aq ~ MethaneSource, col = "lightgrey", add=TRUE, xaxs=FALSE))
dev.off()

#Boxplot - dissolve PROPANE vs source
pdf("Boxplot-Dissolved propane vs Source.pdf", encoding = "MacRoman")
par(mar = c(5,7,3,7))
boxplot(c(-10,-10,-10,-10)~c(1:4), xaxt = "n", 
        ylim = c(0,6), xlab = "Genetic Origin",
        ylab = "Dissolved Propane (mg/l)", cex.lab = 1.25)
grid();box()
with(WW1[is.na(WW1$MethaneSource)==FALSE,],boxplot(c3aq ~ MethaneSource, col = "lightgrey", add=TRUE, xaxs=FALSE))
dev.off()

#Crossplots of dissolved vs free gas for C1, C2, C3...
plot(0,0, pch="", xlab = "Free gas methane (mol %)", ylab = "Dissolved Methane (mg/l", xlim=c(0,100), ylim=c(0,36))
with(WW1[WW1$MethaneSource=="unknown",], points(c2,c2aq))
with(WW1[WW1$MethaneSource=="microbial",], points(c2,c2aq))
with(WW1[WW1$MethaneSource=="thermogenic",], points(c2,c2aq, col="red"))
with(WW1[WW1$MethaneSource=="mixed",], points(c2,c2aq, col="blue"))

plot(0,0, pch="", xlab = "Free Gas Methane (mol %)", ylab = "Dissolved Methane (mg/l", xlim=c(0,10), ylim=c(0,10))
with(WW1[WW1$MethaneSource=="unknown",], points(c2,c2aq))
with(WW1[WW1$MethaneSource=="microbial",], points(c2,c2aq))
with(WW1[WW1$MethaneSource=="thermogenic",], points(c2,c2aq, col="red"))
with(WW1[WW1$MethaneSource=="mixed",], points(c2,c2aq, col="blue"))

plot(0,0, pch="", xlab = "Free Gas Methane (mol %)", ylab = "Dissolved Methane (mg/l", xlim=c(0,10), ylim=c(0,10))
with(WW1[WW1$MethaneSource=="unknown",], points(c3,c3aq))
with(WW1[WW1$MethaneSource=="microbial",], points(c3,c3aq))
with(WW1[WW1$MethaneSource=="thermogenic",], points(c3,c3aq, col="red"))
with(WW1[WW1$MethaneSource=="mixed",], points(c3,c3aq, col="blue"))


#====================================================================================
#METHANE (AND OTHERS) vs AQUIFER
#====================================================================================
WW1$Aquifer <- ordered(WW1$Aquifer, levels= rev(aquifers))

#Assess normality...  (they all fail for c1aq. therefore use bootstats)
hist(WW1$c1aq[WW1$Aquifer == "laramie-foxhills (confined)"])
  shapiro.test(WW1$c1aq[WW1$Aquifer == "laramie-foxhills (confined)"])
hist(WW1$c1aq[WW1$Aquifer == "denver"])
  shapiro.test(WW1$c1aq[WW1$Aquifer == "denver"])


#Dissolved Methane..............................................................
#Bootstrap statistics from Table S6: (boot mean, lower and upper)
bm <- rev(c(0.005,0.05,0.005,0.025,1.8,0.015,0.005,7.57))
bl <- rev(c(0.00,0.005,0.005,0.01,0.30,0.01,0.00,6.725))
bu <- rev(c(0.005,0.05,0.015,0.05,3.625,0.025,0.005,8.50))
ypos = c(8:1)

pdf("Stripchart Dissolved methane by aquifer.pdf", encoding = "MacRoman", width=3.375,height=3.375)
par(omi=c(0,0,0,0), mar = c(4,8,4,.25),cex=.6,mgp=c(2.25,1,0))
plot(0,0, pch="", xlim = c(0,45), ylim = c(0.5,8.5),  
        xlab = "Dissolved Methane (mg/l)", ylab="", yaxt = "n", cex.lab = 1)
abline(h = ypos[1:8], col="lightgrey", lty="dotted", lwd=.75)
abline(v = seq(0,40,5), col="lightgrey", lty="dotted", lwd=.75)
axis(2, at = c(1:8), las = 1, labels = rev(c("Alluvium", "High Plains", "Dakota-Cheyenne", "Dawson", 
                                             "Denver","Aparaphoe", "Laramie-Fox Hills\n(unconfined)",
                                             "Laramie-Fox Hills\n(confined)")))
for(i in seq(1:8)){
  polygon(x=c(bl[i],bu[i],bu[i],bl[i]), y=c(i-.4,i-.4,i+.4,i+.4), col="grey")}
for(i in seq(1:8)){
  lines(c(bm[i],bm[i]), c(i-.4,i+.4), lwd=2)}
for(i in seq(1:8))
{tmp <- WW1[WW1$Aquifer == aquifers[i],]
 tmp <- tmp[tmp$MethaneSource == "unknown" |  tmp$MethaneSource == "no methane",]
 with(tmp, stripchart(c1aq, add=TRUE, at=ypos[i], method="jitter",jitter=.07, pch=21, bg="grey", cex = 1.1))}  
for(i in seq(1:8))
{tmp <- WW1[WW1$MethaneSource == "microbial" & WW1$Aquifer == aquifers[i],]
 with(tmp, stripchart(c1aq, add=TRUE, at=ypos[i]-.15, method="jitter",jitter=.07, pch=21, bg=aq.cols[5], cex = 1.1))}  
for(i in seq(1:8))
{
  tmp <- WW1[WW1$Aquifer == aquifers[i],]
  tmp <- tmp[which(tmp$MethaneSource == "thermogenic" |  tmp$MethaneSource == "mixed"),]
  with(tmp, stripchart(c1aq, add = TRUE, at = ypos[i]+.15, method = "jitter",jitter = .07,  pch=21, bg=fm.cols[5], cex = 1.1))}  
legend("topright", c("microbial", "undetermined", "thermogenic/mixed"), pch=21, bg="white", pt.bg = c(aq.cols[5],"grey",fm.cols[5]), pt.cex = 1.1)
counts <- with(WW1[is.na(WW1$c1aq)==FALSE,], aggregate(c1aq, by=list(Aquifer), NROW)))
maxes <-  with(WW1[is.na(WW1$c1aq)==FALSE,], aggregate(c1aq, by=list(Aquifer), 'max'))
text(c(39,15,13.3,16,1.97,25.4,1,2.7)+3, c(1:8), c(420,108,58,66,16,282,76,134))
dev.off()

###############################################################################
#DENSITY PLOTS...
###############################################################################
library(RColorBrewer)
aq.cols <- brewer.pal(8, "Dark2")
pdf("Dissolved methane by aquifer.pdf", encoding = "MacRoman")
par(mar = c(5,5,3,3))
plot(0,0, pch="", xlim=c(0,37), ylim=c(0,.32), 
     xlab="Dissolved Methane (mg/l)", ylab="Probability Density", cex.lab=1.25); grid()

for(i in c(1:8)){
  lines(density(WW1$c1aq[WW1$Aquifer == aquifers[i]], na.rm=TRUE, from=0, bw=1), 
        lwd=5, col = aq.cols[i])
  }


plot(0,0, pch="", xlim=c(0,37), ylim=c(0,.32), 
     xlab="Dissolved Methane (mg/l)", ylab="Probability Density", cex.lab=1.25); grid()
tmp <- WW1[WW1$MethaneSource == "unknown" ,] 
tmp <- WW1[WW1$MethaneSource == "microbial",] 
tmp <- WW1[WW1$MethaneSource == "unknown" | WW1$MethaneSource == "microbial" | WW1$MethaneSource == "mixed",] 
for(i in c(1:8)){
lines(density(tmp$c1aq[tmp$Aquifer == aquifers[i]], na.rm=TRUE, from=0, bw=1), lwd=5, col = aq.cols[i])
  }

for(i in c(1:8)){
lines(density(WW1$c1aq[WW1$Aquifer == aquifers[i] & 
                         WW1$MethaneSource=="microbial" | 
                         WW1$MethaneSource=="mixed" |
                         WW1$MethaneSource=="unknown"], na.rm=TRUE, from=0, bw=1), 
                                    lwd=5, col = aq.cols[i])}
legend("topright", aq.labs, lwd=5, col = aq.cols)
dev.off()

pdf("Dissolved sulfate by aquifer.pdf", encoding = "MacRoman")
par(mar = c(5,5,3,3))
plot(0,0, pch="", xlim=c(0,1000), ylim=c(0,.008), 
     xlab="Sulfate (mg/l)", ylab="Probability Density", cex.lab=1.25); grid()
for(i in c(1:8)){
  lines(density(WW1$so4[WW1$Aquifer == aquifers[i]], na.rm=TRUE, from=0, bw=50), 
  lwd=5, col = aq.cols[i])}
legend("topright", aq.labs, lwd=5, col = aq.cols)
dev.off()

pdf("Depth by aquifer.pdf", encoding = "MacRoman")
par(mar = c(5,5,3,3))
plot(0,0, pch="", xlim=c(0,500), ylim=c(0,.012), 
     xlab="Well Depth (m)", ylab="Probability Density", cex.lab=1.25); grid()
for(i in c(1:8)){
  lines(density(WW1$Depth[WW1$Aquifer == aquifers[i]]*.3048, na.rm=TRUE, from=0, bw=35), 
        lwd=5, col = aq.cols[i])}
legend("topright", aq.labs, lwd=5, col = aq.cols)
dev.off()

pdf("Dissolved methane by methane origin.pdf", encoding = "MacRoman")
par(mar = c(5,5,3,3))
plot(0,0, pch="", xlim=c(0,40), ylim=c(0,.07), 
     xlab="Dissolved Methane (mg/l)", ylab="Probability Density", cex.lab=1.25); grid()
  lines(density(WW1$c1aq[WW1$MethaneSource == "unknown"], na.rm=TRUE, from=0, bw=3), lwd=5, col = aq.cols[1])
  lines(density(WW1$c1aq[WW1$MethaneSource == "microbial"], na.rm=TRUE, from=0, bw=3), lwd=5, col = aq.cols[2])
  lines(density(WW1$c1aq[WW1$MethaneSource == "thermogenic"], na.rm=TRUE, from=0, bw=3), lwd=5, col = aq.cols[3])
  lines(density(WW1$c1aq[WW1$MethaneSource == "mixed"], na.rm=TRUE, from=0, bw=3), lwd=5, col = aq.cols[4])
legend("topright", c("unknown","microbaial","thermogenic","mixed"), lwd=5, col = aq.cols[c(1:4)])
dev.off()




#====================================================================================
#CROSSPLOTS OF METHANE VS SULPHATE BY AQUIFER
#NB: Tried stripchart but doesn't look good bc LFH has so many data
#====================================================================================

#Crossplot of methane vs sulphate by aquifer:
with(WW1, plot(0,0, pch="", xlim = c(0.001,4000), ylim = c(0.001,50) ))
with(WW1[WW1$Aquifer == "laramie-foxhills (confined)",], points(so4, c1aq, pch=16, col = alpha("red", 0.5)))
with(WW1[WW1$Aquifer == "laramie-foxhills (unconfined)",], points(so4, c1aq, pch=16, col = alpha("green", 0.5)))
with(WW1[WW1$Aquifer == "quaternary alluvium",], points(so4, c1aq, pch=16, col = alpha("cyan", 0.5)))
with(WW1[WW1$Aquifer == "dakota-cheyenne",], points(so4, c1aq, pch=16, col = alpha("orange", 0.5)))
with(WW1[WW1$Aquifer == "dawson",], points(so4, c1aq, pch=16, col = alpha("purple", 0.5)))
with(WW1[WW1$Aquifer == "high plains",], points(so4, c1aq, pch=16, col = alpha("blue", 0.5)))
with(WW1[WW1$Aquifer == "denver",], points(so4, c1aq, pch=16, col = alpha("grey", 0.5)))
with(WW1[WW1$Aquifer == "arapahoe",], points(so4, c1aq, pch=16, col = alpha("black", 0.5)))

with(WW1[WW1$Aquifer == "laramie-foxhills (confined)",], hist(so4, col = 'grey'))
with(WW1[WW1$Aquifer == "laramie-foxhills (unconfined)",], hist(so4, add=TRUE))
with(WW1, boxplot(so4~Aquifer))

with(WW1, aggregate(so4, by = list(Aquifer), 'median', na.rm=TRUE))
aggregate(WW1$tds, by = list(WW1$Aquifer), 'mean', na.rm=TRUE)
aggregate(WW1$cl, by = list(WW1$Aquifer), 'median', na.rm=TRUE)
aggregate(WW1$alk, by = list(WW1$Aquifer), 'mean', na.rm=TRUE)


#====================================================================================
#AQUIFER DEPTHS
#====================================================================================
aggregate(WW1$Depth, by = list(WW1$Aquifer), 'mean', na.rm=TRUE)
aggregate(WW1$Depth, by = list(WW1$Aquifer), 'sd', na.rm=TRUE)

#Calcuate depth of combined LFH confined + unconfined aquifers in meters
LFH.c <- WW1$Depth[WW1$Aquifer == "laramie-foxhills (confined)"]
LFH.c <- LFH.c[is.na(LFH.c) == FALSE]  
LFH.uc <- WW1$Depth[WW1$Aquifer == "laramie-foxhills (unconfined)"]
LFH.uc <- LFH.uc[is.na(LFH.uc) == FALSE]  
LFH <- c(LFH.c, LFH.uc)
mean(LFH)*.3048; sd(LFH)*.3048; length(LFH)

#Calcuate depth of Denver aquifer in meters
DEN <- WW1$Depth[WW1$Aquifer == "denver"]
DEN <- DEN[is.na(DEN) == FALSE]  
#Calculate depth in meters
mean(DEN)*.3048; sd(DEN)*.3048; length(DEN)



#====================================================================================
#DEPTH VS D13C METHANE PLOT...
#====================================================================================
aquifers.tmp <- aquifers[c(1,3,5,6,7,8)]
aq.labs.tmp <- aq.labs[c(1,3,5,6,7,8)]
pdf("Depth-Methane d13c Plot.pdf", encoding = "MacRoman", width = 3.375, height=4 )
depths.m <- seq(2500,0,-500)
depths.ft <- signif(depths.m/.3048, 3)
par(omi=c(0,0,0,0), mar = c(4,4,1,4), cex=.6, mgp=c(2.75,1,0))
plot(-10,-10, xlim = c(-90,-35), ylim = c(2600,0), xlab = d13c1.lab, ylab = "Depth (m)", cex.lab = 1.25)
axis(4, at = depths.m, lab = depths.ft)
mtext("Depth (ft)", 4, line = 3, cex=0.6*1.25)
grid()
for(j in c(1:7)){
  with(OG1[OG1$Prod_Int == fms.list.og[j],], points(d13C1, Perf_Avg*.3048, pch = 21, col = "black", bg = fm.cols[j], cex = 1.5))
}  
for(i in c(6:1)){
  with(WW1[WW1$Aquifer == aquifers[i],], points(d13c1, Depth*.3048, pch = 21, col = "black", bg = aq.cols[i], cex = 1.5))
}
with(COAL, symbols(mean(d13C1), mean(Depth)*.3048, stars = c(1,1,1)) # pch = 24, col = "black", bg = "green", cex = 1.75))
polygon(x=c(-100,-69,-69,-100), y=c(3000, 3000, 1400, 1400), col="white")
legend(-91, 1415, "Coal", cex=.7, pch = 24, pt.cex = 1.75, col = "black", pt.bg = "green", bg="white", bty="n")
legend(-91,1550, title="Groundwater:", title.adj=0,  aq.labs, cex=.7, pch = 21, pt.cex = 1.5, col = "black", pt.bg = aq.cols, bg="white", bty="n")
legend(-91,2100, title="Production Gas:", title.adj=0,  fms.list.og, cex=.7, pch = 21,pt.cex = 1.5, col = "black", pt.bg = fm.cols, bg="white", bty="n")
box()
dev.off()



#====================================================================================
#DEPTH VS D13C ETHANE PLOT...
#====================================================================================
pdf("Depth-Ethane d13c Plot.pdf", encoding = "MacRoman")
depths.m <- seq(2500,0,-500)
depths.ft <- signif(depths.m/.3048, 3)
par(mar = c(5,7,3,7))
plot(-10,-10, xlim = c(-60,-20), ylim = c(2600,0), xlab = d13c2.lab, ylab = "Depth (m)", cex.lab = 1.25)
axis(4, at = depths.m, lab = depths.ft)
mtext("Depth (ft)", 4, line = 3, cex = 1.25)
grid()
with(WW1, points(d13c2, Depth*.3048, pch = 21, col = "black", bg = alpha("blue", 0.65), cex = 1.5))
with(OG1, points(d13C2, Perf_Avg*.3048, pch = 21, col = "black", bg = alpha("red", 0.65), cex = 1.5))
legend("bottomleft", c("Groundwater", "Production Gas"), pch = 21, pt.cex = 1.5, 
       col = "black", pt.bg = c(alpha("blue", 0.65), alpha("red", 0.65)), bg = "white")
box()
dev.off()

#====================================================================================
#DEPTH VS WETNESS PLOT...
#====================================================================================
pdf("Depth-Gas Wetness Plot.pdf", encoding = "MacRoman")
depths.m <- seq(2500,0,-500)
depths.ft <- signif(depths.m/.3048, 3)
par(mar = c(5,7,3,7))
plot(-10,-10, xlim = c(0,60), ylim = c(2600,0), xlab = "Gas Wetness (%)", ylab = "Depth (m)", cex.lab = 1.25)
axis(4, at = depths.m, lab = depths.ft)
mtext("Depth (ft)", 4, line = 3, cex = 1.25)
grid()
with(WW1, points(Wetness, Depth*.3048, pch = 21, col = "black", bg = alpha("blue", 0.65), cex = 1.5))
with(COAL, points(mean(Wetness), mean(Depth)*.3048, pch = 21, col = "black", bg = "orange", cex = 2))
with(OG1, points(Wetness, Perf_Avg*.3048, pch = 21, col = "black", bg = alpha("red", 0.65), cex = 1.5))
legend("topright", c("Groundwater", "Coal", "Production Gas"), pch = 21, pt.cex = 1.5, 
       col = "black", pt.bg = c(alpha("blue", 0.65), "orange", alpha("red", 0.65)), bg = "white")
box()
dev.off()

#====================================================================================
#DEPTH - LONGITUDE PLOT
#====================================================================================
pdf("Depth-Longitude Plot.pdf", encoding = "MacRoman")
par(mar = c(5,7,3,7))
plot(0,0, xlim = c(-105.2, -102), ylim = c(2600,0), xlab = "Longitude", ylab = "Depth(m)", 
     cex.lab = 1.25); grid()
axis(4, at = depths.m, lab = depths.ft)
mtext("Depth (feet)", 4, line = 3, cex = 1.25)
with(WW1, points(Long83, Depth*.3048, pch = 21, col = "black", bg = alpha("blue", 0.65), cex = 1.5))
with(COAL, points(long, Depth*.3048, pch = 21, col = "black", bg = "orange", cex = 2))
with(OG1[OG1$Prod_Int == "Sussex",], points(long, Perf_Avg*.3048, pch = 21, col = "black", bg = alpha(fm.cols.og[1], 0.75), cex = 1.5))
with(OG1[OG1$Prod_Int == "Niobrara",], points(long, Perf_Avg*.3048, pch = 21, col = "black", bg = alpha(fm.cols.og[2], 0.75), cex = 1.5))
with(OG1[OG1$Prod_Int == "Codell/Niobrara",], points(long, Perf_Avg*.3048, pch = 21, col = "black", bg = alpha(fm.cols.og[3], 0.75), cex = 1.5))
with(OG1[OG1$Prod_Int == "J Sand/Codell/Niobrara",], points(long, Perf_Avg*.3048, pch = 21, col = "black", bg = alpha(fm.cols.og[4], 0.75), cex = 1.5))
with(OG1[OG1$Prod_Int == "Codell",], points(long, Perf_Avg*.3048, pch = 21, col = "black", bg = alpha(fm.cols.og[5], 0.75), cex = 1.5))
with(OG1[OG1$Prod_Int == "J Sand/Codell",], points(long, Perf_Avg*.3048, pch = 21, col = "black", bg = alpha(fm.cols.og[6], 0.75), cex = 1.5))
with(OG1[OG1$Prod_Int == "J Sand",], points(long, Perf_Avg*.3048, pch = 21, col = "black", bg = alpha(fm.cols.og[7], 0.75), cex = 1.5))
legend("bottomright", c("Groundwater", "Coal", fms.list.og), pch = 21, pt.cex = 1.5, col = "black", pt.bg = c(alpha("blue", 0.65), "orange", alpha(fm.cols.og, 0.75)))
dev.off()

#====================================================================================
#DEPTH PLOT ... D13C1
#Note: no dawson or high plains data with isotopes
#====================================================================================
aquifers.tmp <- aquifers[c(1,3,5,6,7,8)]
aq.labs.tmp <- aq.labs[c(1,3,5,6,7,8)]
depths.m <- seq(2500,0,-500)
depths.ft <- signif(depths.m/.3048, 3)

pdf("Depth - d13c1 by aquifer.pdf", encoding = "MacRoman", width = 3.375, height=3.5 )
par(omi=c(0,0,0,0), mar = c(4,4,3,0.2), cex=.6, mgp=c(2.75,1,0))
plot(1, 1, xlim = c(-90,-35), ylim = c(9000,0),
     xlab = d13c1.lab <-    expression(paste(~delta^13*C[C[1]]," (", "\u2030", ")")),       
     ylab = "depth (feet)", cex.lab = 1.1)
polygon(x=c(-100,-100,-60,-60), y=c(-1000,10000,10000,-1000), col = "gray93", lty=0 )
polygon(x=c(-55,-55,-30,-30), y=c(-1000,10000,10000,-1000), col = "gray93", lty=0 )
grid(); box()
for(j in c(1:7)){
  with(OG1[OG1$Prod_Int == fms.list.og[j],], points(d13C1, Perf_Avg, pch = 21, col = "black", bg = fm.cols[j], cex = 1.5))
}  
for(i in c(6:1)){
  with(WW1[WW1$Aquifer == aquifers.tmp[i],], points(d13c1, Depth, pch = 21, col = "black", bg = aq.cols[i], cex = 1.5))
}
#with(COAL, points(d13C1, Depth, pch = 21, col = "black", bg = "orange", cex = 2.5))
#legend("topright", title="Groundwater:", title.adj=0.2,  aq.labs.tmp, cex=.9, pch = 21, pt.cex = 1.5, col = "black", pt.bg = aq.cols, bg="white")
#legend("bottomleft", title="Production Gas:", title.adj=0.2,  fms.list.og, cex=.9, pch = 21,pt.cex = 1.5, col = "black", pt.bg = fm.cols, bg="white")
box()
dev.off()





#====================================================================================
#MAP - DATA DISTRIBUTION
#====================================================================================
pdf("Map - Data Distribution.pdf", encoding = "MacRoman")
map.text('county', 'colorado', xlim =c(-105.5, -102), ylim = c(38.5,41))
map.axes()
with(WW1,  points(Long83, Lat83, pch = 21, col = "black", bg = alpha("blue", 0.65), cex = 1.5))
with(COAL, points(long, lat, pch = 21, col = "black", bg = "orange", cex = 2))
with(OG1, points(long, lat, pch = 21, col = "black", bg = alpha("red", 0.65), cex = 1.5))
legend("bottomright", c("Groundwater", "Coal", "Production Gas"), pch = 21, pt.cex = 1.5, 
       col = "black", pt.bg = c(alpha("blue", 0.65), "orange", alpha("red", 0.65)), bg = "white")
dev.off()

#====================================================================================
#BERNARD PLOT... METHANE
#Note: no dawson or high plains data with isotopes
#====================================================================================
aquifers.tmp <- aquifers[c(1,3,5,6,7,8)]
aq.labs.tmp <- aq.labs[c(1,3,5,6,7,8)]

#Errors (from error analysis script... seperately for microbial and thermogenic populations)
err.my <- c(100*1.42, 100*(1-.42)) 
err.mx <- c(-60-1.2, -60+1.2)
err.ty <- c(50*1.49, 50*(1-.42))
err.tx <- c(-55-4.69, -55+4.69)

#Calculate mixing line (end-members):
m.d13c <- c(-72, -47)
m.c1 <- c(99.9,85)
m1 <- lm(m.c1 ~ m.d13c)
new <- data.frame(m.d13c = seq(-72,-46,.05))
pred <- predict(m1, new)
mixingline <- data.frame("d13c"=new, "dryness"=pred/(100-pred) )

#Calculate Oxidation
Bi <- 1000
Bt <- 50
dCi <- -72
dCt <- c(-60, -50, -40)
epsilon <- (log(Bt) - log(Bi)) / ( (log(Bt) - log(Bi)) + ((dCt - dCi)/1000) ) 
m1 <- lm(dCt~epsilon)
predict(m1, data.frame(epsilon = c(1.005, 1.01)))

par(fin=c(8.125000, 3.006944))  #< default par
par(fin=c(8.125000, 2.6))  #< alt par

pdf("Bernard plot - methane by aquifer2.pdf", encoding = "MacRoman", width = 3.375, height=3.5 )
par(omi=c(0,0,0,0), mar = c(4,4,4,0.2), cex=.6, mgp=c(2.75,1,0))
plot(1, 1, xlim = c(-90,-38), ylim = c(2,5000), yaxt = "n",log = "y",
     xlab = d13c1.lab <- expression(paste(~delta^13*C[C[1]]," (", "\u2030", ")")),       
      ylab = dryness.lab, cex.lab = 1.1)
polygon(x=c(-100,-100,-60,-60), y=c(100,100000,100000,100), col = "gray93", lty=0 )
polygon(x=c(-55,-55,-30,-30), y=c(50,.1,.1,50), col = "gray93", lty=0 )
axis(2, c(1,10,100,1000,10000), c("1", "10", "100", "1000", "10000"))
axis(2,c(5,50,500,5000), lab = NA)
abline(h=c(10,50,100,500,1000,5000,10000), col = "lightgray", lty = "dotted")
abline(v=seq(-80,-40,10), col = "lightgray", lty = "dotted")
#error bars  (show microbial error bar only)
#lines(c(err.mx[1],err.mx[2]), c(100,100), lwd=3); lines(c(-60,-60), c(err.my[1],err.my[2]), lwd=3)
#lines(c(err.tx[1],err.tx[2]), c(50,50), lwd=3); lines(c(-55,-55), c(err.ty[1],err.ty[2]), lwd=3)
#Mixing trend
lines(mixingline[,1], mixingline[,2], lwd=2, lty=1)  
#oxidation trend
arrows(dCi, Bi, -57.10128, Bt, length=.05)
text(-59, 100, expression(paste(~alpha, " = 1.005")), srt=-49, cex=.8)
arrows(dCi, Bi, -42.34203, Bt, length=.05)
text(-47, 100, expression(paste(~alpha, " = 1.010")), srt=-34, cex=.8)
for(j in c(1:7)){
  with(OG1[OG1$Prod_Int == fms.list.og[j],], points(d13C1, C1/(C2+C3), pch = 21, col = "black", bg = fm.cols[j], cex = 1.5))
}  
for(i in c(6:1)){
with(WW1[WW1$Aquifer == aquifers.tmp[i],], points(d13c1, c1/(c2+c3), pch = 21, col = "black", bg = aq.cols[i], cex = 1.5))
}
legend("topright", title="Groundwater:", title.adj=0.2,  aq.labs.tmp, cex=.9, pch = 21, pt.cex = 1.5, col = "black", pt.bg = aq.cols, bg="white")
legend("bottomleft", title="Production Gas:", title.adj=0.2,  fms.list.og, cex=.9, pch = 21,pt.cex = 1.5, col = "black", pt.bg = fm.cols, bg="white")
text(-45,37, "Thermogenic")
text(-75,5100, "Microbial")
box()
dev.off()


#====================================================================================
#BERNARD PLOT... OSBORN VS COGCC
#====================================================================================
SRN <- read.csv("Osborn Data.csv")
pdf("COGCC vs Osborn Bernard.pdf", encoding ="MacRoman")
par(omi=c(0,0,0,0), mar = c(4,4,3,0.2), cex=1, mgp=c(2.75,1,0))
plot(1, 1, xlim = c(-90,-38), ylim = c(2,5000), yaxt = "n",log = "y",
     xlab = d13c1.lab <- expression(paste(~delta^13*C[C[1]]," (", "\u2030", ")")),       
     ylab = dryness.lab, cex.lab = 1.1)
polygon(x=c(-100,-100,-60,-60), y=c(100,100000,100000,100), col = "gray93", lty=0 )
polygon(x=c(-55,-55,-30,-30), y=c(50,.1,.1,50), col = "gray93", lty=0 )
axis(2, c(1,10,100,1000,10000), c("1", "10", "100", "1000", "10000"))
axis(2,c(5,50,500,5000), lab = NA)
abline(h=c(10,50,100,500,1000,5000,10000), col = "lightgray", lty = "dotted")
abline(v=seq(-80,-40,10), col = "lightgray", lty = "dotted")
with(WW1, points(d13c1, c1/(c2+c3), pch = 21, col = "black", bg = "blue", cex = 1.5))
with(SRN, points(d13c1, dryness, pch = 22, col = "black", bg = "red", cex = 3))
legend("topright", c("COGCC Data", "Osborn Data"), pch = c(21,22), pt.cex = c(1.5,3), col = "black", pt.bg = c("blue", "red"))
dev.off()

#====================================================================================
#c1aq PLOT... OSBORN VS COGCC
#====================================================================================
SRN <- read.csv("Osborn Data.csv")
pdf("COGCC vs Osborn c1aq.pdf", encoding ="MacRoman")
par(mar = c(4, 3, 1, 0.5), fig = c(0, 1, 0.5, 1), mgp = c(2,1,0))
hist(WW1$c1aq, col="blue", xlab = "Dissolved methane (mg/l)", cex=1, main = "COGCC Data")
text(20, 600, "median = 0.12; max = 39")
par(mar = c(4, 3, 1, 0.5), fig = c(0, 1, 0, 0.5), new=TRUE)
hist(SRN$c1aq, col="red", xlab = "Dissolved methane (mg/l)", cex=1, main = "OSBORN Data")
text(20, 20, "median = 0.045; max = 40")
dev.off()


#====================================================================================
#BERNARD PLOT... ETHANE
#====================================================================================
pdf("Bernard plot - ethane.pdf", encoding = "MacRoman")
par(mar = c(5,5,3,3))
plot(1, 1, xlim = c(-55,-20), ylim = c(1,10000), log = "y", yaxt = "n", 
     xlab = d13c2.lab, ylab = "C1/(C2+C3)", cex.lab = 1.25)
polygon(x=c(-60,-60,-38,-38), y=c(50,100000,100000,50), col = "gray93", lty=0 )
polygon(x=c(-35,-35,-10,-10), y=c(50,.1,.1,50), col = "gray93", lty=0 )
axis(2, c(1,10,100,1000,10000), c("1", "10", "100", "1000", "10000"))
axis(2,c(5,50,500,5000), )
abline(h=c(10,100,1000,10000), col = "lightgray", lty = "dotted")
abline(v=seq(-80,-40,10), col = "lightgray", lty = "dotted")
with(WW1, points(d13c2, c1/(c2+c3), pch = 21, col = "black", bg = alpha("blue", 0.65), cex = 1.5))
with(OG1[OG1$Prod_Int == "Sussex",], points(d13C2, C1/(C2+C3), pch = 21, col = "black", bg = alpha(fm.cols.og[1], 0.75), cex = 1.5))
with(OG1[OG1$Prod_Int == "Niobrara",], points(d13C2, C1/(C2+C3), pch = 21, col = "black", bg = alpha(fm.cols.og[2], 0.75), cex = 1.5))
with(OG1[OG1$Prod_Int == "Codell/Niobrara",], points(d13C2, C1/(C2+C3), pch = 21, col = "black", bg = alpha(fm.cols.og[3], 0.75), cex = 1.5))
with(OG1[OG1$Prod_Int == "J Sand/Codell/Niobrara",], points(d13C2, C1/(C2+C3), pch = 21, col = "black", bg = alpha(fm.cols.og[4], 0.75), cex = 1.5))
with(OG1[OG1$Prod_Int == "Codell",], points(d13C2, C1/(C2+C3), pch = 21, col = "black", bg = alpha(fm.cols.og[5], 0.75), cex = 1.5))
with(OG1[OG1$Prod_Int == "J Sand/Codell",], points(d13C2, C1/(C2+C3), pch = 21, col = "black", bg = alpha(fm.cols.og[6], 0.75), cex = 1.5))
with(OG1[OG1$Prod_Int == "J Sand",], points(d13C2, C1/(C2+C3), pch = 21, col = "black", bg = alpha(fm.cols.og[7], 0.75), cex = 1.5))
legend("topright", c("Groundwater", "Coal", fms.list.og), pch = 21, pt.cex = 1.5, col = "black", pt.bg = c(alpha("blue", 0.65), "orange", alpha(fm.cols.og, 0.75)))
box()
dev.off()

#====================================================================================
#SCHOELL PLOT...
#====================================================================================
pdf("Schoell plot - methane.pdf", encoding = "MacRoman")
par(mar = c(5,5,3,3))
plot(1, 1, xlim = c(0,50), ylim = c(-20,-90), 
     xlab = "Gas Wetness (%)", ylab = d13c1.lab, cex.lab = 1.25)
grid()
with(OG1, points(Wetness, d13C1, pch = 21, col = "black", bg = alpha("red", 0.65), cex = 1.5))
with(WW1, points(Wetness, d13c1, pch = 21, col = "black", bg = alpha("blue", 0.65), cex = 1.5))
legend("bottomleft", c("Groundwater", "Production Gas"), pch = 21, pt.cex = 1.5, 
       col = "black", pt.bg = c(alpha(c("blue","red"), 0.65)))
coal.x <- mean(COAL$d13C1)
arrows(coal.x, 6000, coal.x, 13000, length = 0.2, angle = 30, code = 2, lwd = 2)
text(-67,7000,"Coal")
box()
dev.off()


#====================================================================================
#WHITICAR PLOT...
#====================================================================================
Wh <- readPNG("4_Plots/WhiticarFig9Edited.png")
pdf("Whiticar - methane by aquifer.pdf", encoding = "MacRoman")
par.ops <- par()
par(xaxs = "i", yaxs = "i")
par(mar = c(5,5,3,3))
plot(-100,-100, xlim = c(-450,-50), ylim = c(-10,-100), pch = "",  
     xlab = dDc1.lab, ylab = d13c1.lab, cex.lab = 1.25)
rasterImage(Wh, -451.5, -9, -49 , -101)
grid()
abline(v=c(-350,-250,-150), col = "lightgray", lty = "dotted")
for(j in c(1:7)){
  with(OG1[OG1$Prod_Int == fms.list.og[j],], points(dDC1, d13C1, pch = 21, col = "black", bg = fm.cols[j], cex = 1.5))
}  
for(i in c(6:1)){
  with(WW1[WW1$Aquifer == aquifers.tmp[i],], points(dDc1, d13c1, pch = 21, col = "black", bg = aq.cols[i], cex = 1.5))
}
legend("topright", title="Groundwater:", title.adj=0.2,  aq.labs.tmp, cex=.9, pch = 21, pt.cex = 1.5, col = "black", pt.bg = aq.cols, bg="white")
legend("bottomleft", title="Production Gas:", title.adj=0.2,  fms.list.og, cex=.9, pch = 21,pt.cex = 1.5, col = "black", pt.bg = fm.cols, bg="white")
box()
text(-343, -60, "Microbial\nFermentation", cex = .9)
text(-275, -97, "Microbial", cex=.9)
text(-275, -93.5, expression('CO'[2]*' Reduction'), cex=.9)
text(-380, -72, "Mixed Gas", cex=.9)
text(-150, -40, "Thermo-\ngenic", cex=.9)
arrows(-275, -91.5, -220, -80, lwd=1.5, length=.2)
par(par.ops) #Return to default pars
dev.off()




#====================================================================================
##GAS COMPOSITION PLOTS  #With air-free normalized data... 
#====================================================================================
#Calculate normalized concentrations (to C5)
n <- length(OG1[,1])
og.c1norm <- rep(NA,n); og.c2norm <- rep(NA,n); og.c3norm <- rep(NA,n); og.ic4norm <- rep(NA,n); og.nc4norm <- rep(NA,n); og.ic5norm <- rep(NA,n); og.nc5norm <- rep(NA,n); og.alkanesum <- rep(NA,n)
for(i in c(1:n)){
  og.alkanesum[i] <- with(OG1[i,], sum(C1,C2,C3,iC4,nC4,iC5,nC5, na.rm=TRUE))
  og.c1norm[i] <- 100*OG1$C1[i]/og.alkanesum[i]
  og.c2norm[i] <- 100*OG1$C2[i]/og.alkanesum[i]
  og.c3norm[i] <- 100*OG1$C3[i]/og.alkanesum[i]
  og.ic4norm[i] <- 100*OG1$iC4[i]/og.alkanesum[i]
  og.nc4norm[i] <- 100*OG1$nC4[i]/og.alkanesum[i]
  og.ic5norm[i] <- 100*OG1$iC5[i]/og.alkanesum[i]
  og.nc5norm[i] <- 100*OG1$nC5[i]/og.alkanesum[i]}
og <- data.frame(cbind(og.c1norm,og.c2norm,og.c3norm,og.ic4norm,og.nc4norm,og.ic5norm,og.nc5norm))
n <- length(WW1[,1])
ww.c1norm <- rep(NA,n); ww.c2norm <- rep(NA,n); ww.c3norm <- rep(NA,n); ww.ic4norm <- rep(NA,n); ww.nc4norm <- rep(NA,n); ww.ic5norm <- rep(NA,n); ww.nc5norm <- rep(NA,n); ww.alkanesum <- rep(NA,n)
for(i in c(1:n)){
  ww.alkanesum[i] <- with(WW1[i,], sum(c1,c2,c3,c4,ic4,c5,ic5))
  ww.c1norm[i] <- 100*WW1$c1[i]/ww.alkanesum[i]
  ww.c2norm[i] <- 100*WW1$c2[i]/ww.alkanesum[i]
  ww.c3norm[i] <- 100*WW1$c3[i]/ww.alkanesum[i]
  ww.ic4norm[i] <- 100*WW1$ic4[i]/ww.alkanesum[i]
  ww.nc4norm[i] <- 100*WW1$c4[i]/ww.alkanesum[i]
  ww.ic5norm[i] <- 100*WW1$ic5[i]/ww.alkanesum[i]
  ww.nc5norm[i] <- 100*WW1$c5[i]/ww.alkanesum[i]}
ww <- data.frame(cbind(ww.c1norm,ww.c2norm,ww.c3norm,ww.ic4norm,ww.nc4norm,ww.ic5norm,ww.nc5norm))
#this part gets rid of lines with breaks....
ww[ww<=0]<-NA
tmp2 <- which(is.na(ww[,2])==TRUE & is.na(ww[,c(3:7)])==FALSE)
tmp3 <- which(is.na(ww[,3])==TRUE & is.na(ww[,c(4:7)])==FALSE)
tmp4 <- which(is.na(ww[,4])==TRUE & is.na(ww[,c(5:7)])==FALSE)
tmp5 <- which(is.na(ww[,5])==TRUE & is.na(ww[,c(6:7)])==FALSE)
tmp6 <- which(is.na(ww[,6])==TRUE & is.na(ww[,7])==FALSE)
tmp <- unique(c(tmp2,tmp3,tmp4,tmp5,tmp6))
ww <- ww[-tmp,]


pdf("Chung Plot.pdf", encoding = "MacRoman", 
par(omi=c(0,0,0,0), mar=c(4,4.2,3,0.2), cex=.6, mgp=c(2.25,1,0))
plot(0,0, xlim = c(0.25,1.05), ylim = c(-78,-13 ), xaxt = "n", xlab = "1/Carbon Number",
     ylab = d13c.lab, cex.lab = 1.25)
axis(1, c(0.33, 0.5, 1), c("0.33", "0.5", "1"))
axis(1, c(0.33, 0.5, 1), c("C3", "C2", "C1"), padj = -4, cex = 1.25)



pdf("Gas Composition Plot.pdf", encoding = "MacRoman", width=3.375, height=3.8)
#par(mar = c(5,8,2,8))
par(omi=c(0,0,0,0), mar=c(4,4.2,3,0.2), cex=.6, mgp=c(2.75,1,0))
plot(0,1, xlim = c(1,7), ylim = c(0.0001,200), log = "y", xaxt = "n", yaxt = "n", xlab = "",
     ylab = "Air-free concentration (mol %)", cex.lab = 1.25)
axis(1, at = c(1:7), comp.labs[1:7], cex=1.24)
axis(2, at = c(0.0001, 0.001, 0.01, 0.1, 1, 10, 100,1000), c("0.0001", "0.001", "0.01", "0.1", "1", "10", "100","1000"), cex=1.25)
grid()
#Plot waters
for(i in c(1:length(ww[,1]))){
   lines(c(1:7), ww[i,c(1:7)], col = aq.cols[5], lwd =1)}
#Plot prod gas averages
for (i in c(1:7)){
  index <- which(OG1$Prod_Int == fms.list.og[i])
  lines(c(1:7), c(mean(og[index,1]),mean(og[index,2]),mean(og[index,3]),mean(og[index,4]),mean(og[index,5]),mean(og[index,6]),mean(og[index,7])),
                      col = fm.cols[i], lwd=3)}
legend("topright", c("Groundwater", fms.list.og), cex=.8, lty=1, lwd=4, pch="", col=c(aq.cols[5], fm.cols), bg="white")
box()
dev.off()


#====================================================================================
#CHUNG PLOTS
#====================================================================================
aquifers.tmp <- aquifers[c(1,3,5,6,7,8)]
aq.labs.tmp <- aq.labs[c(1,3,5,6,7,8)]
aq.cols <- brewer.pal(6, "GnBu")
fm.cols <- brewer.pal(7,"YlOrRd")
#Remove outliers
tmp <- which(WW1$d13c3 > -20)
WW1$d13c3[tmp] <- NA
which(WW1$d13c2 < -40 & WW1$d13c3 > -30)


pdf("Chung Plot.pdf", encoding = "MacRoman", width=3.375, height=3.8)
par(omi=c(0,0,0,0), mar=c(4,4.2,6,0.2), cex=.6, mgp=c(2.25,1,0))
plot(0,0, xlim = c(0.25,1.05), ylim = c(-78,-13 ), xaxt = "n", xlab = "1/Carbon Number",
     ylab = d13c.lab, cex.lab = 1)
axis(1, c(0.33, 0.5, 1), c("0.33", "0.5", "1"))
axis(1, c(0.33, 0.5, 1), c("C3", "C2", "C1"), padj = -4, cex = 1.25)
abline(h = c(-70,-60,-50,-40,-30,-20), col = "lightgray", lty = "dotted")
abline(v = c(0.33,0.5,1), col = "lightgray", lty = "dotted")
#Microbial groundwater polygon - all apparent microbial samples min-max
polygon(c(1,0.5,0.5,1), c(-67,-45,-52,-74), col = "grey85", border = NA)
#Thermogenic groundwater polygon - all apparent microbial samples min-max
polygon(c(1,0.5,0.33,0.33,0.5,1), c(-63.5,-34.6,-29.8,-22.4,-26.7,-44.1), col="grey85", border = NA)
#Plot waters
for(j in c(1:length(WW1[,1]))){
  tmp <- with(WW1[j,], sum(d13c1, d13c2))
  if(is.na(tmp) == FALSE){with(WW1[j,], lines(c(1,0.5,0.33), c(d13c1,d13c2,d13c3), col = aq.cols[5], lwd =1))}} 
#Plot prod gas averages
for (i in c(1:7)){
  Fm.data <- OG1[which(OG1$Prod_Int == fms.list.og[i]),]
  with(Fm.data, lines(c(1,0.5,0.33), c(mean(d13C1,na.rm=TRUE),mean(d13C2,na.rm=TRUE),mean(d13C3,na.rm=TRUE)),
       col = fm.cols[i], lwd=3))}
legend("topright", c("Groundwater", fms.list.og), cex=.85, lty=1, lwd=c(1,rep(3,7)), pch="", col=c(aq.cols[5], fm.cols), bg="white")
text(0.33,-38, "Thermogenic")
arrows(.33, -36, .35, -32, lwd=1.5, length=.1)
text(0.4,-55, "Microbial")
arrows(.42, -53, .48, -49, lwd=1.5, length=.1)
arrows(1.02, -65, 1.02, -55, lwd=1.5, code=3, length=.1)
text(1.06,-60,"Mixing", srt=90)
box()
dev.off()





#====================================================================================
#MAPS 
#====================================================================================
library(maps); library(mapdata)
#Thermogenic Gas occurrence...
map.text('county','colorado', xlim = c(-105.5,-102), ylim = c(38.5,41))
map.axes()
with(WW1, points(Long83, Lat83))
with(WW1[is.na(WW1$c1aq)==FALSE,], points(Long83, Lat83, pch = 16, col = "blue"))
with(WW1[(WW1$d13c1 > -60 | WW1$Wetness > 2) ,], points(Long83, Lat83, pch = 16, col = "red"))

#Points colored by parameter value...
Px <- "Long83"
Py <- "Lat83"
Pz <- "c1aq"
round <- 0    #for rounding legend labels
min <- 0
max <- 40
WW1 <- WW1[order(WW1$c1aq),]
map.text('county','colorado', xlim = c(-105.5,-102), ylim = c(38.5,41))
map.axes()
colbar <- colorRampPalette(c("blue", "red"))(9)[rescale(WW1[,Pz], to = c(1, 10), from = c(min, max))] 
points(WW1[,Px], WW1[,Py], pch = 21,  lwd = 0.2, cex = 1.25, bg = colbar)
leg.vals <- rev(round(seq(min, max, length.out = 9),round))
leg.cols <- rev(colorRampPalette(c("blue", "red"))(9))
legend("topright", legend = leg.vals, pch=16, cex = 1.25, col = leg.cols, title = Pz, inset=c(-0.17,0), xpd=TRUE)

Px <- "Long83"
Py <- "Lat83"
Pz <- "c1aq"
round <- 0    #for rounding legend labels
min <- 0
max <- 40
WW1 <- WW1[order(WW1$c1aq),]
map.text('county','colorado', xlim = c(-105.5,-102), ylim = c(38.5,41))
map.axes()
colbar <- colorRampPalette(c("blue", "red"))(9)[rescale(WW1[,Pz], to = c(1, 10), from = c(min, max))] 
points(WW1[,Px], WW1[,Py], pch = 21,  lwd = 0.2, cex = 1.25, bg = colbar)
leg.vals <- rev(round(seq(min, max, length.out = 9),round))
leg.cols <- rev(colorRampPalette(c("blue", "red"))(9))
legend("topright", legend = leg.vals, pch=16, cex = 1.25, col = leg.cols, title = Pz, inset=c(-0.17,0), xpd=TRUE)


#===============================================================================
#SETBACK DISTANCE
#===============================================================================
library(xlsx)
thermo <- read.xlsx("DJ basin thermogenic incidents FINAL.xlsx", 6)

#Histogram of offset distnace of wellbore failures
offsets <- as.numeric(as.vector(thermo$Distance))
offsets <- offsets[which(is.na(offsets)==FALSE)]

pdf("Setback distances.pdf", encoding = "MacRoman", width = 3.375, height=3)
par(omi=c(0,0,0,0), mar=c(4,4,3,0.2), cex=.6, mgp=c(2.75, 1,0))
hist(offsets, na.rm=TRUE, col = "grey", xlab = "Setback Distance (m)",
     ylab = "Number of impacted water wells", main = "", cex.lab =1.25, breaks = 6, na.rm=TRUE)
abline(v = median(offsets, na.rm=TRUE), lty = 2, lwd=2)
mtext("Figure 6", side=3, line=1, adj=0)
#text(mean(offsets), 4, paste("Mean = ", mean(offsets), sep = ""))
dev.off()







#===============================================================================
#FLORES PLOTS
#===============================================================================
#Fig 11:
plot(0,0, xlim = c(-90,-30), ylim = c(-40,30))
WW1.m <- WW1[WW1$MethaneSource != "thermogenic",]
WW1.t <- WW1[WW1$MethaneSource == "thermogenic",]
with(WW1.t, points(d13c1, d13co2, col = "red"))
with(WW1.m[WW1.m$Aquifer == "laramie-foxhills (confined)",], points(d13c1, d13co2, col = "blue"))
with(WW1.m[WW1.m$Aquifer == "laramie-foxhills (confined)",], points(d13c1, d13dic, col = "blue"))
with(WW1.m[WW1.m$Aquifer == "denver",], points(d13c1, d13co2, col = "green"))
with(WW1.m[WW1.m$Aquifer == "denver",], points(d13c1, d13dic, col = "green"))
lines(c(-90,-50), c(-35,5))


#Fig 12:
plot(0,0, xlim = c(-170,-50), ylim = c(-330,-100))
with(WW1.t, points(dDh2o, dDc1, col = "red"))
with(WW1.m[WW1.m$Aquifer == "laramie-foxhills (confined)",], points(dDh2o, dDc1, col = "blue"))
with(WW1.m[WW1.m$Aquifer == "denver",], points(dDh2o, dDc1, col = "green"))
lines(c(-170,-130), c(-320,-280))






#===============================================================================
#OTHER GEOCHEM PLOTS
#===============================================================================
with(WW1, plot(c1aq, he, ylim = c(0,1)))
with(WW1, plot(d13c1, he, ylim = c(0,1)))
with(WW1, plot(d13c1, tds))
with(WW1, plot(d13c1, na, ylim = c(0,1000)))
with(WW1, plot(d13c1, cl, ylim = c(0,1000)))
with(WW1, plot(d13c1, cl, ylim = c(0,1000)))
pdf("d13C vs d13co2.pdf")
with(WW1, plot(d13co2, d13c1,))
dev.off()



boxplot(na~MethaneSource, WW1, ylim = c(0,2000))
boxplot(cl~MethaneSource, WW1, ylim = c(0,500))
boxplot(tds~MethaneSource, WW1, ylim = c(0,2000))
boxplot(spcond~MethaneSource, WW1, ylim = c(0,6000))
boxplot(ph~MethaneSource, WW1, ylim = c(6,9))
boxplot(so4~MethaneSource, WW1, ylim = c(0,1000))
boxplot(d18h2o~MethaneSource, WW1)
boxplot(dDh2o~MethaneSource, WW1)
boxplot(dDc1~MethaneSource, WW1)


with(WW1, plot(c1aq, so4))




#===============================================================================
#STATS MODELS
#===============================================================================
#Proof that c1aq is not dependent on either d13C1 or dryness:
m1 <- lm(log(c1aq+1) ~ d13c1, WW1)
summary(m1)

#c1aq vs aquifer (non-transformed)
#log-transormed ANOVA
m1 <- with(WW1, aov(log(c1aq+1)~Aquifer))
plot(m1)
summary(m1)
anova(m1)
TukeyHSD(m1)

with(WW1, hist(c1aq^.5))

m2 <- kruskal.test(c1aq ~ Aquifer, data = WW1) 
pairwise.wilcox.test(WW1$c1aq, WW1$Aquifer, p.adj="bonferroni", exact=F)

m1 <- lm(c1aq~Date, WW1)
summary(m1)
anova(m1)
m2 <- lm(log(c1aq+1)~Date, WW1)
summary(m2)
anova(m2)
plot(m1)

##### FOR TABLE S3.....#####

tmp <- WW1$tss
Nsamples <- length(which(is.na(tmp) == FALSE))


Nnondetects <- length(which(tmp == 0))
mean <- mean(tmp,  == FALSE)


