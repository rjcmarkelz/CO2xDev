setwd("/Users/Cody/Documents/Experiments/CO2xLeaf/Paper/New Phyt Submission/CO2xDevgitrepo")

CO2xDev <- data.frame(

co2conc=factor(c(370, 370, 750, 750)),
Dev=factor(c(1,6,1,6)),

biomass_mean_aug=c(0.04199583, 0.19474417, 0.04617833, 0.25109583),
biomass_se_aug=c(0.00818658, 0.00818658, 0.00818658, 0.00818658 ),

glucose_mean_aug= c(1.73, 0.336, 2.459, 0.696),
glucose_se_aug= c(0.246, 0.213, 0.227, 0.213),

fructose_mean_aug= c(0.378957, 0.036515, 0.232, 0.01033),
fructose_se_aug= c(0.119, 0.103, 0.11, 0.103),

GFS_mean_aug= c(19.3538704, 4.9030305, 19.288133, 8.680224),
GFS_se_aug= c(0.7494291, 0.6490246, 0.6938365, 0.6490246),

starch_mean_aug= c(0, 7.5787548, 0, 14.3398727),
starch_se_aug= c(0, 0.98, 0, 0.98),

protein_mean_aug= c(3.89703358, 2.443977, 4.22210447, 2.52260079),
protein_se_aug= c(0.19310666, 0.16723527, 0.16723527, 0.16723527),

SLA_mean_aug= c(0.0492081, 0.04445763, 0.04706205, 0.04005566),
SLA_se_aug= c(0.00104331, 0.00104331, 0.00104331, 0.00104331) ,

sucrose_mean_aug = c(17.24, 4.531, 16.5967, 7.9679),
sucrose_se_aug = c(0.59, 0.51, 0.54, 0.511),

sucrose_conc_mean_aug = c(0.8484, 0.2014, 0.781, 0.319),
sucrose_conc_se_aug = c(0.0283, 0.02448, 0.0261, 0.02448),

glucose_conc_mean_aug = c(0.085247, 0.014954, 0.1157, 0.027898),
glucose_conc_se_aug = c(0.011374, 0.00985, 0.01053, 0.00985),

starch_conc_mean_aug= c(0, 0.33693, 0, 0.574393),
starch_conc_se_aug= c(0, 0.043, 0, 0.043)

)


CO2xDev  #check data frame

library(ggplot2)   			#load the ggplot2 library 
# The following code is to change the background colour using themes. ggplot has default non-black and white theme
 th = theme_bw()
theme_rect(fill = "white", colour = NA)
 theme_set(th)
 
#2013_10_08 Need to show andrew that fructose is messed up

fructoselimits <- aes(ymax = fructose_mean_aug + fructose_se_aug, ymin=fructose_mean_aug - fructose_se_aug)                  #set limits
fructoseplot <- ggplot(CO2xDev, aes(y=fructose_mean_aug, x=Dev, fill=co2conc, grid.fill="white")) +     #make a new plot in ggplot
		scale_x_discrete(name = 'Days After Germination', breaks=c('1', '6'), labels=c("23", "30")) +
		scale_y_continuous( limits = c(-0.5, 0.5), name = expression(paste('fructose (mmol', ' m'^{-2},')' )))  +
		theme(title= '', 
			pplot.title = theme_text(face='bold', size =14, hjust=.55),      #plot title size font etc.
			axis.title.x = theme_text(face='bold', size=20, hjust=.55),     #x-axis title
			axis.title.y = theme_text(angle=90, face='bold', size=24), 	    #y-axis title
			axis.text.x = theme_text(size=20, face='bold'),							    #xaxis labels size
			axis.text.y = theme_text(size=18, face='bold'),							    #yaxis label size
			#axis.ticks = theme_blank(), 									#remove tics
			panel.grid.major = theme_blank(), 								#remove major grid lines
			panel.grid.minor = theme_blank(),								#remove minor gridlines
			legend.text = theme_text(size=12),								#edit legend text
			legend.position = c(.27,.85),									#move legend
			legend.background = theme_rect( size=.5, linetype="solid")) +	#put a box around legend
		coord_cartesian(ylim=c(-0.5,0.5)) +										#make y-axis start right at 0
		geom_bar(position=position_dodge(.95), stat= "identity") +			#move over plot so they do not overlap
        geom_errorbar(fructoselimits, position=position_dodge(.95), width=0.25, colour='darkgrey') + #offset error bars
       # geom_segment(aes(x = 0, y = 5, xend= .1, yend = 5))+   #add tick marks inward…this code is not optimized yet
        scale_fill_manual(values=c('gray', 'black'), name= expression(CO[2]~"Concentration")) #bar fill colours
fructoseplot



fructoselimits <- aes(ymax = fructose_mean_aug + fructose_se_aug, ymin=fructose_mean_aug - fructose_se_aug) 
dodge  <- position_dodge(width=0.95)
plotfructose <- ggplot(data = CO2xDev, aes(fill=co2conc, y=fructose_mean_aug, x=Dev))
plotfructose <- plotfructose + geom_bar(position=dodge, stat="identity") 
plotfructose <- plotfructose + geom_errorbar(fructoselimits, position = dodge, width = 0.25) 
plotfructose <- plotfructose + xlab("Development") + ylab("Fructose")
plotfructose <- plotfructose + ggtitle("Fructose")
plotfructose


GFSdata <- read.table("CO2xDev_GFSSP_SAS.csv", header = TRUE, sep = ',', na.strings = ".")
head(GFSdata)
GFSdata$fructose_mmol_m2 <- sub("0.00000000", "NA", GFSdata$fructose_mmol_m2)
GFSdata[GFSdata == 0] <- NA

library
plot(GFSdata$fructose_mmol_m2|GFSdata$Dev)

GFSdata$fructose_mmol_m2
write.table(GFSdata, "/Users/Cody/Documents/Experiments/CO2xLeaf/Paper/New Phyt Submission/CO2xDevgitrepo/GFS_cleaned.txt", sep="\t", row.names=FALSE)

GFSdata <- read.table("GFS_cleaned.txt", header = TRUE, sep = '\t', na.strings = "NA")
str(GFSdata)
GFSdata$rep <- as.factor(GFSdata$rep)
GFSdata$CO2 <- as.factor(GFSdata$CO2)
GFSdata$Dev <- as.factor(GFSdata$Dev)

GFSdata$fructose_mmol_m2 <- as.numeric(GFSdata$fructose_mmol_m2)

GFSdata <- na.omit(GFSdata)
GFSdata
str(GFSdata)

fructose2lm <- lm(fructose_mmol_m2 ~ CO2 + Dev + CO2:Dev, data = GFSdata)
summary(fructose2lm)
# (Intercept)                0.7579     0.1675   4.524  0.00144 **
# CO2750                    -0.2165     0.2369  -0.914  0.38453   
# DevFully_expanded         -0.6867     0.2216  -3.099  0.01275 * 
# CO2750:DevFully_expanded   0.1875     0.3244   0.578  0.57739 

library(lsmeans)
library(ggplot2)
fructose2_lsmeans <- lsmeans(fructose2lm, pairwise ~ CO2|Dev)
fructose2_lsmeans_1 <- fructose2_lsmeans[[1]]
fructose2_lsmeans_1

limits <- aes(ymax= lsmean + SE, ymin = lsmean - SE)
dodge  <- position_dodge(width=0.95)
plotfructose2 <- ggplot(data = fructose2_lsmeans_1, aes(fill=CO2, y=lsmean, x=Dev, grid.fill="white"))
plotfructose2 <- plotfructose2 + geom_bar(position=dodge, stat="identity") 
plotfructose2 <- plotfructose2 + geom_errorbar(limits, position = dodge, width = 0.25, colour='darkgrey') 
plotfructose2 <- plotfructose2 + xlab("Development") 
plotfructose2 <- plotfructose2 + ggtitle("") + theme_bw() + scale_fill_manual(values = c("370" = "grey", "750" = "black"))
plotfructose2 <- plotfructose2 + theme(panel.grid = element_blank(), legend.position = "none") + geom_abline(intercept=0, slope= 0, colour= "grey")
plotfructose2 <- plotfructose2 + scale_y_continuous (name = expression(paste('Fructose (mmol', ' m'^{-2},')' )))
plotfructose2

#need to finish this plot by making axis labels larger etc.
limits <- aes(ymax= lsmean + SE, ymin = lsmean - SE)
dodge  <- position_dodge(width=0.95)
plotfructose2 <- ggplot(data = fructose2_lsmeans_1, aes(fill=CO2, y=lsmean, x=Dev, grid.fill="white"))
plotfructose2 <- plotfructose2 + geom_bar(position=dodge, stat="identity") 
plotfructose2 <- plotfructose2 + geom_errorbar(limits, position = dodge, width = 0.25, colour='darkgrey') 
plotfructose2 <- plotfructose2 + xlab("Development") 
plotfructose2 <- plotfructose2 + ggtitle("") + theme_bw() + scale_fill_manual(values = c("370" = "grey", "750" = "black"))
plotfructose2 <- plotfructose2 + theme(panel.grid = element_blank(), legend.position = "none") + geom_abline(intercept=0, slope= 0, colour= "grey")
plotfructose2 <- plotfructose2 + scale_y_continuous (name = expression(paste('Fructose (mmol', ' m'^{-2},')' )))
plotfructose2

#need to make the same plot for mass based measurements
str(GFSdata)
fructose_glm <- lm(fruc_g ~ CO2 + Dev + CO2:Dev, data = GFSdata)
summary(fructose_glm)

fructose_glm_lsmeans <- lsmeans(fructose_glm, pairwise ~ CO2|Dev)
fructose_glm_lsmeans_1 <- fructose_glm_lsmeans[[1]]
fructose_glm_lsmeans_1






#########################
#Biomass Graph August
#########################
biomasslimits <- aes(ymax = biomass_mean_aug + biomass_se_aug, ymin=biomass_mean_aug - biomass_se_aug)                  #set limits
biomassplot <- ggplot(CO2xDev, aes(y=biomass_mean_aug, x=Dev, fill=co2conc, grid.fill="white")) +     #make a new plot in ggplot
		scale_x_discrete(name = 'Days After Germination', breaks=c('1', '6'), labels=c("23", "30")) +
		scale_y_continuous( name = expression(paste('Biomass (g)' )))  +
		opts(title= '', 
			pplot.title = theme_text(face='bold', size =14, hjust=.55),      #plot title size font etc.
			axis.title.x = theme_text(face='bold', size=20, hjust=.55),     #x-axis title
			axis.title.y = theme_text(angle=90, face='bold', size=24), 	    #y-axis title
			axis.text.x = theme_text(size=20, face='bold'),							    #xaxis labels size
			axis.text.y = theme_text(size=18, face='bold'),							    #yaxis label size
			#axis.ticks = theme_blank(), 									#remove tics
			panel.grid.major = theme_blank(), 								#remove major grid lines
			panel.grid.minor = theme_blank(),								#remove minor gridlines
			legend.text = theme_text(size=12),								#edit legend text
			legend.position = c(.27,.85),									#move legend
			legend.background = theme_rect( size=.5, linetype="solid")) +	#put a box around legend
		coord_cartesian(ylim=c(0,0.299)) +										#make y-axis start right at 0
		geom_bar(position=position_dodge(.95), stat= "identity") +			#move over plot so they do not overlap
        geom_errorbar(biomasslimits, position=position_dodge(.95), width=0.25, colour='darkgrey') + #offset error bars
       # geom_segment(aes(x = 0, y = 5, xend= .1, yend = 5))+   #add tick marks inward…this code is not optimized yet
        scale_fill_manual(values=c('gray', 'black'), name= expression(CO[2]~"Concentration")) #bar fill colours
biomassplot   #view plot

################
glucoselimits <- aes(ymax = glucose_mean_aug + glucose_se_aug, ymin=glucose_mean_aug - glucose_se_aug)                  #set limits
glucoseplot <- ggplot(CO2xDev, aes(y=glucose_mean_aug, x=Dev, fill=co2conc, grid.fill="white")) +     #make a new plot in ggplot
		scale_x_discrete(name = 'Days After Germination', breaks=c('1', '6'), labels=c("23", "30")) +
		scale_y_continuous( limits = c(0, 2.99), name = expression(paste('Glucose (mmol', ' m'^{-2},')' )))  +
		opts(title= '', 
			pplot.title = theme_text(face='bold', size =14, hjust=.55),      #plot title size font etc.
			axis.title.x = theme_text(face='bold', size=20, hjust=.55),     #x-axis title
			axis.title.y = theme_text(angle=90, face='bold', size=24), 	    #y-axis title
			axis.text.x = theme_text(size=20, face='bold'),							    #xaxis labels size
			axis.text.y = theme_text(size=18, face='bold'),							    #yaxis label size
			#axis.ticks = theme_blank(), 									#remove tics
			panel.grid.major = theme_blank(), 								#remove major grid lines
			panel.grid.minor = theme_blank(),								#remove minor gridlines
			legend.text = theme_text(size=12),								#edit legend text
			legend.position = c(.27,.85),									#move legend
			legend.background = theme_rect( size=.5, linetype="solid")) +	#put a box around legend
		coord_cartesian(ylim=c(0,2.99)) +										#make y-axis start right at 0
		geom_bar(position=position_dodge(.95), stat= "identity") +			#move over plot so they do not overlap
        geom_errorbar(glucoselimits, position=position_dodge(.95), width=0.25, colour='darkgrey') + #offset error bars
       # geom_segment(aes(x = 0, y = 5, xend= .1, yend = 5))+   #add tick marks inward…this code is not optimized yet
        scale_fill_manual(values=c('gray', 'black'), name= expression(CO[2]~"Concentration")) #bar fill colours
glucoseplot   #view plot

glucose_conclimits <- aes(ymax = glucose_conc_mean_aug + glucose_conc_se_aug, ymin=glucose_conc_mean_aug - glucose_conc_se_aug)                  #set limits
glucose_concplot <- ggplot(CO2xDev, aes(y=glucose_conc_mean_aug, x=Dev, fill=co2conc, grid.fill="white")) +     #make a new plot in ggplot
		scale_x_discrete(name = 'Days After Germination', breaks=c('1', '6'), labels=c("23", "30")) +
		scale_y_continuous( limits = c(0, 0.15), name = expression(paste('Glucose (mmol', ' g DM'^{-1},')' )))  +
		opts(title= '', 
			pplot.title = theme_text(face='bold', size =14, hjust=.55),      #plot title size font etc.
			axis.title.x = theme_text(face='bold', size=20, hjust=.55),     #x-axis title
			axis.title.y = theme_text(angle=90, face='bold', size=24), 	    #y-axis title
			axis.text.x = theme_text(size=20, face='bold'),							    #xaxis labels size
			axis.text.y = theme_text(size=18, face='bold'),							    #yaxis label size
			#axis.ticks = theme_blank(), 									#remove tics
			panel.grid.major = theme_blank(), 								#remove major grid lines
			panel.grid.minor = theme_blank(),								#remove minor gridlines
			legend.text = theme_text(size=12),								#edit legend text
			legend.position = c(.29,.86),									#move legend
			legend.background = theme_rect( size=.5, linetype="solid")) +	#put a box around legend
		coord_cartesian(ylim=c(0,0.15)) +										#make y-axis start right at 0
		geom_bar(position=position_dodge(.95), stat= "identity") +			#move over plot so they do not overlap
        geom_errorbar(glucose_conclimits, position=position_dodge(.95), width=0.25, colour='darkgrey') + #offset error bars
       # geom_segment(aes(x = 0, y = 5, xend= .1, yend = 5))+   #add tick marks inward…this code is not optimized yet
        scale_fill_manual(values=c('gray', 'black'), name= expression(CO[2]~"Concentration")) #bar fill colours
glucose_concplot   #view plot

################





sucroselimits <- aes(ymax = sucrose_mean_aug + sucrose_se_aug, ymin=sucrose_mean_aug - sucrose_se_aug)                  #set limits
sucroseplot <- ggplot(CO2xDev, aes(y=sucrose_mean_aug, x=Dev, fill=co2conc, grid.fill="white")) +     #make a new plot in ggplot
		scale_x_discrete(name = 'Days After Germination', breaks=c('1', '6'), labels=c("23", "30")) +
		scale_y_continuous( limits = c(0, 24.9), name = expression(paste('Sucrose (mmol', ' m'^{-2},')' )))  +
		opts(title= '', 
			pplot.title = theme_text(face='bold', size =14, hjust=.55),      #plot title size font etc.
			axis.title.x = theme_text(face='bold', size=20, hjust=.55),     #x-axis title
			axis.title.y = theme_text(angle=90, face='bold', size=24), 	    #y-axis title
			axis.text.x = theme_text(size=20, face='bold'),							    #xaxis labels size
			axis.text.y = theme_text(size=18, face='bold'),							    #yaxis label size
			#axis.ticks = theme_blank(), 									#remove tics
			panel.grid.major = theme_blank(), 								#remove major grid lines
			panel.grid.minor = theme_blank(),								#remove minor gridlines
			legend.text = theme_text(size=12),								#edit legend text
			legend.position = c(.27,.85),									#move legend
			legend.background = theme_rect( size=.5, linetype="solid")) +	#put a box around legend
		coord_cartesian(ylim=c(0,24.9)) +										#make y-axis start right at 0
		geom_bar(position=position_dodge(.95), stat= "identity") +			#move over plot so they do not overlap
        geom_errorbar(sucroselimits, position=position_dodge(.95), width=0.25, colour='darkgrey') + #offset error bars
       # geom_segment(aes(x = 0, y = 5, xend= .1, yend = 5))+   #add tick marks inward…this code is not optimized yet
        scale_fill_manual(values=c('gray', 'black'), name= expression(CO[2]~"Concentration")) #bar fill colours
sucroseplot   #view plot



sucroseconc_limits <- aes(ymax = sucrose_conc_mean_aug + sucrose_conc_se_aug, ymin=sucrose_conc_mean_aug - sucrose_conc_se_aug)                  #set limits
sucroseconc_plot <- ggplot(CO2xDev, aes(y=sucrose_conc_mean_aug, x=Dev, fill=co2conc, grid.fill="white")) +     #make a new plot in ggplot
		scale_x_discrete(name = 'Days After Germination', breaks=c('1', '6'), labels=c("23", "30")) +
		scale_y_continuous( limits = c(0, 1.19), name = expression(paste('Sucrose (mmol', ' g DM'^{-1},')' )))  +
		opts(title= '', 
			pplot.title = theme_text(face='bold', size =14, hjust=.55),      #plot title size font etc.
			axis.title.x = theme_text(face='bold', size=20, hjust=.55),     #x-axis title
			axis.title.y = theme_text(angle=90, face='bold', size=24), 	    #y-axis title
			axis.text.x = theme_text(size=20, face='bold'),							    #xaxis labels size
			axis.text.y = theme_text(size=18, face='bold'),							    #yaxis label size
			#axis.ticks = theme_blank(), 									#remove tics
			panel.grid.major = theme_blank(), 								#remove major grid lines
			panel.grid.minor = theme_blank(),								#remove minor gridlines
			legend.text = theme_text(size=12),								#edit legend text
			legend.position = c(.27,.85),									#move legend
			legend.background = theme_rect( size=.5, linetype="solid")) +	#put a box around legend
		coord_cartesian(ylim=c(0,1.19)) +										#make y-axis start right at 0
		geom_bar(position=position_dodge(.95), stat= "identity") +			#move over plot so they do not overlap
        geom_errorbar(sucroseconc_limits, position=position_dodge(.95), width=0.25, colour='darkgrey') + #offset error bars
       # geom_segment(aes(x = 0, y = 5, xend= .1, yend = 5))+   #add tick marks inward…this code is not optimized yet
        scale_fill_manual(values=c('gray', 'black'), name= expression(CO[2]~"Concentration")) #bar fill colours
sucroseconc_plot   #view plot

fructoselimits <- aes(ymax = fructose_mean_aug + fructose_se_aug, ymin=fructose_mean_aug - fructose_se_aug)                  #set limits
fructoseplot <- ggplot(CO2xDev, aes(y=fructose_mean_aug, x=Dev, fill=co2conc, grid.fill="white")) +     #make a new plot in ggplot
		scale_x_discrete(name = 'Days After Germination', breaks=c('1', '6'), labels=c("23", "30")) +
		scale_y_continuous( limits = c(-0.5, 0.5), name = expression(paste('Fructose (mmol', ' m'^{-2},')' )))  +
		opts(title= '', 
			pplot.title = theme_text(face='bold', size =14, hjust=.55),      #plot title size font etc.
			axis.title.x = theme_text(face='bold', size=20, hjust=.55),     #x-axis title
			axis.title.y = theme_text(angle=90, face='bold', size=24), 	    #y-axis title
			axis.text.x = theme_text(size=20, face='bold'),							    #xaxis labels size
			axis.text.y = theme_text(size=18, face='bold'),							    #yaxis label size
			#axis.ticks = theme_blank(), 									#remove tics
			panel.grid.major = theme_blank(), 								#remove major grid lines
			panel.grid.minor = theme_blank(),								#remove minor gridlines
			legend.text = theme_text(size=12),								#edit legend text
			legend.position = c(.27,.85),									#move legend
			legend.background = theme_rect( size=.5, linetype="solid")) +	#put a box around legend
		coord_cartesian(ylim=c(-0.5,0.5)) +										#make y-axis start right at 0
		geom_bar(position=position_dodge(.95), stat= "identity") +			#move over plot so they do not overlap
        geom_errorbar(fructoselimits, position=position_dodge(.95), width=0.25, colour='darkgrey') + #offset error bars
       # geom_segment(aes(x = 0, y = 5, xend= .1, yend = 5))+   #add tick marks inward…this code is not optimized yet
        scale_fill_manual(values=c('gray', 'black'), name= expression(CO[2]~"Concentration")) #bar fill colours
fructoseplot





starchlimits <- aes(ymax = starch_mean_aug + starch_se_aug, ymin=starch_mean_aug - starch_se_aug)                  #set limits
starchplot <- ggplot(CO2xDev, aes(y=starch_mean_aug, x=Dev, fill=co2conc, grid.fill="white")) +     #make a new plot in ggplot
		scale_x_discrete(name = 'Days After Germination', breaks=c('1', '6'), labels=c("23", "30")) +
		scale_y_continuous( limits = c(0, 20), name = expression(paste('Starch (mmol', ' m'^{-2},')')))  +
		opts(title= '', 
			pplot.title = theme_text(face='bold', size =14, hjust=.55),      #plot title size font etc.
			axis.title.x = theme_text(face='bold', size=20, hjust=.55),     #x-axis title
			axis.title.y = theme_text(angle=90, face='bold', size=24), 	    #y-axis title
			axis.text.x = theme_text(size=20, face='bold'),							    #xaxis labels size
			axis.text.y = theme_text(size=18, face='bold'),							    #yaxis label size
			#axis.ticks = theme_blank(), 									#remove tics
			panel.grid.major = theme_blank(), 								#remove major grid lines
			panel.grid.minor = theme_blank(),								#remove minor gridlines
			legend.text = theme_text(size=12),								#edit legend text
			legend.position = c(.27,.85),									#move legend
			legend.background = theme_rect( size=.5, linetype="solid")) +	#put a box around legend
		coord_cartesian(ylim=c(0,20)) +										#make y-axis start right at 0
		geom_bar(position=position_dodge(.95), stat= "identity") +			#move over plot so they do not overlap
        geom_errorbar(starchlimits, position=position_dodge(.95), width=0.25, colour='darkgrey') + #offset error bars
       # geom_segment(aes(x = 0, y = 5, xend= .1, yend = 5))+   #add tick marks inward…this code is not optimized yet
        scale_fill_manual(values=c('gray', 'black'), name= expression(CO[2]~"Concentration")) #bar fill colours
starchplot   #view plot


starchconc_limits <- aes(ymax = starch_conc_mean_aug + starch_conc_se_aug, ymin=starch_conc_mean_aug - starch_conc_se_aug)                  
starchconc_plot <- ggplot(CO2xDev, aes(y=starch_conc_mean_aug, x=Dev, fill=co2conc, grid.fill="white")) +     #make a new plot in ggplot
		scale_x_discrete(name = 'Days After Germination', breaks=c('1', '6'), labels=c("23", "30")) +
		scale_y_continuous( limits = c(0, 0.75), name = expression(paste('Starch (mmol', ' g DM'^{-1},')')))  +
		opts(title= '', 
			pplot.title = theme_text(face='bold', size =14, hjust=.55),      #plot title size font etc.
			axis.title.x = theme_text(face='bold', size=20, hjust=.55),     #x-axis title
			axis.title.y = theme_text(angle=90, face='bold', size=24), 	    #y-axis title
			axis.text.x = theme_text(size=20, face='bold'),							    #xaxis labels size
			axis.text.y = theme_text(size=18, face='bold'),							    #yaxis label size
			#axis.ticks = theme_blank(), 									#remove tics
			panel.grid.major = theme_blank(), 								#remove major grid lines
			panel.grid.minor = theme_blank(),								#remove minor gridlines
			legend.text = theme_text(size=12),								#edit legend text
			legend.position = c(.27,.85),									#move legend
			legend.background = theme_rect( size=.5, linetype="solid")) +	#put a box around legend
		coord_cartesian(ylim=c(0,0.75)) +										#make y-axis start right at 0
		geom_bar(position=position_dodge(.95), stat= "identity") +			#move over plot so they do not overlap
        geom_errorbar(starchconc_limits, position=position_dodge(.95), width=0.25, colour='darkgrey') + #offset error bars
       # geom_segment(aes(x = 0, y = 5, xend= .1, yend = 5))+   #add tick marks inward…this code is not optimized yet
        scale_fill_manual(values=c('gray', 'black'), name= expression(CO[2]~"Concentration")) #bar fill colours
starchconc_plot   #view plot









GFSlimits <- aes(ymax = GFS_mean_aug + GFS_se_aug, ymin=GFS_mean_aug - GFS_se_aug)                  #set limits
GFSplot <- ggplot(CO2xDev, aes(y=GFS_mean_aug, x=Dev, fill=co2conc, grid.fill="white")) +     #make a new plot in ggplot
		scale_x_discrete(name = 'Days After Germination', breaks=c('1', '6'), labels=c("23", "30")) +
		scale_y_continuous( limits = c(0, 24.9), name = expression(paste('Glucose + Fructose + Sucrose (mmol', ' m'^{-2},')')))  +
		opts(title= '', 
			plot.title = theme_text(face='bold', size =14, hjust=.55),      #plot title size font etc.
			axis.title.x = theme_text(face='bold', size=20, hjust=.55),     #x-axis title
			axis.title.y = theme_text(angle=90, face='bold', size=21.5), 	    #y-axis title
			axis.text.x = theme_text(size=20, face='bold'),							    #xaxis labels size
			axis.text.y = theme_text(size=18, face='bold'),							    #yaxis label size
			#axis.ticks = theme_blank(), 									#remove tics
			panel.grid.major = theme_blank(), 								#remove major grid lines
			panel.grid.minor = theme_blank(),								#remove minor gridlines
			legend.text = theme_text(size=14),								#edit legend text
			legend.position = c(.26,.85),									#move legend
			legend.background = theme_rect( size=.5, linetype="solid")) +	#put a box around legend
		coord_cartesian(ylim=c(0,24.9)) +										#make y-axis start right at 0
		geom_bar(position=position_dodge(.95), stat= "identity") +			#move over plot so they do not overlap
        geom_errorbar(GFSlimits, position=position_dodge(.95), width=0.25, colour='darkgrey') + #offset error bars
       # geom_segment(aes(x = 0, y = 5, xend= .1, yend = 5))+   #add tick marks inward…this code is not optimized yet
        scale_fill_manual(values=c('gray', 'black'), name= expression(CO[2]~"Concentration")) #bar fill colours
GFSplot   #view plot


proteinlimits <- aes(ymax = protein_mean_aug + protein_se_aug, ymin=protein_mean_aug - protein_se_aug)                  #set limits
proteinplot <- ggplot(CO2xDev, aes(y=protein_mean_aug, x=Dev, fill=co2conc, grid.fill="white")) +     #make a new plot in ggplot
		scale_x_discrete(name = 'Days After Germination', breaks=c('1', '6'), labels=c("23", "30")) +
		scale_y_continuous( limits = c(0, 5.9), name = expression(paste('Soluble Protein (g', ' m'^{-2},')')))  +
		opts(title= '', 													# add title
			plot.title = theme_text(face='bold', size =14, hjust=.55),      #plot title size font etc.
			axis.title.x = theme_text(face='bold', size=20, hjust=.55),     #x-axis title
			axis.title.y = theme_text(angle=90, face='bold', size=24), 	    #y-axis title
			axis.text.x = theme_text(size=20, face='bold'),							    #xaxis labels size
			axis.text.y = theme_text(size=18, face='bold'),							    #yaxis label size
			#axis.ticks = theme_blank(), 									#remove tics
			panel.grid.major = theme_blank(), 								#remove major grid lines
			panel.grid.minor = theme_blank(),								#remove minor gridlines
			legend.text = theme_text(size=14),								#edit legend text
			legend.position = c(.26,.85),									#move legend
			legend.background = theme_rect( size=.5, linetype="solid")) +	#put a box around legend
		coord_cartesian(ylim=c(0,5.9)) +										#make y-axis start right at 0
		geom_bar(position=position_dodge(.95), stat= "identity") +			#move over plot so they do not overlap
        geom_errorbar(proteinlimits, position=position_dodge(.95), width=0.25, colour='darkgrey') + #offset error bars
       # geom_segment(aes(x = 0, y = 5, xend= .1, yend = 5))+   #add tick marks inward…this code is not optimized yet
        scale_fill_manual(values=c('gray', 'black'), name= expression(CO[2]~"Concentration")) #bar fill colours
proteinplot 


SLAlimits <- aes(ymax = SLA_mean_aug + SLA_se_aug, ymin=SLA_mean_aug - SLA_se_aug)                  #set limits
SLAplot <- ggplot(CO2xDev, aes(y=SLA_mean_aug, x=Dev, fill=co2conc, grid.fill="white")) +     #make a new plot in ggplot
		scale_x_discrete(name = 'Days After Germination', breaks=c('1', '6'), labels=c("23", "30")) +
		scale_y_continuous( limits = c(0, 0.08), name = expression(paste('Specific Leaf Area')))  +
		opts(title= '', 
			plot.title = theme_text(face='bold', size =14, hjust=.55),      #plot title size font etc.
			axis.title.x = theme_text(face='bold', size=14, hjust=.55),     #x-axis title
			axis.title.y = theme_text(angle=90, face='bold', size=18), 	    #y-axis title
			axis.text.x = theme_text(size=12),							    #xaxis labels size
			axis.text.y = theme_text(size=12, face='bold'),							    #yaxis label size
			#axis.ticks = theme_blank(), 									#remove tics
			panel.grid.major = theme_blank(), 								#remove major grid lines
			panel.grid.minor = theme_blank(),								#remove minor gridlines
			legend.text = theme_text(size=12),								#edit legend text
			legend.position = c(.25,.85),									#move legend
			legend.background = theme_rect( size=.5, linetype="solid")) +	#put a box around legend
		coord_cartesian(ylim=c(0,0.08)) +										#make y-axis start right at 0
		geom_bar(position=position_dodge(.95), stat= "identity") +			#move over plot so they do not overlap
        geom_errorbar(SLAlimits, position=position_dodge(.95), width=0.25, colour='darkgrey') + #offset error bars
       # geom_segment(aes(x = 0, y = 5, xend= .1, yend = 5))+   #add tick marks inward…this code is not optimized yet
        scale_fill_manual(values=c('gray', 'black'), name= expression(CO[2]~"Concentration")) #bar fill colours
SLAplot



expression_patterns <- data.frame(co2 = factor(c("A","A","A", "E", "E", "E")), time = factor(c("P","E", "M","P","E", "M"), levels=c("P","E","M")), 
                            cluster_1 = c(1, 1, 2, 1, 1, 1.5),
                            cluster_2 = c(1, 1, 1.5, 1, 1, 2),
                            cluster_3= c(3, 2, 1, 3, 2, 1),
                            cluster_4= c(2, 2, 2, 2, 2, 2),
                            cluster_5= c(3, 1.5, 1, 3, 1.5, 1),
                            cluster_6= c(2, 1, 1.5, 2, 1, 1.5),
                            cluster_7= c(1, 1.5, 3, 1, 1.5, 3),
                            cluster_8= c(1, 2, 3, 1, 2, 3),
                            cluster_9= c(1, 3, 2, 1, 3, 2),
                            cluster_10= c(2, 3, 1, 2, 3, 1)
                            )


cluster_1_plot <- ggplot(expression_patterns, aes(y=cluster_1, x=time, grid.fill="white", group = co2, colour= co2)) +     #make a new plot in ggplot
		scale_x_discrete(name = '', breaks=c("P", "E", "M"), labels=c("P", "E", "M")) +
		scale_y_continuous( limits = c(0, 3), breaks=seq(1, 3, 1), name = expression(paste('')))  +
		opts(title= '', 
			plot.title = theme_text(face='bold', size =14, hjust=.55),      #plot title size font etc.
			axis.title.x = theme_text(face='bold', size=20, hjust=.55),     #x-axis title
			axis.title.y = theme_text(angle=90, face='bold', size=24), 	    #y-axis title
			axis.text.x = theme_text(size=36, face='bold'),							    #xaxis labels size
			axis.text.y = theme_blank(),							    #yaxis label size
			axis.text.y = theme_blank(),
			panel.grid.major = theme_blank(), 								#remove major grid lines
			panel.grid.minor = theme_blank(),
			legend.position= "none")		+						#remove minor gridlines
   
		coord_cartesian(ylim=c(0.5,3.5)) +	
		geom_line(size= 3) +
        scale_colour_manual(values=c('gray', 'black')) # name= expression(CO[2]~"Concentration")) #bar fill colours
        
cluster_1_plot


cluster_2_plot <- ggplot(expression_patterns, aes(y=cluster_2, x=time, grid.fill="white", group = co2, colour= co2)) +     #make a new plot in ggplot
		scale_x_discrete(name = '', breaks=c("P", "E", "M"), labels=c("P", "E", "M")) +
		scale_y_continuous( limits = c(0, 3), breaks=seq(1, 3, 1), name = expression(paste('')))  +
		opts(title= '', 
			plot.title = theme_text(face='bold', size =14, hjust=.55),      #plot title size font etc.
			axis.title.x = theme_text(face='bold', size=20, hjust=.55),     #x-axis title
			axis.title.y = theme_text(angle=90, face='bold', size=24), 	    #y-axis title
			axis.text.x = theme_text(size=36, face='bold'),							    #xaxis labels size
			axis.text.y = theme_blank(),							    #yaxis label size
			panel.grid.major = theme_blank(), 								#remove major grid lines
			panel.grid.minor = theme_blank(),
			legend.position= "none")		+						#remove minor gridlines
       
		coord_cartesian(ylim=c(0.5,3.5)) +	
		geom_line(size= 3) +
        scale_colour_manual(values=c('gray', 'black')) # name= expression(CO[2]~"Concentration")) #bar fill colours
        
cluster_2_plot


cluster_3_plot <- ggplot(expression_patterns, aes(y=cluster_3, x=time, grid.fill="white", group = co2, colour= co2)) +     #make a new plot in ggplot
		scale_x_discrete(name = '', breaks=c("P", "E", "M"), labels=c("P", "E", "M")) +
		scale_y_continuous( limits = c(0, 3), breaks=seq(1, 3, 1), name = expression(paste('')))  +
		opts(title= '', 
			plot.title = theme_text(face='bold', size =14, hjust=.55),      #plot title size font etc.
			axis.title.x = theme_text(face='bold', size=20, hjust=.55),     #x-axis title
			axis.title.y = theme_text(angle=90, face='bold', size=24), 	    #y-axis title
			axis.text.x = theme_text(size=36, face='bold'),							    #xaxis labels size
			axis.text.y = theme_blank(),							    #yaxis label size
			panel.grid.major = theme_blank(), 								#remove major grid lines
			panel.grid.minor = theme_blank(),
			legend.position= "none")		+						#remove minor gridlines

		coord_cartesian(ylim=c(0.5,3.5)) +	
		geom_line(size= 3) +
        scale_colour_manual(values=c('gray', 'black')) # name= expression(CO[2]~"Concentration")) #bar fill colours
        
cluster_3_plot

cluster_4_plot <- ggplot(expression_patterns, aes(y=cluster_4, x=time, grid.fill="white", group = co2, colour= co2)) +     #make a new plot in ggplot
		scale_x_discrete(name = '', breaks=c("P", "E", "M"), labels=c("P", "E", "M")) +
		scale_y_continuous( limits = c(0, 3), breaks=seq(1, 3, 1), name = expression(paste('')))  +
		opts(title= '', 
			plot.title = theme_text(face='bold', size =14, hjust=.55),      #plot title size font etc.
			axis.title.x = theme_text(face='bold', size=20, hjust=.55),     #x-axis title
			axis.title.y = theme_text(angle=90, face='bold', size=24), 	    #y-axis title
			axis.text.x = theme_text(size=36, face='bold'),							    #xaxis labels size
			axis.text.y = theme_blank(),							    #yaxis label size
			panel.grid.major = theme_blank(), 								#remove major grid lines
			panel.grid.minor = theme_blank(),
			legend.position= "none")		+						#remove minor gridlines

		coord_cartesian(ylim=c(0.5,3.5)) +	
		geom_line(size= 3) +
        scale_colour_manual(values=c('gray', 'black')) # name= expression(CO[2]~"Concentration")) #bar fill colours
        
cluster_4_plot

cluster_5_plot <- ggplot(expression_patterns, aes(y=cluster_5, x=time, grid.fill="white", group = co2, colour= co2)) +     #make a new plot in ggplot
		scale_x_discrete(name = '', breaks=c("P", "E", "M"), labels=c("P", "E", "M")) +
		scale_y_continuous( limits = c(0, 3), breaks=seq(1, 3, 1), name = expression(paste('')))  +
		opts(title= '', 
			plot.title = theme_text(face='bold', size =14, hjust=.55),      #plot title size font etc.
			axis.title.x = theme_text(face='bold', size=20, hjust=.55),     #x-axis title
			axis.title.y = theme_text(angle=90, face='bold', size=24), 	    #y-axis title
			axis.text.x = theme_text(size=36, face='bold'),							    #xaxis labels size
			axis.text.y = theme_blank(),							    #yaxis label size
			panel.grid.major = theme_blank(), 								#remove major grid lines
			panel.grid.minor = theme_blank(),
			legend.position= "none")		+						#remove minor gridlines

		coord_cartesian(ylim=c(0.5,3.5)) +	
		geom_line(size= 3) +
        scale_colour_manual(values=c('gray', 'black')) # name= expression(CO[2]~"Concentration")) #bar fill colours
        
cluster_5_plot

cluster_6_plot <- ggplot(expression_patterns, aes(y=cluster_6, x=time, grid.fill="white", group = co2, colour= co2)) +     #make a new plot in ggplot
		scale_x_discrete(name = '', breaks=c("P", "E", "M"), labels=c("P", "E", "M")) +
		scale_y_continuous( limits = c(0, 3), breaks=seq(1, 3, 1), name = expression(paste('')))  +
		opts(title= '', 
			plot.title = theme_text(face='bold', size =14, hjust=.55),      #plot title size font etc.
			axis.title.x = theme_text(face='bold', size=20, hjust=.55),     #x-axis title
			axis.title.y = theme_text(angle=90, face='bold', size=24), 	    #y-axis title
			axis.text.x = theme_text(size=36, face='bold'),							    #xaxis labels size
			axis.text.y = theme_blank(),							    #yaxis label size
			panel.grid.major = theme_blank(), 								#remove major grid lines
			panel.grid.minor = theme_blank(),
			legend.position= "none")		+						#remove minor gridlines

		coord_cartesian(ylim=c(0.5,3.5)) +	
		geom_line(size= 3) +
        scale_colour_manual(values=c('gray', 'black')) # name= expression(CO[2]~"Concentration")) #bar fill colours
        
cluster_6_plot

cluster_7_plot <- ggplot(expression_patterns, aes(y=cluster_7, x=time, grid.fill="white", group = co2, colour= co2)) +     #make a new plot in ggplot
		scale_x_discrete(name = '', breaks=c("P", "E", "M"), labels=c("P", "E", "M")) +
		scale_y_continuous( limits = c(0, 3), breaks=seq(1, 3, 1), name = expression(paste('')))  +
		opts(title= '', 
			plot.title = theme_text(face='bold', size =14, hjust=.55),      #plot title size font etc.
			axis.title.x = theme_text(face='bold', size=24, hjust=.55),     #x-axis title
			axis.title.y = theme_text(angle=90, face='bold', size=24), 	    #y-axis title
			axis.text.x = theme_text(size=36, face='bold'),							    #xaxis labels size
			axis.text.y = theme_blank(),							    #yaxis label size
			panel.grid.major = theme_blank(), 								#remove major grid lines
			panel.grid.minor = theme_blank(),
			legend.position= "none")		+						#remove minor gridlines

		coord_cartesian(ylim=c(0.5,3.5)) +	
		geom_line(size= 3) +
        scale_colour_manual(values=c('gray', 'black')) # name= expression(CO[2]~"Concentration")) #bar fill colours
        
cluster_7_plot

cluster_8_plot <- ggplot(expression_patterns, aes(y=cluster_8, x=time, grid.fill="white", group = co2, colour= co2)) +     #make a new plot in ggplot
		scale_x_discrete(name = '', breaks=c("P", "E", "M"), labels=c("P", "E", "M")) +
		scale_y_continuous( limits = c(0, 3), breaks=seq(1, 3, 1), name = expression(paste('')))  +
		opts(title= '', 
			plot.title = theme_text(face='bold', size =14, hjust=.55),      #plot title size font etc.
			axis.title.x = theme_text(face='bold', size=20, hjust=.55),     #x-axis title
			axis.title.y = theme_text(angle=90, face='bold', size=24), 	    #y-axis title
			axis.text.x = theme_text(size=36, face='bold'),							    #xaxis labels size
			axis.text.y = theme_blank(),							    #yaxis label size
			panel.grid.major = theme_blank(), 								#remove major grid lines
			panel.grid.minor = theme_blank(),
			legend.position= "none")		+						#remove minor gridlines

		coord_cartesian(ylim=c(0.5,3.5)) +	
		geom_line(size= 3) +
        scale_colour_manual(values=c('gray', 'black')) # name= expression(CO[2]~"Concentration")) #bar fill colours
        
cluster_8_plot

cluster_9_plot <- ggplot(expression_patterns, aes(y=cluster_9, x=time, grid.fill="white", group = co2, colour= co2)) +     #make a new plot in ggplot
		scale_x_discrete(name = '', breaks=c("P", "E", "M"), labels=c("P", "E", "M")) +
		scale_y_continuous( limits = c(0, 3), breaks=seq(1, 3, 1), name = expression(paste('')))  +
		opts(title= '', 
			plot.title = theme_text(face='bold', size =14, hjust=.55),      #plot title size font etc.
			axis.title.x = theme_text(face='bold', size=20, hjust=.55),     #x-axis title
			axis.title.y = theme_text(angle=90, face='bold', size=24), 	    #y-axis title
			axis.text.x = theme_text(size=36, face='bold'),							    #xaxis labels size
			axis.text.y = theme_blank(),							    #yaxis label size
			panel.grid.major = theme_blank(), 								#remove major grid lines
			panel.grid.minor = theme_blank(),
			legend.position= "none")		+						#remove minor gridlines

		coord_cartesian(ylim=c(0.5,3.5)) +	
		geom_line(size= 3) +
        scale_colour_manual(values=c('gray', 'black')) # name= expression(CO[2]~"Concentration")) #bar fill colours
        
cluster_9_plot

cluster_10_plot <- ggplot(expression_patterns, aes(y=cluster_10, x=time, grid.fill="white", group = co2, colour= co2)) +     #make a new plot in ggplot
		scale_x_discrete(name = '', breaks=c("P", "E", "M"), labels=c("P", "E", "M")) +
		scale_y_continuous( limits = c(0, 3), breaks=seq(1, 3, 1), name = expression(paste('')))  +
		opts(title= '', 
			plot.title = theme_text(face='bold', size =14, hjust=.55),      #plot title size font etc.
			axis.title.x = theme_text(face='bold', size=20, hjust=.55),     #x-axis title
			axis.title.y = theme_text(angle=90, face='bold', size=24), 	    #y-axis title
			axis.text.x = theme_text(size=36, face='bold'),							    #xaxis labels size
			axis.text.y = theme_blank(),							    #yaxis label size
			panel.grid.major = theme_blank(), 								#remove major grid lines
			panel.grid.minor = theme_blank(),
			legend.position= "none")		+						#remove minor gridlines

		coord_cartesian(ylim=c(0.5,3.5)) +	
		geom_line(size= 3) +
        scale_colour_manual(values=c('gray', 'black')) # name= expression(CO[2]~"Concentration")) #bar fill colours
        
cluster_10_plot
