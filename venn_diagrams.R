library(VennDiagram)
install.packages(pkgs="Vennerable",repos="http://r-forge.r-project.org/")
install.packages(c("graph", "RBGL"), dependencies=TRUE)
library(Vennerable)
setwd("/Users/Cody/Documents/Experiments/CO2xLeaf/Paper/New Phyt Submission/CO2xDevgitrepo")

primordia <- read.table("mapman_co2_primordia_diff_only.txt", header = TRUE)
expanding <- read.table("mapman_co2_expanding_diff_only.txt", header = TRUE)
mature <- read.table("mapman_co2_mature_diff_only.txt", header = TRUE)

head(primordia)
length(primordia)
str(primordia)
primordia$Probe_Set_ID <- as.character(primordia$Probe_Set_ID)
str(primordia)

expanding$Probe_Set_ID <- as.character(expanding$Probe_Set_ID)
mature$Probe_Set_ID <- as.character(mature$Probe_Set_ID)
head(expanding)
head(mature)

length(primordia$Probe_Set_ID)#162
length(expanding$Probe_Set_ID)#854
length(mature$Probe_Set_ID)#1384

########################
########################
pri_exp <- intersect(primordia$Probe_Set_ID, expanding$Probe_Set_ID)
pri_exp_length <- length(pri_exp)
pri_exp_length #40
is.vector(pri_exp)
pri_exp <- data.frame(pri_exp)
pri_exp
is.data.frame(pri_exp)

pri_exp$value <- 1
head(pri_exp)
str(pri_exp)
pri_exp$pri_exp <- as.character(pri_exp$pri_exp)
colnames(pri_exp) <- c("Probe_Set_ID", "Value")
pri_exp
write.table(pri_exp,file="pri_exp_data.txt",sep="\t", quote = FALSE, 
	row.names = FALSE)

########################
########################

exp_mature <- intersect(expanding$Probe_Set_ID, mature$Probe_Set_ID)
exp_mature_length <- length(exp_mature)
exp_mature_length #434

exp_mature <- data.frame(exp_mature)
exp_mature$value <- 1
head(exp_mature)

exp_mature$exp_mature <- as.character(exp_mature$exp_mature)
colnames(exp_mature) <- c("Probe_Set_ID", "Value")
exp_mature
write.table(exp_mature,file="exp_mature_data.txt",sep="\t", quote = FALSE, 
	row.names = FALSE)

########################
########################

pri_mature <- intersect(mature$Probe_Set_ID, primordia$Probe_Set_ID)
pri_mature_length <- length(pri_mature)
pri_mature_length #75

pri_mature <- data.frame(pri_mature)
pri_mature$value <- 1
head(pri_mature)

pri_mature$pri_mature <- as.character(pri_mature$pri_mature)
colnames(pri_mature) <- c("Probe_Set_ID", "Value")
pri_mature
write.table(pri_mature,file="pri_mature_data.txt",sep="\t", quote = FALSE, 
	row.names = FALSE)

########################
########################
pri_exp_mat <- intersect(intersect(mature$Probe_Set_ID, primordia$Probe_Set_ID), 
	                                expanding$Probe_Set_ID)
pri_exp_mat_length <- length(pri_exp_mat)
pri_exp_mat_length #21

pri_exp_mat <- data.frame(pri_exp_mat)
pri_exp_mat$value <- 1
head(pri_exp_mat)

pri_exp_mat$pri_exp_mat <- as.character(pri_exp_mat$pri_exp_mat)
colnames(pri_exp_mat) <- c("Probe_Set_ID", "Value")
pri_exp_mat
write.table(pri_exp_mat,file="pri_exp_mat_data.txt",sep="\t", quote = FALSE, 
	row.names = FALSE)


########################
########################

#########
#Now not necessary, completed above 
#Merge and output for MAPMAN
#########
# cat(pri_exp,file="outfiletest.txt",sep="\n")
# merge1 <- merge(pri_exp, exp_mature, all = TRUE)
# merge1

# merge2 <- merge(merge1, pri_mature, all = TRUE)
# merge2

# merge3 <- merge(merge2, pri_exp_mat, all = TRUE)
# merge3

# write.table(merge3,file="venndiagram_data.txt",sep="\t")


#VennDiagram Package
##################
grid.newpage()
venn.plot <- draw.triple.venn(area1    = 162,
                              area2    = 854,
                              area3    = 1384,
                              n12      = 40,
                              n23      = 434,
                              n13      = 75,
                              n123     = 21,
                              category = c("Primordia", "Expanding", "Mature"),
                              cat.pos  = c(-30, 30, 250),
                              cat.dist = c(0.05, 0.05, 0.05),
                              fill     = c("blue", "red", "green"),
                              alpha    = 0.3,
                              lty      = "blank",
                              cex      = 2,
                              cat.cex  = 2,
                              cat.col  = c("blue", "red", "green"))
grid.draw(venn.plot)
















