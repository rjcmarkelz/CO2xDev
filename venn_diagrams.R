library(VennDiagram)
install.packages(pkgs="Vennerable",repos="http://r-forge.r-project.org/")
install.packages(c("graph", "RBGL"), dependencies=TRUE)
library(Vennerable)
setwd("/Users/Cody/Documents/Experiments/CO2xLeaf/Paper/New Phyt Submission/CO2xDevgitrepo")
setwd("/Users/Cody_2/git.repos/CO2xDev/")

primordia <- read.table("primordia_MAPMAN_cleaned.txt", header = TRUE)
expanding <- read.table("expanding_MAPMAN_cleaned.txt", header = TRUE)
mature <- read.table("fullexpanded_MAPMAN_cleaned.txt", header = TRUE)

head(primordia)
length(primordia)
str(primordia)
primordia$Probe_Set_ID <- as.character(primordia$Probe_Set_ID)
str(primordia)

expanding$Probe_Set_ID <- as.character(expanding$Probe_Set_ID)
mature$Probe_Set_ID <- as.character(mature$Probe_Set_ID)
head(expanding)
head(mature)

length(primordia$Probe_Set_ID)#356
length(expanding$Probe_Set_ID)#1395
length(mature$Probe_Set_ID)#1908

########################
########################
pri_exp <- intersect(primordia$Probe_Set_ID, expanding$Probe_Set_ID)
pri_exp_length <- length(pri_exp)
pri_exp_length #99
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
exp_mature_length #541

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
pri_mature_length #120

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
pri_exp_mat_length #34

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


#VennDiagram Package
##################
grid.newpage()
venn.plot <- draw.triple.venn(area1    = 356,
                              area2    = 1359,
                              area3    = 1908,
                              n12      = 99,
                              n23      = 541,
                              n13      = 120,
                              n123     = 34,
                              euler.d  = TRUE,
                              scaled   = TRUE,
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


##############
#Vennerable
##############


#test
venn.plot2 <- Venn(SetNames= c("Primordia","Expanding","Mature"), Weight=c(0,68,401,19,896,54,413,21))
plot(venn.plot2)












