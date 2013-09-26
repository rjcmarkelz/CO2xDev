
setwd("/Users/Cody/Documents/Experiments/CO2xLeaf/Paper/New Phyt Submission/CO2xDevgitrepo")
data <- read.table("co2_dev_7clusters_scaled_refined.csv", header = TRUE, sep = ',')
data2 <- read.table("fulldataset_developmental_comparisons_refined.csv", header = TRUE, sep = ',')
data3 <- read.table("Copy of Ath_AFFY_ATH1_TAIR9_Jan2010_adbl_refined.csv", header = TRUE, sep = ',')
library(data.table)
library(plyr)

head(data)
length(data)
dim(data)

head(data2)
dim(data2)

head(data3)
dim(data3)

merged <- merge (data, data2, by="Probe_Set_ID")
head(merged)
dim(merged)

merged2 <- merge(x = merged, y = data3, by="Probe_Set_ID", all.x=TRUE)
head(merged2)
dim(merged2)
tail(merged2)
colnames(merged2)
write.table(merged2, "/Users/Cody/Documents/Experiments/CO2xLeaf
	/Paper/New Phyt Submission/CO2xDevgitrepo/merged2.txt", sep="\t")

co2_sig <- subset(data, PrF_T3_co2 < 0.05)
dim(co2_sig)
#2141
co2_merged <- subset(merged2, PrF_T3_co2 < 0.05)
dim(co2_merged)
#2192

dup <- co2_merged[duplicated(co2_merged$Probe_Set_ID),]
dup <- subset(dup, select=c("Probe_Set_ID", "BINCODE", "NAME"))
colnames(dup) <- c("Probe_Set_ID", "BINCODE2", "NAME2")
dim(dup)

co2_merged_undup <- co2_merged[!duplicated(co2_merged$Probe_Set_ID),]
dim(co2_merged_undup)

dup2 <- dup[duplicated(dup$Probe_Set_ID),]
colnames(dup2) <- c("Probe_Set_ID", "BINCODE3", "NAME3")
dim(dup2)

dup3 <- dup[!duplicated(dup$Probe_Set_ID),]
dup3 <- subset(dup3, select=c("Probe_Set_ID", "BINCODE", "NAME"))
colnames(dup3) <- c("Probe_Set_ID", "BINCODE2", "NAME2")
dim(dup3)

merged3 <- merge(x = co2_merged_undup, y = dup3, by="Probe_Set_ID", all.x=TRUE )
dim(merged3)

merged4 <- merge(x = merged3, y = dup2, by="Probe_Set_ID", all.x=TRUE )
dim(merged4)
#2141

colnames(merged4)

merged4$percentchangeco2_primordia <- 100*(merged4$linearLsmean_E_primordia-merged4$linearLsmean_A_primordia)/(merged4$linearLsmean_A_primordia)
head(merged4$percentchangeco2_primordia)

merged4$percentchangeco2_expanding <- 100*(merged4$linearLsmean_E_expanding-merged4$LinearLsmean_A_expanding)/(merged4$LinearLsmean_A_expanding)
head(merged4$percentchangeco2_expanding)

merged4$percentchangeco2_fullexpand <- 100*(merged4$linearLsmean_E_fullexpand-merged4$linearLsmean_A_fullexpand)/(merged4$linearLsmean_A_fullexpand)
head(merged4$percentchangeco2_fullexpand)

write.table(merged4, "/Users/Cody/Documents/Experiments/CO2xLeaf/Paper/New Phyt Submission/CO2xDevgitrepo/final_merged.txt", sep="\t", row.names=FALSE)




















