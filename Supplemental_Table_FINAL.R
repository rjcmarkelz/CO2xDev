
setwd("/Users/Cody/Documents/Experiments/CO2xLeaf/Paper/New Phyt Submission/CO2xDevgitrepo")
data <- read.table("co2_dev_7clusters_scaled_refined.csv", header = TRUE, sep = ',')
data2 <- read.table("fulldataset_developmental_comparisons_refined.csv", header = TRUE, sep = ',')
data3 <- read.table("Copy of Ath_AFFY_ATH1_TAIR9_Jan2010_adbl_refined.csv", header = TRUE, sep = ',')

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

merged$percentchangeco2_primordia <- 100*(merged$linearLsmean_E_primordia-merged$linearLsmean_A_primordia)/(merged$linearLsmean_A_primordia)
head(merged$percentchangeco2_primordia)

merged$percentchangeco2_expanding <- 100*(merged$linearLsmean_E_expanding-merged$LinearLsmean_A_expanding)/(merged$LinearLsmean_A_expanding)
head(merged$percentchangeco2_expanding)

merged$percentchangeco2_fullexpand <- 100*(merged$linearLsmean_E_fullexpand-merged$linearLsmean_A_fullexpand)/(merged$linearLsmean_A_fullexpand)
head(merged$percentchangeco2_fullexpand)

total_sig <- merge(x= merged, y = data3, all.x=TRUE)
dim(total_sig)
head(total_sig)
tail(total_sig)
colnames(total_sig)

#Supplemental Table 2
colnames(merged)
supplemental_2 <- total_sig[,c(1, 2, 10, 11, 12, 40, 41, 42, 33)]
head(supplemental_2)
colnames(supplemental_2)
colnames(supplemental_2) <- c("Probe_Set_ID", "AT_number", "CO2_pvalue", "Dev_pvalue", "CO2byDev_pvalue", 
	"percentchangeco2_primordia", "percentchangeco2_expanding", "percentchangeco2_fullexpand", "Kmeans_cluster")

write.table(supplemental_2, "/Users/Cody/Documents/Experiments/CO2xLeaf/Paper/New Phyt Submission/CO2xDevgitrepo/Supplemental_Table_2.txt", sep="\t", row.names=FALSE)











#Supplemental Table 1
#CO2 Sig genes
co2_dev_significant <- subset(merged,PrF_T3_co2 < 0.05 | PrF_T3_co2_dev < 0.05)
dim(co2_dev_significant)
tail(co2_dev_significant)
head(co2_dev_significant)

#do this last
merged2 <- merge(x = co2_dev_significant, y = data3, by="Probe_Set_ID", all.x =TRUE)
head(merged2)
dim(merged2)
#3274
tail(merged2)


dup <- merged2[duplicated(merged2$Probe_Set_ID),]
dup <- subset(dup, select=c("Probe_Set_ID", "BINCODE", "NAME"))
colnames(dup) <- c("Probe_Set_ID", "BINCODE2", "NAME2")
dim(dup)

co2_merged_undup <- merged2[!duplicated(merged2$Probe_Set_ID),]
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
#3274

colnames(merged4)

merged4$percentchangeco2_primordia <- 100*(merged4$linearLsmean_E_primordia-merged4$linearLsmean_A_primordia)/(merged4$linearLsmean_A_primordia)
head(merged4$percentchangeco2_primordia)

merged4$percentchangeco2_expanding <- 100*(merged4$linearLsmean_E_expanding-merged4$LinearLsmean_A_expanding)/(merged4$LinearLsmean_A_expanding)
head(merged4$percentchangeco2_expanding)

merged4$percentchangeco2_fullexpand <- 100*(merged4$linearLsmean_E_fullexpand-merged4$linearLsmean_A_fullexpand)/(merged4$linearLsmean_A_fullexpand)
head(merged4$percentchangeco2_fullexpand)

#write table up to this point. Some more reshaping to come
write.table(merged4, "/Users/Cody/Documents/Experiments/CO2xLeaf/Paper/New Phyt Submission/CO2xDevgitrepo/final_merged.txt", sep="\t", row.names=FALSE)

head(merged)
#make new dataframe using column number indexes
colnames(merged4)
supplemental_1 <- merged4[,c(1,2,41,10,11,12,46,47,48,33, 40, 42, 44)]
head(supplemental_1)
colnames(supplemental_1)
colnames(supplemental_1) <- c("Probe_Set_ID", "AT_number", "Description", "CO2_pvalue",
	"Dev_pvalue", "CO2byDev_pvalue", "percentchangeco2_primordia", "percentchangeco2_expanding",
	"percentchangeco2_fullexpand", "Kmeans_cluster", "mapman_bin", "mapman_bin2", "mapman_bin3")

write.table(supplemental_1, "/Users/Cody/Documents/Experiments/CO2xLeaf/Paper/New Phyt Submission/CO2xDevgitrepo/Supplemental_table_1.txt", sep="\t", row.names=FALSE)















