
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

merged2 <- merge(x = merged, y = data3, by="Probe_Set_ID", all=TRUE)
head(merged2)
dim(merged2)
tail(merged2)

#CO2 main effects subset
co2_sig <- subset(data, PrF_T3_co2 < 0.05)
dim(co2_sig)
head(co2_sig)
#2141

co2_primordia <- subset(co2_sig, neglog10pvalue_A_primordia_E_primordia > 1.3)
dim(co2_primordia)

co2_expanding <- subset(co2_sig, neglog10pvalue_A_expanding_E_expanding > 1.3)
dim(co2_expanding)

co2_fullexpand <- subset(co2_sig, neglog10pvalue_A_fullexpand_E_fullexpand > 1.3)
dim(co2_fullexpand)

#CO2_dev subset
co2_dev_sig <- subset(data, PrF_T3_co2_dev < 0.05)
dim(co2_dev_sig)
head(co2_dev_sig)
#1696

co2_dev_primordia <- subset(co2_dev_sig, neglog10pvalue_A_primordia_E_primordia > 1.3)
dim(co2_dev_primordia)

co2_dev_expanding <- subset(co2_dev_sig, neglog10pvalue_A_expanding_E_expanding > 1.3)
dim(co2_dev_expanding)

co2_dev_fullexpand <- subset(co2_dev_sig, neglog10pvalue_A_fullexpand_E_fullexpand > 1.3)
dim(co2_dev_fullexpand)

#merging co2 main effects and co2 by development interaction
merged_co2<- merge(x = co2_sig, y = co2_dev_sig, by="Probe_Set_ID", all=TRUE)
dim(merged_co2)
head(merged_co2)
tail(merged_co2)

merged_co2_primordia <- merge(x = co2_primordia, y = co2_dev_primordia, by="Probe_Set_ID", all=TRUE)
dim(merged_co2_primordia)
#356

merged_co2_expanding <- merge(x = co2_expanding, y = co2_dev_expanding, by="Probe_Set_ID", all=TRUE)
dim(merged_co2_expanding)
#1395

merged_co2_fullexpand <- merge(x = co2_fullexpand, y = co2_dev_fullexpand, by="Probe_Set_ID", all=TRUE)
dim(merged_co2_fullexpand)
#1908

head(merged_co2_primordia)
merged_co2_primordia$percentchangeco2_primordia.x <- 100*(merged_co2_primordia$linearLsmean_E_primordia.x-merged_co2_primordia$linearLsmean_A_primordia.x)/(merged_co2_primordia$linearLsmean_A_primordia.x)
merged_co2_primordia$percentchangeco2_primordia.y <- 100*(merged_co2_primordia$linearLsmean_E_primordia.y-merged_co2_primordia$linearLsmean_A_primordia.y)/(merged_co2_primordia$linearLsmean_A_primordia.y)
head(merged_co2_primordia$percentchangeco2_primordia.x)
head(merged_co2_primordia$percentchangeco2_primordia.y)
write.table(merged_co2_primordia, "/Users/Cody/Documents/Experiments/CO2xLeaf/Paper/New Phyt Submission/CO2xDevgitrepo/primordia_MAPMAN.txt", sep="\t", row.names=FALSE)

head(merged_co2_expanding)
merged_co2_expanding$percentchangeco2_expanding.x <- 100*(merged_co2_expanding$linearLsmean_E_expanding.x-merged_co2_expanding$LinearLsmean_A_expanding.x)/(merged_co2_expanding$LinearLsmean_A_expanding.x)
merged_co2_expanding$percentchangeco2_expanding.y <- 100*(merged_co2_expanding$linearLsmean_E_expanding.y-merged_co2_expanding$LinearLsmean_A_expanding.y)/(merged_co2_expanding$LinearLsmean_A_expanding.y)
head(merged_co2_expanding$percentchangeco2_expanding.x)
head(merged_co2_expanding$percentchangeco2_expanding.y)
write.table(merged_co2_expanding, "/Users/Cody/Documents/Experiments/CO2xLeaf/Paper/New Phyt Submission/CO2xDevgitrepo/expanding_MAPMAN.txt", sep="\t", row.names=FALSE)

head(merged_co2_fullexpand)
merged_co2_fullexpand$percentchangeco2_fullexpand.x <- 100*(merged_co2_fullexpand$linearLsmean_E_fullexpand.x-merged_co2_fullexpand$linearLsmean_A_fullexpand.x)/(merged_co2_fullexpand$linearLsmean_A_fullexpand.x)
merged_co2_fullexpand$percentchangeco2_fullexpand.y <- 100*(merged_co2_fullexpand$linearLsmean_E_fullexpand.y-merged_co2_fullexpand$linearLsmean_A_fullexpand.y)/(merged_co2_fullexpand$linearLsmean_A_fullexpand.y)
head(merged_co2_fullexpand$percentchangeco2_fullexpand.x)
head(merged_co2_fullexpand$percentchangeco2_fullexpand.y)
write.table(merged_co2_fullexpand, "/Users/Cody/Documents/Experiments/CO2xLeaf/Paper/New Phyt Submission/CO2xDevgitrepo/fullexpanded_MAPMAN.txt", sep="\t", row.names=FALSE)
#cleaned these output tables up in excel because it was much easier to merge the cells for plotting
#appended _cleaned to the files
#use these files for the VENNDIAGRAM, see other R file.


#output for MAPMAN and VENNDIAGRAM
mapman_co2 <- merged_co2$Probe_Set_ID
head(mapman_co2)
write.table(mapman_co2, "/Users/Cody/Documents/Experiments/CO2xLeaf/Paper/New Phyt Submission/CO2xDevgitrepo/co2_dev_merged_MAPMAN.txt", sep="\t", row.names=FALSE)



#supplemental table
co2_merged <- subset(merged2, PrF_T3_co2_dev < 0.05)
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




















