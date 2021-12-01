#Updated version - 15.07.2019
#Changelog
#Removed openxlsx functions, converted input (and output) files to CSV, fixed capitalization for input file names

#R script for identification of successful conifer root-associated fungal endophyte antagonists against Heterobasidion parviporum based on in vitro dual-culture assays
#Authors - Linda Rigerte, Kathrin Blumenstein, Eeva Terhonen (Georg-August-Universitaet Goettingen)

#Begin code

#Index of common terminology used in this script
#Fab - spherecity index of root fungal endophyte (root FE)
#Pab - spherecity index of pathogen (H. parviporum)
#FabMean/FabSD - average and standard deviation (respectively) of root FE samples (n=3)
#PabMean/PabSD - average and standard deviation of pathogen samples (n=3)

#Packages used + descriptions thereof
#For Excel raw file import/export; Note - replaced by read.table() and write.table() from the utils package; input data files are now CSV files
library(openxlsx)
#For miscellaneous tools
library(data.table)
#For miscellaneous tools
library(dplyr)
#For scatterplot3d()
library(scatterplot3d)
#For plot3d()
library(rgl)

#Changing working directory if necessary; note - all requisite pre-processed data files (.xlsx/.csv files) have to be placed in the appropriate working directory as indicated in this script
#getwd()
#setwd()


library(readxl)
tab0 <- read_excel("Book5.xlsx", sheet = 2)
tab1 <- read_excel("Book5.xlsx")

#Calculate means and SD
#Fungus spherecity mean and SD
tab1$FabMean <- apply(tab1[,7:9], 1,mean)
tab1$FabSD <- apply(tab1[, 7:9], 1, sd)
#Pathogen spherecity mean
tab1$PabMean <- apply(tab1[,10:12], 1,mean)
tab1$PabSD <- apply(tab1[, 10:12], 1, sd)

#Remove all samples from tab1 where FabMean and PabMean are NA in Timedays = 3, 7, and 10; using the tab0 set as guide for this
tab2 <- subset(tab1, tab1$sn %in% tab0$sn)


#First plot PabMean vs FabMean (y~x) to investigate for which of the samples sphericity index for the fungus is >1 AND ALSo the spherecity index for the pathogen is <1
#These are the only cases where the fungus has definitely behaved antagonistically against the pathogen
#All other samples can be considered spurious instances and can be discarded
#Write these out as separate subsets
#All samples
 #There is a data point at ~3.0 which is presumably an outlier
with(tab2, plot(PabMean~FabMean, xlim = c(0.4, 1.3), ylim = c(0.4, 1.3), col= ifelse(PabMean < 1 & FabMean > 1, "green", "red"),ylab ="mean spherical index for antagonist pathogen",xlab= "mean spherical index for antagonist fungal endophyte", pch=20))
abline(a = 0.00, b = 1)
abline(h = 1, v = 1)


#Day 4 subset
with(tab2[which(tab2$Day == 4),], plot(PabMean~FabMean, asp = 1,main="Day 4"))
abline(h = .75, v = .8)

library(dplyr)
tab4 = tab2%>%
  filter(Day == 4, FabMean > 1, PabMean < 1)

with(tab4, plot(PabMean~FabMean, asp = 1,main="Day 4/compare"))
abline(h = 0.74, v = 1.1)

#Day 7 subset
with(tab2[which(tab2$Day == 7),], plot(PabMean~FabMean, asp = 1, xlim = c(0.5, 1.3), ylim = c(0.5, 1),main="Day 7"))
abline(h = .75, v = .9)

tab7 = tab2%>%
  filter(Day == 7, FabMean > 1, PabMean < 1)


with(tab7, plot(PabMean~FabMean,  asp = 1, xlim = c(1, 1.2),main="Day 7/endo"))
abline(h = 0.73, v = 1.1)

#Day 10 subset
with(tab2[which(tab2$Day == 10),], plot(PabMean~FabMean, asp = 1,main="Day 10"))
abline(h = .65, v = .85)

tab10 = tab2%>%
  filter(Day == 10, FabMean > 1, PabMean < 1)
with(tab10, plot(PabMean~FabMean,  asp = 1,main="Day 10/endo"))
abline(h = .675, v = 1.15)


#Day 14 subset
with(tab2[which(tab2$Day == 14),], plot(PabMean~FabMean, asp = 1,main="Day 14"))
abline(h = .63, v = .725)

tab14 = tab2%>%
  filter(Day == 14, FabMean > 1, PabMean < 1)
with(tab14, plot(PabMean~FabMean,  asp = 1,main="Day 14/endo"))
abline(h = .71, v = 1.013)

