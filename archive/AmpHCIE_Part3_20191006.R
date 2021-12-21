##################################################################
# Titel:      Diachronic change in the Irish English amplifier system
# Part:       3
# R version:  3.5.1 (2018-07-02) -- "Feather Spray"
# Autor:      Martin Schweinberger
# Date:       2019-06-26
# Contact:    martin.schweinberger.hh@gmail.com
##################################################################
# Disclaimer: If you have questions,suggestions or you found errors
#             or in case you would to provide feedback, questions
#             write an email to martin.schweinberger.hh@gmail.com.
# Citation:   If you use this script or results thereof, please cite it as:
#             Schweinberger, Martin. 2019. "Diachronic change in 
#             the Irish English amplifier system, Part 3",
#             unpublished R script, The University of Queensland.
###############################################################
#                   START
###############################################################
# remove all lists from the current workspace
rm(list=ls(all=T))
# set wd
setwd("D:\\Uni\\Projekte\\02-Intensification\\AmpHCIE")
# load packages
library(ape)
library(ca)
library(car)
library(cfa)
library(cluster)
library(dplyr)
library(ggnetwork)
library(ggplot2)
library(gsubfn)
library(Hmisc)
library(MASS)
library(languageR)
library(lme4)
#library(plyr)
library(psych)
library(pvclust)
library(QuantPsyc)
library(readr)
library(reshape)
#install.packAges("C:/R/Rling_1.0.tar.gz", repos = NULL, type = "source")
library(Rling)
library(rms)
library(stringr)
library(tm)
library(tnet)
library(network) # keep after tnet
library(zoo)
# load self written Function
source("D:\\R/multiplot_ggplot2.R") # for multiple ggplot2 plots in one window
###############################################################
# Setting options
options(stringsAsFactors = F)
options(scipen = 999)
options(max.prAmplified=10000)
# define imAge dnzectors
imageDirectory<-"images"
###############################################################
# load data
amphcie <- read.delim("amphcie04_clean.txt", sep = "\t", header = T, fill = F, quote ="")
# inspect data
str(amphcie)

###############################################################
#                           EXAMPLES
exdata <- amphcie %>%
  dplyr::select(PreContextLong, Token, PostContext, PreContext, Function, title, date) %>%
  mutate(Example = paste(PreContextLong, Token, PostContext, sep = " ")) %>%
  mutate(ExampleClean = str_replace_all(Example, "/[A-Z]{1,3}", "")) %>%
  dplyr::select(ExampleClean, Function, PreContext, title, date) %>%
  dplyr::rename(Example = ExampleClean, File = title, Amplifier = PreContext, Date = date)
# extract predicative contexts
examples_predicative <- exdata[exdata$Function == "Predicative",]
examples_predicative <- examples_predicative[order(sample(nrow(examples_predicative), nrow(examples_predicative))),]
# variants
very_predicative <- examples_predicative[examples_predicative$Amplifier == "very",]
pretty_predicative <- examples_predicative[examples_predicative$Amplifier == "pretty",]
so_predicative <- examples_predicative[examples_predicative$Amplifier == "so",]
really_predicative <- examples_predicative[examples_predicative$Amplifier == "really",]
# extract attributive contexts
examples_attributive <- exdata[exdata$Function == "Attributive",]
examples_attributive <- examples_attributive[order(sample(nrow(examples_attributive),nrow(examples_attributive))),]
# variants
very_attributive <- examples_attributive[examples_attributive$Amplifier == "very",]
pretty_attributive <- examples_attributive[examples_attributive$Amplifier == "pretty",]
so_attributive <- examples_attributive[examples_attributive$Amplifier == "so",]
real_attributive <- examples_attributive[examples_attributive$Amplifier == "real",]
head(real_attributive)

###############################################################
#                           LD EXAMPLE TABLE
# inspect data
str(amphcie)
ldtb <- amphcie %>%
  dplyr::select(DateCat, Adjective, Variant) %>%
  dplyr::group_by(DateCat, Variant) %>%
  dplyr::summarize(AdjectiveTypes = length(table(Adjective)), AllTypes = sum(table(Adjective))) %>%
  dplyr::filter(Variant == "very") %>%
  dplyr::mutate(LD = AdjectiveTypes/AllTypes)
head(ldtb)

###############################################################
# prepare data for plotting
# create data frame with relevant variables
pd <- data.frame(amphcie$date, amphcie$Function, amphcie$Amplified, amphcie$Variant)
# clean col names
colnames(pd) <- gsub("amphcie.", "", colnames(pd))
# multiply Amplified * 100 to get percent for Variant
pd$Amplified <- ifelse(pd$Amplified == 1, 100, 0)
# convert date and Amplified Amplifiedo a numeric variables
clnm <- c("date", "Amplified")
pd[clnm] <- lapply(pd[clnm], as.numeric)
famps <- names(table(pd$Variant))[which(table(pd$Variant) > 100)]
# reclassify Adjectives - infreq. Adjectives are collapsed Amplifiedo category other
pd$Variant <- ifelse(pd$Variant  %in% famps, pd$Variant , "other")
# create variables 
pd$other <- ifelse(pd$Variant == "other", 100, 0)
pd$so <- ifelse(pd$Variant == "so", 100, 0) 
pd$very <- ifelse(pd$Variant == "very", 100, 0)
pd$zero <- ifelse(pd$Variant == "0", 100, 0)
###############################################################
# p1
p1d <- pd
# start plot: Amplified
p1 <- ggplot(p1d, aes(x = date, y = Amplified)) +
  geom_smooth(aes(y = Amplified), col = "gray30", lty = "solid", se = F, size = .5) +
  facet_grid(vars(Function)) +
  coord_cartesian(ylim = c(0, 100)) +
  labs(x = "Year", y = "Percent of Amplification") +
  theme_bw(base_size = 15) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black")) +
ggsave(file = paste(imageDirectory,"Amplified_Function.png",sep="/"), 
       width = 15, height = 10, units = c("cm"),  dpi = 320)
p1

###############################################################
# p2
p2d <- pd
# remove non-amplified instances
p2d <- p2d[p2d$Amplified != 0,]
# start plot: all
p2 <- ggplot(p2d, aes(x = date, y = very)) +
  facet_grid(vars(Function)) +
  geom_smooth(aes(y = other, color = "other", linetype = "other"), se = F, size = .5) +
  geom_smooth(aes(y = so, color = "so", linetype = "so"), se = F, size = .5) +
  geom_smooth(aes(y = very, color = "very", linetype = "very"), se = F, size = .5) +
  guides(color=guide_legend(override.aes=list(fill=NA))) +
  scale_linetype_manual(values=c("dashed", "dotted", "solid"),
                        name="Variant",
                        breaks = c("other",  "so", "very"), 
                        labels = c("other",  "so", "very")) +
  scale_colour_manual(values=c("grey20",  "grey20", "grey30"),
                      name="Variant", 
                      breaks=c("other",  "so", "very"), 
                      labels = c("other",  "so", "very")) +
  theme_set(theme_bw(base_size = 15)) +
  theme(legend.position="top", 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black")) +
  coord_cartesian(ylim = c(0, 100)) +
  labs(x = "Year", y = "Percent of Amplification") +
  guides(size = FALSE)+
  guides(alpha = FALSE)
ggsave(file = paste(imageDirectory,"Variant_Function.png",sep="/"), 
       width = 15, height = 10, units = c("cm"),  dpi = 320)
p2
###############################################################
#            WARNING: DATA REDUCTION
###############################################################
# use only perdicative contexts
ampphcie <- amphcie[amphcie$Function == "Predicative", ]
ampahcie <- amphcie[amphcie$Function == "Attributive", ]
# recode adjectives
ntfrqadj <- names(table(amphcie$Adjective))[which(table(amphcie$Adjective) <= 100)]
amphcie$AdjectiveSim <- ifelse(amphcie$Adjective %in% ntfrqadj, "other", amphcie$Adjective)
###############################################################
#              SEMANTIC VECTOR SPACE MODEL 1
# tabulate data
t1 <- tapply(amphcie$Amplified, list(amphcie$Adjective, amphcie$Variant), table)
t2 <- apply(t1, 1, function(x) ifelse(is.na(x) == T, 0, x))
t3 <- t(t2)
t3hcie <- t3
#t3hcie <- t3hcie[, 2: ncol(t3hcie)]
# remove Adjectives that were not Amplifiedensified
t3hcie <- t3hcie[rowSums(t3hcie) > 0, ]
# save row and column names
colnameshcie <- colnames(t3hcie)
rownameshcie <- rownames(t3hcie)
# turn dataframe Amplifiedo matrix
svsmhcie <- as.matrix(t3hcie)
svsmhcie <- t(svsmhcie)
svsmhcie

# compute expected values
svsmhcie.exp <- chisq.test(svsmhcie)$expected
# calculate PMI and PPMI
svsmhcie.PMI <- log2(svsmhcie/svsmhcie.exp)
svsmhcie.PPMI <- ifelse(svsmhcie.PMI < 0, 0, svsmhcie.PMI)
# calculate cosine similarity
svsmhcie.tmp1 <- svsmhcie.PPMI
svsmhcie.cos <- cossim(svsmhcie.tmp1)
#round(svsmhcie.cos, 2)
###############################################################
#               CLUSTER SEMANTIC VECTORS
# find max value that is not 1
svsmhcie.cos.test <- apply(svsmhcie.cos, 1, function(x){
  x <- ifelse(x == 1, 0, x) } )
maxval <- max(svsmhcie.cos.test)
# create distance matrix
svsmhcie.dist <- 1 - (svsmhcie.cos/maxval)
clustd <- as.dist(svsmhcie.dist)
# create distance matrix
clustd <- dist(svsmhcie.cos, method = "manhattan") 
# alternative methods
# eucledian - not good when dealing with many dimensions
# manhattan - most popular choice
# method - here the difference between points dominates
# canberra - for count data
# binary - for binary data only!
# minkowski - is not a true distance measure

# find optimal number of clusters
asw <- as.vector(unlist(sapply(2:nrow(svsmhcie)-1, function(x) pam(clustd, k = x)$silinfo$avg.width)))
# determine the optimal number of clusters (max width is optimal)
optclust <- which(asw == max(asw))+1 # optimal number of clusters

# inspect clustering with optimal number of clusters
svsmhcie.clust <- pam(clustd, optclust)
svsmhcie.clust$clustering

# create cluster object
# alternative methods: "single", "ward.D2", "average", "mcquitty", "median", "centroid"
amphciehclust <- hclust(clustd, method="average")    
# load libraries for nicer dendrograms
library(factoextra)
library(dendextend)
# plot with colored clusters
opar <- par(mar = c(5, 4, 4, 2) + 0.1)      # make a copy of current settings
par(mar = c(12, 4, 4, 2) + 0.1)
png("images/ClustAmpHCIE_fviz.png",  width = 500, height = 800) # save plot
fviz_dend(amphciehclust, k = optclust, cex = 1, horiz = T,  
          k_colors = c(rep("grey20", optclust)), 
          rect_border = c(rep("grey20", optclust)), 
          rect_fill = F,  main = "",  labels_track_height=3,  rect = F) +
  ggplot2::annotate("text", label = "not amplified", x = 11, y = -2.5, size = 5, colour = "grey")
dev.off()
ggsave(file = paste(imageDirectory,"ClustAmpHCIE_SVSM1_fviz.png",sep="/"), 
       width = 10, height = 25, units = c("cm"),  dpi = 320)
par(opar)          # restore original settings 
###############################################################
# remove outliers
#outliers <- c("much", "greatly", "highly")
#amphcie <- amphcie[!amphcie$Variant %in% outliers,]
table(amphcie$Variant)[order(table(amphcie$Variant), decreasing = T)]
###############################################################

###############################################################
###             TABULARIZATION
###############################################################
# tb 1
Varianttbhcie <- table(amphcie$Variant)
Varianttbhcie <- Varianttbhcie[order(table(amphcie$Variant), decreasing = T)]
Variantnames <- as.vector(names(Varianttbhcie))
Variantn <- as.vector(Varianttbhcie)
Variantprcnt <- round(Variantn/sum(Variantn)*100, 2)
Variantprcnt2 <-  c(0, round(Variantn[2:length(Variantn)]/sum(Variantn[2:length(Variantn)])*100, 2))
Varianttbhcie <- data.frame(Variantnames, Variantn, Variantprcnt, Variantprcnt2)
colnames(Varianttbhcie) <- c("Variant", "TokenFrequency", "PercentageSlots", "PercentAgeAmplifiedensifiers")
Varianttbhcie <- rbind(Varianttbhcie, c("Total", sum(as.vector(Varianttbhcie$TokenFrequency)), "", ""))
rownames(Varianttbhcie) <- NULL
# inspect data
head(Varianttbhcie)

# save data to disc
write.table(Varianttbhcie, "Varianttbhcie.txt", sep = "\t", row.names = F)
###############################################################
#               PLOTTING LEXICAL DIVESRITY
# load library
famps <- names(table(amphcie$Variant))[which(table(amphcie$Variant) > 100)]
p3d <- amphcie %>%
  dplyr::select(DateCat, Function, Adjective, Variant) %>%
  dplyr::filter(Variant != "0") %>%
  dplyr::mutate(VariantS = ifelse(Variant  %in% famps, Variant , "other")) %>%
  dplyr::group_by(Function, DateCat, VariantS) %>%
  dplyr::summarize(AdjectiveTypes = length(table(Adjective)), AllTypes = sum(table(Adjective))) %>%
  dplyr::mutate(LD = AdjectiveTypes/AllTypes) %>%
  dplyr::select(DateCat, Function, VariantS, LD) %>%
  tidyr::spread(VariantS, LD) %>%
  tidyr::replace_na(list(so = 0.5, other = 0.5)) %>%
  tidyr::gather(VariantS, LD, other:very) %>%
  tidyr::spread(DateCat, LD) %>%
  tidyr::replace_na(list(`1675-1750` = 0.5, other = 0.5)) %>%
  tidyr::gather(DateCat, LD, `1675-1750`:`1851-1930`) %>%
  tidyr::spread(VariantS, LD)
# create a numeric variable from datecat
p3d$DateCat <- ifelse(p3d$DateCat == "1675-1750", 1,
                      ifelse(p3d$DateCat == "1751-1850", 2,
                             ifelse(p3d$DateCat == "1851-1930", 3, p3d$DateCat)))
p3d$DateCat <- as.numeric(p3d$DateCat)
p3d$Function <- as.factor(p3d$Function)
p3d

# start plot: Amplified
p3 <- ggplot(p3d, aes(x = DateCat, y = very)) +
  facet_grid(vars(Function)) +
  geom_smooth(aes(y = other, color = "other", linetype = "other"), se = F, size = .5) +
  geom_smooth(aes(y = so, color = "so", linetype = "so"), se = F, size = .5) +
  geom_smooth(aes(y = very, color = "very", linetype = "very"), se = F, size = .5) +
  scale_linetype_manual(values=c("dashed", "dotted",  "solid"),
                        name="Variant",
                        breaks = c("other",  "so", "very"), 
                        labels = c("other",  "so", "very")) +
  scale_colour_manual(values=c("grey20", "grey20", "grey20"),
                      name="Variant", 
                      breaks=c("other", "so", "very"), 
                      labels = c("other",  "so", "very")) +
  guides(color=guide_legend(override.aes=list(fill=NA))) +
  theme_bw(base_size = 15) +
  theme(legend.position="top", 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black")) +
  #  coord_cartesian(ylim = c(0, 1)) +
  labs(x = "Period", y = "Lexical Diversity") +
  scale_x_continuous(name = "Period",
                     breaks = c(1, 2, 3),
                     labels=c("1675-1750", "1751-1850", "1851-1930"))
ggsave(file = paste(imageDirectory,"LD_VariantTime.png", sep="/"), 
       width = 15,  height = 10, units = c("cm"),  dpi = 320)
p3

##############################################################
#    COVARYING COLLEXEME ANALYSIS: CHANGE IN COLL. STRENGTH
ccd1 <- amphcie %>%
  dplyr::filter(Variant != "0")
ntfrqadj <- names(table(ccd1$Adjective))[which(table(ccd1$Adjective) < 100)]
nfvrnt <- names(table(ccd1$Variant))[which(table(ccd1$Variant) < 100)]
ccd <- amphcie %>%
  dplyr::select(Adjective, Function, Variant, DateCat) %>%
  dplyr::filter(Variant != "0") %>%
  dplyr::mutate(VariantS = ifelse(Variant %in% nfvrnt, "other", Variant)) %>%
  dplyr::mutate(AdjectiveS = ifelse(Adjective %in% ntfrqadj, "other", Adjective)) %>%
  dplyr::select(DateCat, Function, AdjectiveS, VariantS) %>%
  dplyr::group_by(DateCat, Function, AdjectiveS, VariantS) %>%
  dplyr::summarize(CollocationFrequency = n())
ccd <- ccd %>%
  dplyr::group_by(DateCat, Function) %>%
  dplyr::mutate(AdjectiveToken = sum(CollocationFrequency))
ccd <- ccd %>%
  dplyr::group_by(DateCat, Function, VariantS) %>%
  dplyr::mutate(VariantToken = sum(CollocationFrequency))
ccd <- ccd %>%
  dplyr::group_by(DateCat, Function, AdjectiveS) %>%
  dplyr::mutate(AmplifierTypeByAdjectiveType = sum(CollocationFrequency))
ccd <- ccd %>%
  dplyr::rename(Adjective = AdjectiveS, Variant = VariantS)
ccd <- ccd %>%
  dplyr::mutate(VariantAdjective = CollocationFrequency, 
                VariantOtherAdjective = (VariantToken-CollocationFrequency),
                OtherVariantAdjective = (AmplifierTypeByAdjectiveType-CollocationFrequency),
                OtherVariantOtherAdjective = AdjectiveToken-(VariantAdjective+VariantOtherAdjective+OtherVariantAdjective)) %>%
  dplyr::select(DateCat, Function, Variant, Adjective, VariantAdjective,
                VariantOtherAdjective, OtherVariantAdjective, 
                OtherVariantOtherAdjective) %>%
  dplyr::mutate(AmplifierFrequency = (VariantAdjective+VariantOtherAdjective),
                AdjectiveFrequency = (VariantAdjective+OtherVariantAdjective),
                TotalFrequency = (VariantAdjective+VariantOtherAdjective+
                                    OtherVariantAdjective+OtherVariantOtherAdjective))
ccd <- ccd %>%
  dplyr::mutate(Expected = AdjectiveFrequency*AmplifierFrequency/TotalFrequency,
                Probability = Expected/AmplifierFrequency,
                OddsRatio = as.vector(unlist(fisher.test(matrix(c(VariantAdjective, VariantOtherAdjective, OtherVariantAdjective, OtherVariantOtherAdjective), nrow = 2, byrow = T), simulate.p.value=TRUE)))[3],
                p = pbinom(VariantAdjective, AmplifierFrequency, Probability),
                Effect = round(abs(log(as.vector(unlist(p, 10)), 10)), 2))
# save data to disc
write.table(ccd, "ccd.txt", sep = "\t", row.names = F)
# continue data processing
ccd3 <- ccd %>%
  dplyr::rename(Observed = VariantAdjective) %>%
  dplyr::select(DateCat, Function, Variant, Adjective, 
                Observed, Expected, TotalFrequency, p, Effect) %>%
  dplyr::mutate(Type = ifelse(as.numeric(Observed) > as.numeric(Expected), "attr", "repel"),
                Corrected05 = 0.05/TotalFrequency,
                Corrected01 = 0.01/TotalFrequency,
                Corrected001 = 0.001/TotalFrequency,
                Bonferroni = ifelse(p <= Corrected001, "p<.001",
                                    ifelse(p <= Corrected01, "p<.01",
                                           ifelse(p <= Corrected05, "p<.05", "n.s."))))  %>%
  dplyr::select(DateCat, Function, Variant, Adjective, 
                Observed, Expected, Effect, p, Bonferroni)
# reshape data and replace missing values and include missing combinations
p4d <- ccd3 %>%
  dplyr::select(DateCat, Function, Variant, Adjective, Effect) %>%
  tidyr::spread(Variant, Effect) %>%
#  dplyr::filter(Function == "Predicative") %>%
  tidyr::replace_na(list(so = 0, other = 0)) %>%
  tidyr::gather(Variant, Effect, other:very) %>%
  tidyr::spread(DateCat, Effect) %>%
  tidyr::replace_na(list(`1675-1750` = 0, other = 0)) %>%
  tidyr::gather(DateCat, Effect, `1675-1750`:`1851-1930`) %>%
  tidyr::spread(Variant, Effect)
# create a numeric variable from datecat
p4d$DateCat <- ifelse(p4d$DateCat == "1675-1750", 1,
                      ifelse(p4d$DateCat == "1751-1850", 2,
                             ifelse(p4d$DateCat == "1851-1930", 3, p4d$DateCat)))
p4d$DateCat <- as.numeric(p4d$DateCat)
# factorize function and adjective
p4d$Function <- as.factor(p4d$Function)
p4d$Adjective <- as.factor(p4d$Adjective)
# inpsect data
p4d

# start plot
p4 <- ggplot(p4d, aes(x = DateCat, y = very)) +
  facet_grid(Adjective ~ Function, scales = "free") +
  geom_smooth(aes(y = other, color = "other", linetype = "other"), size=.5, se = F) +
  geom_smooth(aes(y = so, color = "so", linetype = "so"), size=.5, se = F) +
  geom_smooth(aes(y = very, color = "very", linetype = "very"), size=.5, se = F) +
  guides(color=guide_legend(override.aes=list(fill=NA))) +
  scale_linetype_manual(values=c("dashed", "dotted",  "solid"),
                        name="Variant",
                        breaks = c("other",  "so", "very"), 
                        labels = c("other",  "so", "very")) +
  scale_colour_manual(values=c("grey20", "grey20", "grey20"),
                      name="Variant", 
                      breaks=c("other", "so", "very"), 
                      labels = c("other",  "so", "very")) +
  theme_set(theme_bw(base_size = 15)) +
  theme(legend.position="top", 
        plot.margin = unit(c(1,1,1,1), "cm"), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"),
        panel.spacing.x = unit(3, "lines")) +
  #  coord_cartesian(ylim = c(-1, 3)) +
  labs(x = "Period", y = "Collocation Strength (LOG(p), 10)") +
  guides(size = FALSE)+
  guides(alpha = FALSE) +
  scale_x_continuous(name = "Period",
                     breaks = c(1, 2, 3),
                     labels=c("1675-1750", "1751-1850", "1851-1930"))
ggsave(file = paste(imageDirectory,"CVCLA_Function.png",sep="/"), 
       width = 20, height = 15, units = c("cm"),  dpi = 320)
p4

# only good
p5d <- p4d %>%
  filter(Adjective == "good")
p5d

# start plot
p5 <- ggplot(p5d, aes(x = DateCat, y = very)) +
  geom_smooth(aes(y = other, color = "other", linetype = "other"), size=.5, se = F) +
  geom_smooth(aes(y = so, color = "so", linetype = "so"), size=.5, se = F) +
  geom_smooth(aes(y = very, color = "very", linetype = "very"), size=.5, se = F) +
  guides(color=guide_legend(override.aes=list(fill=NA))) +
  scale_linetype_manual(values=c("dashed", "dotted",  "solid"),
                        name="Variant",
                        breaks = c("other",  "so", "very"), 
                        labels = c("other",  "so", "very")) +
  scale_colour_manual(values=c("grey20", "grey20", "grey20"),
                      name="Variant", 
                      breaks=c("other", "so", "very"), 
                      labels = c("other",  "so", "very")) +
  theme_set(theme_bw(base_size = 15)) +
  theme(legend.position="top", plot.margin = unit(c(1,1,1,1), "cm"), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black")) +
  coord_cartesian(ylim = c(0, 1)) +
  labs(x = "Period", y = "Collocation Strength (LOG(p), 10)") +
  guides(size = FALSE)+
  guides(alpha = FALSE) +
  scale_x_continuous(name = "Period",
                     breaks = c(1, 2, 3),
                     labels=c("1675-1750", "1751-1850", "1851-1930"))
ggsave(file = paste(imageDirectory,"CVCLA_good.png",sep="/"), 
       width = 15, height = 10, units = c("cm"),  dpi = 320)
p5

##############################################################
#    COVARYING COLLEXEME ANALYSIS: SIG. COLLOCATIONS
ccad1 <- amphcie %>%
  dplyr::filter(Variant != "0")
ccad <- amphcie %>%
  dplyr::select(Adjective, Function, Variant, DateCat) %>%
  dplyr::filter(Variant != "0") %>%
  dplyr::select(DateCat, Function, Adjective, Variant) %>%
  dplyr::group_by(DateCat, Function, Adjective, Variant) %>%
  dplyr::summarize(CollocationFrequency = n())
ccad <- ccad %>%
  dplyr::group_by(DateCat, Function) %>%
  dplyr::mutate(AdjectiveToken = sum(CollocationFrequency))
ccad <- ccad %>%
  dplyr::group_by(DateCat, Function, Variant) %>%
  dplyr::mutate(VariantToken = sum(CollocationFrequency))
ccad <- ccad %>%
  dplyr::group_by(DateCat, Function, Adjective) %>%
  dplyr::mutate(AmplifierTypeByAdjectiveType = sum(CollocationFrequency))
ccad <- ccad %>%
  dplyr::mutate(VariantAdjective = CollocationFrequency, 
                VariantOtherAdjective = (VariantToken-CollocationFrequency),
                OtherVariantAdjective = (AmplifierTypeByAdjectiveType-CollocationFrequency),
                OtherVariantOtherAdjective = AdjectiveToken-(VariantAdjective+VariantOtherAdjective+OtherVariantAdjective)) %>%
  dplyr::select(DateCat, Function, Variant, Adjective, VariantAdjective,
                VariantOtherAdjective, OtherVariantAdjective, 
                OtherVariantOtherAdjective) %>%
  dplyr::mutate(AmplifierFrequency = (VariantAdjective+VariantOtherAdjective),
                AdjectiveFrequency = (VariantAdjective+OtherVariantAdjective),
                TotalFrequency = (VariantAdjective+VariantOtherAdjective+
                                    OtherVariantAdjective+OtherVariantOtherAdjective))
ccad <- ccad %>%
  dplyr::mutate(Expected = AdjectiveFrequency*AmplifierFrequency/TotalFrequency,
                Probability = Expected/AmplifierFrequency,
                OddsRatio = as.vector(unlist(fisher.test(matrix(c(VariantAdjective, VariantOtherAdjective, OtherVariantAdjective, OtherVariantOtherAdjective), nrow = 2, byrow = T), simulate.p.value=TRUE)))[3],
                p = pbinom(VariantAdjective, AmplifierFrequency, Probability),
                Effect = round(abs(log(as.vector(unlist(p, 10)), 10)), 2))
# continue data processing
ccad3 <- ccad %>%
  dplyr::rename(Observed = VariantAdjective) %>%
  dplyr::select(DateCat, Function, Variant, Adjective, 
                Observed, Expected, TotalFrequency, p, Effect) %>%
  dplyr::mutate(Type = ifelse(as.numeric(Observed) > as.numeric(Expected), "attr", "repel"),
                Corrected05 = 0.05/TotalFrequency,
                Corrected01 = 0.01/TotalFrequency,
                Corrected001 = 0.001/TotalFrequency,
                Bonferroni = ifelse(p <= Corrected001, "p<.001",
                                    ifelse(p <= Corrected01, "p<.01",
                                           ifelse(p <= Corrected05, "p<.05", "n.s."))))  %>%
  dplyr::select(DateCat, Function, Variant, Adjective, 
                Observed, Expected, Effect, p, Bonferroni)

# extract significant collocations
ccad3$Bonferroni <- as.factor(ccad3$Bonferroni)
SignificantCollocationsHCIE <- ccad3 %>%
  dplyr::filter(p < .05)
SignificantCollocationsHCIE

write.table(SignificantCollocationsHCIE, "SignificantCollocationsHCIE.txt", sep = "\t", 
            row.names = F, quote = F, eol="\n")
###############################################################
solong <- amphcie %>%
  dplyr::filter(Variant == "so", Function == "Attributive") %>%
  dplyr::select(DateCat, title, author, PreContextLong, Token, PostContext, Function, Variant, Adjective)
head(solong)

###############################################################
#                  CHANGES IN Adjective FREQ
ntfrqadj <- names(table(amphcie$Adjective))[which(table(amphcie$Adjective) < 500)]
AdjectiveChangeData <- amphcie %>%
  dplyr::select(Adjective, Function, DateCat, Amplified) %>%
  dplyr::mutate(AdjectiveS = ifelse(Adjective %in% ntfrqadj, "other", Adjective)) %>%
  dplyr::group_by(DateCat, Function, AdjectiveS) %>%
  dplyr::mutate(AdjectiveTypeFrequency = n()) %>%
  dplyr::select(DateCat, Function, Amplified, AdjectiveS, AdjectiveTypeFrequency)
AdjectiveChangeData <-  AdjectiveChangeData %>%
  dplyr::group_by(DateCat, Function) %>%
  dplyr::mutate(AdjectiveFrequency = n())
AdjectiveChangeData <- AdjectiveChangeData %>%
  dplyr::mutate(RelativeFrequency = AdjectiveTypeFrequency/AdjectiveFrequency*100) %>%
  dplyr::select(DateCat, Function, AdjectiveS, RelativeFrequency) %>%
  unique()
AdjectiveChangeData <- AdjectiveChangeData %>%
  tidyr::spread(AdjectiveS, RelativeFrequency) %>%
  tidyr::replace_na(list(old = 0)) %>%
  dplyr::rename(Period = DateCat)
AdjectiveChangeData$Period <- ifelse(AdjectiveChangeData$Period == "1675-1750", 1,
                                     ifelse(AdjectiveChangeData$Period == "1751-1850", 2, 
                                            ifelse(AdjectiveChangeData$Period == "1851-1930", 3, AdjectiveChangeData$Period)))
AdjectiveChangeData$Period <- as.numeric(AdjectiveChangeData$Period) 
AdjectiveChangeData$Function <- as.factor(AdjectiveChangeData$Function)
AdjectiveChangeData  

# save data to disc
write.table(AdjectiveChangeData, "AdjectiveChangeData.txt", sep = "\t", row.names = F)

# start plot: Adjective
p6 <- ggplot(AdjectiveChangeData, aes(x = Period, y = other)) +
  facet_grid(vars(Function)) +
  geom_smooth(aes(y = dear, color = "dear", lty = "dear"), size=.5, se = F) +
  geom_smooth(aes(y = few, color = "few", lty = "few"), size=.5, se = F) +
  geom_smooth(aes(y = good, color = "good", lty = "good"), size=.5, se = F) +
  geom_smooth(aes(y = great, color = "great", lty = "great"), size=.5, se = F) +
  geom_smooth(aes(y = last, color = "last", lty = "last"), size=.5, se = F) +
  geom_smooth(aes(y = little, color = "little", lty = "little"), size=.5, se = F) +
  geom_smooth(aes(y = old, color = "old", lty = "old"), size=.5, se = F) +
  geom_smooth(aes(y = other, color = "other", lty = "other"), size=.5, se = F) +
  guides(color=guide_legend(override.aes=list(fill=NA))) +
  scale_linetype_manual(values=c("dotted", "dotted", "dashed", "dashed",  "longdash", "longdash", "solid", "solid"),
                        name="Adjective",
                        breaks = c("dear", "few", "good", "great", "last", "little", "old", "other"), 
                        labels = c("dear", "few", "good", "great", "last", "little", "old", "other")) +
  scale_colour_manual(values=c("grey80", "grey20", "grey80", "grey20", "grey80", "grey20", "grey80", "grey20"),
                      name="Adjective", 
                      breaks=c("dear", "few", "good", "great", "last", "little", "old", "other"), 
                      labels = c("dear", "few", "good", "great", "last", "little", "old", "other")) +
  theme_bw(base_size = 15) +
  theme(legend.position="top", 
        plot.margin = unit(c(1,1,1,1), "cm"), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"),
        panel.spacing.x = unit(3, "lines")) +
  coord_cartesian(ylim = c(0, 100)) +
  labs(x = "Period", y = "Percent \n (of all adjectives)") +
  scale_x_continuous(name = "Period",
                     breaks = 1:3,
                     labels= c("1675-1750", "1751-1850", "1851-1930"))
ggsave(file = paste(imageDirectory,"AdjectiveFrequency_Time_Function.png", sep="/"), 
       width = 15,  height = 12, units = c("cm"),  dpi = 320)
p6

###############################################################
#                  CHANGES IN Adjective FREQ
ntfrqadj <- names(table(amphcie$Adjective))[which(table(amphcie$Adjective) < 500)]
AmplifiedAdjectivesData <- amphcie %>%
  dplyr::select(Adjective, Function, DateCat, Amplified) %>%
  dplyr::mutate(AdjectiveS = ifelse(Adjective %in% ntfrqadj, "other", Adjective)) %>%
  dplyr::group_by(DateCat, Function, AdjectiveS) %>%
  dplyr::select(DateCat, Function, Amplified, AdjectiveS) %>%
  dplyr::mutate(AdjectiveTypeFrequency = n(), AmplifiedN = sum(Amplified)) %>%
  dplyr::mutate(RateAmplification = AmplifiedN/AdjectiveTypeFrequency*100) %>%
  dplyr::select(DateCat, Function, AdjectiveS, RateAmplification) %>%
  unique()
AmplifiedAdjectivesData <- AmplifiedAdjectivesData %>%
  tidyr::spread(AdjectiveS, RateAmplification) %>%
  tidyr::replace_na(list(old = 0)) %>%
  dplyr::rename(Period = DateCat)
AmplifiedAdjectivesData$Period <- ifelse(AmplifiedAdjectivesData$Period == "1675-1750", 1,
                                     ifelse(AmplifiedAdjectivesData$Period == "1751-1850", 2, 
                                            ifelse(AmplifiedAdjectivesData$Period == "1851-1930", 3, AdjectiveChangeData$Period)))
AmplifiedAdjectivesData$Period <- as.numeric(AmplifiedAdjectivesData$Period) 
AmplifiedAdjectivesData$Function <- as.factor(AmplifiedAdjectivesData$Function)
AmplifiedAdjectivesData  

# save data to disc
write.table(AmplifiedAdjectivesData, "AmplifiedAdjectivesData.txt", sep = "\t", row.names = F)

# start plot: Adjective
p7 <- ggplot(AmplifiedAdjectivesData, aes(x = Period, y = other)) +
  facet_grid(vars(Function)) +
  geom_smooth(aes(y = dear, color = "dear", lty = "dear"), size=.5, se = F) +
  geom_smooth(aes(y = few, color = "few", lty = "few"), size=.5, se = F) +
  geom_smooth(aes(y = good, color = "good", lty = "good"), size=.5, se = F) +
  geom_smooth(aes(y = great, color = "great", lty = "great"), size=.5, se = F) +
  geom_smooth(aes(y = last, color = "last", lty = "last"), size=.5, se = F) +
  geom_smooth(aes(y = little, color = "little", lty = "little"), size=.5, se = F) +
  geom_smooth(aes(y = old, color = "old", lty = "old"), size=.5, se = F) +
  geom_smooth(aes(y = other, color = "other", lty = "other"), size=.5, se = F) +
  guides(color=guide_legend(override.aes=list(fill=NA))) +
  scale_linetype_manual(values=c("dotted", "dotted", "dashed", "dashed",  "longdash", "longdash", "solid", "solid"),
                        name="",
                        breaks = c("dear", "few", "good", "great", "last", "little", "old", "other"), 
                        labels = c("dear", "few", "good", "great", "last", "little", "old", "other")) +
  scale_colour_manual(values=c("grey80", "grey20", "grey80", "grey20", "grey80", "grey20", "grey80", "grey20"),
                      name="", 
                      breaks=c("dear", "few", "good", "great", "last", "little", "old", "other"), 
                      labels = c("dear", "few", "good", "great", "last", "little", "old", "other")) +
  theme_bw(base_size = 15) +
  theme(legend.position="top", 
        plot.margin = unit(c(1,1,1,1), "cm"), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"),
        panel.spacing.x = unit(3, "lines")) +
  coord_cartesian(ylim = c(0, 100)) +
  labs(x = "Period", y = "Percent (Amplification)") +
  scale_x_continuous(name = "Period",
                     breaks = 1:3,
                     labels= c("1675-1750", "1751-1850", "1851-1930"))
ggsave(file = paste(imageDirectory,"AmplifiedAdjectives_Time_Function.png", sep="/"), 
       width = 15,  height = 12, units = c("cm"),  dpi = 320)
p7

###############################################################
#                  ADJECTIVE FREQUENCY
ntfrqadj <- names(table(amphcie$Adjective))[which(table(amphcie$Adjective) < 500)]
AdjectiveFrequencyData <- amphcie %>%
  dplyr::select(Adjective, Function) %>%
  dplyr::mutate(AdjectiveS = ifelse(Adjective %in% ntfrqadj, "other", Adjective)) %>%
  dplyr::group_by(Function, AdjectiveS) %>%
  dplyr::select(Function, AdjectiveS) %>%
  dplyr::mutate(AdjectiveFrequency = n()) %>%
  unique() %>%
  arrange(-AdjectiveFrequency)
AdjectiveFrequencyData  

###########################################################################
AdjectiveAge <- 1:3
Adjectivelm <- ptbadjhcie
head(Adjectivelm)

str(Adjectivelm)

nrow(Adjectivelm)
sigAdjective <- apply(Adjectivelm, 1, function(x){
  x <- lm(x ~ AdjectiveAge)
  x <- summary(x)[4][[1]][[8]]})

sigAdjectives <- which(sigAdjective < .05)
sigAdjectives

###########################################################################
#                  REGRESSION DATA SET
# WARNING!!! THEORETICAL DECISION!
#amphcie <- amphcie[amphcie$Amplified == 1,]
# remove superfluous columns
amphcie$ID <- NULL
amphcie$SpeechUnit <- NULL
amphcie$CleanSpeechUnit <- NULL
amphcie$PosTaggedSpeechUnit <- NULL
amphcie$OriginalString <- NULL
amphcie$PreContext <- NULL
amphcie$PostContext <- NULL
amphcie$txtspk <- NULL
amphcie$PreContextLong <- NULL
amphcie$Genre <- NULL
# inspect data
nrow(amphcie); str(amphcie)

###############################################################
write.table(amphcie, "icehcieamp03_regdat.txt", sep = "\t", 
            row.names = F, quote = F, eol="\n")
###############################################################
#                   END PART 3
###############################################################
