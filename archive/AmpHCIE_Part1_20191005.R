##################################################################
# Titel:      Diachronic change in the Irish English amplifier system
# Part:       1
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
#             the Irish English amplifier system, Part 1",
#             unpublished R script, The University of Queensland.
###############################################################
#                   START
###############################################################
# remove all lists from the current workspace
rm(list=ls(all=T))
# set wd
setwd("D:\\Uni\\Projekte\\02-Intensification\\AmpHCIE")
# load packages
library(car)
library(gsubfn)
library(Hmisc)
library(plyr)
library(reshape)
library(rms)
library(stringr)
library(tm)
source("D:\\R/POStagObject.R") # for pos-tagging objects in R
###############################################################
# setting options
options(stringsAsFactors = F)
options(scipen = 999)
# define image directory
imageDhciectory<-"images"
# specify path to corpra
corpushciepath <- "D:\\Uni\\Korpora\\Original\\HamburgCorpusofIrishEnglish"
###############################################################
# load corpus files
corpus.files = list.files(path = corpushciepath, all.files = T, full.names = T, recursive = T, ignore.case = T, include.dirs = T)
###############################################################
# load and unlist corpus
corpushcie <- lapply(corpus.files, function(x) {
  x <- scan(x, what = "char", sep = "", quote = "\"", quiet = T, skipNul = T)
  x <- gsub(" {2,}", " ", x)
  x <- str_trim(x, side = "both")
  x <- str_replace_all(x, fixed("\n"), " ")
  x <- paste(x, sep = " ", collapse = " ")
  x <- unlist(x)
} )
# clean files
corpushcie01 <- gsub("{", "", corpushcie, fixed = T)
corpushcie01 <- gsub("}", "", corpushcie01, fixed = T)
# extract date
date <- str_replace(corpushcie, "\\[P 1\\].*", "")
date <- str_replace(date, ".*<O", "") 
date <- str_replace(date, ">.*", "")
date <- str_trim(date, side = "both")
# extract title
title <- str_replace(corpushcie, "\\[P 1\\].*", "")
title <- str_replace(title, ".*<T", "") 
title <- str_replace(title, ">.*", "")
title <- str_trim(title, side = "both")
# extract author
author <- str_replace(corpushcie, "\\[P 1\\].*", "")
author <- str_replace(author, ".*<A", "") 
author <- str_replace(author, ">.*", "")
author <- str_trim(author, side = "both")
# extract region
region <- str_replace(corpushcie, "\\[P 1\\].*", "")
region <- str_replace(region, ".*<G", "") 
region <- str_replace(region, ">.*", "")
region <- str_trim(region, side = "both")
# extract info
info <- str_replace(corpushcie, "\\[P 1\\].*", "")
info <- str_replace(info, ".*<R", "") 
info <- str_replace(info, ">.*", "")
info <- str_trim(info, side = "both")
# create id vector
id <- 1:length(info)
# clean files
corpushcie02 <- gsub(".*\\[P 1\\]", "", corpushcie01)
corpushcie02 <- gsub("\\[\\/{0,1}[a-z]{0,15} {0,1}[A-Z]{0,1}[a-z]{0,15} {0,1}[0-9]{0,2}\\]", "", corpushcie02)
corpushcie02 <- gsub(" \\.", ".", corpushcie02)
corpushcie02 <- gsub(" \\,", ",", corpushcie02)
corpushcie02 <- gsub("\\=", "", corpushcie02)
corpushcie02 <- gsub("\\[", "", corpushcie02)
corpushcie02 <- gsub("\\]", "", corpushcie02)
corpushcie02 <- gsub("\\^", "", corpushcie02)
corpushcie02 <- gsub("\\*", "", corpushcie02)
corpushcie02 <- gsub("\\%", "", corpushcie02)
corpushcie02 <- gsub("\\(", "", corpushcie02)
corpushcie02 <- gsub("\\)", "", corpushcie02)
corpushcie02 <- gsub("\\&", "", corpushcie02)
corpushcie02 <- gsub("\\_", "", corpushcie02)
corpushcie02 <- gsub("\\~", "", corpushcie02)
#corpushcie02 <- tolower(corpushcie02)
corpushcie02 <- str_trim(corpushcie02, side = "both")
# create data frame
corpushcie03 <- data.frame(id, title, author, date, region, info, corpushcie02)
colnames(corpushcie03) <- c("id", "title", "author", "date", "region", "info", "text")
# remove empty elements
corpushcie04 <- corpushcie03[corpushcie03$text != " ",]
# check if data cleaning ahs worked properly
#t1 <- corpushcie04[grep("unclear", corpushcie04$text), ] #test
#head(t1)

# inspect data
head(corpushcie04)

###############################################################
# extract overview table of the HCIE data
# load library
library(dplyr)
hcietb <- corpushcie04
hcietb$date <- as.numeric(hcietb$date)
hcietb$DateCat <- ifelse(hcietb$date < 1750, "1675-1750", 
                          ifelse(hcietb$date < 1850, "1751-1850", 
                                 ifelse(hcietb$date < 1950, "1851-1935", hcietb$date)))
hcietb$Period <- ifelse(hcietb$date < 1750, "1675-1750",
                        ifelse(hcietb$date < 1800, "1751-1800",
                        ifelse(hcietb$date < 1850, "1801-1850",
                        ifelse(hcietb$date < 1900, "1851-1900",       
                        ifelse(hcietb$date < 1950, "1901-1950",hcietb$date)))))
hcietb$Words <- as.vector(unlist(sapply(hcietb$text, function(x){
  x <- length(as.vector(unlist(str_split(x, " ")))) } )))
hcietb$DateCat <- as.factor(hcietb$DateCat)
hcietb$title <- as.factor(hcietb$title)
hcietb <- hcietb %>%
  dplyr::rename(File = title, Author = author) %>%
  dplyr::select(File, Author, DateCat, Words) %>%
  dplyr::group_by(DateCat) %>%
  dplyr::summarize(WordsN = sum(Words), LettersN = sum(table(File)), AuthorsN = sum(table(Author)))
# inspect resulting table
hcietb

write.table(hcietb, "hcietb.txt", sep = "\t", row.names = F, col.names = T)
###############################################################
# remove letter with unknown date
nrow(corpushcie04)

corpushcie04 <- corpushcie04[corpushcie04$date != "?",]
nrow(corpushcie04)

###############################################################
# save raw data to disc
write.table(corpushcie04, "corpushcieraw.txt", sep = "\t", row.names = F, col.names = T)
#corpushcie04 <- read.delim("corpushcieraw.txt", sep = "\t", header = T)
###############################################################
# split data into smaller chunks
pos01 <- corpushcie04$text[1:100]
pos02 <- corpushcie04$text[101:200]
pos03 <- corpushcie04$text[201:300]
pos04 <- corpushcie04$text[301:400]
pos05 <- corpushcie04$text[401:500]
pos06 <- corpushcie04$text[501:600]
pos07 <- corpushcie04$text[601:700]
pos08 <- corpushcie04$text[701:800]
pos09 <- corpushcie04$text[801:900]
pos10 <- corpushcie04$text[901:nrow(corpushcie04)]
# reload libraries
source("D:\\R/POStagObject.R") # for pos-tagging objects in R
library(NLP)
library(openNLP)
library(openNLPmodels.en)
# pos tagging data
hciepos01 <- POStag(object = pos01)
hciepos01 <- as.vector(unlist(hciepos01))
writeLines(hciepos01, con = "hciepos01.txt", sep = "\n", useBytes = FALSE)
# chunk 2
hciepos02 <- POStag(object = pos02)
hciepos02 <- as.vector(unlist(hciepos02))
writeLines(hciepos02, con = "hciepos02.txt", sep = "\n", useBytes = FALSE)
# chunk 2
hciepos02 <- POStag(object = pos02)
hciepos02 <- as.vector(unlist(hciepos02))
writeLines(hciepos02, con = "hciepos02.txt", sep = "\n", useBytes = FALSE)
# chunk 03
hciepos03 <- POStag(object = pos03)
hciepos03 <- as.vector(unlist(hciepos03))
writeLines(hciepos03, con = "hciepos03.txt", sep = "\n", useBytes = FALSE)
# chunk 04
hciepos04 <- POStag(object = pos04)
hciepos04 <- as.vector(unlist(hciepos04))
writeLines(hciepos04, con = "hciepos04.txt", sep = "\n", useBytes = FALSE)
# chunk 05
hciepos05 <- POStag(object = pos05)
hciepos05 <- as.vector(unlist(hciepos05))
writeLines(hciepos05, con = "hciepos05.txt", sep = "\n", useBytes = FALSE)
# chunk 06
hciepos06 <- POStag(object = pos06)
hciepos06 <- as.vector(unlist(hciepos06))
writeLines(hciepos06, con = "hciepos06.txt", sep = "\n", useBytes = FALSE)
# chunk 07
hciepos07 <- POStag(object = pos07)
hciepos07 <- as.vector(unlist(hciepos07))
writeLines(hciepos07, con = "hciepos07.txt", sep = "\n", useBytes = FALSE)
# chunk 08
hciepos08 <- POStag(object = pos08)
hciepos08 <- as.vector(unlist(hciepos08))
writeLines(hciepos08, con = "hciepos08.txt", sep = "\n", useBytes = FALSE)
# chunk 09
hciepos09 <- POStag(object = pos09)
hciepos09 <- as.vector(unlist(hciepos09))
writeLines(hciepos09, con = "hciepos09.txt", sep = "\n", useBytes = FALSE)
# chunk 10
hciepos10 <- POStag(object = pos10)
hciepos10 <- as.vector(unlist(hciepos10))
writeLines(hciepos10, con = "hciepos10.txt", sep = "\n", useBytes = FALSE)
# list pos tagged elements
postag.files = c("hciepos01.txt", "hciepos02.txt", "hciepos03.txt", "hciepos04.txt",
                 "hciepos05.txt", "hciepos06.txt", "hciepos07.txt", "hciepos08.txt",
                 "hciepos09.txt", "hciepos10.txt")
# load pos tagged elements
hciepos <- sapply(postag.files, function(x) {
  x <- scan(x, what = "char", sep = "\n", quote = "", quiet = T, skipNul = T)
  x <- gsub(" {2,}", " ", x)
  x <- str_trim(x, side = "both")
  x <- str_replace_all(x, fixed("\n"), " ")
})
# unlist pos tagged elements
corpushcie04$hciepos <- unlist(hciepos)
###############################################################
# extract adjs and element preceeding and following it
pstggd <- corpushcie04$hciepos
lpstggd <- strsplit(pstggd, " ")
nlpstggd <- as.vector(unlist(sapply(lpstggd, function(x){
  x <- x[grep("[A-Z]{0,1}[a-z]{1,}\\/JJ[A-Z]{0,1}", x)]
  x <- length(x) } )))
rp <- nlpstggd
rp <- ifelse(rp == 0, 1, rp)
# load function for concordancing
source("D:\\R/ConcR_2.3_loadedfiles.R")
# set parameters for concordancing
pattern <- "[A-Z]{0,1}[a-z]{1,}\\/JJ[A-Z]{0,1}"
context <- 50
# extract all adjectives (concordance)
concjjhcie <- ConcR(corpushcie04$hciepos, pattern, context, all.pre = FALSE)
# repeat rows in data farem as often as there are adjectives in it (if 0 adj, repeat once)
corpushcieadjdf <- corpushcie04[rep(seq(nrow(corpushcie04)), rp),]
# combine data sets
corpushcieadj <- data.frame(1:nrow(corpushcieadjdf), corpushcieadjdf, concjjhcie)
# remove rows without Tokens
amphcie <- corpushcieadj[is.na(corpushcieadj$Token) == F,]
# remove superfluous columns
amphcie$text <- NULL                    
amphcie$hciepos <- NULL
amphcie$OriginalString <- NULL
# inspect data
head(amphcie)

# add clean column names
colnames(amphcie)[1:2] <- c("id", "origid")
# clean adjectives
amphcie$Adjective <- amphcie$Token
rmv <- gsub(".*/", "", amphcie$Adjective)
amphcie <- amphcie[rmv == "JJ",]
amphcie$Adjective <- str_replace_all(amphcie$Adjective, fixed("/JJ"), "")
# add Variant column
amphcie$Variant <- gsub(".* ", "", str_trim(amphcie$PreContext, side = "both")) 
amphcie$Variant <- gsub("/.*", "", amphcie$Variant)
# inspect data
nrow(amphcie); head(amphcie)

###############################################################
# clean ice hcie data
amphcie$Function <- str_trim(amphcie$PostContext, side = "both")
amphcie$Function <- tolower(amphcie$Function)
amphcie$Function <- gsub(" {2,}", " ", amphcie$Function)
amphcie$Function <- gsub(" .*", "", amphcie$Function)
amphcie$Function <- gsub(".*\\/n.*", "Attributive", amphcie$Function)
amphcie$Function <- ifelse(amphcie$Function == "Attributive", amphcie$Function, "Predicative")
# shorten post Context
amphcie$PostContext <- substr(amphcie$PostContext, 1, ifelse((nchar(amphcie$PostContext)+25) <25, maamphcie(nchar(amphcie$PostContext)), 25))
# pre Context
amphcie$PreContext <- str_trim(amphcie$PreContext, side = "both")
amphcie$PreContextLong <- amphcie$PreContext
amphcie$PreContextLong <- substr(amphcie$PreContextLong, ifelse(nchar(amphcie$PreContextLong)-25 <=0, 1, 
                                                                nchar(amphcie$PreContextLong)-25), nchar(amphcie$PreContextLong))
amphcie$PreContext <- gsub(".* ", "", amphcie$PreContext)
# amplifier variant
amphcie$PreContext <- gsub("\\/.*", "", amphcie$PreContext)
# inspect PreContext
preadj <- names(table(tolower(amphcie$PreContext)))
# save data to disc for inspection
write.table(preadj, "preadj.txt", sep = "\n", row.names = F, col.names = T)
###############################################################

# define amplifiers
amplifiers <- c("awefull", "awful", "awfull", "awfully", "badly", 
                "bllood", "blooming", "certain", "dangerously", 
                "dead", "dearly", "deeply", "dreadful", "dreadfull", 
                "espasialy", "especially", "especily", "espeshly", 
                "exceeding", "exceedingly", "excessive", "excessively", 
                "exeeding", "exellent", "extreamly", "extremely", "full", 
                "fully", "greatly", "imensly", "iminently", "increasingly", 
                "infinitely", "miserably", "perfect", "perfectly", 
                "perfictly", "plenty", "pretey", "pretty", "prety", 
                "pritty", "properly", "quite", "real", "really", "realy", 
                "remarkably", "sincerely", "so", "soo", "specially", 
                "surely", "surly", "terible", "terrible", "total", 
                "totally", "true", "truely", "truly", "uncommonly", 
                "unusually", "utterly", "vastly", "vearey", "veary", 
                "veery", "vehemently", "verey", "verrey", "verrry", 
                "verry", "very")
amphcie$Variant <- ifelse(amphcie$PreContext %in% amplifiers, amphcie$PreContext, "0")
# amplified y/n
amphcie$Amplified <- ifelse(amphcie$Variant == "0", 0, 1) 
# adjective
amphcie$Adjective <- tolower(amphcie$Adjective)
# inspect data
nrow(amphcie); head(amphcie); table(amphcie$Variant)

# remove misclassified adjectives
# normalize Adjectives
library(qdap)
cspl <- check_spelling(amphcie$Adjective, assume.first.correct = F, n.suggests = 1)
cspl <- cspl$not.found
cspl <- names(table(cspl))
cspl <- cspl[order(cspl, decreasing = F)]
# save data to disc for inspection
write.table(cspl, "cspl.txt", sep = "\n", row.names = F, col.names = T)
# determine wrong classifications
missclass <- c("abourd", "adew", "adriatic", "afarse", "affcte", 
               "affectanly", "afine", "aforesd", "aking", "alen", 
               "allrite", "amarican", "amerrican", "amind", "annything", 
               "antrim", "armagh", "ashvile", "atlantic", "att", "aug", 
               "austrila", "austrilian", "backpage", "becaus", "biscake", 
               "ble", "boonsville", "br", "briens", "britten", "brodesent", 
               "brung", "caclapodul", "calledonian", "camomite", "cannal", 
               "cf", "cheapist", "chiden", "citty", "constitutioned", 
               "cont", "contery", "coockstown", "cookstown", "cornish", 
               "corr", "cottsville", "cristain", "ct", "cuntry", "cwt", 
               "dail", "darnt", "ded", "dem", "derect", "dich", "dident", 
               "disaperent", "dl", "dolar", "doncey", "dont", "doubtfull", 
               "douing", "douther", "dropsical", "drumal", "drunkerd", 
               "dt", "duary", "duz", "dy", "eable", "edicate", "eet", 
               "efecent", "effectionete", "eliotsville", "elliar", 
               "emlough", "encapt", "eniskilen", "enniskillen", "eny", 
               "ethipone", "famely", "famiely", "familey", "familly", 
               "famyly", "farr", "farre", "faundry", "feading", 
               "feberwary", "febuary", "feess", "felow", "ffather", "fi", 
               "fifteeth", "firken", "firther", "fite", "flatery", 
               "fleete", "fogot", "foillid", "foostal", "foren", "forke", 
               "forthnight", "fosterville", "foxey", "fr", "fraiday", 
               "frd", "frend", "freq", "friel", "frined", "fronteirs", 
               "ft", "ftllworth", "gallent", "gfather", "gient", "glasgow", 
               "goen", "goodbuy", "goot", "gorden", "gother", "gowing", 
               "graizing", "gran", "grany", "grdfather", "greace", "grean", 
               "greanery", "greenvile", "grive", "grone", "grous", 
               "grubbs", "halaflax", "halling", "hardshipp", "harvast", 
               "haymeakin", "heary", "herticular", "hibernian", 
               "hieghtborough", "holdsome", "hondred", "hous", "houserent", 
               "hovil", "hundard", "hunderd", "hundredfold", "hundrid", 
               "hurrican", "hust", "icey", "ieth", "ig", "iinquered", 
               "imidiate", "incuredgd", "indien", "indisterious", 
               "infarnal", "inservitable", "insted", "intall", "irash", 
               "ireish", "ish", "ity", "jeleasy", "jenewary", "jenwary", 
               "joliet", "jubely", "keynen", "ky", "larable", "larrakin", 
               "lastone", "layen", "lb", "leat", "leavenworth", 
               "leesborrough", "leftanant", "leftantant", "legimate", 
               "lerge", "lete", "lext", "likelyhood", "likenes", "likin", 
               "likly", "lincey", "linnen", "livill", "lokken", "london", 
               "looken", "louisbille", "lous", "lre", "lst", "ltary", 
               "lusing", "ly", "macon", "madow", "madrid", "maiar", 
               "malen", "manfold", "marchd", "marcht", "mascheen", 
               "mashenist", "mcgough", "meaghonay", "medisene", "medling", 
               "melbourne", "mertiall", "mery", "mical", "mich", 
               "middleaged", "midling", "millenar", "millenary", "minate", 
               "montreal", "mounth", "munifactorring", "munifica", 
               "muther", "naiborhood", "nal", "napy", "naturelized", "nd", 
               "nect", "neerly", "neglecte", "neighther", "nenian", 
               "nesary", "newes", "newry", "newtown", "nich", "niken", 
               "nikt", "nise", "non", "nonste", "norfick", 
               "notwithstending", "nt", "obedt", "ocationd", "occasined", 
               "oncertent", "oneay", "onesay", "oneseay", "ong", "orall", 
               "ornement", "ounless", "oure", "outelive", "ownsborow", 
               "owny", "ozark", "parlatic", "parrish", "pastrough", "pd", 
               "pdf", "pheasent", "phildefy", "pirty", "plean", "plese", 
               "poasable", "policey", "potatous", "pr", "praty", "prcent", 
               "prepaird", "prinsable", "prinsipal", "pritendid", 
               "progtive", "proportionable", "prussian", "purtested", 
               "purty", "putmock", "putomock", "qr", "quadraped", "quear", 
               "queenstown", "quelld", "ragler", "raind", "rapidarm", 
               "raplagh", "rd", "readdy", "reate", "rebecky", "recd", 
               "redstone", "remonstrant", "resignd", "ristless", "rotid", 
               "rumflip", "rummey", "rusal", "sabeth", "safty", "samul", 
               "sargant", "saterdy", "saturdy", "sayed", "scangry", 
               "scensable", "scerse", "scholden", "scoch", "se", "seaf", 
               "seaven", "secent", "secmel", "seconed", "sencable", 
               "senceable", "sep", "severist", "shippy", "shorlate", 
               "shuch", "sinewey", "sinth", "sisterss", "sitty", "sixs", 
               "sleated", "slye", "smyth", "som", "sorefull", "southarma", 
               "southside", "spewy", "spockled", "squalerous", "staunton", 
               "steem", "steirs", "stirrlesome", "strate", "strinth", 
               "stuch", "sudent", "sufisent", "sullavan", "surendered", 
               "sutch", "tailorville", "tauth", "te", "tenne", "th", "thd", 
               "thene", "theng", "thenk", "thimothy", "thrue", "thunon", 
               "tierd", "timbred", "tonite", "tosic", "toun", "tryal", 
               "tusday", "twe", "twelvepenny", "twilve", "twoo", "tyhte", 
               "unadill", "unadilla", "unalterable", "unavenged", 
               "uncilicited", "uncl", "unconcidret", "unconquered", 
               "understant", "undutiful", "uneable", "unfaned", "unguilty", 
               "unkel", "unkil", "unkle", "unmindful", "unsoshable", 
               "untenanted", "unter", "upt", "useal", "useeal", "vandue", 
               "vegitible", "vermilian", "vermonnt", "verrey", "verrry", 
               "verry", "victorian", "vissed", "waless", "wasent", 
               "watchfull", "wch", "weldisposed", "welldoing", "wellfair", 
               "wellminded", "wensday", "wesleyan", "westren", "wh", 
               "whirborn", "whitning", "wichh", "widdow", "wirk", "wm", 
               "wod", "wollen", "wolling", "worled", "wourldy", "writen", 
               "xxx", "yanky", "yeare", "yelow", "yor", "youar", "younen", 
               "youngstown", "youngtown", "yous", "yr", "yt")
nrow(amphcie)

amphcie <- amphcie[!amphcie$Adjective %in% missclass,]
nrow(amphcie)

# correct spelling
nspl <- check_spelling(amphcie$Adjective, assume.first.correct = F, n.suggests = 1)
nspl <- nspl[order(nspl$not.found),]
nspl <- as.data.frame(nspl)
nspl$more.suggestions <- NULL
# save data to disc for inspection
write.table(nspl, "nspl_20190703.txt", sep = "\t", row.names = F, col.names = T)
# save original adjective
amphcie$OriginalAdjective <- amphcie$Adjective
# replace original adjective with suggestion by spell checker
amphcie$Adjective <- sapply(amphcie$Adjective, function(x){
  x <- ifelse(x %in% nspl$not.found, nspl$suggestion[grep(x, nspl$not.found)], x)
})
# inspect which original adjectives differ from suggested adjectives
testspellingcorrection <- amphcie[which(amphcie$Adjective != amphcie$OriginalAdjective),]
testspellingcorrection <- data.frame(testspellingcorrection$OriginalAdjective, testspellingcorrection$Adjective)
colnames(testspellingcorrection) <- gsub("testspellingcorrection.", "", colnames(testspellingcorrection))
testspellingcorrection <- testspellingcorrection[order(testspellingcorrection$OriginalAdjective),]
# save data to disc for inspection
write.table(testspellingcorrection, "testspellingcorrection_20190703.txt", sep = "\t", row.names = F, col.names = T)
# load manual corrections
coradj <- read.delim("ManuallyCorrectedAdjectives.txt", sep = "\t", header = T, fill = F, quote ="")
head(coradj)
# ecxchange wrong forms with corrected forms
amphcie$Adjective <- sapply(amphcie$Adjective, function(x){
  x <- ifelse(x %in% coradj$Original, coradj$Corrected[grep(x, coradj$Original)], x)
})
# define forms that requhcie removal
sups <- c(".*most.*", ".*more.*") 
negs <- c(".*not.*", ".*never.*", ".*n't.*")
downtoners <- c(".*sort/.*", ".*kind/.*", ".* bit/.*", ".*somewhat.*", ".*fairly.*", 
                ".*rather.*", ".*reasonably.*", ".*slightly.*", ".*comparatively.*", ".*semi.*", 
                ".*relatively.*", ".*little.*", ".*somehow.*", ".*almost.*", ".*partly.*", 
                ".*hardly.*", ".* less.*", ".*barely.*", ".* just/.*")
specialforms <- c(".* too.*", ".*quite.*")
PostContextdowntoners <- c(".*enough.*")
nonpropadj <- c("only", "other", "much", "many", "cheaper", "cheaperr", "bests", "larger", "like", "morer", "uhm", "uhr")
# check length of dataset
str(amphcie); head(amphcie); nrow(amphcie)#; table(amphcie$pint); head(amphcie$PreContextLong); head(amphcie$PreContextLong)

# find items to be removed
supsidx <- unique(grep(paste(sups,collapse="|"), amphcie$PreContextLong, value=F))
negsidx <- unique(grep(paste(negs,collapse="|"), amphcie$PreContextLong, value=F))
downtonersidx <- unique(grep(paste(downtoners,collapse="|"), amphcie$PreContextLong, value=F))
specialformsidx <- unique(grep(paste(specialforms,collapse="|"), amphcie$PreContextLong, value=F))
PostContextdowntonersidx <- unique(grep(paste(PostContextdowntoners,collapse="|"), amphcie$PostContext, value=F))
nonpropadjidx <- unique(grep(paste(nonpropadj,collapse="|"), amphcie$Adjective, value=F))
# combine indices
idxs <- unique(c(supsidx, negsidx, downtonersidx, specialformsidx, PostContextdowntonersidx, nonpropadjidx))
# remove forms that requhcie removal
amphcie <- amphcie[-idxs,]
# remove empty values
amphcie <- amphcie[!amphcie$Variant == "", ]

###############################################################
# save raw data to disc
write.table(amphcie, "amphcie02_wo_neg.txt", sep = "\t", row.names = F)
###############################################################
# code priming
prim1 <- c(rep(0, 1), amphcie$Variant[1:length(amphcie$Variant)-1])
prim2 <- c(rep(0, 2), amphcie$Variant[1:(length(amphcie$Variant)-2)])
prim3 <- c(rep(0, 3), amphcie$Variant[1:(length(amphcie$Variant)-3)])
primtb <- cbind(amphcie$Variant, prim1, prim2, prim3)
amphcie$Priming <- as.vector(unlist(apply(primtb, 1, function(x){
  x <- ifelse(x[1]== "0" , "NoPrime",
              ifelse(x[1] == x[2] | x[1] == x[3] | x[1] == x[4], "Prime", "NoPrime"))
})))
# remove items that were not intensified by a minimum of 2 intensifier variants
nrow(amphcie)

pintadjtb <- table(amphcie$Adjective, amphcie$Variant)
##pintadjtb <- pintadjtb[2:nrow(pintadjtb),]
pintadjtb <- pintadjtb[,2:ncol(pintadjtb)]
pintadjtb2 <- apply(pintadjtb, 1, function(x){
  x <- ifelse(x > 1, 1, x)})
pintadjtb3 <- colSums(pintadjtb2)
pintadjschildes <- names(pintadjtb3)[which(pintadjtb3 >=2 )]
amphcie <- amphcie[amphcie$Adjective %in% pintadjschildes, ]
nrow(amphcie)

# remove questions with really e.g. "is she really pregnant?"
amphcie$rmv <- tolower(amphcie$PreContextLong)
amphcie$rmv <- ifelse(amphcie$Variant == "really", amphcie$rmv, "")
amphcie$rmv <- gsub(" really.*", "", amphcie$rmv)
amphcie$rmv <- gsub(".*/", "", amphcie$rmv)
rmv <- c("nn", "nnp", "prp")
amphcie$rmv <- ifelse(amphcie$rmv %in% rmv, "remove", amphcie$rmv)
amphcie <- amphcie[amphcie$rmv != "remove",]
amphcie$rmv <- NULL

# inspect adjectives
names(table(amphcie$Adjective))

# normalize amplifiers
names(table(amphcie$Variant))

awful <- c("awefull", "awful","awfull")
amphcie$Variant <- ifelse(amphcie$Variant %in% awful, "awfully", amphcie$Variant)
amphcie$Variant <- ifelse(amphcie$Variant == "bllood", "blood", amphcie$Variant)
dreadful <- c("dreadful", "dreadfull")
amphcie$Variant <- ifelse(amphcie$Variant %in% dreadful, "dreadful", amphcie$Variant)
amphcie$Variant <- ifelse(amphcie$Variant == "espeshly", "especially", amphcie$Variant)
amphcie$Variant <- ifelse(amphcie$Variant == "exeeding", "exceeding", amphcie$Variant)
amphcie$Variant <- ifelse(amphcie$Variant == "extreamly", "extremely", amphcie$Variant)
amphcie$Variant <- ifelse(amphcie$Variant == "perfictly", "perfectly", amphcie$Variant)
pretty <- c("pretey", "pretty", "prety", "pritty")
amphcie$Variant <- ifelse(amphcie$Variant %in% pretty, "pretty", amphcie$Variant)
amphcie$Variant <- ifelse(amphcie$Variant == "realy", "really", amphcie$Variant)
amphcie$Variant <- ifelse(amphcie$Variant == "terible", "terrible", amphcie$Variant)
amphcie$Variant <- ifelse(amphcie$Variant == "truely", "truly", amphcie$Variant)
very <- c("veery", "verey", "verrey", "verrry", "verry", "very")
amphcie$Variant <- ifelse(amphcie$Variant %in% very, "very", amphcie$Variant)
# manually check amplification
testAmp <- amphcie[amphcie$Amplified == 1,]
testAmp$test <- paste(testAmp$PreContextLong, testAmp$Adjective, testAmp$PostContext, sep = " ", collapse = NULL)
testAmp$test <- gsub("\\/[A-Z]{1,3}", "", testAmp$test)
# save data to disc
write.table(testAmp, "testAmp.txt", sep = "\t", row.names = F)
# correct misclassifcation
# blood
amphcie$Variant <- ifelse(amphcie$Variant == "blood", "0", amphcie$Variant)
# certain
amphcie$Variant <- ifelse(amphcie$Variant == "certain", "certainly", amphcie$Variant)
# dangerously
amphcie$Variant <- ifelse(amphcie$Variant == "dangerously", "0", amphcie$Variant)
# dead
amphcie$Variant <- ifelse(amphcie$Variant == "dead" & amphcie$Adjective == "long", "0", amphcie$Variant)
# dearly
amphcie$Variant <- ifelse(amphcie$Variant == "dearly", "0", amphcie$Variant)
# dreadful
amphcie$Variant <- ifelse(amphcie$Variant == "dreadful", "dreadfully", amphcie$Variant)
# especially
amphcie$Variant <- ifelse(amphcie$Variant == "especially", "0", amphcie$Variant)
# exceeding
amphcie$Variant <- ifelse(amphcie$Variant == "exceeding", "exceedingly", amphcie$Variant)
# exellent
amphcie$Variant <- ifelse(amphcie$Variant == "exellent", "exellently", amphcie$Variant)
#  iminently
amphcie$Variant <- ifelse(amphcie$Variant == "iminently", "imminently", amphcie$Variant)
# perfect
amphcie$Variant <- ifelse(amphcie$Variant == "perfect" & amphcie$Adjective == "little", "0", amphcie$Variant)
# plenty
amphcie <- amphcie[amphcie$Variant != "plenty",]
# mutch
amphcie <- amphcie[amphcie$Adjective != "mutch",]
# so
amphcie$Variant <- ifelse(amphcie$Variant == "so" & grepl("^ a[n]{0,1}\\/", amphcie$PostContext), "0", amphcie$Variant)
#  terrible
amphcie$Variant <- ifelse(amphcie$Variant == "terrible", "terriblely", amphcie$Variant)
# correct amplification
amphcie$Amplified <- ifelse(amphcie$Variant == "0", "0", amphcie$Amplified)
# remove questions with really e.g. "is she really pregnant?"
amphcie$rmv <- tolower(amphcie$PreContextLong)
amphcie$rmv <- ifelse(amphcie$Variant == "really", amphcie$rmv, "")
amphcie$rmv <- gsub(" really.*", "", amphcie$rmv)
amphcie$rmv <- gsub(".*/", "", amphcie$rmv)
rmv <- c("nn", "nnp", "prp")
amphcie$rmv <- ifelse(amphcie$rmv %in% rmv, "remove", amphcie$rmv)
amphcie <- amphcie[amphcie$rmv != "remove",]
amphcie$rmv <- NULL
# inspecta data
nrow(amphcie); length(table(amphcie$Adjective)); head(amphcie)

###############################################################
# save raw data to disc
write.table(amphcie, "amphcie03_semiclean.txt", sep = "\t", row.names = F, 
            quote = F, eol="\n")
###############################################################
#                        END PART 1
###############################################################

