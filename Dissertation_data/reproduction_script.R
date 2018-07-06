rm(list=ls(all=TRUE))

# Set working directory
setwd("c:\\Users\\Bobabtar\\Desktop\\Dissertation_data")

# Load the required data sets
# 
# mkl <- read.csv("masskillings.csv")
# wdi <- read.csv("wdi.csv")
# coups <- read.csv("coups.csv")
# polity <- read.csv("polity.csv")
# pitf <- read.csv("pitfproblemset.csv")
# mepv <- read.csv("mepv.csv")
# ios <- read.csv("ios.csv")
# imrate <- read.csv("imrate.csv")
# elc <- read.csv("elc.csv")
# dis <- read.csv("dis.csv")
# elf <- read.csv("elf.csv")
# econ <- read.csv("econstats.csv")
# 
# # Merge those data sets
# df <- merge(rack, mkl, all.x = TRUE)
# df <- merge(df, wdi, all.x = TRUE)
# df <- merge(df, coups, all.x = TRUE)
# df <- merge(df, polity, all.x = TRUE)
# df <- df[!duplicated(df),]
# df <- merge(df, pitf, all.x = TRUE)
# df <- merge(df, mepv, all.x = TRUE)
# df <- merge(df, ios, all.x = TRUE)
# df <- merge(df, imrate, all.x = TRUE)
# df <- merge(df, elc, all.x = TRUE)
# df <- merge(df, dis, all.x = TRUE)
# df <- merge(df, elf, all.x = TRUE)
# df <- merge(df, econ, all.x = TRUE)
# df <- df[order(df$country, df$year),]
# 
# # DEPENDENT VARIABLE (1-YEAR LEAD)
# df <- df[order(df$country, df$year),]
# for (i in 1:dim(df)[1]) df$start.1[i] <- df$start[i+1]
# df$start.1 <- replace(df$start.1, which(df$year >= 2013 | df$year==df$yrdied), NA)
# 
# # COUP ATTEMPT (1-YEAR LEAD)
# df <- df[order(df$country, df$year),]
# for (i in 1:dim(df)[1]) df$cou.a.d.1[i] <- df$cou.a.d[i+1]
# df$cou.a.d.1 <- replace(df$cou.a.d.1, which(df$year >= 2013 | df$year==df$yrdied), NA)
# 
# # CIVIL-WAR ONSET (1-YEAR LEAD)
# df <- df[order(df$country, df$year),]
# for (i in 1:dim(df)[1]) df$pitf.cwar.onset.1[i] <- df$pitf.cwar.onset[i+1]
# df$pitf.cwar.onset.1 <- replace(df$pitf.cwar.onset.1, which(df$year >= 2013 | df$year==df$yrdied), NA)
# 
# # PITF INSTABILITY ONSET (1-YEAR LEAD)
# df <- df[order(df$country, df$year),]
# for (i in 1:dim(df)[1]) df$pitf.any.onset.1[i] <- df$pitf.any.onset[i+1]
# df$pitf.any.onset.1 <- replace(df$pitf.any.onset.1, which(df$year >= 2013 | df$year==df$yrdied), NA)
# 
# write.csv(df, "statrisk2014.merge.20140121.csv", row.names=FALSE)

df<-read.csv("statrisk2014.merge.20140121.csv",header=TRUE)
write.csv(df,"dataout.csv")
####################################
# Model Formulae
####################################

f.coup <- formula(cou.a.d.1 ~
                    xxxcimrln + 
                    regacln +
                    postcw +
                    cou.tries5d +
                    polcat2 + polcat3 + polcat7 +
                    durableln +
                    mix.gdppcgrowsr +
                    civconln +
                    io.iccpr1)

f.cwar <- formula(pitf.cwar.onset.1 ~
                    wdi.popsizeln + 
                    xxxcimrln +
                    polcat2 + polcat3 + polcat7 +
                    regacln +
                    civconln)

f.threat <- formula(start.1 ~ log(coup.p) + log(cwar.p))

f.pitf <- formula(pitf.any.onset.1 ~
                    reg.eap + reg.eur + reg.mna + reg.sca + reg.amr +
                    xxxcimrln +
                    pitfcat2 + pitfcat3 + pitfcat4 + pitfcat5 + pitfcat6 +
                    dispota4ln +
                    regacln)

f.harff <- formula(start.1 ~
                     log(pitf.p) +
                     ever +
                     sftpuhvl2.10ln +
                     autocracy +
                     elceliti +
                     elcelethc +
                     wdi.tradeln)

f.rf <- formula(as.factor(start.1) ~
                  reg.afr + reg.eap + reg.eur + reg.mna + reg.sca + reg.amr +
                  ongoing +
                  ever +
                  ageln + 
                  wdi.popsizeln + 
                  xxxcimrln +
                  mix.gdppcgrowsr +
                  wdi.tradeln +
                  io.iccpr1 +
                  postcw +
                  polcat1 + polcat2 + polcat3 + polcat7 +
                  durableln +
                  dispota4ln +
                  efcat1 + efcat2 + efcat3 + efcat9 +
                  elceleth1 + elceleth2 +
                  elceliti +
                  cou.tries5d +
                  sftpuhvl2.10ln +
                  regacln +
                  civconln)

#############################
# Model Validation
#############################
library(caret)
require(caret)

valdat <- subset(df, year >= 1960 & year <= 2013 & df$year >= df$yrborn & df$year <= df$yrdied &
                   is.na(start.1) == FALSE)
y <- valdat$start.1
valdat$k <- createFolds(y, k = 10, list = FALSE)
