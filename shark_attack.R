#Shark Attacks

rm(list=ls())
setwd("E:/UBA/Data Mining/TP2")

sharksData <- read.csv(file="Shark_Attack_Data.csv", header=TRUE, sep=",")
glimpse(sharksData)



sharksData$Case.Number <- gsub(".R", "", sharksData$Case.Number)
sharksData$Case.Number <- gsub(".a", "", sharksData$Case.Number)
sharksData$Case.Number <- gsub(".b", "", sharksData$Case.Number)
sharksData$Case.Number <- gsub(".c", "", sharksData$Case.Number)
sharksData$Case.Number <- gsub(".d", "", sharksData$Case.Number)

sharksData$Date <- gsub("Reported ", "", sharksData$Date)
