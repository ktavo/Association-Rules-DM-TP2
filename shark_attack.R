#Shark Attacks

rm(list=ls())
setwd("E:/UBA/Data Mining/TP2")

sharksData <- read.csv(file="Shark_Attack_Data.csv", header=TRUE, sep=",")
glimpse(sharksData)
