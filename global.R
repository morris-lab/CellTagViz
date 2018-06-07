
#This is the global.R file for out CellTag Vizualtion Shiny App.

#In this file you should load all libraries, functions, or data required for use with the shiny app.

#This loads the shiny package required to run the app.
library(shiny)

#This packages allows multiple custom themes to be used with shiny webapps. 
library(shinythemes)

#This loads useful "tidy" libraries for typically used for manipulating data.
library(tidyverse)

#This is a package that customizes ggplot2 plots. It creates a more minimal output and creates more appealing plots in my opinion. 
library(cowplot)

#This is a package used to visualize graphs. Particularly the clonal graphs which visualizes clones as well as their related clones from subsequent rounds of CellTagging.
library(igraph)

#This package is used to construct the clonal CellTag networks.
library(networkD3)

library(viridis)

shinyjs::useShinyjs()

dataPath <- "celltag.viz.data.txt"

seuratPath <- "seurat.gene.expression.subset.Rds"

monoclePath <- "monocle.gene.expression.subset.Rds"

#Data required for Clone Network Visualiztions
#load(file = "links_and_Nodes.RData")

#load(file = "tag_metadata.RData")

load(file = "functions.RData")

load(file = "links_and_Nodes.HF1.RData")
load(file = "links_and_Nodes.HF2.RData")
load(file = "tag_metadata.HF1.RData")
load(file = "tag_metadata.HF2.RData")

netColorChoice <- colnames(hf1.Nodes)[c(6, 8, 10:15, 18)]

names(netColorChoice) <- netColorChoice

netColorChoice[9] <- "tag"

all_tags <- hf1.all_tags
hf1Names <- hf1.all_tags
hf1Names <- strsplit(hf1Names, split = "_")
hf1Names <- lapply(hf1Names, "[[", 2)
hf1Names <- unlist(hf1Names)

hf2Names <- hf2.all_tags
hf2Names <- strsplit(hf2Names, split = "_")
hf2Names <- lapply(hf2Names, "[[", 2)
hf2Names <- unlist(hf2Names)

names(hf1.all_tags) <- hf1Names
names(hf2.all_tags) <- hf2Names


#Functions for generating ggplots of tsne or pseudotime data. Functions for normal, contour, and clonal plots. 
source(file = "tsne.plot.functions.R")

source(file = "plotBase.R")

source(file = "plotContour.R")

source(file = "subsetCloneData.R")

source(file = "generatePlot.R")

source(file = "plotCloneLayer.R")

source(file = "plotStackedBar.R")

source(file = "cloneVersion.R")

source(file = "addGeneExpr.R")

#This loads the data and meta data used for plotting.

celltagData <- read.table(file = dataPath, header = TRUE)

monocleExpr <- readRDS(file = monoclePath)

seuratExpr <- readRDS(file = seuratPath)

#monExpr <- read.table(file = monPath, header = TRUE)

colnames(celltagData)[c(21, 22)] <- c("tSNE_1", "tSNE_2")

selectChoices <- colnames(celltagData)[c(1:15)]

#geneChoiceMonocle <- rownames(monExpr)

celltagCols <- grep(pattern = "CellTag.D", x = colnames(celltagData), value = TRUE)

successfulD0 <- c(573, 2600, 2752, 2721, 3026)

successfulD3 <- c(329, 2024)

deadendD0 <- c(271, 487, 2352)

deadendD3 <- c(473, 488, 2388, 2396)

otherClonesD0 <- names(sort(table(celltagData$CellTag.D0), decreasing = TRUE))

otherClonesD0 <- otherClonesD0[!grepl(pattern = ".NA", x = otherClonesD0)]

otherClonesD0 <- otherClonesD0[!otherClonesD0 %in% successfulD0]

otherClonesD0 <- otherClonesD0[!otherClonesD0 %in% deadendD0]

cloneChoicesD0 <- list(Successful = successfulD0, DeadEnd = deadendD0, Others = otherClonesD0)

otherClonesD3 <- names(sort(table(celltagData$CellTag.D3), decreasing = TRUE))

otherClonesD3 <- otherClonesD3[!grepl(pattern = ".NA", x = otherClonesD3)]

otherClonesD3 <- otherClonesD3[!otherClonesD3 %in% successfulD3]

otherClonesD3 <- otherClonesD3[!otherClonesD3 %in% deadendD3]

cloneChoicesD3 <- list(Successful = successfulD3, DeadEnd = deadendD3, Others = otherClonesD3)

otherClonesD13 <- names(sort(table(celltagData$CellTag.D13), decreasing = TRUE))

otherClonesD13 <- otherClonesD13[!grepl(pattern = ".NA", x = otherClonesD13)]

cloneChoicesD13 <- list(Others = otherClonesD13)

celltagData$Cluster.Seurat <- as.factor(celltagData$Cluster.Seurat)

celltagData$State.Monocle <- as.factor(celltagData$State.Monocle)

celltagData$Timepoint <- factor(celltagData$Timepoint, levels = c("Day 0", "Day 3", "Day 6", "Day 9", "Day 12", "Day 15", "Day 21", "Day 28"))

celltagData$CellTag.Version <- as.character(celltagData$CellTag.Version)

cloneInput <- list()
geneChoice <- NULL

netTagD0 <- grep(x = all_tags, patter = "CellTag.D0", value = TRUE)

namesD0 <- strsplit(netTagD0, split = "_", fixed = TRUE)

namesD0 <- sapply(namesD0, "[[", 2)

names(netTagD0) <- namesD0

netTagD3 <- grep(x = all_tags, patter = "CellTag.D3", value = TRUE)

namesD3 <- strsplit(netTagD3, split = "_", fixed = TRUE)

namesD3 <- sapply(namesD3, "[[", 2)

names(netTagD3) <- namesD3

netTagD13 <- grep(x = all_tags, patter = "CellTag.D13", value = TRUE)

namesD13 <- strsplit(netTagD13, split = "_", fixed = TRUE)

namesD13 <- sapply(namesD13, "[[", 2)

names(netTagD13) <- namesD13

#networkVars <- colnames(Nodes)[c(1:2, 7, 9, 11, 12, 13, 14, 15, 16, 18, 19)]

