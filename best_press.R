library(MPV)
library(dplyr)

#' Extract predictor names from bitcode.
#'
#' @param code An integer in the interval [0, 2^length(predictors) - 1] that 
#' encodes the selected predictors
#' @param predictors A vector of strings with predictor names
selectPredictors <- function(code, predictors) {
  selected <- c()
  for (i in 1:length(predictors)) {
    if (code %% 2 == 1)  # check if current predictor is encoded
      selected <- c(selected, predictors[i])
    code <- code %/% 2  # right shift to obtain next predictor
  }
  return(selected)
}

# demo
p <- c("A", "B", "C")
selectPredictors(0, p)  # selects NULL
selectPredictors(1, p)  # selects "A"
selectPredictors(3, p)  # selects "A" and "B"
selectPredictors(5, p)  # selects "A" and "C"


#' Count all bits in code that are set to 1.
#'
#' @param code An integer
#' @param nBits number of least significant bits to be counted
bitSum <- function(code, nBits=32) {
  s <- 0
  while (nBits > 0) {
    if (code %% 2 == 1)  # check if current bit is set
      s <- s + 1
    code <- code %/% 2  # right shift to obtain next bit
    nBits <- nBits - 1
  }
  return(s)
}


#' Compute PRESS for a set of predictors specified by the given code
#' 
#' @param code An integer in the interval [0, 2^length(predictors) - 1] that 
#' encodes the selected predictors
#' @param target Name of the target variable in the linear model
#' @param predictors A vector of strings with predictor names
#' @param data Dataframe that contains measurements for the target and all 
#' predictors
computePress <- function(code, target, predictors, data) {
  p <- selectPredictors(code, predictors)
  if (is.null(p))  # empty model (iff code == 0)
    p <- "1"
  formula <- paste(target, "~", paste(p, collapse = "+"))
  model <- lm(formula, data)
  return(MPV::PRESS(model))
}


#' Perform best subset variable selection for linear models based on Allen's 
#' PRESS statistic. Computational complexity grows exponentially (!) in the 
#' number of predictors. Hence, we recommend this technique only for small sets
#' of preselected predictor variables.
#' 
#' @param target Name of the target variable in the linear model
#' @param predictors A vector of strings with predictor names
#' @param data Dataframe that contains measurements for the target and all 
#' predictors
bestPress <- function(target, predictors, data) {
  nModels <- 2**length(predictors)
  modelCodes <- seq(0, nModels - 1)
  presses <- sapply(modelCodes, computePress, target, predictors, data)
  minCode <- which.min(presses)
  bestPredictors <- selectPredictors(minCode, predictors)
  return(bestPredictors)
}


#' Alternative implementation that computes a matrix with best predictors for
#' each subset size.
#' 
#' @param target Name of the target variable in the linear model
#' @param predictors A vector of strings with predictor names
#' @param data Dataframe that contains measurements for the target and all 
#' predictors
bestPressDf <- function(target, predictors, data) {
  nPred <- length(predictors)
  nModels <- 2**nPred
  modelCodes <- seq(1, nModels - 1)  # skip empty model
  modelSizes <- sapply(modelCodes, bitSum, nPred)
  presses <- sapply(modelCodes, computePress, target, predictors, data)
  bestCodes <- data.frame(modelCodes, modelSizes, presses) %>% 
    arrange(modelSizes, presses) %>%
    distinct(modelSizes, .keep_all = T) %>% 
    dplyr::select(modelCodes)  # namespace only needed for RMarkdown
  bestPred <- apply(bestCodes, 1, selectPredictors, predictors)  # sizes differ
  bestDf <- lapply(bestPred, function(v) c(v, rep(NA, nPred - length(v)))) %>% 
    as.data.frame(row.names = paste("X", seq(nPred), sep = "")) %>% 
    t()
  rownames(bestDf) <- seq(nPred)
  return(bestDf)
}

# DEMO
data <- read.table("data/case1_bodyfat.txt", header = T, sep = ";")
target <- "siri"

# compute best subset of predictors for a lm after manual pre-selection
predictors <- c("age", "weight_kg", "height_cm", "neck", "chest", "abdomen",
                "hip", "thigh", "knee", "ankle", "biceps", "forearm", "wrist")

bestPress(target, predictors, data)

bestPressDf(target, predictors, data)




