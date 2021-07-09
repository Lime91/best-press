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


#' Compute PRESS for a set of predictors specified by the given code
#' 
#' @param code An integer in the interval [0, 2^length(predictors) - 1] that 
#' encodes the selected predictors
#' @param target Name of the target variable in the linear model
#' @param predictors A vector of strings with predictor names
#' @param data Dataframe that contains measurements for the target and all 
#' predictors
computePRESS <- function(code, target, predictors, data) {
  p <- selectPredictors(code, predictors)
  if (is.null(p))  # empty model (iff code == 0)
    p <- "1"
  formula <- paste(target, "~", paste(p, collapse = "+"))
  model <- lm(formula, data)
  return(MPV::PRESS(model))
}


#' Perform best subset varaible selection for linear models based on Allen's 
#' PRESS statistic. Computational complexity grows exponentially (!) in the 
#' number of predictors. Hence, we recommend this technique only for small sets
#' of preselected predictor variables.
#' 
#' @param target Name of the target variable in the linear model
#' @param predictors A vector of strings with predictor names
#' @param data Dataframe that contains measurements for the target and all 
#' predictors
bestPRESS <- function(target, predictors, data) {
  nModels <- 2**length(predictors)
  modelCodes <- seq(0, nModels - 1)
  presses <- sapply(X=modelCodes, FUN=computePRESS, target, predictors, data)
  minCode <- which.min(presses)
  bestPredictors <- selectPredictors(minCode, predictors)
  return(bestPredictors)
}

#' Alternative implementation which outputs a matrix with best predictors for
#' each subset size
#' 
#' @param target Name of the target variable in the linear model
#' @param predictors A vector of strings with predictor names
#' @param data Dataframe that contains measurements for the target and all 

bestPRESS_df <- function(target, predictors, data) {
  nModels <- 2**length(predictors)
  modelCodes <- seq(0, nModels - 1)
  presses <- sapply(X=modelCodes, FUN=computePRESS, target, predictors, data)
  nrPredictors <- unlist(lapply(sapply(X=modelCodes, FUN=selectPredictors, predictors), FUN = length))
  presses_predictors_df = data.frame(modelCodes, presses, nrPredictors)
  minCode_nrPredictors <- presses_predictors_df %>% arrange(nrPredictors, presses) %>% distinct(nrPredictors, .keep_all = T)
  bestPredictors <- sapply(X=minCode_nrPredictors$modelCodes, FUN = selectPredictors, predictors)
  bestPredictorsdf <- data.frame(do.call(rbind, bestPredictors))
  bestPredictorsdf[upper.tri(bestPredictorsdf)] <- NA
  return(bestPredictorsdf)
}

# DEMO
data <- read.table("data/case1_bodyfat.txt", header = T, sep = ";")
target <- "siri"

# compute best subset of predictors for a lm after manual pre-selection
predictors <- c("age", "weight_kg", "height_cm", "neck", "chest", "abdomen",
                "hip", "thigh", "knee", "ankle", "biceps", "forearm", "wrist")

bestPRESS(target, predictors, data)

bestPRESS_df(target, predictors, data)
