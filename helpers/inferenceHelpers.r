makeModel <- function(header) {
  
  return(paste(read_file(header), read_file("models/engine.txt"), sep = "\n"))
  
}

wrapInference <- function(model, target, inferenceType, samples, lag, burn) {
  
  if(inferenceType == "incrementalContinuous") {
    inferenceCommand <- read_file("models/incrementalContinuous.txt")
    
  } else if(inferenceType == "incremental") {
    inferenceCommand <- read_file("models/incremental.txt")
    
  } else if(inferenceType == "continuous") {
    inferenceCommand <- read_file("models/continuous.txt")
    
  } else if(inferenceType == "vanilla") {
    inferenceCommand <- read_file("models/vanilla.txt")
    
  } else if(inferenceType == "continuous_equalcost") {
    inferenceCommand <- read_file("models/continuous_equalcost.txt")
    
  } else if(inferenceType == "vanilla_equalcost") {
    inferenceCommand <- read_file("models/vanilla_equalcost.txt")
    
  } else if(inferenceType == "incremental_equalcost") {
    inferenceCommand <- read_file("models/incremental_equalcost.txt")
    
  } else if(inferenceType == "incrementalContinuous_equalcost") {
    inferenceCommand <- read_file("models/incrementalContinuous_equalcost.txt")
  } 
  
  inferenceCommand <- gsub("TARGET_REFERENT", target, inferenceCommand, fixed = TRUE)
  inferenceCommand <- gsub("NUM_SAMPLES", samples, inferenceCommand, fixed = TRUE)
  inferenceCommand <- gsub("LAG", lag, inferenceCommand, fixed = TRUE)
  inferenceCommand <- gsub("BURN_IN", burn, inferenceCommand, fixed = TRUE)
  
  return(paste(read_file(model), inferenceCommand, sep = "\n"))
  
}

estimate_mode <- function(s) {
  d <- density(s)
  return(d$x[which.max(d$y)])
}

getEstimates <- function(posteriors) {
  
  estimates <- posteriors %>%
    group_by(Parameter) %>%
    dplyr::summarize(estimate = estimate_mode(value)) %>%
    mutate(estimate = ifelse(estimate < 0, 0, estimate)) %>%
    pivot_wider(names_from = Parameter, values_from = estimate)
  
  return(estimates)
  
}

wrapPrediction = function(model, estimates, overmodifyingUtterance, targetReferent, inferenceType) {
  
  if(inferenceType == "incrementalContinuous" | inferenceType == "incremental" ) {
    
    predictionCommand <- read_file("models/getIncrementalPredictions.txt")
    
  } else if (inferenceType == "continuous" | inferenceType == "vanilla" ) {
    
    predictionCommand <- read_file("models/getGlobalPredictions.txt")
    
  }
  
  predictionCommand <- paste((sprintf("var estimates = %s[0]", toJSON(estimates, digits = NA))), predictionCommand, sep = "\n")
  predictionCommand <- gsub("OVERMODIFYING_UTTERANCE", overmodifyingUtterance, predictionCommand, fixed = TRUE)
  predictionCommand <- gsub("TARGET_REFERENT", targetReferent, predictionCommand, fixed = TRUE)
  
  return(paste(read_file(model), predictionCommand, sep = "\n"))
  
}
