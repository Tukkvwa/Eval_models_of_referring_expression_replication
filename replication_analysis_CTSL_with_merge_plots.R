setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tidyverse)
library(jsonlite)
library(rwebppl)
source("helpers/helpers.R")

source("helpers/BDA_dataprep.R")
source("helpers/inferenceHelpers.R")
source("helpers/BDA_vizhelpers.R")

# PUT IN AN "UNCOLLAPSED" DATAFILE WITH DEGEN ET AL.'S FORMAT

d_uncollapsed <- read_csv("data/ctsl_perTrial.csv")

# COLLAPSE DATA (STILL NECESSARY FOR VISUALIZATIONS)

d_collapsed <- collapse_dataset(d_uncollapsed)

# MAKE A TIBBLE: COLUMNS CONDITION, REFERENTS IN THAT CONDITION (STATES), ALTERNATIVES IN THAT CONDITION (UTTERANCES)

statesUtterances <- makeStatesUtterances(d_uncollapsed, "spanish")

# MAKE INPUT DATA TO BDA: EACH DATUM INCLUDES RESPONSE, STATES, UTTERANCES

df <- d_uncollapsed %>%
  merge(statesUtterances) %>%
  mutate(response = case_when(response == "color" ~ "START color STOP",
                              response == "size" ~ "START size STOP",
                              response == "size_color" ~ "START color size STOP")) %>%
  select(response, states, utterances, condition)

# MAKE THE MODEL 

model <- makeModel("models/modelAndSemantics.txt")

# MODEL 1: VANILLA RSA

# POSTERIORS

vanillaInferenceScript <- wrapInference(model, "color_size", "vanilla", 5000, 10, 50000)

vanillaInferenceScript00 <- wrapInference(model, "color_size", "vanilla", 5000, 10, 70000)
vanillaInferenceScript01 <- wrapInference(model, "color_size", "vanilla", 5000, 10, 60000)
vanillaInferenceScript02 <- wrapInference(model, "color_size", "vanilla", 5000, 10, 40000)
vanillaInferenceScript03 <- wrapInference(model, "color_size", "vanilla", 5000, 10, 30000)

vanillaInferenceScript10 <- wrapInference(model, "color_size", "vanilla", 7000, 10, 50000)
vanillaInferenceScript11 <- wrapInference(model, "color_size", "vanilla", 6000, 10, 50000)
vanillaInferenceScript12 <- wrapInference(model, "color_size", "vanilla", 4000, 10, 50000)
vanillaInferenceScript13 <- wrapInference(model, "color_size", "vanilla", 3000, 10, 50000)

vanillaInferenceScript20 <- wrapInference(model, "color_size", "vanilla", 5000, 20, 50000)
vanillaInferenceScript21 <- wrapInference(model, "color_size", "vanilla", 5000, 15, 50000)
vanillaInferenceScript22 <- wrapInference(model, "color_size", "vanilla", 5000, 5, 50000)
vanillaInferenceScript23 <- wrapInference(model, "color_size", "vanilla", 5000, 1, 50000)

# "TRAIN" MODEL

vanillaPosteriors00 <- webppl(vanillaInferenceScript00, data = df, data_var = "df", random_seed=3333)
vanillaPosteriors01 <- webppl(vanillaInferenceScript01, data = df, data_var = "df", random_seed=3333)
vanillaPosteriors02 <- webppl(vanillaInferenceScript02, data = df, data_var = "df", random_seed=3333)
vanillaParamGrp02 <- vanillaPosteriors02 %>% group_by(Parameter) %>% summarise(variance = var(value))

vanillaPosteriors03 <- webppl(vanillaInferenceScript03, data = df, data_var = "df", random_seed=3333)
vanillaParamGrp03 <- vanillaPosteriors03 %>% group_by(Parameter) %>% summarise(variance = var(value))

vanillaPosteriors10 <- webppl(vanillaInferenceScript10, data = df, data_var = "df", random_seed=3333)
vanillaPosteriors11 <- webppl(vanillaInferenceScript11, data = df, data_var = "df", random_seed=3333)
vanillaPosteriors12 <- webppl(vanillaInferenceScript12, data = df, data_var = "df", random_seed=3333)
vanillaPosteriors13 <- webppl(vanillaInferenceScript13, data = df, data_var = "df", random_seed=3333)

vanillaPosteriors20 <- webppl(vanillaInferenceScript20, data = df, data_var = "df", random_seed=3333)
vanillaPosteriors21 <- webppl(vanillaInferenceScript21, data = df, data_var = "df", random_seed=3333)
vanillaPosteriors22 <- webppl(vanillaInferenceScript22, data = df, data_var = "df", random_seed=3333)
vanillaPosteriors23 <- webppl(vanillaInferenceScript23, data = df, data_var = "df", random_seed=3333)

# PLOT GRAPHS
graphPosteriors(vanillaPosteriors) + ggtitle("Vanilla posteriors")
ggsave("results/vanilla/ctsl_vanillaPosteriors.png")

graphPosteriors(vanillaPosteriors00) + ggtitle("Vanilla posteriors: burn-in 70000")
ggsave("results/vanilla/ctsl_vanillaPosteriors00.png")

graphPosteriors(vanillaPosteriors01) + ggtitle("Vanilla posteriors: burn-in 60000")
ggsave("results/vanilla/ctsl_vanillaPosteriors01.png")

graphPosteriors(vanillaPosteriors02) + ggtitle("Vanilla posteriors: burn-in 40000")
ggsave("results/vanilla/ctsl_vanillaPosteriors02.png")

graphPosteriors(vanillaPosteriors03) + ggtitle("Vanilla posteriors: burn-in 30000")
ggsave("results/vanilla/ctsl_vanillaPosteriors03.png")

graphPosteriors(vanillaPosteriors10) + ggtitle("Vanilla posteriors: samples 7000")
ggsave("results/vanilla/ctsl_vanillaPosteriors10.png")

graphPosteriors(vanillaPosteriors11) + ggtitle("Vanilla posteriors: samples 6000")
ggsave("results/vanilla/ctsl_vanillaPosteriors11.png")

graphPosteriors(vanillaPosteriors12) + ggtitle("Vanilla posteriors: samples 4000")
ggsave("results/vanilla/ctsl_vanillaPosteriors12.png")

graphPosteriors(vanillaPosteriors13) + ggtitle("Vanilla posteriors: samples 3000")
ggsave("results/vanilla/ctsl_vanillaPosteriors13.png")

graphPosteriors(vanillaPosteriors20) + ggtitle("Vanilla posteriors: lag 20")
ggsave("results/vanilla/ctsl_vanillaPosteriors20.png")

graphPosteriors(vanillaPosteriors21) + ggtitle("Vanilla posteriors: lag 15")
ggsave("results/vanilla/ctsl_vanillaPosteriors21.png")

graphPosteriors(vanillaPosteriors22) + ggtitle("Vanilla posteriors: lag 5")
ggsave("results/vanilla/ctsl_vanillaPosteriors22.png")

graphPosteriors(vanillaPosteriors23) + ggtitle("Vanilla posteriors: lag 1")
ggsave("results/vanilla/ctsl_vanillaPosteriors23.png")


# GET ESTIMATES

vanillaEstimates <- getEstimates(vanillaPosteriors) 
vanillaEstimates00 <- getEstimates(vanillaPosteriors00)
vanillaEstimates01 <- getEstimates(vanillaPosteriors01)
vanillaEstimates02 <- getEstimates(vanillaPosteriors02)
vanillaEstimates03 <- getEstimates(vanillaPosteriors03)

vanillaEstimates10 <- getEstimates(vanillaPosteriors10)
vanillaEstimates11 <- getEstimates(vanillaPosteriors11)
vanillaEstimates12 <- getEstimates(vanillaPosteriors12)
vanillaEstimates13 <- getEstimates(vanillaPosteriors13)

vanillaEstimates20 <- getEstimates(vanillaPosteriors20)
vanillaEstimates21 <- getEstimates(vanillaPosteriors21)
vanillaEstimates22 <- getEstimates(vanillaPosteriors22)
vanillaEstimates23 <- getEstimates(vanillaPosteriors23)

vanillaPredictionScript <- wrapPrediction(model, vanillaEstimates,
                                             "START color size STOP", 
                                             "color_size",
                                             "vanilla")
vanillaPredictionScript00 <- wrapPrediction(model, vanillaEstimates00,
                                               "START color size STOP", 
                                               "color_size",
                                               "vanilla")
vanillaPredictionScript01 <- wrapPrediction(model, vanillaEstimates01,
                                               "START color size STOP", 
                                               "color_size",
                                               "vanilla")
vanillaPredictionScript02 <- wrapPrediction(model, vanillaEstimates02,
                                               "START color size STOP", 
                                               "color_size",
                                               "vanilla")
vanillaPredictionScript03 <- wrapPrediction(model, vanillaEstimates03,
                                               "START color size STOP", 
                                               "color_size",
                                               "vanilla")

vanillaPredictionScript10 <- wrapPrediction(model, vanillaEstimates10,
                                               "START color size STOP", 
                                               "color_size",
                                               "vanilla")
vanillaPredictionScript11 <- wrapPrediction(model, vanillaEstimates11,
                                               "START color size STOP", 
                                               "color_size",
                                               "vanilla")
vanillaPredictionScript12 <- wrapPrediction(model, vanillaEstimates12,
                                               "START color size STOP", 
                                               "color_size",
                                               "vanilla")
vanillaPredictionScript13 <- wrapPrediction(model, vanillaEstimates13,
                                               "START color size STOP", 
                                               "color_size",
                                               "vanilla")


vanillaPredictionScript20 <- wrapPrediction(model, vanillaEstimates20,
                                               "START color size STOP", 
                                               "color_size",
                                               "vanilla")

vanillaPredictionScript21 <- wrapPrediction(model, vanillaEstimates21,
                                               "START color size STOP", 
                                               "color_size",
                                               "vanilla")

vanillaPredictionScript22 <- wrapPrediction(model, vanillaEstimates22,
                                               "START color size STOP", 
                                               "color_size",
                                               "vanilla")
vanillaPredictionScript23 <- wrapPrediction(model, vanillaEstimates23,
                                               "START color size STOP", 
                                               "color_size",
                                               "vanilla")



vanillaPredictives <- webppl(vanillaPredictionScript, data = unique(df %>%  select(condition,states,utterances)), data_var = "df")

vanillaPredictives00 <- webppl(vanillaPredictionScript00, data = unique(df %>%  select(condition,states,utterances)), data_var = "df")
vanillaPredictives01 <- webppl(vanillaPredictionScript01, data = unique(df %>%  select(condition,states,utterances)), data_var = "df")
vanillaPredictives02 <- webppl(vanillaPredictionScript02, data = unique(df %>%  select(condition,states,utterances)), data_var = "df")
vanillaPredictives03 <- webppl(vanillaPredictionScript03, data = unique(df %>%  select(condition,states,utterances)), data_var = "df")

vanillaPredictives20 <- webppl(vanillaPredictionScript10, data = unique(df %>%  select(condition,states,utterances)), data_var = "df")
vanillaPredictives21 <- webppl(vanillaPredictionScript11, data = unique(df %>%  select(condition,states,utterances)), data_var = "df")
vanillaPredictives22 <- webppl(vanillaPredictionScript12, data = unique(df %>%  select(condition,states,utterances)), data_var = "df")
vanillaPredictives23 <- webppl(vanillaPredictionScript13, data = unique(df %>%  select(condition,states,utterances)), data_var = "df")

vanillaPredictives10 <- webppl(vanillaPredictionScript20, data = unique(df %>%  select(condition,states,utterances)), data_var = "df")
vanillaPredictives11 <- webppl(vanillaPredictionScript21, data = unique(df %>%  select(condition,states,utterances)), data_var = "df")
vanillaPredictives12 <- webppl(vanillaPredictionScript22, data = unique(df %>%  select(condition,states,utterances)), data_var = "df")
vanillaPredictives13 <- webppl(vanillaPredictionScript23, data = unique(df %>%  select(condition,states,utterances)), data_var = "df")



# MODEL 2: CONTINUOUS RSA

# POSTERIORS

continuousInferenceScript <- wrapInference(model, "color_size", "continuous", 5000, 10, 50000)

continuousInferenceScript00 <- wrapInference(model, "color_size", "continuous", 5000, 10, 70000)
continuousInferenceScript01 <- wrapInference(model, "color_size", "continuous", 5000, 10, 60000)
continuousInferenceScript02 <- wrapInference(model, "color_size", "continuous", 5000, 10, 40000)
continuousInferenceScript03 <- wrapInference(model, "color_size", "continuous", 5000, 10, 30000)

continuousInferenceScript10 <- wrapInference(model, "color_size", "continuous", 7000, 10, 50000)
continuousInferenceScript11 <- wrapInference(model, "color_size", "continuous", 6000, 10, 50000)
continuousInferenceScript12 <- wrapInference(model, "color_size", "continuous", 4000, 10, 50000)
continuousInferenceScript13 <- wrapInference(model, "color_size", "continuous", 3000, 10, 50000)

continuousInferenceScript20 <- wrapInference(model, "color_size", "continuous", 5000, 20, 50000)
continuousInferenceScript21 <- wrapInference(model, "color_size", "continuous", 5000, 15, 50000)
continuousInferenceScript22 <- wrapInference(model, "color_size", "continuous", 5000, 5, 50000)
continuousInferenceScript23 <- wrapInference(model, "color_size", "continuous", 5000, 1, 50000)

# "TRAIN" MODEL

continuousPosteriors00 <- webppl(continuousInferenceScript00, data = df, data_var = "df", random_seed=3333)
continuousPosteriors01 <- webppl(continuousInferenceScript01, data = df, data_var = "df", random_seed=3333)
continuousPosteriors02 <- webppl(continuousInferenceScript02, data = df, data_var = "df", random_seed=3333)
continuousParamGrp02 <- continuousPosteriors02 %>% group_by(Parameter) %>% summarise(variance = var(value))

continuousPosteriors03 <- webppl(continuousInferenceScript03, data = df, data_var = "df", random_seed=3333)
continuousParamGrp03 <- continuousPosteriors03 %>% group_by(Parameter) %>% summarise(variance = var(value))

continuousPosteriors10 <- webppl(continuousInferenceScript10, data = df, data_var = "df", random_seed=3333)
continuousPosteriors11 <- webppl(continuousInferenceScript11, data = df, data_var = "df", random_seed=3333)
continuousPosteriors12 <- webppl(continuousInferenceScript12, data = df, data_var = "df", random_seed=3333)
continuousPosteriors13 <- webppl(continuousInferenceScript13, data = df, data_var = "df", random_seed=3333)

continuousPosteriors20 <- webppl(continuousInferenceScript20, data = df, data_var = "df", random_seed=3333)
continuousPosteriors21 <- webppl(continuousInferenceScript21, data = df, data_var = "df", random_seed=3333)
continuousPosteriors22 <- webppl(continuousInferenceScript22, data = df, data_var = "df", random_seed=3333)
continuousPosteriors23 <- webppl(continuousInferenceScript23, data = df, data_var = "df", random_seed=3333)

# PLOT GRAPHS
graphPosteriors(continuousPosteriors) + ggtitle("continuous posteriors")
ggsave("results/continuous/ctsl_continuousPosteriors.png")

graphPosteriors(continuousPosteriors00) + ggtitle("continuous posteriors: burn-in 70000")
ggsave("results/continuous/ctsl_continuousPosteriors00.png")

graphPosteriors(continuousPosteriors01) + ggtitle("continuous posteriors: burn-in 60000")
ggsave("results/continuous/ctsl_continuousPosteriors01.png")

graphPosteriors(continuousPosteriors02) + ggtitle("continuous posteriors: burn-in 40000")
ggsave("results/continuous/ctsl_continuousPosteriors02.png")

graphPosteriors(continuousPosteriors03) + ggtitle("continuous posteriors: burn-in 30000")
ggsave("results/continuous/ctsl_continuousPosteriors03.png")

graphPosteriors(continuousPosteriors10) + ggtitle("continuous posteriors: samples 7000")
ggsave("results/continuous/ctsl_continuousPosteriors10.png")

graphPosteriors(continuousPosteriors11) + ggtitle("continuous posteriors: samples 6000")
ggsave("results/continuous/ctsl_continuousPosteriors11.png")

graphPosteriors(continuousPosteriors12) + ggtitle("continuous posteriors: samples 4000")
ggsave("results/continuous/ctsl_continuousPosteriors12.png")

graphPosteriors(continuousPosteriors13) + ggtitle("continuous posteriors: samples 3000")
ggsave("results/continuous/ctsl_continuousPosteriors13.png")

graphPosteriors(continuousPosteriors20) + ggtitle("continuous posteriors: lag 20")
ggsave("results/continuous/ctsl_continuousPosteriors20.png")

graphPosteriors(continuousPosteriors21) + ggtitle("continuous posteriors: lag 15")
ggsave("results/continuous/ctsl_continuousPosteriors21.png")

graphPosteriors(continuousPosteriors22) + ggtitle("continuous posteriors: lag 5")
ggsave("results/continuous/ctsl_continuousPosteriors22.png")

graphPosteriors(continuousPosteriors23) + ggtitle("continuous posteriors: lag 1")
ggsave("results/continuous/ctsl_continuousPosteriors23.png")

# ESTIMATES

continuousEstimates <- getEstimates(continuousPosteriors) 
continuousEstimates00 <- getEstimates(continuousPosteriors00)
continuousEstimates01 <- getEstimates(continuousPosteriors01)
continuousEstimates02 <- getEstimates(continuousPosteriors02)
continuousEstimates03 <- getEstimates(continuousPosteriors03)

continuousEstimates10 <- getEstimates(continuousPosteriors10)
continuousEstimates11 <- getEstimates(continuousPosteriors11)
continuousEstimates12 <- getEstimates(continuousPosteriors12)
continuousEstimates13 <- getEstimates(continuousPosteriors13)

continuousEstimates20 <- getEstimates(continuousPosteriors20)
continuousEstimates21 <- getEstimates(continuousPosteriors21)
continuousEstimates22 <- getEstimates(continuousPosteriors22)
continuousEstimates23 <- getEstimates(continuousPosteriors23)


continuousPredictionScript <- wrapPrediction(model, continuousEstimates,
                                                        "START color size STOP", 
                                                        "color_size",
                                                        "continuous")
continuousPredictionScript00 <- wrapPrediction(model, continuousEstimates00,
                                                          "START color size STOP", 
                                                          "color_size",
                                                          "continuous")
continuousPredictionScript01 <- wrapPrediction(model, continuousEstimates01,
                                                          "START color size STOP", 
                                                          "color_size",
                                                          "continuous")
continuousPredictionScript02 <- wrapPrediction(model, continuousEstimates02,
                                                          "START color size STOP", 
                                                          "color_size",
                                                          "continuous")
continuousPredictionScript03 <- wrapPrediction(model, continuousEstimates03,
                                                          "START color size STOP", 
                                                          "color_size",
                                                          "continuous")

continuousPredictionScript10 <- wrapPrediction(model, continuousEstimates10,
                                                          "START color size STOP", 
                                                          "color_size",
                                                          "continuous")
continuousPredictionScript11 <- wrapPrediction(model, continuousEstimates11,
                                                          "START color size STOP", 
                                                          "color_size",
                                                          "continuous")
continuousPredictionScript12 <- wrapPrediction(model, continuousEstimates12,
                                                          "START color size STOP", 
                                                          "color_size",
                                                          "continuous")
continuousPredictionScript13 <- wrapPrediction(model, continuousEstimates13,
                                                          "START color size STOP", 
                                                          "color_size",
                                                          "continuous")


continuousPredictionScript20 <- wrapPrediction(model, continuousEstimates20,
                                                          "START color size STOP", 
                                                          "color_size",
                                                          "continuous")

continuousPredictionScript21 <- wrapPrediction(model, continuousEstimates21,
                                                          "START color size STOP", 
                                                          "color_size",
                                                          "continuous")

continuousPredictionScript22 <- wrapPrediction(model, continuousEstimates22,
                                                          "START color size STOP", 
                                                          "color_size",
                                                          "continuous")
continuousPredictionScript23 <- wrapPrediction(model, continuousEstimates23,
                                                          "START color size STOP", 
                                                          "color_size",
                                                          "continuous")



continuousPredictives <- webppl(continuousPredictionScript, data = unique(df %>%  select(condition,states,utterances)), data_var = "df")

continuousPredictives00 <- webppl(continuousPredictionScript00, data = unique(df %>%  select(condition,states,utterances)), data_var = "df")
continuousPredictives01 <- webppl(continuousPredictionScript01, data = unique(df %>%  select(condition,states,utterances)), data_var = "df")
continuousPredictives02 <- webppl(continuousPredictionScript02, data = unique(df %>%  select(condition,states,utterances)), data_var = "df")
continuousPredictives03 <- webppl(continuousPredictionScript03, data = unique(df %>%  select(condition,states,utterances)), data_var = "df")

continuousPredictives20 <- webppl(continuousPredictionScript10, data = unique(df %>%  select(condition,states,utterances)), data_var = "df")
continuousPredictives21 <- webppl(continuousPredictionScript11, data = unique(df %>%  select(condition,states,utterances)), data_var = "df")
continuousPredictives22 <- webppl(continuousPredictionScript12, data = unique(df %>%  select(condition,states,utterances)), data_var = "df")
continuousPredictives23 <- webppl(continuousPredictionScript13, data = unique(df %>%  select(condition,states,utterances)), data_var = "df")

continuousPredictives10 <- webppl(continuousPredictionScript20, data = unique(df %>%  select(condition,states,utterances)), data_var = "df")
continuousPredictives11 <- webppl(continuousPredictionScript21, data = unique(df %>%  select(condition,states,utterances)), data_var = "df")
continuousPredictives12 <- webppl(continuousPredictionScript22, data = unique(df %>%  select(condition,states,utterances)), data_var = "df")
continuousPredictives13 <- webppl(continuousPredictionScript23, data = unique(df %>%  select(condition,states,utterances)), data_var = "df")





# MODEL 3: INCREMENTAL 
# POSTERIORS

incrementalInferenceScript <- wrapInference(model, "color_size", "incremental", 5000, 10, 50000)

incrementalInferenceScript00 <- wrapInference(model, "color_size", "incremental", 5000, 10, 70000)
incrementalInferenceScript01 <- wrapInference(model, "color_size", "incremental", 5000, 10, 60000)

incrementalInferenceScript02 <- wrapInference(model, "color_size", "incremental", 5000, 10, 40000)
incrementalInferenceScript03 <- wrapInference(model, "color_size", "incremental", 5000, 10, 30000)

incrementalInferenceScript10 <- wrapInference(model, "color_size", "incremental", 7000, 10, 50000)
incrementalInferenceScript11 <- wrapInference(model, "color_size", "incremental", 6000, 10, 50000)
incrementalInferenceScript12 <- wrapInference(model, "color_size", "incremental", 4000, 10, 50000)
incrementalInferenceScript13 <- wrapInference(model, "color_size", "incremental", 3000, 10, 50000)

incrementalInferenceScript20 <- wrapInference(model, "color_size", "incremental", 5000, 20, 50000)
incrementalInferenceScript21 <- wrapInference(model, "color_size", "incremental", 5000, 15, 50000)
incrementalInferenceScript22 <- wrapInference(model, "color_size", "incremental", 5000, 5, 50000)
incrementalInferenceScript23 <- wrapInference(model, "color_size", "incremental", 5000, 1, 50000)

# "TRAIN" MODEL

incrementalPosteriors <- webppl(incrementalInferenceScript, data = df, data_var = "df", random_seed=3333)

incrementalPosteriors00 <- webppl(incrementalInferenceScript00, data = df, data_var = "df", random_seed=3333)
incrementalPosteriors01 <- webppl(incrementalInferenceScript01, data = df, data_var = "df", random_seed=3333)

incrementalPosteriors02 <- webppl(incrementalInferenceScript02, data = df, data_var = "df", random_seed=3333)
incrementalParamGrp02 <- incrementalPosteriors02 %>% group_by(Parameter) %>% summarise(variance = var(value))

incrementalPosteriors03 <- webppl(incrementalInferenceScript03, data = df, data_var = "df", random_seed=3333)
incrementalParamGrp03 <- incrementalPosteriors03 %>% group_by(Parameter) %>% summarise(variance = var(value))

incrementalPosteriors10 <- webppl(incrementalInferenceScript10, data = df, data_var = "df", random_seed=3333)
incrementalPosteriors11 <- webppl(incrementalInferenceScript11, data = df, data_var = "df", random_seed=3333)
incrementalPosteriors12 <- webppl(incrementalInferenceScript12, data = df, data_var = "df", random_seed=3333)
incrementalPosteriors13 <- webppl(incrementalInferenceScript13, data = df, data_var = "df", random_seed=3333)

incrementalPosteriors20 <- webppl(incrementalInferenceScript20, data = df, data_var = "df", random_seed=3333)
incrementalPosteriors21 <- webppl(incrementalInferenceScript21, data = df, data_var = "df", random_seed=3333)
incrementalPosteriors22 <- webppl(incrementalInferenceScript22, data = df, data_var = "df", random_seed=3333)
incrementalPosteriors23 <- webppl(incrementalInferenceScript23, data = df, data_var = "df", random_seed=3333)

# PLOT GRAPHS
graphPosteriors(incrementalPosteriors) + ggtitle("incremental posteriors")
ggsave("results/incremental/ctsl_incrementalPosteriors.png")

graphPosteriors(incrementalPosteriors00) + ggtitle("incremental posteriors: burn-in 70000")
ggsave("results/incremental/ctsl_incrementalPosteriors00.png")

graphPosteriors(incrementalPosteriors01) + ggtitle("incremental posteriors: burn-in 60000")
ggsave("results/incremental/ctsl_incrementalPosteriors01.png")

graphPosteriors(incrementalPosteriors02) + ggtitle("incremental posteriors: burn-in 40000")
ggsave("results/incremental/ctsl_incrementalPosteriors02.png")

graphPosteriors(incrementalPosteriors03) + ggtitle("incremental posteriors: burn-in 30000")
ggsave("results/incremental/ctsl_incrementalPosteriors03.png")

graphPosteriors(incrementalPosteriors10) + ggtitle("incremental posteriors: samples 7000")
ggsave("results/incremental/ctsl_incrementalPosteriors10.png")

graphPosteriors(incrementalPosteriors11) + ggtitle("incremental posteriors: samples 6000")
ggsave("results/incremental/ctsl_incrementalPosteriors11.png")

graphPosteriors(incrementalPosteriors12) + ggtitle("incremental posteriors: samples 4000")
ggsave("results/incremental/ctsl_incrementalPosteriors12.png")

graphPosteriors(incrementalPosteriors13) + ggtitle("incremental posteriors: samples 3000")
ggsave("results/incremental/ctsl_incrementalPosteriors13.png")

graphPosteriors(incrementalPosteriors20) + ggtitle("incremental posteriors: lag 20")
ggsave("results/incremental/ctsl_incrementalPosteriors20.png")

graphPosteriors(incrementalPosteriors21) + ggtitle("incremental posteriors: lag 15")
ggsave("results/incremental/ctsl_incrementalPosteriors21.png")

graphPosteriors(incrementalPosteriors22) + ggtitle("incremental posteriors: lag 5")
ggsave("results/incremental/ctsl_incrementalPosteriors22.png")

graphPosteriors(incrementalPosteriors23) + ggtitle("incremental posteriors: lag 1")
ggsave("results/incremental/ctsl_incrementalPosteriors23.png")

# ESTIMATES

incrementalEstimates <- getEstimates(incrementalPosteriors) 
incrementalEstimates00 <- getEstimates(incrementalPosteriors00)
incrementalEstimates01 <- getEstimates(incrementalPosteriors01)
incrementalEstimates02 <- getEstimates(incrementalPosteriors02)
incrementalEstimates03 <- getEstimates(incrementalPosteriors03)

incrementalEstimates10 <- getEstimates(incrementalPosteriors10)
incrementalEstimates11 <- getEstimates(incrementalPosteriors11)
incrementalEstimates12 <- getEstimates(incrementalPosteriors12)
incrementalEstimates13 <- getEstimates(incrementalPosteriors13)

incrementalEstimates20 <- getEstimates(incrementalPosteriors20)
incrementalEstimates21 <- getEstimates(incrementalPosteriors21)
incrementalEstimates22 <- getEstimates(incrementalPosteriors22)
incrementalEstimates23 <- getEstimates(incrementalPosteriors23)


incrementalPredictionScript <- wrapPrediction(model, incrementalEstimates,
                                              "START color size STOP", 
                                              "color_size",
                                              "incremental")
incrementalPredictionScript00 <- wrapPrediction(model, incrementalEstimates00,
                                                "START color size STOP", 
                                                "color_size",
                                                "incremental")
incrementalPredictionScript01 <- wrapPrediction(model, incrementalEstimates01,
                                                "START color size STOP", 
                                                "color_size",
                                                "incremental")
incrementalPredictionScript02 <- wrapPrediction(model, incrementalEstimates02,
                                                "START color size STOP", 
                                                "color_size",
                                                "incremental")
incrementalPredictionScript03 <- wrapPrediction(model, incrementalEstimates03,
                                                "START color size STOP", 
                                                "color_size",
                                                "incremental")

incrementalPredictionScript10 <- wrapPrediction(model, incrementalEstimates10,
                                                "START color size STOP", 
                                                "color_size",
                                                "incremental")
incrementalPredictionScript11 <- wrapPrediction(model, incrementalEstimates11,
                                                "START color size STOP", 
                                                "color_size",
                                                "incremental")
incrementalPredictionScript12 <- wrapPrediction(model, incrementalEstimates12,
                                                "START color size STOP", 
                                                "color_size",
                                                "incremental")
incrementalPredictionScript13 <- wrapPrediction(model, incrementalEstimates13,
                                                "START color size STOP", 
                                                "color_size",
                                                "incremental")


incrementalPredictionScript20 <- wrapPrediction(model, incrementalEstimates20,
                                                "START color size STOP", 
                                                "color_size",
                                                "incremental")

incrementalPredictionScript21 <- wrapPrediction(model, incrementalEstimates21,
                                                "START color size STOP", 
                                                "color_size",
                                                "incremental")

incrementalPredictionScript22 <- wrapPrediction(model, incrementalEstimates22,
                                                "START color size STOP", 
                                                "color_size",
                                                "incremental")
incrementalPredictionScript23 <- wrapPrediction(model, incrementalEstimates23,
                                                "START color size STOP", 
                                                "color_size",
                                                "incremental")



incrementalPredictives <- webppl(incrementalPredictionScript, data = unique(df %>%  select(condition,states,utterances)), data_var = "df")

incrementalPredictives00 <- webppl(incrementalPredictionScript00, data = unique(df %>%  select(condition,states,utterances)), data_var = "df")
incrementalPredictives01 <- webppl(incrementalPredictionScript01, data = unique(df %>%  select(condition,states,utterances)), data_var = "df")
incrementalPredictives02 <- webppl(incrementalPredictionScript02, data = unique(df %>%  select(condition,states,utterances)), data_var = "df")
incrementalPredictives03 <- webppl(incrementalPredictionScript03, data = unique(df %>%  select(condition,states,utterances)), data_var = "df")

incrementalPredictives20 <- webppl(incrementalPredictionScript10, data = unique(df %>%  select(condition,states,utterances)), data_var = "df")
incrementalPredictives21 <- webppl(incrementalPredictionScript11, data = unique(df %>%  select(condition,states,utterances)), data_var = "df")
incrementalPredictives22 <- webppl(incrementalPredictionScript12, data = unique(df %>%  select(condition,states,utterances)), data_var = "df")
incrementalPredictives23 <- webppl(incrementalPredictionScript13, data = unique(df %>%  select(condition,states,utterances)), data_var = "df")

incrementalPredictives10 <- webppl(incrementalPredictionScript20, data = unique(df %>%  select(condition,states,utterances)), data_var = "df")
incrementalPredictives11 <- webppl(incrementalPredictionScript21, data = unique(df %>%  select(condition,states,utterances)), data_var = "df")
incrementalPredictives12 <- webppl(incrementalPredictionScript22, data = unique(df %>%  select(condition,states,utterances)), data_var = "df")
incrementalPredictives13 <- webppl(incrementalPredictionScript23, data = unique(df %>%  select(condition,states,utterances)), data_var = "df")



# MODEL 4 : incrementalContinuous 
# POSTERIORS

incrementalContinuousInferenceScript <- wrapInference(model, "color_size", "incrementalContinuous", 5000, 10, 50000)

incrementalContinuousInferenceScript00 <- wrapInference(model, "color_size", "incrementalContinuous", 5000, 10, 70000)
incrementalContinuousInferenceScript01 <- wrapInference(model, "color_size", "incrementalContinuous", 5000, 10, 60000)
incrementalContinuousInferenceScript02 <- wrapInference(model, "color_size", "incrementalContinuous", 5000, 10, 40000)
incrementalContinuousInferenceScript03 <- wrapInference(model, "color_size", "incrementalContinuous", 5000, 10, 30000)

incrementalContinuousInferenceScript10 <- wrapInference(model, "color_size", "incrementalContinuous", 7000, 10, 50000)
incrementalContinuousInferenceScript11 <- wrapInference(model, "color_size", "incrementalContinuous", 6000, 10, 50000)
incrementalContinuousInferenceScript12 <- wrapInference(model, "color_size", "incrementalContinuous", 4000, 10, 50000)
incrementalContinuousInferenceScript13 <- wrapInference(model, "color_size", "incrementalContinuous", 3000, 10, 50000)

incrementalContinuousInferenceScript20 <- wrapInference(model, "color_size", "incrementalContinuous", 5000, 20, 50000)
incrementalContinuousInferenceScript21 <- wrapInference(model, "color_size", "incrementalContinuous", 5000, 15, 50000)
incrementalContinuousInferenceScript22 <- wrapInference(model, "color_size", "incrementalContinuous", 5000, 5, 50000)
incrementalContinuousInferenceScript23 <- wrapInference(model, "color_size", "incrementalContinuous", 5000, 1, 50000)

# "TRAIN" MODEL

incrementalContinuousPosteriors <- webppl(incrementalContinuousInferenceScript, data = df, data_var = "df", random_seed=3333)

incrementalContinuousPosteriors00 <- webppl(incrementalContinuousInferenceScript00, data = df, data_var = "df", random_seed=3333)
incrementalContinuousPosteriors01 <- webppl(incrementalContinuousInferenceScript01, data = df, data_var = "df", random_seed=3333)

incrementalContinuousPosteriors02 <- webppl(incrementalContinuousInferenceScript02, data = df, data_var = "df", random_seed=3333)
incrementalContinuousParamGrp02 <- incrementalContinuousPosteriors02 %>% group_by(Parameter) %>% summarise(variance = var(value))

incrementalContinuousPosteriors03 <- webppl(incrementalContinuousInferenceScript03, data = df, data_var = "df", random_seed=3333)
incrementalContinuousParamGrp03 <- incrementalContinuousPosteriors03 %>% group_by(Parameter) %>% summarise(variance = var(value))

incrementalContinuousPosteriors10 <- webppl(incrementalContinuousInferenceScript10, data = df, data_var = "df", random_seed=3333)
incrementalContinuousPosteriors11 <- webppl(incrementalContinuousInferenceScript11, data = df, data_var = "df", random_seed=3333)
incrementalContinuousPosteriors12 <- webppl(incrementalContinuousInferenceScript12, data = df, data_var = "df", random_seed=3333)
incrementalContinuousPosteriors13 <- webppl(incrementalContinuousInferenceScript13, data = df, data_var = "df", random_seed=3333)

incrementalContinuousPosteriors20 <- webppl(incrementalContinuousInferenceScript20, data = df, data_var = "df", random_seed=3333)
incrementalContinuousPosteriors21 <- webppl(incrementalContinuousInferenceScript21, data = df, data_var = "df", random_seed=3333)
incrementalContinuousPosteriors22 <- webppl(incrementalContinuousInferenceScript22, data = df, data_var = "df", random_seed=3333)
incrementalContinuousPosteriors23 <- webppl(incrementalContinuousInferenceScript23, data = df, data_var = "df", random_seed=3333)

# PLOT GRAPHS
graphPosteriors(incrementalContinuousPosteriors) + ggtitle("incrementalContinuous posteriors")
ggsave("results/incrementalContinuous/ctsl_incrementalContinuousPosteriors.png")

graphPosteriors(incrementalContinuousPosteriors00) + ggtitle("incrementalContinuous posteriors: burn-in 70000")
ggsave("results/incrementalContinuous/ctsl_incrementalContinuousPosteriors00.png")

graphPosteriors(incrementalContinuousPosteriors01) + ggtitle("incrementalContinuous posteriors: burn-in 60000")
ggsave("results/incrementalContinuous/ctsl_incrementalContinuousPosteriors01.png")

graphPosteriors(incrementalContinuousPosteriors02) + ggtitle("incrementalContinuous posteriors: burn-in 40000")
ggsave("results/incrementalContinuous/ctsl_incrementalContinuousPosteriors02.png")

graphPosteriors(incrementalContinuousPosteriors03) + ggtitle("incrementalContinuous posteriors: burn-in 30000")
ggsave("results/incrementalContinuous/ctsl_incrementalContinuousPosteriors03.png")

graphPosteriors(incrementalContinuousPosteriors10) + ggtitle("incrementalContinuous posteriors: samples 7000")
ggsave("results/incrementalContinuous/ctsl_incrementalContinuousPosteriors10.png")

graphPosteriors(incrementalContinuousPosteriors11) + ggtitle("incrementalContinuous posteriors: samples 6000")
ggsave("results/incrementalContinuous/ctsl_incrementalContinuousPosteriors11.png")

graphPosteriors(incrementalContinuousPosteriors12) + ggtitle("incrementalContinuous posteriors: samples 4000")
ggsave("results/incrementalContinuous/ctsl_incrementalContinuousPosteriors12.png")

graphPosteriors(incrementalContinuousPosteriors13) + ggtitle("incrementalContinuous posteriors: samples 3000")
ggsave("results/incrementalContinuous/ctsl_incrementalContinuousPosteriors13.png")

graphPosteriors(incrementalContinuousPosteriors20) + ggtitle("incrementalContinuous posteriors: lag 20")
ggsave("results/incrementalContinuous/ctsl_incrementalContinuousPosteriors20.png")

graphPosteriors(incrementalContinuousPosteriors21) + ggtitle("incrementalContinuous posteriors: lag 15")
ggsave("results/incrementalContinuous/ctsl_incrementalContinuousPosteriors21.png")

graphPosteriors(incrementalContinuousPosteriors22) + ggtitle("incrementalContinuous posteriors: lag 5")
ggsave("results/incrementalContinuous/ctsl_incrementalContinuousPosteriors22.png")

graphPosteriors(incrementalContinuousPosteriors23) + ggtitle("incrementalContinuous posteriors: lag 1")
ggsave("results/incrementalContinuous/ctsl_incrementalContinuousPosteriors23.png")

# ESTIMATES

incrementalContinuousEstimates <- getEstimates(incrementalContinuousPosteriors) 
incrementalContinuousEstimates00 <- getEstimates(incrementalContinuousPosteriors00)
incrementalContinuousEstimates01 <- getEstimates(incrementalContinuousPosteriors01)
incrementalContinuousEstimates02 <- getEstimates(incrementalContinuousPosteriors02)
incrementalContinuousEstimates03 <- getEstimates(incrementalContinuousPosteriors03)

incrementalContinuousEstimates10 <- getEstimates(incrementalContinuousPosteriors10)
incrementalContinuousEstimates11 <- getEstimates(incrementalContinuousPosteriors11)
incrementalContinuousEstimates12 <- getEstimates(incrementalContinuousPosteriors12)
incrementalContinuousEstimates13 <- getEstimates(incrementalContinuousPosteriors13)

incrementalContinuousEstimates20 <- getEstimates(incrementalContinuousPosteriors20)
incrementalContinuousEstimates21 <- getEstimates(incrementalContinuousPosteriors21)
incrementalContinuousEstimates22 <- getEstimates(incrementalContinuousPosteriors22)
incrementalContinuousEstimates23 <- getEstimates(incrementalContinuousPosteriors23)



incrementalContinuousPredictionScript <- wrapPrediction(model, incrementalContinuousEstimates,
                                              "START color size STOP", 
                                              "color_size",
                                              "incrementalContinuous")
incrementalContinuousPredictionScript00 <- wrapPrediction(model, incrementalContinuousEstimates00,
                                              "START color size STOP", 
                                              "color_size",
                                              "incrementalContinuous")
incrementalContinuousPredictionScript01 <- wrapPrediction(model, incrementalContinuousEstimates01,
                                              "START color size STOP", 
                                              "color_size",
                                              "incrementalContinuous")
incrementalContinuousPredictionScript02 <- wrapPrediction(model, incrementalContinuousEstimates02,
                                              "START color size STOP", 
                                              "color_size",
                                              "incrementalContinuous")
incrementalContinuousPredictionScript03 <- wrapPrediction(model, incrementalContinuousEstimates03,
                                              "START color size STOP", 
                                              "color_size",
                                              "incrementalContinuous")

incrementalContinuousPredictionScript10 <- wrapPrediction(model, incrementalContinuousEstimates10,
                                              "START color size STOP", 
                                              "color_size",
                                              "incrementalContinuous")
incrementalContinuousPredictionScript11 <- wrapPrediction(model, incrementalContinuousEstimates11,
                                              "START color size STOP", 
                                              "color_size",
                                              "incrementalContinuous")
incrementalContinuousPredictionScript12 <- wrapPrediction(model, incrementalContinuousEstimates12,
                                              "START color size STOP", 
                                              "color_size",
                                              "incrementalContinuous")
incrementalContinuousPredictionScript13 <- wrapPrediction(model, incrementalContinuousEstimates13,
                                              "START color size STOP", 
                                              "color_size",
                                              "incrementalContinuous")


incrementalContinuousPredictionScript20 <- wrapPrediction(model, incrementalContinuousEstimates20,
                                              "START color size STOP", 
                                              "color_size",
                                              "incrementalContinuous")

incrementalContinuousPredictionScript21 <- wrapPrediction(model, incrementalContinuousEstimates21,
                                              "START color size STOP", 
                                              "color_size",
                                              "incrementalContinuous")

incrementalContinuousPredictionScript22 <- wrapPrediction(model, incrementalContinuousEstimates22,
                                              "START color size STOP", 
                                              "color_size",
                                              "incrementalContinuous")
incrementalContinuousPredictionScript23 <- wrapPrediction(model, incrementalContinuousEstimates23,
                                              "START color size STOP", 
                                              "color_size",
                                              "incrementalContinuous")



incrementalContinuousPredictives <- webppl(incrementalContinuousPredictionScript, data = unique(df %>%  select(condition,states,utterances)), data_var = "df")

incrementalContinuousPredictives00 <- webppl(incrementalContinuousPredictionScript00, data = unique(df %>%  select(condition,states,utterances)), data_var = "df")
incrementalContinuousPredictives01 <- webppl(incrementalContinuousPredictionScript01, data = unique(df %>%  select(condition,states,utterances)), data_var = "df")
incrementalContinuousPredictives02 <- webppl(incrementalContinuousPredictionScript02, data = unique(df %>%  select(condition,states,utterances)), data_var = "df")
incrementalContinuousPredictives03 <- webppl(incrementalContinuousPredictionScript03, data = unique(df %>%  select(condition,states,utterances)), data_var = "df")

incrementalContinuousPredictives20 <- webppl(incrementalContinuousPredictionScript10, data = unique(df %>%  select(condition,states,utterances)), data_var = "df")
incrementalContinuousPredictives21 <- webppl(incrementalContinuousPredictionScript11, data = unique(df %>%  select(condition,states,utterances)), data_var = "df")
incrementalContinuousPredictives22 <- webppl(incrementalContinuousPredictionScript12, data = unique(df %>%  select(condition,states,utterances)), data_var = "df")
incrementalContinuousPredictives23 <- webppl(incrementalContinuousPredictionScript13, data = unique(df %>%  select(condition,states,utterances)), data_var = "df")

incrementalContinuousPredictives10 <- webppl(incrementalContinuousPredictionScript20, data = unique(df %>%  select(condition,states,utterances)), data_var = "df")
incrementalContinuousPredictives11 <- webppl(incrementalContinuousPredictionScript21, data = unique(df %>%  select(condition,states,utterances)), data_var = "df")
incrementalContinuousPredictives12 <- webppl(incrementalContinuousPredictionScript22, data = unique(df %>%  select(condition,states,utterances)), data_var = "df")
incrementalContinuousPredictives13 <- webppl(incrementalContinuousPredictionScript23, data = unique(df %>%  select(condition,states,utterances)), data_var = "df")

graphPredictives(incrementalPredictives, d_collapsed) + ggtitle("Incremental predictives")

ggsave("results/ctsl_incrementalPredictives.png", width = 4, height = 3, units = "in")


# # STEP 1: WRAP INFERENCE COMMAND AROUND CORE MODEL

incrementalContinuousVGlobalInferenceCommand <- read_file("incrementalContinuousVGlobalComparison/inferenceCommand.txt")

# # # (TODO [LEYLA]: UP THE SAMPLE/LAG/BURN/RATE)

incrementalContinuousVGlobalInferenceCommand <- gsub("TARGET_REFERENT", "color_size", incrementalContinuousVGlobalInferenceCommand, fixed = TRUE)
incrementalContinuousVGlobalInferenceCommand <- gsub("NUM_SAMPLES", 5000, incrementalContinuousVGlobalInferenceCommand, fixed = TRUE)
incrementalContinuousVGlobalInferenceCommand <- gsub("LAG", 10, incrementalContinuousVGlobalInferenceCommand, fixed = TRUE)
incrementalContinuousVGlobalInferenceCommand <- gsub("BURN_IN", 50000, incrementalContinuousVGlobalInferenceCommand, fixed = TRUE)

incrementalContinuousVGlobalInferenceScript <- paste(read_file(model), incrementalContinuousVGlobalInferenceCommand, sep = "\n")

# # STEP 2: RUN SCRIPT AND GRAPH POSTERIORS 

incrementalContinuousVGlobalPosteriors <- webppl(incrementalContinuousVGlobalInferenceScript, data = df, data_var = "df", random_seed = 3333)

graphPosteriors(incrementalContinuousVGlobalPosteriors %>% filter(!(Parameter == "incrementalContinuousOrGlobal")) %>% mutate(value = as.numeric(value))) + ggtitle("Model parameter posteriors")

ggsave("incrementalContinuousVGlobalComparison/modelPosteriors.png")

# # STEP 3: CALCULATE POSTERIOR PROBABILITY OF incrementalContinuous VS. GLOBAL

modelPosterior <- incrementalContinuousVGlobalPosteriors %>% filter(Parameter == "incrementalContinuousOrGlobal") %>%
  count(value) %>%
  group_by(value) %>%
  summarize(posteriorProb = n / sum(n))

View(modelPosterior)

save.image("results/ctsl_comparison.RData")



######################################################
# MERGED PLOTS
######################################################
# modification type
empirical_toplot = d_uncollapsed %>%
  mutate(size = ifelse(response=="size",1,0)) %>%
  mutate(color = ifelse(response=="color",1,0)) %>%
  mutate(size_color = ifelse(response=="size_color",1,0)) %>%
  select(gameId,roundNumber,condition,response,size,color,size_color) %>%
  gather(utterance,value,size:size_color) %>%
  group_by(utterance,condition) %>%
  dplyr::summarize(Mean=mean(value)) %>%
  #summarise(Mean=mean(value),CILow=ci.low(value),CIHigh=ci.high(value)) %>%
  #ungroup() %>%
  #mutate(YMin=Mean-CILow,YMax=Mean+CIHigh) %>%
  mutate(model="empirical")

vanilla_toplot = vanillaPredictives %>%
  gather(utterance,Mean,size_color:size) %>%
  mutate(model="vanilla")

continuous_toplot = continuousPredictives %>%
  gather(utterance,Mean,size_color:size) %>%
  mutate(model="continuous")

incremental_toplot = incrementalPredictives %>%
  gather(utterance,Mean,size_color:size) %>%
  mutate(model="incremental")

incrementalContinuous_toplot = incrementalContinuousPredictives %>%
  gather(utterance,Mean,size_color:size) %>%
  mutate(model="incrementalContinuous")


# CONDITION 00

vanilla_toplot00 = vanillaPredictives00 %>%
  gather(utterance,Mean,size_color:size) %>%
  mutate(model="vanilla")

continuous_toplot00 = continuousPredictives00 %>%
  gather(utterance,Mean,size_color:size) %>%
  mutate(model="continuous")

incremental_toplot00 = incrementalPredictives00 %>%
  gather(utterance,Mean,size_color:size) %>%
  mutate(model="incremental")

incrementalContinuous_toplot00 = incrementalContinuousPredictives00 %>%
  gather(utterance,Mean,size_color:size) %>%
  mutate(model="incrementalContinuous")

# CONDITION 01

vanilla_toplot01 = vanillaPredictives01 %>%
  gather(utterance,Mean,size_color:size) %>%
  mutate(model="vanilla")

continuous_toplot01 = continuousPredictives01 %>%
  gather(utterance,Mean,size_color:size) %>%
  mutate(model="continuous")

incremental_toplot01 = incrementalPredictives01 %>%
  gather(utterance,Mean,size_color:size) %>%
  mutate(model="incremental")

incrementalContinuous_toplot01 = incrementalContinuousPredictives01 %>%
  gather(utterance,Mean,size_color:size) %>%
  mutate(model="incrementalContinuous")

# CONDITION 02

vanilla_toplot02 = vanillaPredictives02 %>%
  gather(utterance,Mean,size_color:size) %>%
  mutate(model="vanilla")

continuous_toplot02 = continuousPredictives02 %>%
  gather(utterance,Mean,size_color:size) %>%
  mutate(model="continuous")

incremental_toplot02 = incrementalPredictives02 %>%
  gather(utterance,Mean,size_color:size) %>%
  mutate(model="incremental")

incrementalContinuous_toplot02 = incrementalContinuousPredictives02 %>%
  gather(utterance,Mean,size_color:size) %>%
  mutate(model="incrementalContinuous")

# CONDITION 03

vanilla_toplot03 = vanillaPredictives03 %>%
  gather(utterance,Mean,size_color:size) %>%
  mutate(model="vanilla")

continuous_toplot03 = continuousPredictives03 %>%
  gather(utterance,Mean,size_color:size) %>%
  mutate(model="continuous")

incremental_toplot03 = incrementalPredictives03 %>%
  gather(utterance,Mean,size_color:size) %>%
  mutate(model="incremental")

incrementalContinuous_toplot03 = incrementalContinuousPredictives03 %>%
  gather(utterance,Mean,size_color:size) %>%
  mutate(model="incrementalContinuous")

# CONDITION 10

vanilla_toplot10 = vanillaPredictives10 %>%
  gather(utterance,Mean,size_color:size) %>%
  mutate(model="vanilla")

continuous_toplot10 = continuousPredictives10 %>%
  gather(utterance,Mean,size_color:size) %>%
  mutate(model="continuous")

incremental_toplot10 = incrementalPredictives10 %>%
  gather(utterance,Mean,size_color:size) %>%
  mutate(model="incremental")

incrementalContinuous_toplot10 = incrementalContinuousPredictives10 %>%
  gather(utterance,Mean,size_color:size) %>%
  mutate(model="incrementalContinuous")


# CONDITION 11

vanilla_toplot11 = vanillaPredictives11 %>%
  gather(utterance,Mean,size_color:size) %>%
  mutate(model="vanilla")

continuous_toplot11 = continuousPredictives11 %>%
  gather(utterance,Mean,size_color:size) %>%
  mutate(model="continuous")

incremental_toplot11 = incrementalPredictives11 %>%
  gather(utterance,Mean,size_color:size) %>%
  mutate(model="incremental")

incrementalContinuous_toplot11 = incrementalContinuousPredictives11 %>%
  gather(utterance,Mean,size_color:size) %>%
  mutate(model="incrementalContinuous")

# CONDITION 12

vanilla_toplot12 = vanillaPredictives12 %>%
  gather(utterance,Mean,size_color:size) %>%
  mutate(model="vanilla")

continuous_toplot12 = continuousPredictives12 %>%
  gather(utterance,Mean,size_color:size) %>%
  mutate(model="continuous")

incremental_toplot12 = incrementalPredictives12 %>%
  gather(utterance,Mean,size_color:size) %>%
  mutate(model="incremental")

incrementalContinuous_toplot12 = incrementalContinuousPredictives12 %>%
  gather(utterance,Mean,size_color:size) %>%
  mutate(model="incrementalContinuous")

# CONDITION 13

vanilla_toplot13 = vanillaPredictives13 %>%
  gather(utterance,Mean,size_color:size) %>%
  mutate(model="vanilla")

continuous_toplot13 = continuousPredictives13 %>%
  gather(utterance,Mean,size_color:size) %>%
  mutate(model="continuous")

incremental_toplot13 = incrementalPredictives13 %>%
  gather(utterance,Mean,size_color:size) %>%
  mutate(model="incremental")

incrementalContinuous_toplot13 = incrementalContinuousPredictives13 %>%
  gather(utterance,Mean,size_color:size) %>%
  mutate(model="incrementalContinuous")

# CONDITION 20

vanilla_toplot20 = vanillaPredictives20 %>%
  gather(utterance,Mean,size_color:size) %>%
  mutate(model="vanilla")

continuous_toplot20 = continuousPredictives20 %>%
  gather(utterance,Mean,size_color:size) %>%
  mutate(model="continuous")

incremental_toplot20 = incrementalPredictives20 %>%
  gather(utterance,Mean,size_color:size) %>%
  mutate(model="incremental")

incrementalContinuous_toplot20 = incrementalContinuousPredictives20 %>%
  gather(utterance,Mean,size_color:size) %>%
  mutate(model="incrementalContinuous")


# CONDITION 21

vanilla_toplot21 = vanillaPredictives21 %>%
  gather(utterance,Mean,size_color:size) %>%
  mutate(model="vanilla")

continuous_toplot21 = continuousPredictives21 %>%
  gather(utterance,Mean,size_color:size) %>%
  mutate(model="continuous")

incremental_toplot21 = incrementalPredictives21 %>%
  gather(utterance,Mean,size_color:size) %>%
  mutate(model="incremental")

incrementalContinuous_toplot21 = incrementalContinuousPredictives21 %>%
  gather(utterance,Mean,size_color:size) %>%
  mutate(model="incrementalContinuous")


# CONDITION 22

vanilla_toplot22 = vanillaPredictives22 %>%
  gather(utterance,Mean,size_color:size) %>%
  mutate(model="vanilla")

continuous_toplot22 = continuousPredictives22 %>%
  gather(utterance,Mean,size_color:size) %>%
  mutate(model="continuous")

incremental_toplot22 = incrementalPredictives22 %>%
  gather(utterance,Mean,size_color:size) %>%
  mutate(model="incremental")

incrementalContinuous_toplot22 = incrementalContinuousPredictives22 %>%
  gather(utterance,Mean,size_color:size) %>%
  mutate(model="incrementalContinuous")

# CONDITION 23

vanilla_toplot23 = vanillaPredictives23 %>%
  gather(utterance,Mean,size_color:size) %>%
  mutate(model="vanilla")

continuous_toplot23 = continuousPredictives23 %>%
  gather(utterance,Mean,size_color:size) %>%
  mutate(model="continuous")

incremental_toplot23 = incrementalPredictives23 %>%
  gather(utterance,Mean,size_color:size) %>%
  mutate(model="incremental")

incrementalContinuous_toplot23 = incrementalContinuousPredictives23 %>%
  gather(utterance,Mean,size_color:size) %>%
  mutate(model="incrementalContinuous")


#merge ctsl datasets
ctsl_merged = rbind(empirical_toplot,vanilla_toplot,continuous_toplot,incremental_toplot,incrementalContinuous_toplot) %>%
  mutate(condition=ifelse(condition=="color31","color sufficient",ifelse(condition=="size31","size sufficient",NA))) %>%
  mutate(language="CTSL")

ctsl_merged00 = rbind(empirical_toplot,vanilla_toplot00,continuous_toplot00,incremental_toplot00,incrementalContinuous_toplot00) %>%
  mutate(condition=ifelse(condition=="color31","color sufficient",ifelse(condition=="size31","size sufficient",NA))) %>%
  mutate(language="CTSL")

ctsl_merged01 = rbind(empirical_toplot,vanilla_toplot01,continuous_toplot01,incremental_toplot01,incrementalContinuous_toplot01) %>%
  mutate(condition=ifelse(condition=="color31","color sufficient",ifelse(condition=="size31","size sufficient",NA))) %>%
  mutate(language="CTSL")

ctsl_merged02 = rbind(empirical_toplot,vanilla_toplot02,continuous_toplot02,incremental_toplot02,incrementalContinuous_toplot02) %>%
  mutate(condition=ifelse(condition=="color31","color sufficient",ifelse(condition=="size31","size sufficient",NA))) %>%
  mutate(language="CTSL")

ctsl_merged03 = rbind(empirical_toplot,vanilla_toplot03,continuous_toplot03,incremental_toplot03,incrementalContinuous_toplot03) %>%
  mutate(condition=ifelse(condition=="color31","color sufficient",ifelse(condition=="size31","size sufficient",NA))) %>%
  mutate(language="CTSL")

ctsl_merged10 = rbind(empirical_toplot,vanilla_toplot10,continuous_toplot10,incremental_toplot10,incrementalContinuous_toplot10) %>%
  mutate(condition=ifelse(condition=="color31","color sufficient",ifelse(condition=="size31","size sufficient",NA))) %>%
  mutate(language="CTSL")

ctsl_merged11 = rbind(empirical_toplot,vanilla_toplot11,continuous_toplot11,incremental_toplot11,incrementalContinuous_toplot11) %>%
  mutate(condition=ifelse(condition=="color31","color sufficient",ifelse(condition=="size31","size sufficient",NA))) %>%
  mutate(language="CTSL")

ctsl_merged12 = rbind(empirical_toplot,vanilla_toplot12,continuous_toplot12,incremental_toplot12,incrementalContinuous_toplot12) %>%
  mutate(condition=ifelse(condition=="color31","color sufficient",ifelse(condition=="size31","size sufficient",NA))) %>%
  mutate(language="CTSL")

ctsl_merged13 = rbind(empirical_toplot,vanilla_toplot13,continuous_toplot13,incremental_toplot13,incrementalContinuous_toplot13) %>%
  mutate(condition=ifelse(condition=="color31","color sufficient",ifelse(condition=="size31","size sufficient",NA))) %>%
  mutate(language="CTSL")

ctsl_merged20 = rbind(empirical_toplot,vanilla_toplot20,continuous_toplot20,incremental_toplot20,incrementalContinuous_toplot20) %>%
  mutate(condition=ifelse(condition=="color31","color sufficient",ifelse(condition=="size31","size sufficient",NA))) %>%
  mutate(language="CTSL")

ctsl_merged21 = rbind(empirical_toplot,vanilla_toplot21,continuous_toplot21,incremental_toplot21,incrementalContinuous_toplot21) %>%
  mutate(condition=ifelse(condition=="color31","color sufficient",ifelse(condition=="size31","size sufficient",NA))) %>%
  mutate(language="CTSL")

ctsl_merged22 = rbind(empirical_toplot,vanilla_toplot22,continuous_toplot22,incremental_toplot22,incrementalContinuous_toplot22) %>%
  mutate(condition=ifelse(condition=="color31","color sufficient",ifelse(condition=="size31","size sufficient",NA))) %>%
  mutate(language="CTSL")

ctsl_merged23 = rbind(empirical_toplot,vanilla_toplot23,continuous_toplot23,incremental_toplot23,incrementalContinuous_toplot23) %>%
  mutate(condition=ifelse(condition=="color31","color sufficient",ifelse(condition=="size31","size sufficient",NA))) %>%
  mutate(language="CTSL")


######################################################
# MERGED PLOTS FOR REPLICATION STUDY
######################################################

ctsl_merged02$model = factor(ctsl_merged02$model, levels = c("empirical", "vanilla","continuous","incremental","incrementalContinuous"))
ctsl_merged03$model = factor(ctsl_merged03$model, levels = c("empirical", "vanilla","continuous","incremental","incrementalContinuous"))

ggplot(merged, aes(x=utterance,y=Mean, fill=model)) +
  geom_bar(position="dodge", stat = "identity") +
  facet_grid(~condition)

write.csv(ctsl_merged02, "ctsl02_modelComp.csv", row.names=TRUE)
ggsave("results/ctsl02_modelComparison.png")

ggplot(ctsl_merged03, aes(x=utterance,y=Mean, fill=model)) +
  geom_bar(position="dodge", stat = "identity") +
  facet_grid(~condition)

write.csv(merged, "ctsl03_modelComp.csv", row.names=TRUE)
ggsave("results/ctsl03_modelComparison.png")

write.csv(merged, "ctsl02_modelComp.csv", row.names=TRUE)
ggsave("results/ctsl02_modelComparison.png")


#merge ctsl and english datasets 
english_merged = read_csv("english_modelComp.csv")
english_merged_english02 = read_csv("models/english_modelComp02.csv")
english_merged_english03 = read_csv("models/english_modelComp03.csv")


merged = rbind(ctsl_merged,english_merged)

merged02 = rbind(ctsl_merged02,english_merged_english02)
merged03 = rbind(ctsl_merged03,english_merged_english03)

merged02$model = factor(merged$model, levels = c("empirical", "vanilla","continuous","incremental","incrementalContinuous"))
merged03$model = factor(merged$model, levels = c("empirical", "vanilla","continuous","incremental","incrementalContinuous"))

ggplot(merged02, aes(x=utterance,y=Mean, fill=model)) +
  geom_bar(position="dodge", stat = "identity") +
  facet_grid(language~condition)

ggsave("results/merged_modelComparison02.png")

ggplot(merged03, aes(x=utterance,y=Mean, fill=model)) +
  geom_bar(position="dodge", stat = "identity") +
  facet_grid(language~condition)

ggsave("results/merged_modelComparison03.png")
