setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tidyverse)
library(jsonlite)
library(rwebppl)
source("helpers/helpers.R")

source("helpers/BDA_dataprep.R")
source("helpers/inferenceHelpers.R")
source("helpers/BDA_vizhelpers.R")

# PUT IN AN "UNCOLLAPSED" DATAFILE WITH DEGEN ET AL.'S FORMAT

d_uncollapsed <- read_csv("data/english_perTrial.csv")

# COLLAPSE DATA (STILL NECESSARY FOR VISUALIZATIONS)

d_collapsed <- collapse_dataset(d_uncollapsed)

# MAKE A TIBBLE: COLUMNS CONDITION, REFERENTS IN THAT CONDITION (STATES), ALTERNATIVES IN THAT CONDITION (UTTERANCES)

statesUtterances <- makeStatesUtterances(d_uncollapsed, "english")

# MAKE INPUT DATA TO BDA: EACH DATUM INCLUDES RESPONSE, STATES, UTTERANCES

df <- d_uncollapsed %>%
  merge(statesUtterances) %>%
  mutate(response = case_when(response == "color" ~ "START color STOP",
                              response == "size" ~ "START size STOP",
                              response == "size_color" ~ "START size color STOP")) %>%
  select(response, states, utterances, condition)
# MAKE THE MODEL 

model <- makeModel("models/modelAndSemantics.txt")

# MODEL 1: VANILLA RSA

# POSTERIORS

vanillaInferenceScript_english <- wrapInference(model, "color_size", "vanilla", 5000, 10, 50000)
vanillaInferenceScript_english02 <- wrapInference(model, "color_size", "vanilla", 5000, 10, 40000)
vanillaInferenceScript_english03 <- wrapInference(model, "color_size", "vanilla", 5000, 10, 30000)

# "TRAIN" MODEL

vanillaPosteriors_english <- webppl(vanillaInferenceScript_english, data = df, data_var = "df", random_seed=3333)
vanillaPosteriors_english02 <- webppl(vanillaInferenceScript_english02, data = df, data_var = "df", random_seed=3333)
vanillaPosteriors_english03 <- webppl(vanillaInferenceScript_english03, data = df, data_var = "df", random_seed=3333)

graphPosteriors(vanillaPosteriors_english02) + ggtitle("Vanilla posteriors: burn-in 40000")
ggsave("results/vanilla/english/vanillaPosteriors_english02.png")

graphPosteriors(vanillaPosteriors_english03) + ggtitle("Vanilla posteriors: burn-in 30000")
ggsave("results/vanilla/english/vanillaPosteriors_english03.png")


# GET ESTIMATES

vanillaEstimates_english <- getEstimates(vanillaPosteriors) 
vanillaEstimates_english02 <- getEstimates(vanillaPosteriors_english02)
vanillaEstimates_english03 <- getEstimates(vanillaPosteriors_english03)

vanillaPredictionScript_english <- wrapPrediction(model, vanillaEstimates_english,
                                          "START size color STOPP", 
                                          "color_size",
                                          "vanilla")
vanillaPredictionScript_english02 <- wrapPrediction(model, vanillaEstimates_english02,
                                            "START size color STOP", 
                                            "color_size",
                                            "vanilla")
vanillaPredictionScript_english03 <- wrapPrediction(model, vanillaEstimates_english03,
                                            "START size color STOP", 
                                            "color_size",
                                            "vanilla")


vanillaPredictives_english <- webppl(vanillaPredictionScript, data = unique(df %>%  select(condition,states,utterances)), data_var = "df")

vanillaPredictives_english00 <- webppl(vanillaPredictionScript_english00, data = unique(df %>%  select(condition,states,utterances)), data_var = "df")
vanillaPredictives_english01 <- webppl(vanillaPredictionScript_english01, data = unique(df %>%  select(condition,states,utterances)), data_var = "df")
vanillaPredictives_english02 <- webppl(vanillaPredictionScript_english02, data = unique(df %>%  select(condition,states,utterances)), data_var = "df")
vanillaPredictives_english03 <- webppl(vanillaPredictionScript_english03, data = unique(df %>%  select(condition,states,utterances)), data_var = "df")

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

continuousInferenceScript_english <- wrapInference(model, "color_size", "continuous", 5000, 10, 50000)

continuousInferenceScript_english00 <- wrapInference(model, "color_size", "continuous", 5000, 10, 70000)
continuousInferenceScript_english01 <- wrapInference(model, "color_size", "continuous", 5000, 10, 60000)
continuousInferenceScript_english02 <- wrapInference(model, "color_size", "continuous", 5000, 10, 40000)
continuousInferenceScript_english03 <- wrapInference(model, "color_size", "continuous", 5000, 10, 30000)

continuousInferenceScript_english10 <- wrapInference(model, "color_size", "continuous", 7000, 10, 50000)
continuousInferenceScript_english11 <- wrapInference(model, "color_size", "continuous", 6000, 10, 50000)
continuousInferenceScript_english12 <- wrapInference(model, "color_size", "continuous", 4000, 10, 50000)
continuousInferenceScript_english13 <- wrapInference(model, "color_size", "continuous", 3000, 10, 50000)

continuousInferenceScript_english20 <- wrapInference(model, "color_size", "continuous", 5000, 20, 50000)
continuousInferenceScript_english21 <- wrapInference(model, "color_size", "continuous", 5000, 15, 50000)
continuousInferenceScript_english22 <- wrapInference(model, "color_size", "continuous", 5000, 5, 50000)
continuousInferenceScript_english23 <- wrapInference(model, "color_size", "continuous", 5000, 1, 50000)

# "TRAIN" MODEL

continuousPosteriors_english <- webppl(continuousInferenceScript_english, data = df, data_var = "df", random_seed=3333)
continuousPosteriors_english01 <- webppl(continuousInferenceScript_english01, data = df, data_var = "df", random_seed=3333)
continuousPosteriors_english02 <- webppl(continuousInferenceScript_english02, data = df, data_var = "df", random_seed=3333)
continuousPosteriors_english03 <- webppl(continuousInferenceScript_english03, data = df, data_var = "df", random_seed=3333)

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
ggsave("results/continuous/english_continuousPosteriors.png")

graphPosteriors(continuousPosteriors00) + ggtitle("continuous posteriors: burn-in 70000")
ggsave("results/continuous/english_continuousPosteriors00.png")

graphPosteriors(continuousPosteriors_english01) + ggtitle("continuous posteriors: burn-in 60000")
ggsave("results/continuous/english_continuousPosteriors_english01.png")

graphPosteriors(continuousPosteriors_english02) + ggtitle("continuous posteriors: burn-in 40000")
ggsave("results/continuous/english_continuousPosteriors_english02.png")

graphPosteriors(continuousPosteriors_english03) + ggtitle("continuous posteriors: burn-in 30000")
ggsave("results/continuous/english_continuousPosteriors_english03.png")

graphPosteriors(continuousPosteriors10) + ggtitle("continuous posteriors: samples 7000")
ggsave("results/continuous/english_continuousPosteriors10.png")

graphPosteriors(continuousPosteriors11) + ggtitle("continuous posteriors: samples 6000")
ggsave("results/continuous/english_continuousPosteriors11.png")

graphPosteriors(continuousPosteriors12) + ggtitle("continuous posteriors: samples 4000")
ggsave("results/continuous/english_continuousPosteriors12.png")

graphPosteriors(continuousPosteriors13) + ggtitle("continuous posteriors: samples 3000")
ggsave("results/continuous/english_continuousPosteriors13.png")

graphPosteriors(continuousPosteriors20) + ggtitle("continuous posteriors: lag 20")
ggsave("results/continuous/english_continuousPosteriors20.png")

graphPosteriors(continuousPosteriors21) + ggtitle("continuous posteriors: lag 15")
ggsave("results/continuous/english_continuousPosteriors21.png")

graphPosteriors(continuousPosteriors22) + ggtitle("continuous posteriors: lag 5")
ggsave("results/continuous/english_continuousPosteriors22.png")

graphPosteriors(continuousPosteriors23) + ggtitle("continuous posteriors: lag 1")
ggsave("results/continuous/english_continuousPosteriors23.png")

# ESTIMATES

continuousEstimates_english <- getEstimates(continuousPosteriors_english) 
continuousEstimates00 <- getEstimates(continuousPosteriors00)
continuousEstimates_english01 <- getEstimates(continuousPosteriors_english01)
continuousEstimates_english02 <- getEstimates(continuousPosteriors_english02)
continuousEstimates_english03 <- getEstimates(continuousPosteriors_english03)

continuousEstimates10 <- getEstimates(continuousPosteriors10)
continuousEstimates11 <- getEstimates(continuousPosteriors11)
continuousEstimates12 <- getEstimates(continuousPosteriors12)
continuousEstimates13 <- getEstimates(continuousPosteriors13)

continuousEstimates20 <- getEstimates(continuousPosteriors20)
continuousEstimates21 <- getEstimates(continuousPosteriors21)
continuousEstimates22 <- getEstimates(continuousPosteriors22)
continuousEstimates23 <- getEstimates(continuousPosteriors23)


continuousPredictionScript <- wrapPrediction(model, continuousEstimates_english,
                                             "START size color STOPP", 
                                             "color_size",
                                             "continuous")
continuousPredictionScript00 <- wrapPrediction(model, continuousEstimates00,
                                               "START size color STOPP", 
                                               "color_size",
                                               "continuous")
continuousPredictionScript_english01 <- wrapPrediction(model, continuousEstimates_english01,
                                               "START size color STOPP", 
                                               "color_size",
                                               "continuous")
continuousPredictionScript_english02 <- wrapPrediction(model, continuousEstimates_english02,
                                               "START size color STOP", 
                                               "color_size",
                                               "continuous")
continuousPredictionScript_english03 <- wrapPrediction(model, continuousEstimates_english03,
                                               "START size color STOP", 
                                               "color_size",
                                               "continuous")

continuousPredictionScript10 <- wrapPrediction(model, continuousEstimates10,
                                               "START size color STOPP", 
                                               "color_size",
                                               "continuous")
continuousPredictionScript11 <- wrapPrediction(model, continuousEstimates11,
                                               "START size color STOPP", 
                                               "color_size",
                                               "continuous")
continuousPredictionScript12 <- wrapPrediction(model, continuousEstimates12,
                                               "START size color STOPP", 
                                               "color_size",
                                               "continuous")
continuousPredictionScript13 <- wrapPrediction(model, continuousEstimates13,
                                               "START size color STOPP", 
                                               "color_size",
                                               "continuous")


continuousPredictionScript20 <- wrapPrediction(model, continuousEstimates20,
                                               "START size color STOPP", 
                                               "color_size",
                                               "continuous")

continuousPredictionScript21 <- wrapPrediction(model, continuousEstimates21,
                                               "START size color STOPP", 
                                               "color_size",
                                               "continuous")

continuousPredictionScript22 <- wrapPrediction(model, continuousEstimates22,
                                               "START size color STOPP", 
                                               "color_size",
                                               "continuous")
continuousPredictionScript23 <- wrapPrediction(model, continuousEstimates23,
                                               "START size color STOPP", 
                                               "color_size",
                                               "continuous")



continuousPredictives_english <- webppl(continuousPredictionScript_english, data = unique(df %>%  select(condition,states,utterances)), data_var = "df")

continuousPredictives00 <- webppl(continuousPredictionScript00, data = unique(df %>%  select(condition,states,utterances)), data_var = "df")
continuousPredictives_english01 <- webppl(continuousPredictionScript_english01, data = unique(df %>%  select(condition,states,utterances)), data_var = "df")
continuousPredictives_english02 <- webppl(continuousPredictionScript_english02, data = unique(df %>%  select(condition,states,utterances)), data_var = "df")
continuousPredictives_english03 <- webppl(continuousPredictionScript_english03, data = unique(df %>%  select(condition,states,utterances)), data_var = "df")

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

incrementalInferenceScript_english <- wrapInference(model, "color_size", "incremental", 5000, 10, 50000)

incrementalInferenceScript00 <- wrapInference(model, "color_size", "incremental", 5000, 10, 70000)
incrementalInferenceScript_english01 <- wrapInference(model, "color_size", "incremental", 5000, 10, 60000)
incrementalInferenceScript_english02 <- wrapInference(model, "color_size", "incremental", 5000, 10, 40000)
incrementalInferenceScript_english03 <- wrapInference(model, "color_size", "incremental", 5000, 10, 30000)

incrementalInferenceScript10 <- wrapInference(model, "color_size", "incremental", 7000, 10, 50000)
incrementalInferenceScript11 <- wrapInference(model, "color_size", "incremental", 6000, 10, 50000)
incrementalInferenceScript12 <- wrapInference(model, "color_size", "incremental", 4000, 10, 50000)
incrementalInferenceScript13 <- wrapInference(model, "color_size", "incremental", 3000, 10, 50000)

incrementalInferenceScript20 <- wrapInference(model, "color_size", "incremental", 5000, 20, 50000)
incrementalInferenceScript21 <- wrapInference(model, "color_size", "incremental", 5000, 15, 50000)
incrementalInferenceScript22 <- wrapInference(model, "color_size", "incremental", 5000, 5, 50000)
incrementalInferenceScript23 <- wrapInference(model, "color_size", "incremental", 5000, 1, 50000)

# "TRAIN" MODEL

incrementalPosteriors_english <- webppl(incrementalInferenceScript_english, data = df, data_var = "df", random_seed=3333)

incrementalPosteriors00 <- webppl(incrementalInferenceScript00, data = df, data_var = "df", random_seed=3333)
incrementalPosteriors_english01 <- webppl(incrementalInferenceScript_english01, data = df, data_var = "df", random_seed=3333)
incrementalPosteriors_english02 <- webppl(incrementalInferenceScript_english02, data = df, data_var = "df", random_seed=3333)
incrementalPosteriors_english03 <- webppl(incrementalInferenceScript_english03, data = df, data_var = "df", random_seed=3333)

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
ggsave("results/incremental/english_incrementalPosteriors.png")

graphPosteriors(incrementalPosteriors00) + ggtitle("incremental posteriors: burn-in 70000")
ggsave("results/incremental/english_incrementalPosteriors00.png")

graphPosteriors(incrementalPosteriors_english01) + ggtitle("incremental posteriors: burn-in 60000")
ggsave("results/incremental/english_incrementalPosteriors_english01.png")

graphPosteriors(incrementalPosteriors_english02) + ggtitle("incremental posteriors: burn-in 40000")
ggsave("results/incremental/english_incrementalPosteriors_english02.png")

graphPosteriors(incrementalPosteriors_english03) + ggtitle("incremental posteriors: burn-in 30000")
ggsave("results/incremental/english_incrementalPosteriors_english03.png")

graphPosteriors(incrementalPosteriors10) + ggtitle("incremental posteriors: samples 7000")
ggsave("results/incremental/english_incrementalPosteriors10.png")

graphPosteriors(incrementalPosteriors11) + ggtitle("incremental posteriors: samples 6000")
ggsave("results/incremental/english_incrementalPosteriors11.png")

graphPosteriors(incrementalPosteriors12) + ggtitle("incremental posteriors: samples 4000")
ggsave("results/incremental/english_incrementalPosteriors12.png")

graphPosteriors(incrementalPosteriors13) + ggtitle("incremental posteriors: samples 3000")
ggsave("results/incremental/english_incrementalPosteriors13.png")

graphPosteriors(incrementalPosteriors20) + ggtitle("incremental posteriors: lag 20")
ggsave("results/incremental/english_incrementalPosteriors20.png")

graphPosteriors(incrementalPosteriors21) + ggtitle("incremental posteriors: lag 15")
ggsave("results/incremental/english_incrementalPosteriors21.png")

graphPosteriors(incrementalPosteriors22) + ggtitle("incremental posteriors: lag 5")
ggsave("results/incremental/english_incrementalPosteriors22.png")

graphPosteriors(incrementalPosteriors23) + ggtitle("incremental posteriors: lag 1")
ggsave("results/incremental/english_incrementalPosteriors23.png")

# ESTIMATES

incrementalEstimates_english <- getEstimates(incrementalPosteriors_english) 
incrementalEstimates00 <- getEstimates(incrementalPosteriors00)
incrementalEstimates_english01 <- getEstimates(incrementalPosteriors_english01)
incrementalEstimates_english02 <- getEstimates(incrementalPosteriors_english02)
incrementalEstimates_english03 <- getEstimates(incrementalPosteriors_english03)

incrementalEstimates10 <- getEstimates(incrementalPosteriors10)
incrementalEstimates11 <- getEstimates(incrementalPosteriors11)
incrementalEstimates12 <- getEstimates(incrementalPosteriors12)
incrementalEstimates13 <- getEstimates(incrementalPosteriors13)

incrementalEstimates20 <- getEstimates(incrementalPosteriors20)
incrementalEstimates21 <- getEstimates(incrementalPosteriors21)
incrementalEstimates22 <- getEstimates(incrementalPosteriors22)
incrementalEstimates23 <- getEstimates(incrementalPosteriors23)


incrementalPredictionScript_english <- wrapPrediction(model, incrementalEstimates_english,
                                              "START size color STOPP", 
                                              "color_size",
                                              "incremental")
incrementalPredictionScript00 <- wrapPrediction(model, incrementalEstimates00,
                                                "START size color STOPP", 
                                                "color_size",
                                                "incremental")
incrementalPredictionScript_english01 <- wrapPrediction(model, incrementalEstimates_english01,
                                                "START size color STOPP", 
                                                "color_size",
                                                "incremental")
incrementalPredictionScript_english02 <- wrapPrediction(model, incrementalEstimates_english02,
                                                "START size color STOP", 
                                                "color_size",
                                                "incremental")
incrementalPredictionScript_english03 <- wrapPrediction(model, incrementalEstimates_english03,
                                                "START size color STOP", 
                                                "color_size",
                                                "incremental")

incrementalPredictionScript10 <- wrapPrediction(model, incrementalEstimates10,
                                                "START size color STOPP", 
                                                "color_size",
                                                "incremental")
incrementalPredictionScript11 <- wrapPrediction(model, incrementalEstimates11,
                                                "START size color STOPP", 
                                                "color_size",
                                                "incremental")
incrementalPredictionScript12 <- wrapPrediction(model, incrementalEstimates12,
                                                "START size color STOPP", 
                                                "color_size",
                                                "incremental")
incrementalPredictionScript13 <- wrapPrediction(model, incrementalEstimates13,
                                                "START size color STOPP", 
                                                "color_size",
                                                "incremental")


incrementalPredictionScript20 <- wrapPrediction(model, incrementalEstimates20,
                                                "START size color STOPP", 
                                                "color_size",
                                                "incremental")

incrementalPredictionScript21 <- wrapPrediction(model, incrementalEstimates21,
                                                "START size color STOPP", 
                                                "color_size",
                                                "incremental")

incrementalPredictionScript22 <- wrapPrediction(model, incrementalEstimates22,
                                                "START size color STOPP", 
                                                "color_size",
                                                "incremental")
incrementalPredictionScript23 <- wrapPrediction(model, incrementalEstimates23,
                                                "START size color STOPP", 
                                                "color_size",
                                                "incremental")



incrementalPredictives_english <- webppl(incrementalPredictionScript_english, data = unique(df %>%  select(condition,states,utterances)), data_var = "df")

incrementalPredictives00 <- webppl(incrementalPredictionScript00, data = unique(df %>%  select(condition,states,utterances)), data_var = "df")
incrementalPredictives_english01 <- webppl(incrementalPredictionScript_english01, data = unique(df %>%  select(condition,states,utterances)), data_var = "df")
incrementalPredictives_english02 <- webppl(incrementalPredictionScript_english02, data = unique(df %>%  select(condition,states,utterances)), data_var = "df")
incrementalPredictives_english03 <- webppl(incrementalPredictionScript_english03, data = unique(df %>%  select(condition,states,utterances)), data_var = "df")

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

incrementalContinuousInferenceScript_english <- wrapInference(model, "color_size", "incrementalContinuous", 5000, 10, 50000)

incrementalContinuousInferenceScript00 <- wrapInference(model, "color_size", "incrementalContinuous", 5000, 10, 70000)
incrementalContinuousInferenceScript_english01 <- wrapInference(model, "color_size", "incrementalContinuous", 5000, 10, 60000)
incrementalContinuousInferenceScript_english02 <- wrapInference(model, "color_size", "incrementalContinuous", 5000, 10, 40000)
incrementalContinuousInferenceScript_english03 <- wrapInference(model, "color_size", "incrementalContinuous", 5000, 10, 30000)

incrementalContinuousInferenceScript10 <- wrapInference(model, "color_size", "incrementalContinuous", 7000, 10, 50000)
incrementalContinuousInferenceScript11 <- wrapInference(model, "color_size", "incrementalContinuous", 6000, 10, 50000)
incrementalContinuousInferenceScript12 <- wrapInference(model, "color_size", "incrementalContinuous", 4000, 10, 50000)
incrementalContinuousInferenceScript13 <- wrapInference(model, "color_size", "incrementalContinuous", 3000, 10, 50000)

incrementalContinuousInferenceScript20 <- wrapInference(model, "color_size", "incrementalContinuous", 5000, 20, 50000)
incrementalContinuousInferenceScript21 <- wrapInference(model, "color_size", "incrementalContinuous", 5000, 15, 50000)
incrementalContinuousInferenceScript22 <- wrapInference(model, "color_size", "incrementalContinuous", 5000, 5, 50000)
incrementalContinuousInferenceScript23 <- wrapInference(model, "color_size", "incrementalContinuous", 5000, 1, 50000)

# "TRAIN" MODEL

incrementalContinuousPosteriors_english <- webppl(incrementalContinuousInferenceScript_english, data = df, data_var = "df", random_seed=3333)

incrementalContinuousPosteriors00 <- webppl(incrementalContinuousInferenceScript00, data = df, data_var = "df", random_seed=3333)
incrementalContinuousPosteriors_english01 <- webppl(incrementalContinuousInferenceScript_english01, data = df, data_var = "df", random_seed=3333)
incrementalContinuousPosteriors_english02 <- webppl(incrementalContinuousInferenceScript_english02, data = df, data_var = "df", random_seed=3333)
incrementalContinuousPosteriors_english03 <- webppl(incrementalContinuousInferenceScript_english03, data = df, data_var = "df", random_seed=3333)

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
ggsave("results/incrementalContinuous/english_incrementalContinuousPosteriors.png")

graphPosteriors(incrementalContinuousPosteriors00) + ggtitle("incrementalContinuous posteriors: burn-in 70000")
ggsave("results/incrementalContinuous/english_incrementalContinuousPosteriors00.png")

graphPosteriors(incrementalContinuousPosteriors_english01) + ggtitle("incrementalContinuous posteriors: burn-in 60000")
ggsave("results/incrementalContinuous/english_incrementalContinuousPosteriors_english01.png")

graphPosteriors(incrementalContinuousPosteriors_english02) + ggtitle("incrementalContinuous posteriors: burn-in 40000")
ggsave("results/incrementalContinuous/english_incrementalContinuousPosteriors_english02.png")

graphPosteriors(incrementalContinuousPosteriors_english03) + ggtitle("incrementalContinuous posteriors: burn-in 30000")
ggsave("results/incrementalContinuous/english_incrementalContinuousPosteriors_english03.png")

graphPosteriors(incrementalContinuousPosteriors10) + ggtitle("incrementalContinuous posteriors: samples 7000")
ggsave("results/incrementalContinuous/english_incrementalContinuousPosteriors10.png")

graphPosteriors(incrementalContinuousPosteriors11) + ggtitle("incrementalContinuous posteriors: samples 6000")
ggsave("results/incrementalContinuous/english_incrementalContinuousPosteriors11.png")

graphPosteriors(incrementalContinuousPosteriors12) + ggtitle("incrementalContinuous posteriors: samples 4000")
ggsave("results/incrementalContinuous/english_incrementalContinuousPosteriors12.png")

graphPosteriors(incrementalContinuousPosteriors13) + ggtitle("incrementalContinuous posteriors: samples 3000")
ggsave("results/incrementalContinuous/english_incrementalContinuousPosteriors13.png")

graphPosteriors(incrementalContinuousPosteriors20) + ggtitle("incrementalContinuous posteriors: lag 20")
ggsave("results/incrementalContinuous/english_incrementalContinuousPosteriors20.png")

graphPosteriors(incrementalContinuousPosteriors21) + ggtitle("incrementalContinuous posteriors: lag 15")
ggsave("results/incrementalContinuous/english_incrementalContinuousPosteriors21.png")

graphPosteriors(incrementalContinuousPosteriors22) + ggtitle("incrementalContinuous posteriors: lag 5")
ggsave("results/incrementalContinuous/english_incrementalContinuousPosteriors22.png")

graphPosteriors(incrementalContinuousPosteriors23) + ggtitle("incrementalContinuous posteriors: lag 1")
ggsave("results/incrementalContinuous/english_incrementalContinuousPosteriors23.png")

# ESTIMATES

incrementalContinuousEstimates_english <- getEstimates(incrementalContinuousPosteriors_english) 
incrementalContinuousEstimates00 <- getEstimates(incrementalContinuousPosteriors00)
incrementalContinuousEstimates_english01 <- getEstimates(incrementalContinuousPosteriors_english01)
incrementalContinuousEstimates_english02 <- getEstimates(incrementalContinuousPosteriors_english02)
incrementalContinuousEstimates_english03 <- getEstimates(incrementalContinuousPosteriors_english03)

incrementalContinuousEstimates10 <- getEstimates(incrementalContinuousPosteriors10)
incrementalContinuousEstimates11 <- getEstimates(incrementalContinuousPosteriors11)
incrementalContinuousEstimates12 <- getEstimates(incrementalContinuousPosteriors12)
incrementalContinuousEstimates13 <- getEstimates(incrementalContinuousPosteriors13)

incrementalContinuousEstimates20 <- getEstimates(incrementalContinuousPosteriors20)
incrementalContinuousEstimates21 <- getEstimates(incrementalContinuousPosteriors21)
incrementalContinuousEstimates22 <- getEstimates(incrementalContinuousPosteriors22)
incrementalContinuousEstimates23 <- getEstimates(incrementalContinuousPosteriors23)



incrementalContinuousPredictionScript_english <- wrapPrediction(model, incrementalContinuousEstimates_english,
                                                        "START size color STOPP", 
                                                        "color_size",
                                                        "incrementalContinuous")
incrementalContinuousPredictionScript00 <- wrapPrediction(model, incrementalContinuousEstimates00,
                                                          "START size color STOPP", 
                                                          "color_size",
                                                          "incrementalContinuous")
incrementalContinuousPredictionScript_english01 <- wrapPrediction(model, incrementalContinuousEstimates_english01,
                                                          "START size color STOPP", 
                                                          "color_size",
                                                          "incrementalContinuous")
incrementalContinuousPredictionScript_english02 <- wrapPrediction(model, incrementalContinuousEstimates_english02,
                                                          "START size color STOP", 
                                                          "color_size",
                                                          "incrementalContinuous")
incrementalContinuousPredictionScript_english03 <- wrapPrediction(model, incrementalContinuousEstimates_english03,
                                                          "START size color STOP", 
                                                          "color_size",
                                                          "incrementalContinuous")

incrementalContinuousPredictionScript10 <- wrapPrediction(model, incrementalContinuousEstimates10,
                                                          "START size color STOPP", 
                                                          "color_size",
                                                          "incrementalContinuous")
incrementalContinuousPredictionScript11 <- wrapPrediction(model, incrementalContinuousEstimates11,
                                                          "START size color STOPP", 
                                                          "color_size",
                                                          "incrementalContinuous")
incrementalContinuousPredictionScript12 <- wrapPrediction(model, incrementalContinuousEstimates12,
                                                          "START size color STOPP", 
                                                          "color_size",
                                                          "incrementalContinuous")
incrementalContinuousPredictionScript13 <- wrapPrediction(model, incrementalContinuousEstimates13,
                                                          "START size color STOPP", 
                                                          "color_size",
                                                          "incrementalContinuous")


incrementalContinuousPredictionScript20 <- wrapPrediction(model, incrementalContinuousEstimates20,
                                                          "START size color STOPP", 
                                                          "color_size",
                                                          "incrementalContinuous")

incrementalContinuousPredictionScript21 <- wrapPrediction(model, incrementalContinuousEstimates21,
                                                          "START size color STOPP", 
                                                          "color_size",
                                                          "incrementalContinuous")

incrementalContinuousPredictionScript22 <- wrapPrediction(model, incrementalContinuousEstimates22,
                                                          "START size color STOPP", 
                                                          "color_size",
                                                          "incrementalContinuous")
incrementalContinuousPredictionScript23 <- wrapPrediction(model, incrementalContinuousEstimates23,
                                                          "START size color STOPP", 
                                                          "color_size",
                                                          "incrementalContinuous")



incrementalContinuousPredictives_english <- webppl(incrementalContinuousPredictionScript_english, data = unique(df %>%  select(condition,states,utterances)), data_var = "df")

incrementalContinuousPredictives00 <- webppl(incrementalContinuousPredictionScript00, data = unique(df %>%  select(condition,states,utterances)), data_var = "df")
incrementalContinuousPredictives_english01 <- webppl(incrementalContinuousPredictionScript_english01, data = unique(df %>%  select(condition,states,utterances)), data_var = "df")
incrementalContinuousPredictives_english02 <- webppl(incrementalContinuousPredictionScript_english02, data = unique(df %>%  select(condition,states,utterances)), data_var = "df")
incrementalContinuousPredictives_english03 <- webppl(incrementalContinuousPredictionScript_english03, data = unique(df %>%  select(condition,states,utterances)), data_var = "df")

incrementalContinuousPredictives20 <- webppl(incrementalContinuousPredictionScript10, data = unique(df %>%  select(condition,states,utterances)), data_var = "df")
incrementalContinuousPredictives21 <- webppl(incrementalContinuousPredictionScript11, data = unique(df %>%  select(condition,states,utterances)), data_var = "df")
incrementalContinuousPredictives22 <- webppl(incrementalContinuousPredictionScript12, data = unique(df %>%  select(condition,states,utterances)), data_var = "df")
incrementalContinuousPredictives23 <- webppl(incrementalContinuousPredictionScript13, data = unique(df %>%  select(condition,states,utterances)), data_var = "df")

incrementalContinuousPredictives10 <- webppl(incrementalContinuousPredictionScript20, data = unique(df %>%  select(condition,states,utterances)), data_var = "df")
incrementalContinuousPredictives11 <- webppl(incrementalContinuousPredictionScript21, data = unique(df %>%  select(condition,states,utterances)), data_var = "df")
incrementalContinuousPredictives12 <- webppl(incrementalContinuousPredictionScript22, data = unique(df %>%  select(condition,states,utterances)), data_var = "df")
incrementalContinuousPredictives13 <- webppl(incrementalContinuousPredictionScript23, data = unique(df %>%  select(condition,states,utterances)), data_var = "df")

graphPredictives(incrementalPredictives, d_collapsed) + ggtitle("Incremental predictives")

ggsave("results/english_incrementalPredictives.png", width = 4, height = 3, units = "in")


######################################################
# MERGED PLOTS
######################################################
# modification type
empirical_toplot_english = d_uncollapsed %>%
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

# CONDITION _english01

vanilla_toplot_english01 = vanillaPredictives_english01 %>%
  gather(utterance,Mean,size_color:size) %>%
  mutate(model="vanilla")

continuous_toplot_english01 = continuousPredictives_english01 %>%
  gather(utterance,Mean,size_color:size) %>%
  mutate(model="continuous")

incremental_toplot_english01 = incrementalPredictives_english01 %>%
  gather(utterance,Mean,size_color:size) %>%
  mutate(model="incremental")

incrementalContinuous_toplot_english01 = incrementalContinuousPredictives_english01 %>%
  gather(utterance,Mean,size_color:size) %>%
  mutate(model="incrementalContinuous")

# CONDITION English 02

vanilla_toplot_english02 = vanillaPredictives_english02 %>%
  gather(utterance,Mean,size_color:size) %>%
  mutate(model="vanilla")

continuous_toplot_english02 = continuousPredictives_english02 %>%
  gather(utterance,Mean,size_color:size) %>%
  mutate(model="continuous")

incremental_toplot_english02 = incrementalPredictives_english02 %>%
  gather(utterance,Mean,size_color:size) %>%
  mutate(model="incremental")

incrementalContinuous_toplot_english02 = incrementalContinuousPredictives_english02 %>%
  gather(utterance,Mean,size_color:size) %>%
  mutate(model="incrementalContinuous")

# CONDITION English 03

vanilla_toplot_english03 = vanillaPredictives_english03 %>%
  gather(utterance,Mean,size_color:size) %>%
  mutate(model="vanilla")

continuous_toplot_english03 = continuousPredictives_english03 %>%
  gather(utterance,Mean,size_color:size) %>%
  mutate(model="continuous")

incremental_toplot_english03 = incrementalPredictives_english03 %>%
  gather(utterance,Mean,size_color:size) %>%
  mutate(model="incremental")

incrementalContinuous_toplot_english03 = incrementalContinuousPredictives_english03 %>%
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


#merge english datasets
english_merged = rbind(empirical_toplot,vanilla_toplot,continuous_toplot,incremental_toplot,incrementalContinuous_toplot) %>%
  mutate(condition=ifelse(condition=="color31","color sufficient",ifelse(condition=="size31","size sufficient",NA))) %>%
  mutate(language="english")

english_merged00 = rbind(empirical_toplot,vanilla_toplot00,continuous_toplot00,incremental_toplot00,incrementalContinuous_toplot00) %>%
  mutate(condition=ifelse(condition=="color31","color sufficient",ifelse(condition=="size31","size sufficient",NA))) %>%
  mutate(language="english")

english_merged_english01 = rbind(empirical_toplot_english,vanilla_toplot_english01,continuous_toplot_english01,incremental_toplot_english01,incrementalContinuous_toplot_english01) %>%
  mutate(condition=ifelse(condition=="color31","color sufficient",ifelse(condition=="size31","size sufficient",NA))) %>%
  mutate(language="english")

english_merged_english02 = rbind(empirical_toplot_english,vanilla_toplot_english02,continuous_toplot_english02,incremental_toplot_english02,incrementalContinuous_toplot_english02) %>%
  mutate(condition=ifelse(condition=="color31","color sufficient",ifelse(condition=="size31","size sufficient",NA))) %>%
  mutate(language="english")

english_merged_english03 = rbind(empirical_toplot_english,vanilla_toplot_english03,continuous_toplot_english03,incremental_toplot_english03,incrementalContinuous_toplot_english03) %>%
  mutate(condition=ifelse(condition=="color31","color sufficient",ifelse(condition=="size31","size sufficient",NA))) %>%
  mutate(language="english")

english_merged10 = rbind(empirical_toplot,vanilla_toplot10,continuous_toplot10,incremental_toplot10,incrementalContinuous_toplot10) %>%
  mutate(condition=ifelse(condition=="color31","color sufficient",ifelse(condition=="size31","size sufficient",NA))) %>%
  mutate(language="english")

english_merged11 = rbind(empirical_toplot,vanilla_toplot11,continuous_toplot11,incremental_toplot11,incrementalContinuous_toplot11) %>%
  mutate(condition=ifelse(condition=="color31","color sufficient",ifelse(condition=="size31","size sufficient",NA))) %>%
  mutate(language="english")

english_merged12 = rbind(empirical_toplot,vanilla_toplot12,continuous_toplot12,incremental_toplot12,incrementalContinuous_toplot12) %>%
  mutate(condition=ifelse(condition=="color31","color sufficient",ifelse(condition=="size31","size sufficient",NA))) %>%
  mutate(language="english")

english_merged13 = rbind(empirical_toplot,vanilla_toplot13,continuous_toplot13,incremental_toplot13,incrementalContinuous_toplot13) %>%
  mutate(condition=ifelse(condition=="color31","color sufficient",ifelse(condition=="size31","size sufficient",NA))) %>%
  mutate(language="english")

english_merged20 = rbind(empirical_toplot,vanilla_toplot20,continuous_toplot20,incremental_toplot20,incrementalContinuous_toplot20) %>%
  mutate(condition=ifelse(condition=="color31","color sufficient",ifelse(condition=="size31","size sufficient",NA))) %>%
  mutate(language="english")

english_merged21 = rbind(empirical_toplot,vanilla_toplot21,continuous_toplot21,incremental_toplot21,incrementalContinuous_toplot21) %>%
  mutate(condition=ifelse(condition=="color31","color sufficient",ifelse(condition=="size31","size sufficient",NA))) %>%
  mutate(language="english")

english_merged22 = rbind(empirical_toplot,vanilla_toplot22,continuous_toplot22,incremental_toplot22,incrementalContinuous_toplot22) %>%
  mutate(condition=ifelse(condition=="color31","color sufficient",ifelse(condition=="size31","size sufficient",NA))) %>%
  mutate(language="english")

english_merged23 = rbind(empirical_toplot,vanilla_toplot23,continuous_toplot23,incremental_toplot23,incrementalContinuous_toplot23) %>%
  mutate(condition=ifelse(condition=="color31","color sufficient",ifelse(condition=="size31","size sufficient",NA))) %>%
  mutate(language="english")

write.csv(english_merged_english02, "models/english_modelComp02.csv", row.names=TRUE)
write.csv(english_merged_english03, "models/english_modelComp03.csv", row.names=TRUE)

#merge ctsl and english datasets 
english_merged = read_csv("english_modelComp.csv")

merged = rbind(ctsl_merged,english_merged)

merged$model = factor(merged$model, levels = c("empirical", "vanilla","continuous","incremental","incrementalContinuous"))

ggplot(merged, aes(x=utterance,y=Mean, fill=model)) +
  geom_bar(position="dodge", stat = "identity") +
  facet_grid(language~condition)

ggsave("results/merged_modelComparison.png")
