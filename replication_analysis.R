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

model <- makeModel("modelAndSemantics.txt")

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
vanillaPosteriors03 <- webppl(vanillaInferenceScript03, data = df, data_var = "df", random_seed=3333)

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


# Save results


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
continuousPosteriors03 <- webppl(continuousInferenceScript03, data = df, data_var = "df", random_seed=3333)

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

incrementalPosteriors00 <- webppl(incrementalInferenceScript00, data = df, data_var = "df", random_seed=3333)
incrementalPosteriors01 <- webppl(incrementalInferenceScript01, data = df, data_var = "df", random_seed=3333)
incrementalPosteriors02 <- webppl(incrementalInferenceScript02, data = df, data_var = "df", random_seed=3333)
incrementalPosteriors03 <- webppl(incrementalInferenceScript03, data = df, data_var = "df", random_seed=3333)

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





# # STEP 1: WRAP INFERENCE COMMAND AROUND CORE MODEL

incrementalVGlobalInferenceCommand <- read_file("incrementalVGlobalComparison/inferenceCommand.txt")

# # # (TODO [LEYLA]: UP THE SAMPLE/LAG/BURN/RATE)

incrementalVGlobalInferenceCommand <- gsub("TARGET_REFERENT", "color_size", incrementalVGlobalInferenceCommand, fixed = TRUE)
incrementalVGlobalInferenceCommand <- gsub("NUM_SAMPLES", 5000, incrementalVGlobalInferenceCommand, fixed = TRUE)
incrementalVGlobalInferenceCommand <- gsub("LAG", 10, incrementalVGlobalInferenceCommand, fixed = TRUE)
incrementalVGlobalInferenceCommand <- gsub("BURN_IN", 50000, incrementalVGlobalInferenceCommand, fixed = TRUE)

incrementalVGlobalInferenceScript <- paste(read_file(model), incrementalVGlobalInferenceCommand, sep = "\n")

# # STEP 2: RUN SCRIPT AND GRAPH POSTERIORS 

incrementalVGlobalPosteriors <- webppl(incrementalVGlobalInferenceScript, data = df, data_var = "df", random_seed = 3333)

graphPosteriors(incrementalVGlobalPosteriors %>% filter(!(Parameter == "incrementalOrGlobal")) %>% mutate(value = as.numeric(value))) + ggtitle("Model parameter posteriors")

ggsave("incrementalVGlobalComparison/modelPosteriors.png")

# # STEP 3: CALCULATE POSTERIOR PROBABILITY OF INCREMENTAL VS. GLOBAL

modelPosterior <- incrementalVGlobalPosteriors %>% filter(Parameter == "incrementalOrGlobal") %>%
  count(value) %>%
  group_by(value) %>%
  summarize(posteriorProb = n / sum(n))

View(modelPosterior)

save.image("results/ctsl_comparison.RData")
