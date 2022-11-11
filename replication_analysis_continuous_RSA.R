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


