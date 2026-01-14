################################################################################
#                      Simulation for PNC interpretation                       #
################################################################################

# Last edited date: 10-Jun-2025
# This script is to conduct simulation analyses for interpreting paternal negative control results.

################################################################################

rm(list = ls())
sessionInfo()
setwd("Z:/working/results/")

################################################################################

# Scenario 1: U → A, U → B; no other direct effects

## Set seed for reproducibility
set.seed(19705)

## Sample size
n <- 1000

## Number of repetitions
n_sim <- 100

## Create empty list to store simulation results
sim_res <- vector("list", n_sim)

## Begin simulation loop
for (i in 1:n_sim) {
  ## Generate U: unmeasured confounder, normally distributed
  U <- rnorm(n, 0, 1)
  
  ### Step 1: Generate raw A and B influenced by U
  A_raw <- 0.4 * U + rnorm(n, 0, 1)
  B_raw <- 0.4 * U + rnorm(n, 0, 1)
  
  ### Step 2: Residualize B to orthogonalise it from A, then rescale to r = 0.4
  resid_B <- lm(B_raw ~ A_raw)$residuals
  B <- 0.4 * scale(A_raw) + sqrt(1 - 0.4^2) * scale(resid_B)
  A <- scale(A_raw)
  
  ### Confirm correlation
  stopifnot(abs(cor(A, B) - 0.4) < 0.001)
  
  ## Generate outcome Y: purely random
  Y <- rnorm(n)
  
  ## Fit models
  M <- summary(lm(Y ~ A + U))
  m1 <- summary(lm(Y ~ A))
  m2 <- summary(lm(Y ~ A + B))
  p1 <- summary(lm(Y ~ B))
  p2 <- summary(lm(Y ~ B + A))
  
  ## Extract estimates
  res <- data.frame(
    Model = c(
      "True maternal effect",
      "Confounding effect",
      "Maternal Model 1",
      "Maternal Model 2",
      "Paternal Model 1",
      "Paternal Model 2"
    ),
    b = c(
      0,
      0,
      m1$coefficients["A", "Estimate"],
      m2$coefficients["A", "Estimate"],
      p1$coefficients["B", "Estimate"],
      p2$coefficients["B", "Estimate"]
    ),
    se = c(
      0,
      0,
      m1$coefficients["A", "Std. Error"],
      m2$coefficients["A", "Std. Error"],
      p1$coefficients["B", "Std. Error"],
      p2$coefficients["B", "Std. Error"]
    ),
    pval = c(
      0,
      0,
      m1$coefficients["A", "Pr(>|t|)"],
      m2$coefficients["A", "Pr(>|t|)"],
      p1$coefficients["B", "Pr(>|t|)"],
      p2$coefficients["B", "Pr(>|t|)"]
    )
  )
  
  sim_res[[i]] <- res
}

## Combine all results and compute mean values
res_1 <- do.call(rbind, sim_res) |>
  dplyr::group_by(Model) |>
  dplyr::summarise(beta = mean(b),
                   SE = mean(se),
                   pvalue = 2 * (1 - pnorm(abs(beta / SE)))) |>
  dplyr::mutate(
    Scenario = "Scenario 1",
    Group = "Scenario: maternal effect: NULL; shared confounding: NULL",
    Model = factor(
      Model,
      levels = c(
        "Paternal Model 2",
        "Paternal Model 1",
        "Maternal Model 2",
        "Maternal Model 1",
        "Confounding effect",
        "True maternal effect"
      )
    )
  ) |>
  dplyr::mutate(pvalue = ifelse(beta != 0 &
                                  SE == 0, 0.0001, pvalue)) |>
  dplyr::arrange(factor(Model, levels = rev(levels(Model))))

## Forest plot
forest_1 <- ggforestplot::forestplot(
  df = res_1,
  name = Model,
  estimate = beta,
  se = SE,
  pvalue = pvalue,
  psignif = 0.05,
  colour = Model,
  shape = Model,
  xlab = "Beta or (or logOR) and 95% CI",
  title = "",
  logodds = F
) +
  ggplot2::scale_colour_manual(values = c("darkblue", "blue", "darkred", "red", "darkorange", "darkgreen")) +
  ggplot2::scale_shape_manual(values = c(21, 21, 21, 21, 21, 21)) +
  ggplot2::theme(legend.position = "none") +
  ggforce::facet_col(facets = ~ Group,
                     scales = "free_y",
                     space = "free") +
  ggplot2::coord_cartesian(xlim = c(-0.4, 0.4))

forest_1

ggplot2::ggsave(forest_1,
                file = "simulation_PNC(1).png",
                height = 2.7,
                width = 7)

################################################################################
################################################################################

# Scenario 2: U → A, U → B, U → Y (negative); no other direct effects

## Set seed for reproducibility
set.seed(19705)

## Sample size per simulation
n <- 1000

## Number of repetitions
n_sim <- 100

## Create empty list to store simulation results
sim_res <- vector("list", n_sim)

## Begin simulation loop
for (i in 1:n_sim) {
  ## Generate U: unmeasured confounder, normally distributed
  U <- rnorm(n, 0, 1)
  
  ### Step 1: Generate raw A and B influenced by U
  A_raw <- 0.8 * U + rnorm(n, 0, 1)
  B_raw <- 0.8 * U + rnorm(n, 0, 1)
  
  ### Step 2: Residualize B to orthogonalise it from A, then rescale to r = 0.4
  resid_B <- lm(B_raw ~ A_raw)$residuals
  B <- 0.4 * scale(A_raw) + sqrt(1 - 0.4^2) * scale(resid_B)
  A <- scale(A_raw)
  
  ### Confirm correlation
  stopifnot(abs(cor(A, B) - 0.4) < 0.001)
  
  ## Generate outcome Y: negatively influenced by U, not directly by A or B
  Y <- -0.2 * U + rnorm(n, 0, 1)
  
  ## Fit models
  M <- summary(lm(Y ~ A + U))
  m1 <- summary(lm(Y ~ A))
  m2 <- summary(lm(Y ~ A + B))
  p1 <- summary(lm(Y ~ B))
  p2 <- summary(lm(Y ~ B + A))
  
  ## Extract estimates
  res <- data.frame(
    Model = c(
      "True maternal effect",
      "Confounding effect",
      "Maternal Model 1",
      "Maternal Model 2",
      "Paternal Model 1",
      "Paternal Model 2"
    ),
    b = c(
      0,
      -0.2 * 0.8,
      m1$coefficients["A", "Estimate"],
      m2$coefficients["A", "Estimate"],
      p1$coefficients["B", "Estimate"],
      p2$coefficients["B", "Estimate"]
    ),
    se = c(
      0,
      0,
      m1$coefficients["A", "Std. Error"],
      m2$coefficients["A", "Std. Error"],
      p1$coefficients["B", "Std. Error"],
      p2$coefficients["B", "Std. Error"]
    ),
    pval = c(
      0,
      0,
      m1$coefficients["A", "Pr(>|t|)"],
      m2$coefficients["A", "Pr(>|t|)"],
      p1$coefficients["B", "Pr(>|t|)"],
      p2$coefficients["B", "Pr(>|t|)"]
    )
  )
  
  sim_res[[i]] <- res
}

## Combine all results and compute mean values
res_2 <- do.call(rbind, sim_res) |>
  dplyr::group_by(Model) |>
  dplyr::summarise(beta = mean(b),
                   SE = mean(se),
                   pvalue = 2 * (1 - pnorm(abs(beta / SE)))) |>
  dplyr::mutate(
    Scenario = "Scenario 2",
    Group = "Scenario: maternal effect: NULL; shared confounding: NEGATIVE",
    Model = factor(
      Model,
      levels = c(
        "Paternal Model 2",
        "Paternal Model 1",
        "Maternal Model 2",
        "Maternal Model 1",
        "Confounding effect",
        "True maternal effect"
      )
    )
  ) |>
  dplyr::mutate(pvalue = ifelse(beta != 0 &
                                  SE == 0, 0.0001, pvalue)) |>
  dplyr::arrange(factor(Model, levels = rev(levels(Model))))

################################################################################
################################################################################

# Scenario 3: U → A, U → B, U → Y (positive); no other direct effects

## Set seed for reproducibility
set.seed(19705)

## Sample size per simulation
n <- 1000

## Number of repetitions
n_sim <- 100

## Create empty list to store simulation results
sim_res <- vector("list", n_sim)

## Begin simulation loop
for (i in 1:n_sim) {
  ## Generate U: unmeasured confounder, normally distributed
  U <- rnorm(n, 0, 1)
  
  ### Step 1: Generate raw A and B influenced by U
  A_raw <- 0.8 * U + rnorm(n, 0, 1)
  B_raw <- 0.8 * U + rnorm(n, 0, 1)
  
  ### Step 2: Residualize B to orthogonalise it from A, then rescale to r = 0.4
  resid_B <- lm(B_raw ~ A_raw)$residuals
  B <- 0.4 * scale(A_raw) + sqrt(1 - 0.4^2) * scale(resid_B)
  A <- scale(A_raw)
  
  ### Confirm correlation
  stopifnot(abs(cor(A, B) - 0.4) < 0.001)
  
  ## Generate outcome Y: positively influenced by U, not directly by A or B
  Y <- 0.2 * U + rnorm(n, 0, 1)
  
  ## Fit models
  M <- summary(lm(Y ~ A + U))
  m1 <- summary(lm(Y ~ A))
  m2 <- summary(lm(Y ~ A + B))
  p1 <- summary(lm(Y ~ B))
  p2 <- summary(lm(Y ~ B + A))
  
  ## Extract estimates
  res <- data.frame(
    Model = c(
      "True maternal effect",
      "Confounding effect",
      "Maternal Model 1",
      "Maternal Model 2",
      "Paternal Model 1",
      "Paternal Model 2"
    ),
    b = c(
      0,
      0.2 * 0.8,
      m1$coefficients["A", "Estimate"],
      m2$coefficients["A", "Estimate"],
      p1$coefficients["B", "Estimate"],
      p2$coefficients["B", "Estimate"]
    ),
    se = c(
      0,
      0,
      m1$coefficients["A", "Std. Error"],
      m2$coefficients["A", "Std. Error"],
      p1$coefficients["B", "Std. Error"],
      p2$coefficients["B", "Std. Error"]
    ),
    pval = c(
      0,
      0,
      m1$coefficients["A", "Pr(>|t|)"],
      m2$coefficients["A", "Pr(>|t|)"],
      p1$coefficients["B", "Pr(>|t|)"],
      p2$coefficients["B", "Pr(>|t|)"]
    )
  )
  
  sim_res[[i]] <- res
}

## Combine all results and compute mean values
res_3 <- do.call(rbind, sim_res) |>
  dplyr::group_by(Model) |>
  dplyr::summarise(beta = mean(b),
                   SE = mean(se),
                   pvalue = 2 * (1 - pnorm(abs(beta / SE)))) |>
  dplyr::mutate(
    Scenario = "Scenario 3",
    Group = "Scenario: maternal effect: NULL; shared confounding: POSITIVE",
    Model = factor(
      Model,
      levels = c(
        "Paternal Model 2",
        "Paternal Model 1",
        "Maternal Model 2",
        "Maternal Model 1",
        "Confounding effect",
        "True maternal effect"
      )
    )
  ) |>
  dplyr::mutate(pvalue = ifelse(beta != 0 &
                                  SE == 0, 0.0001, pvalue)) |>
  dplyr::arrange(factor(Model, levels = rev(levels(Model))))

################################################################################
## Forest plot
res_2_3 <- dplyr::bind_rows(res_2, res_3)

res_2_3$Group <- factor(res_2_3$Group, levels = unique(res_2_3$Group))

forest_2_3 <- ggforestplot::forestplot(
  df = res_2_3,
  name = Model,
  estimate = beta,
  se = SE,
  pvalue = pvalue,
  psignif = 0.05,
  colour = Model,
  shape = Model,
  xlab = "Beta or (or logOR) and 95% CI",
  title = "",
  logodds = F
) +
  ggplot2::scale_colour_manual(values = c("darkblue", "blue", "darkred", "red", "darkorange", "darkgreen")) +
  ggplot2::scale_shape_manual(values = c(21, 21, 21, 21, 21, 21)) +
  ggplot2::theme(legend.position = "none") +
  ggforce::facet_col(facets = ~ Group,
                     scales = "free_y",
                     space = "free") +
  ggplot2::coord_cartesian(xlim = c(-0.4, 0.4))

ggplot2::ggsave(forest_2_3,
                file = "simulation_PNC(2).png",
                height = 4.5,
                width = 7)
################################################################################

################################################################################
################################################################################

# Scenario 4: U → A, U → B, A → Y (negative); no other direct effects

## Set seed for reproducibility
set.seed(19705)

## Sample size per simulation
n <- 1000

## Number of repetitions
n_sim <- 100

## Create empty list to store simulation results
sim_res <- vector("list", n_sim)

## Begin simulation loop
for (i in 1:n_sim) {
  ## Generate U: unmeasured confounder, normally distributed
  U <- rnorm(n, 0, 1)
  
  ### Step 1: Generate raw A and B influenced by U
  A_raw <- 0.4 * U + rnorm(n, 0, 1)
  B_raw <- 0.4 * U + rnorm(n, 0, 1)
  
  ### Step 2: Residualize B to orthogonalise it from A, then rescale to r = 0.4
  resid_B <- lm(B_raw ~ A_raw)$residuals
  B <- 0.4 * scale(A_raw) + sqrt(1 - 0.4^2) * scale(resid_B)
  A <- scale(A_raw)
  
  ### Confirm correlation
  stopifnot(abs(cor(A, B) - 0.4) < 0.001)
  
  ## Generate outcome Y: negatively influenced by A only
  Y <- -0.2 * A + rnorm(n, 0, 1)
  
  ## Fit models
  M <- summary(lm(Y ~ A + U))
  m1 <- summary(lm(Y ~ A))
  m2 <- summary(lm(Y ~ A + B))
  p1 <- summary(lm(Y ~ B))
  p2 <- summary(lm(Y ~ B + A))
  
  ## Extract estimates
  res <- data.frame(
    Model = c(
      "True maternal effect",
      "Confounding effect",
      "Maternal Model 1",
      "Maternal Model 2",
      "Paternal Model 1",
      "Paternal Model 2"
    ),
    b = c(
      -0.2,
      0,
      m1$coefficients["A", "Estimate"],
      m2$coefficients["A", "Estimate"],
      p1$coefficients["B", "Estimate"],
      p2$coefficients["B", "Estimate"]
    ),
    se = c(
      0,
      0,
      m1$coefficients["A", "Std. Error"],
      m2$coefficients["A", "Std. Error"],
      p1$coefficients["B", "Std. Error"],
      p2$coefficients["B", "Std. Error"]
    ),
    pval = c(
      0,
      0,
      m1$coefficients["A", "Pr(>|t|)"],
      m2$coefficients["A", "Pr(>|t|)"],
      p1$coefficients["B", "Pr(>|t|)"],
      p2$coefficients["B", "Pr(>|t|)"]
    )
  )
  
  sim_res[[i]] <- res
}

## Combine all results and compute mean values
res_4 <- do.call(rbind, sim_res) |>
  dplyr::group_by(Model) |>
  dplyr::summarise(beta = mean(b),
                   SE = mean(se),
                   pvalue = 2 * (1 - pnorm(abs(beta / SE)))) |>
  dplyr::mutate(
    Scenario = "Scenario 4",
    Group = "Scenario: maternal effect: NEGATIVE; shared confounding: NULL",
    Model = factor(
      Model,
      levels = c(
        "Paternal Model 2",
        "Paternal Model 1",
        "Maternal Model 2",
        "Maternal Model 1",
        "Confounding effect",
        "True maternal effect"
      )
    )
  ) |>
  dplyr::mutate(pvalue = ifelse(beta != 0 &
                                  SE == 0, 0.0001, pvalue)) |>
  dplyr::arrange(factor(Model, levels = rev(levels(Model))))

################################################################################
################################################################################

# Scenario 5: U → A, U → B, A → Y (positive); no other direct effects

## Set seed for reproducibility
set.seed(19705)

## Sample size per simulation
n <- 1000

## Number of repetitions
n_sim <- 100

## Create empty list to store simulation results
sim_res <- vector("list", n_sim)

## Begin simulation loop
for (i in 1:n_sim) {
  ## Generate U: unmeasured confounder, normally distributed
  U <- rnorm(n, 0, 1)
  
  ### Step 1: Generate raw A and B influenced by U
  A_raw <- 0.4 * U + rnorm(n, 0, 1)
  B_raw <- 0.4 * U + rnorm(n, 0, 1)
  
  ### Step 2: Residualize B to orthogonalise it from A, then rescale to r = 0.4
  resid_B <- lm(B_raw ~ A_raw)$residuals
  B <- 0.4 * scale(A_raw) + sqrt(1 - 0.4^2) * scale(resid_B)
  A <- scale(A_raw)
  
  ### Confirm correlation
  stopifnot(abs(cor(A, B) - 0.4) < 0.001)
  
  ## Generate outcome Y: positively influenced by A only
  Y <- 0.2 * A + rnorm(n, 0, 1)
  
  ## Fit models
  M <- summary(lm(Y ~ A + U))
  m1 <- summary(lm(Y ~ A))
  m2 <- summary(lm(Y ~ A + B))
  p1 <- summary(lm(Y ~ B))
  p2 <- summary(lm(Y ~ B + A))
  
  ## Extract estimates
  res <- data.frame(
    Model = c(
      "True maternal effect",
      "Confounding effect",
      "Maternal Model 1",
      "Maternal Model 2",
      "Paternal Model 1",
      "Paternal Model 2"
    ),
    b = c(
      0.2,
      0,
      m1$coefficients["A", "Estimate"],
      m2$coefficients["A", "Estimate"],
      p1$coefficients["B", "Estimate"],
      p2$coefficients["B", "Estimate"]
    ),
    se = c(
      0,
      0,
      m1$coefficients["A", "Std. Error"],
      m2$coefficients["A", "Std. Error"],
      p1$coefficients["B", "Std. Error"],
      p2$coefficients["B", "Std. Error"]
    ),
    pval = c(
      0,
      0,
      m1$coefficients["A", "Pr(>|t|)"],
      m2$coefficients["A", "Pr(>|t|)"],
      p1$coefficients["B", "Pr(>|t|)"],
      p2$coefficients["B", "Pr(>|t|)"]
    )
  )
  
  sim_res[[i]] <- res
}

## Combine all results and compute mean values
res_5 <- do.call(rbind, sim_res) |>
  dplyr::group_by(Model) |>
  dplyr::summarise(beta = mean(b),
                   SE = mean(se),
                   pvalue = 2 * (1 - pnorm(abs(beta / SE)))) |>
  dplyr::mutate(
    Scenario = "Scenario 5",
    Group = "Scenario: maternal effect: POSITIVE; shared confounding: NULL",
    Model = factor(
      Model,
      levels = c(
        "Paternal Model 2",
        "Paternal Model 1",
        "Maternal Model 2",
        "Maternal Model 1",
        "Confounding effect",
        "True maternal effect"
      )
    )
  ) |>
  dplyr::mutate(pvalue = ifelse(beta != 0 &
                                  SE == 0, 0.0001, pvalue)) |>
  dplyr::arrange(factor(Model, levels = rev(levels(Model))))

################################################################################
################################################################################

# Scenario 6: U → A, U → B, U → Y (negative), A → Y (negative); no other direct effects

## Set seed for reproducibility
set.seed(19705)

## Sample size per simulation
n <- 1000

## Number of repetitions
n_sim <- 100

## Create empty list to store simulation results
sim_res <- vector("list", n_sim)

## Begin simulation loop
for (i in 1:n_sim) {
  ## Generate U: unmeasured confounder, normally distributed
  U <- rnorm(n, 0, 1)
  
  ### Step 1: Generate raw A and B influenced by U
  A_raw <- 0.4 * U + rnorm(n, 0, 1)
  B_raw <- 0.4 * U + rnorm(n, 0, 1)
  
  ### Step 2: Residualize B to orthogonalise it from A, then rescale to r = 0.4
  resid_B <- lm(B_raw ~ A_raw)$residuals
  B <- 0.4 * scale(A_raw) + sqrt(1 - 0.4^2) * scale(resid_B)
  A <- scale(A_raw)
  
  ### Confirm correlation
  stopifnot(abs(cor(A, B) - 0.4) < 0.001)
  
  ## Generate outcome Y: negatively influenced by both A and U
  Y <- -0.2 * U - 0.2 * A + rnorm(n, 0, 1)
  
  ## Fit models
  M <- summary(lm(Y ~ A + U))
  m1 <- summary(lm(Y ~ A))
  m2 <- summary(lm(Y ~ A + B))
  p1 <- summary(lm(Y ~ B))
  p2 <- summary(lm(Y ~ B + A))
  
  ## Extract estimates
  res <- data.frame(
    Model = c(
      "True maternal effect",
      "Confounding effect",
      "Maternal Model 1",
      "Maternal Model 2",
      "Paternal Model 1",
      "Paternal Model 2"
    ),
    b = c(
      -0.2,
      -0.2 * 0.4,
      m1$coefficients["A", "Estimate"],
      m2$coefficients["A", "Estimate"],
      p1$coefficients["B", "Estimate"],
      p2$coefficients["B", "Estimate"]
    ),
    se = c(
      0,
      0,
      m1$coefficients["A", "Std. Error"],
      m2$coefficients["A", "Std. Error"],
      p1$coefficients["B", "Std. Error"],
      p2$coefficients["B", "Std. Error"]
    ),
    pval = c(
      0,
      0,
      m1$coefficients["A", "Pr(>|t|)"],
      m2$coefficients["A", "Pr(>|t|)"],
      p1$coefficients["B", "Pr(>|t|)"],
      p2$coefficients["B", "Pr(>|t|)"]
    )
  )
  
  sim_res[[i]] <- res
}

## Combine all results and compute mean values
res_6 <- do.call(rbind, sim_res) |>
  dplyr::group_by(Model) |>
  dplyr::summarise(beta = mean(b),
                   SE = mean(se),
                   pvalue = 2 * (1 - pnorm(abs(beta / SE)))) |>
  dplyr::mutate(
    Scenario = "Scenario 6",
    Group = "Scenario: maternal effect: NEGATIVE; shared confounding: NEGATIVE",
    Model = factor(
      Model,
      levels = c(
        "Paternal Model 2",
        "Paternal Model 1",
        "Maternal Model 2",
        "Maternal Model 1",
        "Confounding effect",
        "True maternal effect"
      )
    )
  ) |>
  dplyr::mutate(pvalue = ifelse(beta != 0 &
                                  SE == 0, 0.0001, pvalue)) |>
  dplyr::arrange(factor(Model, levels = rev(levels(Model))))

################################################################################
################################################################################

# Scenario 7: U → A, U → B, U → Y (positive), A → Y (positive); no other direct effects

## Set seed for reproducibility
set.seed(19705)

## Sample size per simulation
n <- 1000

## Number of repetitions
n_sim <- 100

## Create empty list to store simulation results
sim_res <- vector("list", n_sim)

## Begin simulation loop
for (i in 1:n_sim) {
  ## Generate U: unmeasured confounder, normally distributed
  U <- rnorm(n, 0, 1)
  
  ### Step 1: Generate raw A and B influenced by U
  A_raw <- 0.4 * U + rnorm(n, 0, 1)
  B_raw <- 0.4 * U + rnorm(n, 0, 1)
  
  ### Step 2: Residualize B to orthogonalise it from A, then rescale to r = 0.4
  resid_B <- lm(B_raw ~ A_raw)$residuals
  B <- 0.4 * scale(A_raw) + sqrt(1 - 0.4^2) * scale(resid_B)
  A <- scale(A_raw)
  
  ### Confirm correlation
  stopifnot(abs(cor(A, B) - 0.4) < 0.001)
  
  ## Generate outcome Y: positively influenced by both U and A
  Y <- 0.2 * U + 0.2 * A + rnorm(n, 0, 1)
  
  ## Fit models
  M <- summary(lm(Y ~ A + U))
  m1 <- summary(lm(Y ~ A))
  m2 <- summary(lm(Y ~ A + B))
  p1 <- summary(lm(Y ~ B))
  p2 <- summary(lm(Y ~ B + A))
  
  ## Extract estimates
  res <- data.frame(
    Model = c(
      "True maternal effect",
      "Confounding effect",
      "Maternal Model 1",
      "Maternal Model 2",
      "Paternal Model 1",
      "Paternal Model 2"
    ),
    b = c(
      0.2,
      0.2 * 0.4,
      m1$coefficients["A", "Estimate"],
      m2$coefficients["A", "Estimate"],
      p1$coefficients["B", "Estimate"],
      p2$coefficients["B", "Estimate"]
    ),
    se = c(
      0,
      0,
      m1$coefficients["A", "Std. Error"],
      m2$coefficients["A", "Std. Error"],
      p1$coefficients["B", "Std. Error"],
      p2$coefficients["B", "Std. Error"]
    ),
    pval = c(
      0,
      0,
      m1$coefficients["A", "Pr(>|t|)"],
      m2$coefficients["A", "Pr(>|t|)"],
      p1$coefficients["B", "Pr(>|t|)"],
      p2$coefficients["B", "Pr(>|t|)"]
    )
  )
  
  sim_res[[i]] <- res
}

## Combine all results and compute mean values
res_7 <- do.call(rbind, sim_res) |>
  dplyr::group_by(Model) |>
  dplyr::summarise(beta = mean(b),
                   SE = mean(se),
                   pvalue = 2 * (1 - pnorm(abs(beta / SE)))) |>
  dplyr::mutate(
    Scenario = "Scenario 7",
    Group = "Scenario: maternal effect: POSITIVE; shared confounding: POSITIVE",
    Model = factor(
      Model,
      levels = c(
        "Paternal Model 2",
        "Paternal Model 1",
        "Maternal Model 2",
        "Maternal Model 1",
        "Confounding effect",
        "True maternal effect"
      )
    )
  ) |>
  dplyr::mutate(pvalue = ifelse(beta != 0 &
                                  SE == 0, 0.0001, pvalue)) |>
  dplyr::arrange(factor(Model, levels = rev(levels(Model))))

################################################################################
## Forest plot
res_4_5_6_7 <- dplyr::bind_rows(res_4, res_5, res_6, res_7)

res_4_5_6_7$Group <- factor(res_4_5_6_7$Group, levels = unique(res_4_5_6_7$Group))

forest_4_5_6_7 <- ggforestplot::forestplot(
  df = res_4_5_6_7,
  name = Model,
  estimate = beta,
  se = SE,
  pvalue = pvalue,
  psignif = 0.05,
  colour = Model,
  shape = Model,
  xlab = "Beta or (or logOR) and 95% CI",
  title = "",
  logodds = F
) +
  ggplot2::scale_colour_manual(values = c("darkblue", "blue", "darkred", "red", "darkorange", "darkgreen")) +
  ggplot2::scale_shape_manual(values = c(21, 21, 21, 21, 21, 21)) +
  ggplot2::theme(legend.position = "none") +
  ggforce::facet_col(facets = ~ Group,
                     scales = "free_y",
                     space = "free") +
  ggplot2::coord_cartesian(xlim = c(-0.4, 0.4))

ggplot2::ggsave(forest_4_5_6_7,
                file = "simulation_PNC(3).png",
                height = 7.5,
                width = 7)
################################################################################

################################################################################
################################################################################

# Scenario 8: U → A, U → B, U → Y (positive), A → Y (negative); no other direct effects

## Set seed for reproducibility
set.seed(19705)

## Sample size per simulation
n <- 1000

## Number of repetitions
n_sim <- 100

## Create empty list to store simulation results
sim_res <- vector("list", n_sim)

## Begin simulation loop
for (i in 1:n_sim) {
  ## Generate U: unmeasured confounder, normally distributed
  U <- rnorm(n, 0, 1)
  
  ### Step 1: Generate raw A and B influenced by U
  A_raw <- 0.4 * U + rnorm(n, 0, 1)
  B_raw <- 0.4 * U + rnorm(n, 0, 1)
  
  ### Step 2: Residualize B to orthogonalise it from A, then rescale to r = 0.4
  resid_B <- lm(B_raw ~ A_raw)$residuals
  B <- 0.4 * scale(A_raw) + sqrt(1 - 0.4^2) * scale(resid_B)
  A <- scale(A_raw)
  
  ### Confirm correlation
  stopifnot(abs(cor(A, B) - 0.4) < 0.001)
  
  ## Generate outcome Y: positively influenced by U, negatively by A
  Y <- 0.6 * U - 0.1 * A + rnorm(n, 0, 1)
  
  ## Fit models
  M <- summary(lm(Y ~ A + U))
  m1 <- summary(lm(Y ~ A))
  m2 <- summary(lm(Y ~ A + B))
  p1 <- summary(lm(Y ~ B))
  p2 <- summary(lm(Y ~ B + A))
  
  ## Extract estimates
  res <- data.frame(
    Model = c(
      "True maternal effect",
      "Confounding effect",
      "Maternal Model 1",
      "Maternal Model 2",
      "Paternal Model 1",
      "Paternal Model 2"
    ),
    b = c(
      -0.1,
      0.6 * 0.4,
      m1$coefficients["A", "Estimate"],
      m2$coefficients["A", "Estimate"],
      p1$coefficients["B", "Estimate"],
      p2$coefficients["B", "Estimate"]
    ),
    se = c(
      0,
      0,
      m1$coefficients["A", "Std. Error"],
      m2$coefficients["A", "Std. Error"],
      p1$coefficients["B", "Std. Error"],
      p2$coefficients["B", "Std. Error"]
    ),
    pval = c(
      0,
      0,
      m1$coefficients["A", "Pr(>|t|)"],
      m2$coefficients["A", "Pr(>|t|)"],
      p1$coefficients["B", "Pr(>|t|)"],
      p2$coefficients["B", "Pr(>|t|)"]
    )
  )
  
  sim_res[[i]] <- res
}

## Combine all results and compute mean values
res_8 <- do.call(rbind, sim_res) |>
  dplyr::group_by(Model) |>
  dplyr::summarise(beta = mean(b),
                   SE = mean(se),
                   pvalue = 2 * (1 - pnorm(abs(beta / SE)))) |>
  dplyr::mutate(
    Scenario = "Scenario 8",
    Group = "Scenario: maternal effect (NEGATIVE) < shared confounding (POSITIVE)",
    Model = factor(
      Model,
      levels = c(
        "Paternal Model 2",
        "Paternal Model 1",
        "Maternal Model 2",
        "Maternal Model 1",
        "Confounding effect",
        "True maternal effect"
      )
    )
  ) |>
  dplyr::mutate(pvalue = ifelse(beta != 0 &
                                  SE == 0, 0.0001, pvalue)) |>
  dplyr::arrange(factor(Model, levels = rev(levels(Model))))

################################################################################
################################################################################

# Scenario 9: U → A, U → B, U → Y (positive), A → Y (negative); no other direct effects

## Set seed for reproducibility
set.seed(19705)

## Sample size per simulation
n <- 1000

## Number of repetitions
n_sim <- 100

## Create empty list to store simulation results
sim_res <- vector("list", n_sim)

## Begin simulation loop
for (i in 1:n_sim) {
  ## Generate U: unmeasured confounder, normally distributed
  U <- rnorm(n, 0, 1)
  
  ### Step 1: Generate raw A and B influenced by U
  A_raw <- 0.1 * U + rnorm(n, 0, 1)
  B_raw <- 0.1 * U + rnorm(n, 0, 1)
  
  ### Step 2: Residualize B to orthogonalise it from A, then rescale to r = 0.4
  resid_B <- lm(B_raw ~ A_raw)$residuals
  B <- 0.4 * scale(A_raw) + sqrt(1 - 0.4^2) * scale(resid_B)
  A <- scale(A_raw)
  
  ### Confirm correlation
  stopifnot(abs(cor(A, B) - 0.4) < 0.001)
  
  ## Generate outcome Y: positively influenced by U, negatively by A
  Y <- 2 * U - 0.2 * A + rnorm(n, 0, 1)
  
  ## Fit models
  M <- summary(lm(Y ~ A + U))
  m1 <- summary(lm(Y ~ A))
  m2 <- summary(lm(Y ~ A + B))
  p1 <- summary(lm(Y ~ B))
  p2 <- summary(lm(Y ~ B + A))
  
  ## Extract estimates
  res <- data.frame(
    Model = c(
      "True maternal effect",
      "Confounding effect",
      "Maternal Model 1",
      "Maternal Model 2",
      "Paternal Model 1",
      "Paternal Model 2"
    ),
    b = c(
      -0.2,
      2 * 0.1,
      m1$coefficients["A", "Estimate"],
      m2$coefficients["A", "Estimate"],
      p1$coefficients["B", "Estimate"],
      p2$coefficients["B", "Estimate"]
    ),
    se = c(
      0,
      0,
      m1$coefficients["A", "Std. Error"],
      m2$coefficients["A", "Std. Error"],
      p1$coefficients["B", "Std. Error"],
      p2$coefficients["B", "Std. Error"]
    ),
    pval = c(
      0,
      0,
      m1$coefficients["A", "Pr(>|t|)"],
      m2$coefficients["A", "Pr(>|t|)"],
      p1$coefficients["B", "Pr(>|t|)"],
      p2$coefficients["B", "Pr(>|t|)"]
    )
  )
  
  sim_res[[i]] <- res
}

## Combine all results and compute mean values
res_9 <- do.call(rbind, sim_res) |>
  dplyr::group_by(Model) |>
  dplyr::summarise(beta = mean(b),
                   SE = mean(se),
                   pvalue = 2 * (1 - pnorm(abs(beta / SE)))) |>
  dplyr::mutate(
    Scenario = "Scenario 9",
    Group = "Scenario: maternal effect (NEGATIVE) ≈ shared confounding (POSITIVE)",
    Model = factor(
      Model,
      levels = c(
        "Paternal Model 2",
        "Paternal Model 1",
        "Maternal Model 2",
        "Maternal Model 1",
        "Confounding effect",
        "True maternal effect"
      )
    )
  ) |>
  dplyr::mutate(pvalue = ifelse(beta != 0 &
                                  SE == 0, 0.0001, pvalue)) |>
  dplyr::arrange(factor(Model, levels = rev(levels(Model))))

################################################################################
################################################################################

# Scenario 10: U → A, U → B, U → Y (positive), A → Y (negative); no other direct effects

## Set seed for reproducibility
set.seed(19705)

## Sample size per simulation
n <- 1000

## Number of repetitions
n_sim <- 100

## Create empty list to store simulation results
sim_res <- vector("list", n_sim)

## Begin simulation loop
for (i in 1:n_sim) {
  ## Generate U: unmeasured confounder, normally distributed
  U <- rnorm(n, 0, 1)
  
  ### Step 1: Generate raw A and B influenced by U
  A_raw <- 0.4 * U + rnorm(n, 0, 1)
  B_raw <- 0.4 * U + rnorm(n, 0, 1)
  
  ### Step 2: Residualize B to orthogonalise it from A, then rescale to r = 0.4
  resid_B <- lm(B_raw ~ A_raw)$residuals
  B <- 0.4 * scale(A_raw) + sqrt(1 - 0.4^2) * scale(resid_B)
  A <- scale(A_raw)
  
  ### Confirm correlation
  stopifnot(abs(cor(A, B) - 0.4) < 0.001)
  
  ## Generate outcome Y: positively influenced by U, negatively by A
  Y <- 0.4 * U - 0.3 * A + rnorm(n, 0, 1)
  
  ## Fit models
  M <- summary(lm(Y ~ A + U))
  m1 <- summary(lm(Y ~ A))
  m2 <- summary(lm(Y ~ A + B))
  p1 <- summary(lm(Y ~ B))
  p2 <- summary(lm(Y ~ B + A))
  
  ## Extract estimates
  res <- data.frame(
    Model = c(
      "True maternal effect",
      "Confounding effect",
      "Maternal Model 1",
      "Maternal Model 2",
      "Paternal Model 1",
      "Paternal Model 2"
    ),
    b = c(
      -0.3,
      0.4 * 0.4,
      m1$coefficients["A", "Estimate"],
      m2$coefficients["A", "Estimate"],
      p1$coefficients["B", "Estimate"],
      p2$coefficients["B", "Estimate"]
    ),
    se = c(
      0,
      0,
      m1$coefficients["A", "Std. Error"],
      m2$coefficients["A", "Std. Error"],
      p1$coefficients["B", "Std. Error"],
      p2$coefficients["B", "Std. Error"]
    ),
    pval = c(
      0,
      0,
      m1$coefficients["A", "Pr(>|t|)"],
      m2$coefficients["A", "Pr(>|t|)"],
      p1$coefficients["B", "Pr(>|t|)"],
      p2$coefficients["B", "Pr(>|t|)"]
    )
  )
  
  sim_res[[i]] <- res
}

## Combine all results and compute mean values
res_10 <- do.call(rbind, sim_res) |>
  dplyr::group_by(Model) |>
  dplyr::summarise(beta = mean(b),
                   SE = mean(se),
                   pvalue = 2 * (1 - pnorm(abs(beta / SE)))) |>
  dplyr::mutate(
    Scenario = "Scenario 10",
    Group = "Scenario: maternal effect (NEGATIVE) > shared confounding (POSITIVE)",
    Model = factor(
      Model,
      levels = c(
        "Paternal Model 2",
        "Paternal Model 1",
        "Maternal Model 2",
        "Maternal Model 1",
        "Confounding effect",
        "True maternal effect"
      )
    )
  ) |>
  dplyr::mutate(pvalue = ifelse(beta != 0 &
                                  SE == 0, 0.0001, pvalue)) |>
  dplyr::arrange(factor(Model, levels = rev(levels(Model))))

################################################################################
################################################################################

# Scenario 11: U → A, U → B, U → Y (negative), A → Y (positive); no other direct effects

## Set seed for reproducibility
set.seed(19705)

## Sample size per simulation
n <- 1000

## Number of repetitions
n_sim <- 100

## Create empty list to store simulation results
sim_res <- vector("list", n_sim)

## Begin simulation loop
for (i in 1:n_sim) {
  ## Generate U: unmeasured confounder, normally distributed
  U <- rnorm(n, 0, 1)
  
  ### Step 1: Generate raw A and B influenced by U
  A_raw <- 0.4 * U + rnorm(n, 0, 1)
  B_raw <- 0.4 * U + rnorm(n, 0, 1)
  
  ### Step 2: Residualize B to orthogonalise it from A, then rescale to r = 0.4
  resid_B <- lm(B_raw ~ A_raw)$residuals
  B <- 0.4 * scale(A_raw) + sqrt(1 - 0.4^2) * scale(resid_B)
  A <- scale(A_raw)
  
  ### Confirm correlation
  stopifnot(abs(cor(A, B) - 0.4) < 0.001)
  
  ## Generate outcome Y: positively influenced by U, negatively by A
  Y <- -0.6 * U + 0.1 * A + rnorm(n, 0, 1)
  
  ## Fit models
  M <- summary(lm(Y ~ A + U))
  m1 <- summary(lm(Y ~ A))
  m2 <- summary(lm(Y ~ A + B))
  p1 <- summary(lm(Y ~ B))
  p2 <- summary(lm(Y ~ B + A))
  
  ## Extract estimates
  res <- data.frame(
    Model = c(
      "True maternal effect",
      "Confounding effect",
      "Maternal Model 1",
      "Maternal Model 2",
      "Paternal Model 1",
      "Paternal Model 2"
    ),
    b = c(
      0.1,
      -0.6 * 0.4,
      m1$coefficients["A", "Estimate"],
      m2$coefficients["A", "Estimate"],
      p1$coefficients["B", "Estimate"],
      p2$coefficients["B", "Estimate"]
    ),
    se = c(
      0,
      0,
      m1$coefficients["A", "Std. Error"],
      m2$coefficients["A", "Std. Error"],
      p1$coefficients["B", "Std. Error"],
      p2$coefficients["B", "Std. Error"]
    ),
    pval = c(
      0,
      0,
      m1$coefficients["A", "Pr(>|t|)"],
      m2$coefficients["A", "Pr(>|t|)"],
      p1$coefficients["B", "Pr(>|t|)"],
      p2$coefficients["B", "Pr(>|t|)"]
    )
  )
  
  sim_res[[i]] <- res
}

## Combine all results and compute mean values
res_11 <- do.call(rbind, sim_res) |>
  dplyr::group_by(Model) |>
  dplyr::summarise(beta = mean(b),
                   SE = mean(se),
                   pvalue = 2 * (1 - pnorm(abs(beta / SE)))) |>
  dplyr::mutate(
    Scenario = "Scenario 11",
    Group = "Scenario: maternal effect (POSITIVE) < shared confounding (NEGATIVE)",
    Model = factor(
      Model,
      levels = c(
        "Paternal Model 2",
        "Paternal Model 1",
        "Maternal Model 2",
        "Maternal Model 1",
        "Confounding effect",
        "True maternal effect"
      )
    )
  ) |>
  dplyr::mutate(pvalue = ifelse(beta != 0 &
                                  SE == 0, 0.0001, pvalue)) |>
  dplyr::arrange(factor(Model, levels = rev(levels(Model))))

################################################################################
################################################################################

# Scenario 12: U → A, U → B, U → Y (negative), A → Y (positive); no other direct effects

## Set seed for reproducibility
set.seed(19705)

## Sample size per simulation
n <- 1000

## Number of repetitions
n_sim <- 100

## Create empty list to store simulation results
sim_res <- vector("list", n_sim)

## Begin simulation loop
for (i in 1:n_sim) {
  ## Generate U: unmeasured confounder, normally distributed
  U <- rnorm(n, 0, 1)
  
  ### Step 1: Generate raw A and B influenced by U
  A_raw <- 0.1 * U + rnorm(n, 0, 1)
  B_raw <- 0.1 * U + rnorm(n, 0, 1)
  
  ### Step 2: Residualize B to orthogonalise it from A, then rescale to r = 0.4
  resid_B <- lm(B_raw ~ A_raw)$residuals
  B <- 0.4 * scale(A_raw) + sqrt(1 - 0.4^2) * scale(resid_B)
  A <- scale(A_raw)
  
  ### Confirm correlation
  stopifnot(abs(cor(A, B) - 0.4) < 0.001)
  
  ## Generate outcome Y: positively influenced by U, negatively by A
  Y <- -2 * U + 0.2 * A + rnorm(n, 0, 1)
  
  ## Fit models
  M <- summary(lm(Y ~ A + U))
  m1 <- summary(lm(Y ~ A))
  m2 <- summary(lm(Y ~ A + B))
  p1 <- summary(lm(Y ~ B))
  p2 <- summary(lm(Y ~ B + A))
  
  ## Extract estimates
  res <- data.frame(
    Model = c(
      "True maternal effect",
      "Confounding effect",
      "Maternal Model 1",
      "Maternal Model 2",
      "Paternal Model 1",
      "Paternal Model 2"
    ),
    b = c(
      0.2,
      -2 * 0.1,
      m1$coefficients["A", "Estimate"],
      m2$coefficients["A", "Estimate"],
      p1$coefficients["B", "Estimate"],
      p2$coefficients["B", "Estimate"]
    ),
    se = c(
      0,
      0,
      m1$coefficients["A", "Std. Error"],
      m2$coefficients["A", "Std. Error"],
      p1$coefficients["B", "Std. Error"],
      p2$coefficients["B", "Std. Error"]
    ),
    pval = c(
      0,
      0,
      m1$coefficients["A", "Pr(>|t|)"],
      m2$coefficients["A", "Pr(>|t|)"],
      p1$coefficients["B", "Pr(>|t|)"],
      p2$coefficients["B", "Pr(>|t|)"]
    )
  )
  
  sim_res[[i]] <- res
}

## Combine all results and compute mean values
res_12 <- do.call(rbind, sim_res) |>
  dplyr::group_by(Model) |>
  dplyr::summarise(beta = mean(b),
                   SE = mean(se),
                   pvalue = 2 * (1 - pnorm(abs(beta / SE)))) |>
  dplyr::mutate(
    Scenario = "Scenario 12",
    Group = "Scenario: maternal effect (POSITIVE) ≈ shared confounding (NEGATIVE)",
    Model = factor(
      Model,
      levels = c(
        "Paternal Model 2",
        "Paternal Model 1",
        "Maternal Model 2",
        "Maternal Model 1",
        "Confounding effect",
        "True maternal effect"
      )
    )
  ) |>
  dplyr::mutate(pvalue = ifelse(beta != 0 &
                                  SE == 0, 0.0001, pvalue)) |>
  dplyr::arrange(factor(Model, levels = rev(levels(Model))))

################################################################################
################################################################################

# Scenario 13: U → A, U → B, U → Y (negative), A → Y (positive); no other direct effects

## Set seed for reproducibility
set.seed(19705)

## Sample size per simulation
n <- 1000

## Number of repetitions
n_sim <- 100

## Create empty list to store simulation results
sim_res <- vector("list", n_sim)

## Begin simulation loop
for (i in 1:n_sim) {
  ## Generate U: unmeasured confounder, normally distributed
  U <- rnorm(n, 0, 1)
  
  ### Step 1: Generate raw A and B influenced by U
  A_raw <- 0.4 * U + rnorm(n, 0, 1)
  B_raw <- 0.4 * U + rnorm(n, 0, 1)
  
  ### Step 2: Residualize B to orthogonalise it from A, then rescale to r = 0.4
  resid_B <- lm(B_raw ~ A_raw)$residuals
  B <- 0.4 * scale(A_raw) + sqrt(1 - 0.4^2) * scale(resid_B)
  A <- scale(A_raw)
  
  ### Confirm correlation
  stopifnot(abs(cor(A, B) - 0.4) < 0.001)
  
  ## Generate outcome Y: positively influenced by U, negatively by A
  Y <- -0.4 * U + 0.3 * A + rnorm(n, 0, 1)
  
  ## Fit models
  M <- summary(lm(Y ~ A + U))
  m1 <- summary(lm(Y ~ A))
  m2 <- summary(lm(Y ~ A + B))
  p1 <- summary(lm(Y ~ B))
  p2 <- summary(lm(Y ~ B + A))
  
  ## Extract estimates
  res <- data.frame(
    Model = c(
      "True maternal effect",
      "Confounding effect",
      "Maternal Model 1",
      "Maternal Model 2",
      "Paternal Model 1",
      "Paternal Model 2"
    ),
    b = c(
      0.3,
      -0.4 * 0.4,
      m1$coefficients["A", "Estimate"],
      m2$coefficients["A", "Estimate"],
      p1$coefficients["B", "Estimate"],
      p2$coefficients["B", "Estimate"]
    ),
    se = c(
      0,
      0,
      m1$coefficients["A", "Std. Error"],
      m2$coefficients["A", "Std. Error"],
      p1$coefficients["B", "Std. Error"],
      p2$coefficients["B", "Std. Error"]
    ),
    pval = c(
      0,
      0,
      m1$coefficients["A", "Pr(>|t|)"],
      m2$coefficients["A", "Pr(>|t|)"],
      p1$coefficients["B", "Pr(>|t|)"],
      p2$coefficients["B", "Pr(>|t|)"]
    )
  )
  
  sim_res[[i]] <- res
}

## Combine all results and compute mean values
res_13 <- do.call(rbind, sim_res) |>
  dplyr::group_by(Model) |>
  dplyr::summarise(beta = mean(b),
                   SE = mean(se),
                   pvalue = 2 * (1 - pnorm(abs(beta / SE)))) |>
  dplyr::mutate(
    Scenario = "Scenario 13",
    Group = "Scenario: maternal effect (POSITIVE) > shared confounding (NEGATIVE)",
    Model = factor(
      Model,
      levels = c(
        "Paternal Model 2",
        "Paternal Model 1",
        "Maternal Model 2",
        "Maternal Model 1",
        "Confounding effect",
        "True maternal effect"
      )
    )
  ) |>
  dplyr::mutate(pvalue = ifelse(beta != 0 &
                                  SE == 0, 0.0001, pvalue)) |>
  dplyr::arrange(factor(Model, levels = rev(levels(Model))))

################################################################################
################################################################################

################################################################################
## Forest plot
res_9_12 <- dplyr::bind_rows(res_9, res_12)

res_9_12$Group <- factor(res_9_12$Group, levels = unique(res_9_12$Group))

forest_9_12 <- ggforestplot::forestplot(
  df = res_9_12,
  name = Model,
  estimate = beta,
  se = SE,
  pvalue = pvalue,
  psignif = 0.05,
  colour = Model,
  shape = Model,
  xlab = "Beta or (or logOR) and 95% CI",
  title = "",
  logodds = F
) +
  ggplot2::scale_colour_manual(values = c("darkblue", "blue", "darkred", "red", "darkorange", "darkgreen")) +
  ggplot2::scale_shape_manual(values = c(21, 21, 21, 21, 21, 21)) +
  ggplot2::theme(legend.position = "none") +
  ggforce::facet_col(facets = ~ Group,
                     scales = "free_y",
                     space = "free") +
  ggplot2::coord_cartesian(xlim = c(-0.4, 0.4))

ggplot2::ggsave(forest_9_12,
                file = "simulation_PNC(4).png",
                height = 4.5,
                width = 7)
################################################################################
################################################################################
res_8_11 <- dplyr::bind_rows(res_8, res_11)

res_8_11$Group <- factor(res_8_11$Group, levels = unique(res_8_11$Group))

forest_8_11 <- ggforestplot::forestplot(
  df = res_8_11,
  name = Model,
  estimate = beta,
  se = SE,
  pvalue = pvalue,
  psignif = 0.05,
  colour = Model,
  shape = Model,
  xlab = "Beta or (or logOR) and 95% CI",
  title = "",
  logodds = F
) +
  ggplot2::scale_colour_manual(values = c("darkblue", "blue", "darkred", "red", "darkorange", "darkgreen")) +
  ggplot2::scale_shape_manual(values = c(21, 21, 21, 21, 21, 21)) +
  ggplot2::theme(legend.position = "none") +
  ggforce::facet_col(facets = ~ Group,
                     scales = "free_y",
                     space = "free") +
  ggplot2::coord_cartesian(xlim = c(-0.4, 0.4))

ggplot2::ggsave(forest_8_11,
                file = "simulation_PNC(5).png",
                height = 4.5,
                width = 7)
################################################################################
################################################################################
res_10_13 <- dplyr::bind_rows(res_10, res_13)

res_10_13$Group <- factor(res_10_13$Group, levels = unique(res_10_13$Group))

forest_10_13 <- ggforestplot::forestplot(
  df = res_10_13,
  name = Model,
  estimate = beta,
  se = SE,
  pvalue = pvalue,
  psignif = 0.05,
  colour = Model,
  shape = Model,
  xlab = "Beta or (or logOR) and 95% CI",
  title = "",
  logodds = F
) +
  ggplot2::scale_colour_manual(values = c("darkblue", "blue", "darkred", "red", "darkorange", "darkgreen")) +
  ggplot2::scale_shape_manual(values = c(21, 21, 21, 21, 21, 21)) +
  ggplot2::theme(legend.position = "none") +
  ggforce::facet_col(facets = ~ Group,
                     scales = "free_y",
                     space = "free") +
  ggplot2::coord_cartesian(xlim = c(-0.4, 0.4))

ggplot2::ggsave(forest_10_13,
                file = "simulation_PNC(6).png",
                height = 4.5,
                width = 7)
################################################################################

# Combine all results into a single forest plot

res_all <- dplyr::bind_rows(
  res_1,
  res_2,
  res_3,
  res_4,
  res_5,
  res_6,
  res_7,
  res_8,
  res_9,
  res_10,
  res_11,
  res_12,
  res_13
)

res_all$Group <- factor(res_all$Group, levels = unique(res_all$Group))

forest_all <- ggforestplot::forestplot(
  df = res_all,
  name = Scenario,
  estimate = beta,
  se = SE,
  pvalue = pvalue,
  psignif = 0.05,
  colour = Model,
  shape = Model,
  xlab = "Beta or (or logOR) and 95% CI",
  title = "",
  logodds = F
) +
  ggplot2::scale_colour_manual(values = c("darkblue", "blue", "darkred", "red", "darkorange", "darkgreen")) +
  ggplot2::scale_shape_manual(values = c(21, 21, 21, 21, 21, 21)) +
  ggforce::facet_col(facets = ~ Group,
                     scales = "free_y",
                     space = "free")

ggplot2::ggsave(forest_all,
                file = "simulation_PNC.png",
                height = 18,
                width = 10)

################################################################################
################################################################################
