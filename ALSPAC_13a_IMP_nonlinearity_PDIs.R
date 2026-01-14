################################################################################
#      Maternal Vegetarian/Plant-based Diets & Perinatal Health - ALSPAC       #
################################################################################

# Last edited date: 01-Aug-2025
# This script is to perform non-linearity analysis (with imputed data) for plant-based diet indices (PDIs) in ALSPAC.

################################################################################

#------------------------------------------------------------------------------#
#                               House‑keeping                                  #
#------------------------------------------------------------------------------#

rm(list = ls())                                   # Clear environment
sessionInfo()                                     # Session info

pacman::p_load(
  # Load packages
  tidyverse,
  openxlsx,
  haven,
  expss,
  gtsummary,
  kableExtra,
  flextable,
  readr,
  magrittr,
  ggplot2,
  hrbrthemes,
  gridExtra,
  mice,
  splines,
  broom,
  rms                     # <- 新增
)

setwd("Z:/working/")                              # Working dir

#------------------------------------------------------------------------------#
#                               Read imputed data                              #
#------------------------------------------------------------------------------#

load("data/ALSPAC/dat_exp_cov_out_pat_IMP_PDIs.RData")  # 载入 dat_imp (mids)
m          <- dat_imp$m                               # 插补次数
complete_1 <- complete(dat_imp, 1)                   # 第一份完成数据用于定义 knots

#------------------------------------------------------------------------------#
#                           Outcome & covariate setup                          #
#------------------------------------------------------------------------------#

MRPREG_outcome_labels <- read.xlsx("data/MRPREG_outcome_labels.xlsx", "Label")
primary_bin          <- read.xlsx("data/MRPREG_outcome_labels.xlsx", "Primary_bin")
secondary_bin        <- read.xlsx("data/MRPREG_outcome_labels.xlsx", "Secondary_bin")
secondary_con        <- read.xlsx("data/MRPREG_outcome_labels.xlsx", "Secondary_con")
secondary_cat        <- read.xlsx("data/MRPREG_outcome_labels.xlsx", "Secondary_cat")

ALSPAC_primary_bin   <- primary_bin$varname  [primary_bin$varname   %in% names(complete_1)]
ALSPAC_secondary_bin <- secondary_bin$varname[secondary_bin$varname %in% names(complete_1)]
ALSPAC_secondary_con <- secondary_con$varname[secondary_con$varname %in% names(complete_1)]
ALSPAC_secondary_cat <- secondary_cat$varname[secondary_cat$varname %in% names(complete_1)]

ALSPAC_out_bin <- c(ALSPAC_primary_bin, ALSPAC_secondary_bin)
ALSPAC_out_con <- ALSPAC_secondary_con                       # (ordinal bf_dur_4c 合并进 binary)
full_covs <- c(
  "age_Mat_con",
  "ethnic_Mat_bin",
  "edu_Mat_3cat",
  "IMD_Fam_cat",
  "parity_Mat_bin",
  "BMI_Mat_PRE.p_con",
  "smoking_Mat_EAR.p_bin",
  "alcohol_Mat_EAR.p_bin",
  "sex_Chi_bin",
  "any.supp_Mat_EAR.p_bin",
  "energy_Mat_DUR.p_con"
)

#------------------------------------------------------------------------------#
#                     Helper 1 : determine fixed knots (5)                     #
#------------------------------------------------------------------------------#
get_knots <- function(x)
  stats::quantile(x, probs = c(.05, .275, .5, .725, .95), na.rm = TRUE)

knots_list <- list(
  PDI_z  = get_knots(complete_1$PDI_z),
  hPDI_z = get_knots(complete_1$hPDI_z),
  uPDI_z = get_knots(complete_1$uPDI_z)
)

#------------------------------------------------------------------------------#
#                     Helper 2 : Rubin‑rule prediction pooling                  #
#------------------------------------------------------------------------------#
pool_pred <- function(fit_list,
                      newdata,
                      type = c("link", "response"),
                      ref_row = NULL) {
  type <- match.arg(type)
  ## 对每个插补模型取预测值和 SE ：------------------------------------------
  preds <- lapply(
    fit_list,
    predict,
    newdata = newdata,
    type = type,
    se.fit = TRUE
  )
  fit_mat <- sapply(preds, `[[`, "fit")          # (n_grid × m)
  se_mat  <- sapply(preds, function(x)
    x$se.fit)
  ## Rubin 规则合并：----------------------------------------------------------
  m <- ncol(fit_mat)
  fit_bar <- rowMeans(fit_mat)
  within  <- rowMeans(se_mat^2)
  between <- apply(fit_mat, 1, var)
  tot_var <- within + (1 + 1 / m) * between
  se_bar  <- sqrt(tot_var)
  
  out <- tibble(fit = fit_bar, se = se_bar)
  if (!is.null(ref_row)) {
    ## 调整为 OR / mean difference 相对参考点 ------------------------------
    ref_fit <- out$fit[ref_row]
    out$fit <- out$fit - ref_fit
  }
  out
}

#------------------------------------------------------------------------------#
#                    Helper 3 : main plotting function (ggplot)                 #
#------------------------------------------------------------------------------#
rcsplot_mi <- function(data_mids,
                       outcome,
                       exposure,
                       knots_vec,
                       covars,
                       grid,
                       family,
                       xlab,
                       ylab,
                       title_lab) {
  ## formula strings -----------------------------------------------------------
  spline_term <- paste0("ns(",
                        exposure,
                        ", knots = c(",
                        paste(knots_vec, collapse = ", "),
                        "))")
  fmla_spline <- as.formula(paste(outcome, "~", spline_term, "+", paste(covars, collapse = " + ")))
  fmla_linear <- as.formula(paste(outcome, "~", exposure, "+", paste(covars, collapse = " + ")))
  fmla_null   <- as.formula(paste(outcome, "~", paste(covars, collapse = " + ")))
  
  ## fit across m imputations --------------------------------------------------
  fit_spline  <- with(data_mids, glm(fmla_spline, family = family))
  fit_linear  <- with(data_mids, glm(fmla_linear, family = family))
  fit_null    <- with(data_mids, glm(fmla_null  , family = family))
  
  ## pooled coefficient table --------------------------------------------------
  pooled_spline <- pool(fit_spline)
  
  ## Wald tests ----------------------------------------------------------------
  p_overall     <- pool.compare(fit_spline, fit_null  , dfcom = Inf)$pvalue  # Spline vs no‑exp
  p_nonlinear   <- pool.compare(fit_spline, fit_linear, dfcom = Inf)$pvalue  # Spline vs linear
  
  ## grid newdata  -------------------------------------------------------------
  grid_nd <- grid %>%
    mutate(across(all_of(covars), ~ if (is.numeric(complete_1[[cur_column()]])) {
      median(complete_1[[cur_column()]], na.rm = TRUE)
    } else{
      Mode(complete_1[[cur_column()]])
    })) # Mode() from expss
  
  ## pooled prediction (Rubin) -------------------------------------------------
  link_or_resp  <- if (family$family == "binomial") {
    "link"
  } else {
    "response"
  }
  pooled_pred   <- pool_pred(fit_spline$analyses,
                             grid_nd,
                             type = link_or_resp,
                             ref_row = which(grid_nd[[exposure]] == 0))
  
  ## OR / beta & 95% CI --------------------------------------------------------
  if (family$family == "binomial") {
    grid_nd$est <- exp(pooled_pred$fit)
    grid_nd$lo  <- exp(pooled_pred$fit - 1.96 * pooled_pred$se)
    grid_nd$hi  <- exp(pooled_pred$fit + 1.96 * pooled_pred$se)
  } else {
    grid_nd$est <- pooled_pred$fit
    grid_nd$lo  <- pooled_pred$fit - 1.96 * pooled_pred$se
    grid_nd$hi  <- pooled_pred$fit + 1.96 * pooled_pred$se
  }
  
  ## ggplot --------------------------------------------------------------------
  p <- ggplot(grid_nd, aes_string(exposure, "est")) +
    geom_ribbon(aes(ymin = lo, ymax = hi),
                fill = "#c6dbef",
                alpha = .45) +
    geom_line  (colour = "#2c7fb8", size = 1) +
    geom_hline(yintercept = ifelse(family$family == "binomial", 1, 0),
               linetype = "dashed") +
    labs(
      title = title_lab,
      subtitle = paste0(
        "P overall = ",
        formatC(p_overall , digits = 3, format = "f"),
        "\nP for non‑linearity = ",
        formatC(p_nonlinear, digits = 3, format = "f")
      ),
      x = xlab,
      y = ylab
    ) +
    theme_classic(base_size = 9)
  p
}

#------------------------------------------------------------------------------#
#                            Analysis & plotting                               #
#------------------------------------------------------------------------------#

## 生成暴露格点（‑4~+4 SD 足够覆盖） ------------------------------------------
grid_base <- tibble(grid_x = seq(-4, 4, 0.1))

## 暴露列表循环 ---------------------------------------------------------------
exp_list <- list(
  PDI  = list(
    var = "PDI_z" ,
    knots = knots_list$PDI_z ,
    xlab = "Standardised PDI" ,
    ylab_bin = "OR (95% CI)"
  ),
  hPDI = list(
    var = "hPDI_z",
    knots = knots_list$hPDI_z,
    xlab = "Standardised hPDI",
    ylab_bin = "OR (95% CI)"
  ),
  uPDI = list(
    var = "uPDI_z",
    knots = knots_list$uPDI_z,
    xlab = "Standardised uPDI",
    ylab_bin = "OR (95% CI)"
  )
)

## ------------------------ Binary & ordinal outcomes --------------------------
for (exp_nm in names(exp_list)) {
  plot_list <- list()                       # collect ggplots per exposure
  exposure  <- exp_list[[exp_nm]]$var
  knots_vec <- exp_list[[exp_nm]]$knots
  xlab_exp  <- exp_list[[exp_nm]]$xlab
  
  for (outcome in c(ALSPAC_out_bin, "bf_dur_4c")) {
    grid_df <- grid_base %>% rename(!!exposure := grid_x)
    
    title_lab <- var_lab(complete_1[[outcome]])
    p <- rcsplot_mi(
      dat_imp,
      outcome,
      exposure,
      knots_vec,
      covars = full_covs,
      grid   = grid_df,
      family = binomial(),
      xlab   = xlab_exp,
      ylab   = exp_list[[exp_nm]]$ylab_bin,
      title_lab = title_lab
    )
    
    plot_list <- append(plot_list, list(p))
  }
  
  pngfile <- paste0("results/ALSPAC/IMP_RCS.plots_", exp_nm, "_bin.png")
  RCS_all <- do.call(grid.arrange, c(plot_list, ncol = 4))
  ggsave(
    RCS_all,
    filename = pngfile,
    width = 16,
    height = 19,
    limitsize = FALSE
  )
}

## ----------------------------- Continuous outcomes ---------------------------
for (exp_nm in names(exp_list)) {
  plot_list <- list()
  exposure  <- exp_list[[exp_nm]]$var
  knots_vec <- exp_list[[exp_nm]]$knots
  xlab_exp  <- exp_list[[exp_nm]]$xlab
  
  for (outcome in ALSPAC_out_con) {
    grid_df  <- grid_base %>% rename(!!exposure := grid_x)
    ylab_con <- paste0("β (95% CI)")
    
    title_lab <- var_lab(complete_1[[outcome]])
    p <- rcsplot_mi(
      dat_imp,
      outcome,
      exposure,
      knots_vec,
      covars = full_covs,
      grid   = grid_df,
      family = gaussian(),
      xlab   = xlab_exp,
      ylab   = ylab_con,
      title_lab = title_lab
    )
    
    plot_list <- append(plot_list, list(p))
  }
  
  pngfile <- paste0("results/ALSPAC/IMP_RCS.plots_", exp_nm, "_con.png")
  RCS_all <- do.call(grid.arrange, c(plot_list, ncol = 4))
  ggsave(
    RCS_all,
    filename = pngfile,
    width = 16,
    height = 6,
    limitsize = FALSE
  )
}

################################################################################
