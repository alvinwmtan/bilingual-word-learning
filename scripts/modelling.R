cats <- c("nouns", "verbs", "adjectives", "function_words")

base_formula <- formula(value ~
                          (age_t + freq_t * lang_prop_t +
                             concreteness_t + mlu_t) +
                          (1|child_id) + (1|lang_item_id))
te_formula <- formula(value ~
                        (age_t + freq_t * lang_prop_t +
                           concreteness_t + mlu_t) * te_known +
                        (1|child_id) + (1|lang_item_id))
full_formula <- formula(value ~
                          (age_t + freq_t * lang_prop_t +
                             concreteness_t + mlu_t) *
                          (te_known * overlap_t) +
                          (1|child_id) + (1|lang_item_id))

data_src_abbrevs <- c("marchman" = "mr",
                      "hoff" = "hf",
                      "poulin-dubois" = "pl",
                      "mitchell" = "mt")

make_filename <- function(data, formula, infix = "model") {
  data_src <- data_src_abbrevs[data |> pull(data_src) |> head(1)]
  if("overlap_t" %in% (terms(formula) |> attr("term.labels"))) {
    form_name <- "full"
  } else if("te_known" %in% (terms(formula) |> attr("term.labels"))) {
    form_name <- "te"
  } else {form_name <- "base"}
  glue("{data_src}_{infix}_{form_name}.rds")
}

run_model <- function(data, formula, cache = TRUE,
                      cache_path = here("models/modelling")) {
  full_path <- here(cache_path, make_filename(data, formula))

  if(file.exists(full_path)) {
    message("Reading from cache")
    return(readRDS(full_path))
  }

  message("Running model")
  model <- brm(formula = formula,
               data = data,
               family = 'bernoulli',
               prior = set_prior('horseshoe(3)'),
               iter = 3000,
               chains = 4,
               cores = 4,
               control = list(adapt_delta = 0.85),
               backend = "cmdstanr")

  if(cache) {
    if(!file.exists(cache_path)) dir.create(cache_path, recursive = TRUE)
    saveRDS(model, full_path)
  }
  model
}

run_lc_models <- function(data, formula, cache = TRUE,
                          cache_path = here("models/modelling")) {
  full_path <- here(cache_path,
                    make_filename(data, formula, infix = "lc_models"))

  if(file.exists(full_path)) {
    message("Reading from cache")
    return(readRDS(full_path))
  }

  models <- list()
  for (c in cats) {
    models[c] <- run_model(data |> filter(lexical_class == c),
                           formula, cache = FALSE) |>
      list()
  }

  if(cache) {
    if(!file.exists(cache_path)) dir.create(cache_path, recursive = TRUE)
    saveRDS(model, full_path)
  }
  models
}

model_posteriors <- function(model) {
  describe_posterior(model,
                     centrality = "all",
                     ci = .89,
                     test = c("pd", "rope", "bf"))
}

fix_params <- function(mod_post, dataset_name, trans_te = FALSE, phon_sim_int = TRUE) {
  mod_post |>
    mutate(te = grepl("te_known", .data$Parameter),
           ps = grepl("overlap", .data$Parameter),
           Effect = case_when(te & ps & phon_sim_int ~ "TE * Phon sim",
                              te ~ "TE",
                              ps & phon_sim_int ~ "Phon sim",
                              TRUE ~ "Main"),
           Predictor = case_when(grepl("age", .data$Parameter) ~ "Age",
                                 grepl("freq_t:lang_prop", .data$Parameter) ~ "Freq * exp",
                                 grepl("log_pf", .data$Parameter) ~ "Freq * exp",
                                 grepl("freq", .data$Parameter) ~ "Frequency",
                                 grepl("lang_prop", .data$Parameter) ~ "Prop exposure",
                                 grepl("concreteness", .data$Parameter) ~ "Concreteness",
                                 grepl("mlu", .data$Parameter) ~ "MLU-w",
                                 !phon_sim_int & ps ~ "Phon sim",
                                 TRUE ~ "Intercept"),
           Dataset = dataset_name,
           Estimate = ifelse(trans_te & grepl("TE", .data$Effect), MAP*-2, MAP))
}

get_model_posteriors <- function(model, dataset_name) {
  model_posteriors(model) |>
    fix_params(dataset_name = dataset_name)
}

get_lc_models_posteriors <- function(models, dataset_name) {
  posteriors <- list()
  for (c in models |> names()) {
    posteriors[c] <- model_posteriors(models[[c]]) |>
      fix_params(dataset_name = dataset_name) |>
      mutate(lexical_class = c) |>
      list()
  }
  posteriors |> bind_rows()
}

plot_coefs <- function(posteriors) {
  ggplot(posteriors |> mutate(Predictor = fct_inorder(Predictor)),
         aes(x = Estimate,
             y = fct_rev(Predictor),
             colour = Predictor)) +
    facet_grid(. ~ Effect, scales = "free") +
    geom_rect(aes(xmin = posteriors$ROPE_low[1], xmax = posteriors$ROPE_high[1],
                  ymin = -Inf, ymax = Inf),
              fill = "gray90", alpha = .1, color = NA) +
    geom_vline(xintercept = 0, color = "gray30", linetype = "dashed") +
    geom_point(aes(shape = log_BF <= 3), size = 1, alpha = 1) +
    stat_summary(geom = "crossbar",
                 fun = mean, fun.min = mean, fun.max = mean,
                 width = 0.5, fatten = 2) +
    scale_colour_discrete(guide = "none") +
    scale_shape_manual(values = c(19, 1), guide = "none") +
    labs(x = "Coefficient estimate", y = "")
}
