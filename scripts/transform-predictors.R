transform_counts <- function(childes_metrics, smooth = TRUE, normalize = TRUE,
                             log_transform = TRUE) {
  trans_metrics <- childes_metrics |> group_by(language)
  trans_funs <- c()
  if (smooth) trans_funs <- c(trans_funs, \(count) count + 1)
  if (normalize) trans_funs <- c(trans_funs, \(count) count / sum(count))
  if (log_transform) trans_funs <- c(trans_funs, \(count) log(count))

  for (fun in trans_funs) {
    trans_metrics <- trans_metrics |> mutate(across(starts_with("count"), fun))
  }

  trans_metrics |> ungroup() |>
    rename_with(\(col) str_replace(col, "count", "freq"), starts_with("count"))
}

transform_data <- function(df,
                           vars = c("age", "eng_prop", "freq",
                                    "concreteness", "mlu", "overlap"),
                           fun = {\(x) (x - mean(x, na.rm = T)) /
                               sd(x, na.rm = T)}) {
  df |>
    mutate(across(all_of(vars), fun, .names = "{col}_t"),
           lang_item_id = paste(item_id, "_",
                                ifelse(substr(language, 1, 7) == "English",
                                       "E", "O"),
                                sep = ""),
           lang_prop = ifelse(substr(language, 1, 7) == "English",
                              eng_prop, 100 - eng_prop),
           lang_prop_t = (lang_prop - mean(lang_prop, na.rm=T)) /
             sd(lang_prop, na.rm=T),
           across(c("value", "data_id", "language",
                    "te_known", "lang_item_id"), as.factor),
           language = `contrasts<-`(language,
                  value = language |> unique() |> length() |> contr.sum()),
           te_known = `contrasts<-`(te_known, value = -0.5 * contr.sum(2)))
}

retransform <- function(target, source) {
  (target - mean(source, na.rm = T)) / sd(source, na.rm = T)
}

transform_data_ <- function(df_target,
                            df_source,
                            vars = c("age", "eng_prop", "freq",
                                     "concreteness", "mlu", "overlap"),
                            fun = {\(x) (x - mean(x, na.rm = T)) /
                                sd(x, na.rm = T)}) {
  df_trans <- df |>
    mutate(across(all_of(vars), fun, .names = "{col}_t"),
           lang_item_id = paste(item_id, "_",
                                ifelse(substr(language, 1, 7) == "English",
                                       "E", "O"),
                                sep = ""),
           across(c("value", "data_id", "language",
                    "te_known", "lang_item_id"), as.factor))
  contrasts(df_trans$language) <- "contr.sum"
  contrasts(df_trans$te_known) <- "contr.treatment"
  df_trans
}

get_log_pf <- function(df) {
  df |>
    mutate(log_pf = log(lang_prop + 1e-16) + freq,
           log_pf_t = (log_pf - mean(log_pf, na.rm=T)) / sd(log_pf, na.rm=T))
}

join_and_transform <- function(admins, childes_metrics,
                               concreteness, phon_sim){
  admins |>
    left_join(childes_metrics, by = c("language", "uni_lemma")) |>
    left_join(concreteness, by = c("language", "uni_lemma")) |>
    left_join(phon_sim |> select(item_id, language, overlap),
              by = c("item_id", "language")) |>
    transform_data() |>
    get_log_pf()
}

select_complete <- function(df){
  comp <- df |> select(-c(tokens, options, length_phon))
  comp[complete.cases(comp), ]
}


