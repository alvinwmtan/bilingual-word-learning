# new data loading script

cat_to_lc <- read_csv("data/other/categories.csv")

get_lang_data <- function(language, ds_origin, oth_lang,
                          lang_grp, data_src, db_args = NULL) {
  admins <- get_administration_data(language = language, form = "WS",
                          filter_age = FALSE,
                          include_demographic_info = TRUE,
                          include_language_exposure = TRUE,
                          db_args = db_args) |>
    filter(dataset_origin_name == ds_origin) |>
    rename(form_language = language)

  if (admins$language_exposures |> sapply(\(x) {!is.null(x)}) |> sum() > 0) {
    admins <- admins |>
      unnest(language_exposures) |>
      filter(language != "") |>
      pivot_wider(names_from = language,
                  values_from = exposure_proportion)
  } else {
    admins <- admins |>
      select(-language_exposures) |>
      mutate(English = NA,
             !!oth_lang := NA)
  }

  admins <- admins |>
    rename(language = form_language) |>
    select(child_id, data_id, language, form, age,
           eng_prop = English,
           oth_prop = !!oth_lang) |>
    mutate(lang_grp = lang_grp,
           data_src = data_src,
           eng_prop = coalesce(eng_prop, 100 - oth_prop),
           oth_prop = coalesce(oth_prop, 100 - eng_prop))

  items <- get_instrument_data(language = language, form = "WS",
                               item_info = TRUE, db_args = db_args) |>
    filter(item_kind == "word",
           data_id %in% (admins |> pull(data_id))) |>
    left_join(cat_to_lc, by = "category") |>
    select(data_id,
           item_id, item_definition, uni_lemma, category, lexical_class,
           value = produces)

  items |> left_join(admins, by = "data_id")
}

fetch_eng_spa_marchman_admins <- function(db_args = NULL) {
  eng <- get_lang_data("English (American)", "Marchman Dallas Bilingual",
                       "Spanish", "eng_spa", "marchman", db_args)
  spa <- get_lang_data("Spanish (Mexican)", "Marchman Dallas Bilingual",
                       "Spanish", "eng_spa", "marchman", db_args)
  rbind(eng, spa)
}

fetch_eng_spa_hoff_admins <- function(db_args = NULL) {
  # note: eng contains both bilinguals (lang group 2) and monolinguals (lang group 1)
  lang_group_eng <- read_csv("data/cdi/lang_group_eng.csv")

  eng <- get_lang_data("English (American)", "Hoff_English_Mexican_Bilingual",
                       "Spanish", "eng_spa", "hoff", db_args) |>
    left_join(lang_group_eng, by = "data_id") |>
    filter(lang_group == 2) |>
    select(-lang_group)
  spa <- get_lang_data("Spanish (Mexican)", "Hoff_English_Mexican_Bilingual",
                       "Spanish", "eng_spa", "hoff", db_args)
  rbind(eng, spa)
}

fetch_eng_fra_poulindubois_admins <- function(db_args = NULL) {
  eng <- get_lang_data("English (American)", "PoulinDubois_English_French_Bilingual",
                       "French", "eng_fra", "poulin-dubois", db_args)
  fra <- get_lang_data("French (Quebecois)", "PoulinDubois_English_French_Bilingual",
                       "French", "eng_fra", "poulin-dubois", db_args)
  rbind(eng, fra)
}

fetch_eng_ire_otoole_admins <- function(db_args = NULL) {
  eng <- get_lang_data("English (Irish)", "OToole_English_Irish_Bilingual_WS",
                       "Irish", "eng_ire", "otoole", db_args)
  ire <- get_lang_data("Irish", "OToole_English_Irish_Bilingual_WS",
                       "Irish", "eng_ire", "otoole", db_args)
  rbind(eng, ire)
}

fetch_eng_heb_armonlotem_admins <- function(db_args = NULL) {
  eng <- get_lang_data("English (American)", "Armon-Lotem_Hebrew_English_Bilingual",
                       "Hebrew", "eng_heb", "armon-lotem", db_args)
  heb <- get_lang_data("Hebrew", "Armon-Lotem_Hebrew_English_Bilingual",
                       "Hebrew", "eng_heb", "armon-lotem", db_args)
  rbind(eng, heb)
}

fetch_eng_fra_mitchell_admins <- function() {
  tidy_lang_data <- function(language, path) {
    items <- get_item_data(language = language, form = "WS") |>
      filter(item_kind == "word") |>
      left_join(cat_to_lc, by = "category") |>
      select(item_id, item_definition, uni_lemma, category, lexical_class) |>
      distinct()
    data <- read_csv(path) |>
      pivot_longer(cols = starts_with("item_"),
                   names_to = "item_id",
                   values_to = "value") |>
      left_join(items, by = "item_id") |>
      mutate(data_id = subject_id * 100 + n_months,
             value = as.logical(value),
             language = language,
             form = "WS") |>
      select(data_id, item_id, item_definition, uni_lemma,
             category, lexical_class,
             value, child_id = subject_id, language, form, age)
    data
  }

  eng <- tidy_lang_data("English (American)",
                        here("data", "cdi", "public_clean_cdi_eng.csv"))
  fra <- tidy_lang_data("French (Quebecois)",
                        here("data", "cdi", "public_clean_cdi_fr.csv"))

  # match exposure values by closest month (as it was not recorded at every point)
  exp_repair <- function(df) {
    df <- df |> mutate(exp_na = is.na(exposure_eng))
    for (m in df |> filter(exp_na) |> pull(n_months)) {
      dt_temp <- df |> filter(!exp_na) |>
        mutate(m_diff = abs(n_months - m))
      min_m_diff <- dt_temp |>
        pull(m_diff) |>
        min()
      df[which(df$n_months == m), ]$exposure_eng <-
        df[min(dt_temp[which(dt_temp$m_diff == min_m_diff), ]$n_months), ]$exposure_eng
      df[which(df$n_months == m), ]$exposure_fr <-
        df[min(dt_temp[which(dt_temp$m_diff == min_m_diff), ]$n_months), ]$exposure_fr
    }
    df
  }

  demog <- read_csv(here("data", "cdi", "public_clean_demog.csv")) |>
    mutate(data_id = subject_id * 100 + n_months) |>
    select(data_id, subject_id, n_months, exposure_eng, exposure_fr) |>
    nest(data = -subject_id) |>
    mutate(data = lapply(data, exp_repair)) |>
    unnest(data) |>
    select(data_id, eng_prop = exposure_eng, oth_prop = exposure_fr)

  rbind(eng, fra) |>
    left_join(demog, by = "data_id") |>
    mutate(lang_grp = "eng_fra",
           data_src = "mitchell")
}