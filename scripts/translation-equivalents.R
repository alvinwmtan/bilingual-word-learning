get_trans_equiv <- function(df) {
  # get TEs for a particular unilemma
  df$te_known <- sapply(1:nrow(df), \(r) {
    cur_row <- df[r,]
    opp_row <- which(df$language != cur_row$language &
                       abs(df$age - cur_row$age) <= 1)
    opp_val <- df[opp_row,3]
    return(if(nrow(opp_val) == 0) NA else max(opp_val))
  })
  df$is_doublet <- sapply(1:nrow(df), \(r) {
    cur_row <- df[r,]
    cur_val <- cur_row$value
    opp_row <- which(df$language != cur_row$language &
                       abs(df$age - cur_row$age) <= 1)
    opp_val <- df[opp_row,3]
    return(if(nrow(opp_val) == 0) NA else as.numeric(max(opp_val) & cur_val))
  })
  return(df)
}

add_trans_equiv <- function(df) {
  # add TEs for all unilemmas in a df
  # note: takes a while to run
  te <- df |>
    select(c(child_id, language, age, uni_lemma, value)) |>
    nest(val = c(language, age, value)) |>
    {\(x) mutate(x, val = lapply(x$val, get_trans_equiv))}() |>
    unnest(val) |>
    distinct()
  df |> left_join(te, by = c("child_id", "language", "age", "uni_lemma", "value"))
}

lang_map <- read_csv("data/misc/language_map.csv")

convert_lang_espeak <- function(lang) {
  lang_map |> filter(wordbank == lang) |> pull(`espeak-ng`)
}

clean_words <- function(word_set){
  word_set %>%
    # dog / doggo -> c("dog", "doggo")
    strsplit("/") %>% flatten_chr() %>%
    # dog (animal) | (a) dog
    strsplit(" \\(.*\\)|\\(.*\\) ") %>% flatten_chr() %>%
    # dog* | dog? | dog! | ¡dog! | dog's
    gsub("[*?!¡']", "", .) %>%
    # dog(go) | (a)dog
    map_if(
      # if "dog(go)"
      ~grepl("\\(.*\\)", .x),
      # replace with "dog" and "doggo"
      ~c(sub("\\(.*\\)", "", .x),
         sub("(.*)\\((.*)\\)", "\\1\\2", .x))
    ) %>%
    flatten_chr() %>%
    # trim
    gsub("^ +| +$", "", .) %>%
    keep(nchar(.) > 0) %>%
    tolower() %>%
    `[`(1)
}

get_ipa <- function(word, lang) {
  # get ipa from eSpeak(-NG), with minor cleaning to decrease granularity of
  # the en-us output
  lang_code <- convert_lang_espeak(lang)
  word <- clean_words(word)
  system2("espeak",
          args = c("--ipa=3", "-v", lang_code, "-q", paste0('"', word, '"')),
          stdout = TRUE) |>
    {\(x) gsub("[ˈˌ ː[:punct:]]", "", x)}() |>
    {\(x) gsub("ɚ", "əɹ", x)}() |>
    {\(x) gsub("ɫ", "l", x)}() |>
    {\(x) gsub("ɾ", "t", x)}() |>
    {\(x) gsub("ᵻ", "ɪ", x)}()
}

get_phons <- function(words, lang) {
  # get phonology for a list of words
  words |> map_chr(function(word) get_ipa(word, lang))
}

get_spec_ipa <- function(lang) {
  # get IPA for a whole form spec
  spec <- get_item_data(lang, "WS")
  spec |>
    filter(item_kind == "word") |>
    mutate(ipa = get_phons(item_definition, lang),
           length_phon = sapply(ipa, str_length)) |>
    select(language, item_id, category,
           item_definition, uni_lemma, ipa, length_phon)
}

get_overlap <- function(df) {
  # get phonological similarity
  if(nrow(df) < 2) {
    df$overlap <- NA
  } else {
    df$overlap <- sapply(1:nrow(df), \(r) {
      cur_row <- df[r,]
      cur_val <- cur_row$ipa
      opp_row <- which(df$language != cur_row$language)
      opp_val <- df[opp_row,3] |> flatten()
      return(if(length(opp_val) == 0) NA else
        max(1 - (adist(cur_val, opp_val) /
                   pmax(str_length(cur_val), str_length(opp_val)))))
    })
  }
  return(df)
}

get_phon_sim <- function(lang1, lang2) {
  lang1_spec <- get_spec_ipa(lang1)
  lang2_spec <- get_spec_ipa(lang2)

  ipa <- rbind(lang1_spec, lang2_spec) |>
    nest(val = -c(category, uni_lemma))

  overlap <- lapply(ipa$val, get_overlap)

  ipa |>
    mutate(val = overlap) |>
    unnest(val)
}
