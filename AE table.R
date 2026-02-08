install.packages("tern")

library(pharmaverseadam)
library(tern)
library(dplyr)


adsl <- adsl %>%
  df_explicit_na()

adae <- adae %>%
  df_explicit_na()

adae <- adae %>%
  var_relabel(
    AESOC = "Primary System Organ Class",
    AEDECOD = "Reported Term for the Adverse Event"
  ) %>%
  filter(TRTEMFL == "Y")

# Define the split function
split_fun <- drop_split_levels
# ... (previous code remains unchanged)

lyt <- basic_table(show_colcounts = TRUE) %>%
  split_cols_by(var = "ACTARM") %>%
  #add_overall_col(label = "All Patients") %>%
  analyze_num_patients(
    vars = "USUBJID",
    .stats = "unique", # MODIFIED: Only 'unique' statistic is kept
    .labels = "Treatment Emergent AEs" # MODIFIED: Only the label for 'unique' is kept
  ) %>%
  split_rows_by(
    "AEBODSYS",
    child_labels = "visible",
    nested = FALSE,
    split_fun = split_fun,
    label_pos = "topleft",
    split_label = obj_label(adae$AESOC)
  ) %>%
  summarize_num_patients(
    var = "USUBJID",
    .stats = "unique",
    .labels = c(
      unique = "CARDIAC DISORDER"
    )
  ) %>%
  count_occurrences(
    vars = "AEDECOD",
    .indent_mods = -1L
  ) %>%
  append_varlabels(adae, "AEDECOD", indent = 1L)

result <- build_table(lyt, df = adae, alt_counts_df = adsl)

result
