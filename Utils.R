require(fs)

figures_path <- here("Figures")

# List all PNG files that end with FINAL
(final_pngs <- list.files(
  path = figures_path,
  pattern = "FINAL\\.png$",
  ignore.case = TRUE,
  full.names = FALSE
))

file_info <- file_info(final_pngs) %>%
  mutate(path = as.character(path)) %>%
  dplyr::select(path, modification_time) %>% 
  arrange(desc(modification_time))

