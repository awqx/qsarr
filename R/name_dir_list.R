name_dir_list <- function(df_list, df_dir) {
  df_name <- str_remove(
    list.files(df_dir),
    "\\.[[:alpha:]]+$"
  )
  names(df_list) <- df_name
  df_list
}
