##  Declaration of global variables.

my_globalVariables <- c(
    "folder_defaults", # LG_bookkeeping
    "dir_bookmark", # LG_bookkeeping
    "dmt", # TS_key_dmt_bivariate
    "info", # LG_bookkeeping
    "info_path", # LG_bookkeeping
    "value", # LG_create_plot_df
    "omega", # LG_create_plot_df
    "orig", # LG_create_plot_df
    ".tmp", # LG_lookup_details_NC_fail
    "x", # LG_plot_complex
    "y", # LG_plot_complex
    "bw_points", # LG_plot_heatmap
    "value", # LG_plot_heatmap
    "omega", # LG_plot_heatmap
    "m", # LG_plot_heatmap
    "info", # LG_sanity_checks
    "save_dir" # TS_acr
)

utils::globalVariables(my_globalVariables)
rm(my_globalVariables)
