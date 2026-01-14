#' Plot ANCOM-BC2 results
#'
#' @param data ANCOM-BC2 result list
#' @param var variable name
#' @param title plot title
#' @param col_sig color for significant taxa
#' @param non_sig color for non-significant taxa
#' @param method analysis method
#'
#' @return ggplot object
#' @export
### Results for numerical variables --------------------------------------------

lfc_num <- function(data_prim, variable, sig_color = "#0FA060", nonsig_color = "black",
                    title_prefix = "Log fold changes as one unit increase of") {

  # Dynamic variable names
  lfc_col         <- paste0("lfc_", variable)
  diff_col        <- paste0("diff_", variable)
  se_col          <- paste0("se_", variable)
  robust_col      <- paste0("diff_robust_", variable)

  groups <- get_utils(data_prim, prefix="^diff_", var=variable)

  # Filtering and sorting
  df_fig <- data_prim %>%
    dplyr::select(all_of(c('taxon', lfc_col, diff_col, se_col, robust_col))) %>%
    dplyr::filter(.data[[diff_col]] == 1) %>%
    dplyr::arrange(desc(.data[[lfc_col]])) %>%
    dplyr::mutate(
      direct = ifelse(.data[[lfc_col]] > 0, "Positive LFC", "Negative LFC"),
      color  = ifelse(.data[[robust_col]], sig_color, nonsig_color)
    )

  # factor level
  df_fig$taxon <- factor(df_fig$taxon, levels = df_fig$taxon)
  df_fig$direct <- factor(df_fig$direct, levels = c("Positive LFC", "Negative LFC"))

  # Visualization
  fig <- df_fig %>% ggplot(aes(x = taxon, y = .data[[lfc_col]], fill = direct)) +
    geom_bar(stat = "identity", width = 0.7, color = "black",
             position = position_dodge(width = 0.4)) +
    geom_errorbar(aes(ymin = .data[[lfc_col]] - .data[[se_col]],
                      ymax = .data[[lfc_col]] + .data[[se_col]]),
                  width = 0.2, color = "black",
                  position = position_dodge(0.05)) +
    labs(x = NULL, y = "Log fold change",
         title = paste(title_prefix, variable)) +
    scale_fill_discrete(name = NULL) +
    scale_color_discrete(name = NULL) +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5),
          panel.grid.minor.y = element_blank(),
          axis.text.x = element_text(angle = 60, hjust = 1,
                                     color = df_fig$color))

  return(fig)
}

### Results for categorical variables ------------------------------------------
# shared between primary, dunnett, and multiple pairwise

lfc_cat <- function(data_else, variable, title = NULL,
                    sig_color = "#0FA060", nonsig_color = "black") {

  util_var <- get_utils(data_else, prefix="^diff_", var=variable)
  diff_cols <- util_var$diff_cols
  groups <- util_var$groups

  # Data filtering and sorting
  df_cat <- data_else %>%
    dplyr::select(taxon, contains(variable)) %>%
    filter(if_any(all_of(diff_cols), ~ .x == 1))

  # -------- value --------
  df_value <- lapply(groups, function(g) {
    df_cat %>%
      transmute(taxon, group = g,
                value = ifelse(
                  .data[[paste0("diff_", variable, g)]] == 1,
                  round(.data[[paste0("lfc_", variable, g)]], 2), 0))
  }) %>% bind_rows()

  # -------- color --------
  df_color <- lapply(groups, function(g) {
    df_cat %>%
      transmute(taxon, group = g,
                color = ifelse(
                  .data[[paste0("diff_robust_", variable, g)]],
                  sig_color, nonsig_color))
  }) %>% bind_rows()

  # -------- merge --------
  df_fig <- df_value %>%
    left_join(df_color, by = c("taxon", "group"))

  df_fig$group <- factor(df_fig$group, levels = groups)

  return(df_fig)
}


### Results for global test ----------------------------------------------------

lfc_global <- function(data_prim, data_glob, variable, title = NULL,
                       sig_color = "#0FA060", nonsig_color = "black") {

  util_var <- get_utils(data_prim, prefix="^diff_", var=variable)
  diff_cols <- util_var$diff_cols
  groups <- util_var$groups

  # Data filtering and sorting

  df_global <- data_prim %>%
    dplyr::select(taxon, contains(variable)) %>%
    dplyr::left_join(data_glob %>%
                       dplyr::select(taxon, diff_abn, diff_robust_abn)) %>%
    dplyr::filter(diff_abn == 1)

  df_fig <- lapply(groups, function(g) {
    df_global %>%
      transmute( taxon, group = g,
                 value = round(.data[[paste0("lfc_", variable, g)]], 2),
                 color = ifelse(diff_robust_abn, sig_color, nonsig_color))
  }) %>% bind_rows() %>% dplyr::arrange(taxon)

  df_fig$group <- factor(df_fig$group, levels = groups)

  return(df_fig)
}


### Pattern Analysis -----------------------------------------------------------

lfc_trend <- function(data_trend, variable, title = title,
                      sig_color = "#0FA060", nonsig_color = "black") {

  util_var <- get_utils(data_trend, prefix="^lfc_", var=variable)
  groups <- util_var$groups

  # Data filtering and sorting
  df_cat <- data_trend %>% dplyr::filter(diff_abn==1) %>%
    dplyr::mutate(color = ifelse(diff_robust_abn, sig_color, nonsig_color))
  df_fig <- lapply(groups, function(g) {
    df_cat %>% transmute(taxon, group=g, color,
                         value = round(.data[[paste0("lfc_", variable, g)]], 2))
  }) %>% bind_rows() %>% dplyr::arrange(taxon)
  df_fig$group <- factor(df_fig$group, levels = groups)

  return (df_fig)
}


### Visualization --------------------------------------------------------------

get_plot <- function(df_fig, method, title=title, text_size=4){
  max.abs = ceiling(max(abs(df_fig$value)))
  fig.tmp = df_fig %>%
    ggplot(aes(x = group, y = taxon, fill = value)) +
    geom_tile(color = "black") +
    scale_fill_gradient2(low = "blue", high = "red", mid = "white",
                         midpoint = 0, limit = c(-max.abs, max.abs),
                         na.value = "white", name = NULL) +
    labs(x = NULL, y = NULL, title = title)

  if (method %in% c("global", "trend")){
    ## Global & Pattern
    fig = fig.tmp +
      geom_text(aes(group, taxon,
                    label = ifelse(value == 0, "", value)),
                color = "black", size = text_size) +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5),
            axis.text.y = element_text(
              color = df_fig %>% distinct(taxon, color) %>% .$color))
  } else {
    ## Primary & Dunnett & Pairwise
    fig = fig.tmp +
      geom_text(aes(group, taxon, color = color,
                    label = ifelse(value == 0, "", value)),
                size = text_size) +
      scale_color_identity(guide = FALSE) +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5))
  }

  return(fig)
}


### Integrated Function --------------------------------------------------------

abc2_plot <- function(data, var=NULL, title="", col_sig="#0FA060", non_sig="black",
                      method=c("primary", "dunnett", "global", "pairwise", "trend")){

  if (!missing(method)) {
    method <- tolower(method) %>% ifelse(.=='pattern', 'trend', .)
  }

  method <- match.arg(method)
  var_num = grepl(paste0(var, "$"), colnames(data$res)) %>% sum() > 0

  if (var_num) {
    fig <- lfc_num(data$res, var, sig_color=col_sig, nonsig_color=non_sig)
  } else {
    fig <- switch(
      method,
      primary  = lfc_cat(data$res, var, title, col_sig, non_sig),
      dunnett  = lfc_cat(data$res_dunn, var, title, col_sig, non_sig),
      pairwise = lfc_cat(data$res_pair, var, title, col_sig, non_sig),
      global   = lfc_global(data$res, data$res_global, var, title, col_sig, non_sig),
      trend    = lfc_trend(data$res_trend, var, title, col_sig, non_sig)
    ) %>% get_plot(., method, title=title)
  }

  return(fig)
}


