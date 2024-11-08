#' format summary statistics
#' need factors as factors, numeric as numerics
#' @requires tidyverse, here
#' Warning: the table need to be formatted properly before
#' eg. df <- df %>% mutate_at(.vars = c("Year.Isolation", "Avg.Scaffold.coverage", "Burst.size"), as.numeric) 
#' eg. or as.factor or as.character, as.double, as.integer
#'  @param df - must be formatted properly, factors, numeric, character
#'  @param maxsum - number of factors to summarize 
#'  @param table_out - name.tsv for table output
#'  @param graph_out - name.png for output table formated as png (eg. for import in presentation)

format_summary_statistics_fun <- function(df,
                                          maxsum = 20,
                                          table_out = here::here("results", "summary.tsv")) {
  df %>%
    base::summary(., maxsum = maxsum) -> summary_object

  new_df <-
    base::as.data.frame.matrix(summary_object, row.names = NULL) %>%
    # remove NAs and replace by nothing - to make it pretty
    dplyr::mutate_all(~dplyr::coalesce(., "")) %>%
    `rownames<-`(NULL)

  # export the table as tsv
  write.csv2(new_df, table_out, col.names = T, row.names = F, quote = F)

  # return table for new manipulation
  return(new_df)

}
