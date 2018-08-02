#' @title regulonDB
#'
#' @description
#' @param
#' @examples
#' @export
#'


listDatasets <- function(x){
  dbListTables(x)
}

#' @export
listAttributes <- function(database, dataset) {
  tryCatch(
    {
      regulonAttributes = DBI::dbListFields(database, dataset)
      regulonAttributes
    },
    error=function(cond) {
      message("Error: Database or dataset does not exist")
    }
  )
}

#' @export
GetRegulatedGenesByTF <- function(x,tf){
  genes_regulon <- tbl(x, "GENE")
  tf_table <- genes_regulon %>%
    select(name, gene_tf)
  result <- tf_table %>% filter( dplyr::sql( paste0("gene_tf LIKE '%",tf,"%' ") )) %>% select("name")
  tibble_result <- collect(select(result, name))[,1]
  as.matrix(tibble_result)

}




