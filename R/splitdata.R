ADToRatio <- function(strAD) {
    vec_ad <- strAD %>%
        str_split(",") %>%
        unlist() %>%
        as.numeric()
    return(vec_ad[2] / sum(vec_ad))
}

#' Return heteroplasmy from value of AD.
#' @param strAD Value of AD as string.
#' @return Heteroplasmy as double.
#' @importFrom dplyr %>%
#' @importFrom stringr str_split
#' @importFrom purrr map
#' @importFrom tidyr as_tibble
#' @export
ADToRatio <- Vectorize(ADToRatio)

SplitEqual <- function(string) {
    string %>%
        str_split(pattern = "=", n = 2, simplify = TRUE) %>%
        return()
}

ConvertToTibble <- function(string) {
    convertedDataFrame <- string %>%
        str_split(pattern = ";", simplify = TRUE) %>%
        apply(2, SplitEqual) %>%
        as.data.frame()
    colnames(convertedDataFrame) <- convertedDataFrame[1, ]
    return(convertedDataFrame[-1,])
}

#' Extract each values from INFO section.
#' @param target_table table from VCF file.
#' @importFrom tibble as_tibble_col
#' @importFrom tibble rownames_to_column
#' @importFrom dplyr full_join
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr arrange
#' @importFrom stringr str_c
#' @importFrom stringr str_remove
#' @export
ExtractINFO <- function (target_table) {
    info_table <- target_table$INFO %>%
        as_tibble_col(column_name = "str") %>%
        rownames_to_column(var = "ID")
    info_table <- info_table %>%
        mutate(retval = str_c("ID=", info_table$ID, ";",
                              info_table$str, sep = ""))
    info_table <- info_table$retval %>%
        as.list() %>%
        map(ConvertToTibble)
    tmpTable <- info_table[[1]]
    for (i in 2:length(info_table)) {
        tmpTable <- suppressMessages(full_join(tmpTable, info_table[[i]]))
    }
    tmpTable <- tmpTable %>%
        arrange("ID") %>%
        select(-"ID")
    target_table %>%
        cbind(tmpTable) %>%
        as_tibble()  %>%
        return()
}

#' read VCF file as tibble
#' @param VCFFile VCF file path
#' @return Tibble object
#' @importFrom stringr str_remove_all
#' @importFrom readr read_tsv
#' @importFrom dplyr filter
#' @importFrom stringr str_starts
#' @export
VCFToTibble <- function(VCFFile) {
    retTibble <- read_tsv(VCFFile, comment = '##')
    colnames(retTibble) <- colnames(retTibble) %>%
        str_remove_all(pattern = '#')
    return(retTibble)
}

SearchSingleRow <- function(targetStr, query) {
    queryPattern <- str_c(query, "=")
    targetStr %>%
        str_split(pattern = ";", simplify = T) %>%
        t() %>%
        as_tibble_col(column_name = "searchSingleRowInner") %>%
        filter(str_starts(`searchSingleRowInner`, pattern = queryPattern)) %>%
        as.character() %>%
        str_remove(".*=") %>%
        return()
}

#' Extract the specified element from INFO section.
#' @param targetTibble Target table as tibble.
#' @param query String. the element that you want to extract.
#' @param colName Name of column that is being added.
#' @export
ExtractFromINFO <- function(targetTibble, query, colName = query) {
    result <- targetTibble$INFO %>%
        sapply(SearchSingleRow, query = query) %>%
        as.vector() %>%
        as_tibble_col(column_name = colName)
    targetTibble %>%
        cbind(result) %>%
        as_tibble() %>%
        return()
}