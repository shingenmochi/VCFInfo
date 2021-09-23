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
#' @importFrom plyr join_all
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
vec_ConvertToTibble <- Vectorize(ConvertToTibble)

#' Extract each values from INFO section.
#' @param target_table table from VCF file.
#' @importFrom tibble as_tibble_col
#' @importFrom tibble rownames_to_column
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom stringr str_c
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
    info_table <- suppressMessages(join_all(info_table, type="full")) %>%
        select(-"ID")
    target_table %>%
        cbind(info_table) %>%
        as_tibble()  %>%
        return()
}

#' read VCF file as tibble
#' @param VCFFile VCF file path
#' @return Tibble object
#' @importFrom stringr str_remove_all
#' @importFrom readr read_tsv
#' @export
VCFToTibble <- function(VCFFile) {
    retTibble <- read_tsv(VCFFile, comment = '##')
    colnames(retTibble) <- colnames(retTibble) %>%
        str_remove_all(pattern = '#')
    return(retTibble)
}
