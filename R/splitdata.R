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
        str_split(pattern = "=", simplify = TRUE) %>%
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
#' @export
ExtractINFO <- function (target_table) {
    info_table <- target_table$INFO %>%
        as.list() %>%
        map(ConvertToTibble)
    info_table <- suppressMessages(join_all(info_table, type="full"))
    target_table %>%
        cbind(info_table) %>%
        as_tibble()  %>%
        return()
}