#' Return heteroplasmy from value of AD.
#' @param strAD Value of AD as string.
#' @return Heteroplasmy as double.
#' @importFrom dplyr %>%
#' @importFrom stringr str_split
#' @importFrom purrr map
#' @importFrom plyr join_all
#' @importFrom tidyr as_tibble
#' @export
ADToRatio <- function(strAD) {
    vec_ad <- str_ad %>%
        str_split(",") %>%
        unlist() %>%
        as.numeric()
    return(vec_ad[2] / sum(vec_ad))
}

ADToRatio <- Vectorize(ADToRatio)

ConvertToTibble <- function(string) {
    convertedDataFrame <- string %>% 
                            str_split(pattern = ";") %>%
                            sapply(FUN = str_split, pattern = "=") %>%
                            sapply(unlist) %>%
                            as.data.frame()
    colnames(convertedDataFrame) <- convertedDataFrame[1, ]
    return(convertedDataFrame[-1,])
}
vec_ConvertToTibble <- Vectorize(ConvertToTibble)

ExtractINFO <- function (target_table) {
    info_table <- target_table$INFO %>%
        as.list() %>%
        map(ConvertToTibble) %>%
        join_all(type="full")
    target_table %>%
        cbind(info_table) %>%
        as_tibble()  %>%
        return()
}