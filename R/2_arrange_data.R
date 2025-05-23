#' Arrange Data based on Block and Trial
#'
#' @param data [data.frame] A data frame resulting from the 'step1' process of the `unique_choice` function.
#' 
#' @param time_line [vector] A vector specifying the name of the column that 
#'  the sequence of the experiment. This argument defines how the experiment is 
#'  structured, such as whether it is organized by "Block" with breaks in 
#'  between, and multiple trials within each block. 
#'  e.g., `time_line = c("Block", "Trial")`
#'
#' @returns data frame:
#'   \itemize{
#'     \item{\code{data}: step1 arranged by 'time_line'.}
#'   }
#'
#' @noRd
#' 
arrange_data <- function(data, time_line = c("Block", "Trial")){
  # 基于time_line这个向量, 录入排序向量
  order_vector <- lapply(time_line, function(col) data[[col]])
  
  # 基于排序向量对输入数据集进行排序
  data <- data[do.call(order, order_vector), ]
  
  return(data)
}
