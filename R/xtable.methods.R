#' Methods for Function xtable in Package `annotationTable'
#' 
#' \code{xtable} methods for several a4 objects, such as \code{annotationTable} objects, 
#' \code{topTable} objects etc.
#' @section Methods:
#' \describe{
#' \item{x = "annotationTable", caption = "missing", label = "missing", align = "missing", digits = "missing", display = "missing"}{generates a LaTeX representation for the given annotationTable}
#' \item{x = "annotationTable", caption = "ANY", label = "ANY", align = "ANY", digits = "ANY", display = "ANY"}{generates a LaTeX representation for the given annotationTable}
#' \item{x = "annotationTable", caption = "ANY", label = "ANY", align = "ANY", digits = "numeric", display = "ANY"}{generates a LaTeX representation for the given annotationTable}
#' }
#' @examples 
#'  ## some dummy data
#'   dData <- data.frame(someSymbol = LETTERS[1:5], accessionNumber = c("X83928", "V00540", "U21090", "L38487", "M34057"))
#'  at <- annotationTable(displayData = dData,  displayCols = list(accessionNumber = "EntrezId"))
#'  xat <- xtable(at)
#'  print(xat, include.rownames = FALSE)
#' @keywords methods manip
#' @name xtable-methods
#' @docType methods
NULL

#' @export
#' @method xtable topTableGlmnet
#' @rdname xtable-methods
xtable.topTableGlmnet <- function(x, caption = NULL, label = NULL, align = NULL, 
    digits = NULL, display = NULL, ...){
  
  xtable:::xtable.data.frame(x$topList, caption = caption, label = label, align = align,
      digits = digits, display = display, ...)
}

#' @export
#' @import xtable
#' @method xtable topTableLognet
#' @rdname xtable-methods
xtable.topTableLognet <- function(x, caption = NULL, label = NULL, align = NULL, 
    digits = NULL, display = NULL, ...){
  
  xtable:::xtable.data.frame(x$topList, caption = caption, label = label, align = align,
      digits = digits, display = display, ...)
}

#' @export
#' @import xtable
#' @method xtable topTableElnet
#' @rdname xtable-methods
xtable.topTableElnet <- function(x, caption = NULL, label = NULL, align = NULL, 
    digits = NULL, display = NULL, ...){
  
  xtable:::xtable.data.frame(x$topList, caption = caption, label = label, align = align,
      digits = digits, display = display, ...)
}

#' @export
#' @import xtable
#' @method xtable pamClassConfusionTable
#' @rdname xtable-methods
xtable.pamClassConfusionTable <- function(x, caption = NULL, label = NULL, align = NULL, 
    digits = NULL, display = NULL, ...){
  
  # TODO: some preprocessing
  xtable:::xtable.matrix(x, caption = caption, label = label, align = align,
      digits = digits, display = display, ...)
}

#' @export
#' @importFrom xtable xtable
#' @method xtable topTablePam
#' @rdname xtable-methods
xtable.topTablePam <- function(x, ...){
  xtable(x$topList, ...)
}

#' @export
#' @importFrom xtable xtable
#' @method xtable topTableRfClass
#' @rdname xtable-methods
xtable.topTableRfClass <- function(x, caption = NULL, label = NULL, align = NULL, digits = NULL, 
    display = NULL, ...){
  xtable(x$topList, ...)
}

