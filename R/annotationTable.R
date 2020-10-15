# first stab at providing xtable methods for dataframes with hyperlinks
#                          for inclusion in Sweave reports
# Author: Tobias Verbeke, 2008-05-19
###############################################################################

#' Class 'annotationTable'
#' 
#' Class to represent both displayed information and hyperlink information
#' to prepare tabular output for LaTeX (with hyperlinks)
#' @section Objects from the Class:
#' Objects can be created by calls of the form \code{new("annotationTable", ...)}
#' or using the wrapper function \code{annotationTable}
#' @section Slots:
#' 	 \describe{
#' 	 \item{\code{displayData}:}{Object of class \code{"data.frame"} containing the columns to be
#' 	  displayed in the table}
#' 	 \item{\code{displayCols}:}{Object of class \code{"list"} giving key-value pairs that allow
#' 	 to automatically generate the hyperlinks for the corresponding columns of the \code{displayData}}
#' 	 \item{\code{hrefData}:}{Object of class \code{"data.frame"} giving the hyperlink information for
#' 	 the corresponding columns of the \code{displayData}}
#' }
#' @section Methods:
#'   \describe{
#'   \item{show}{\code{signature(object = "annotationTable")}: print an annotationTable 
#'   (without displaying the hyperlink information)}
#' }
#' @author Tobias Verbeke
#' @examples showClass("annotationTable")
#' @keywords classes
#' @exportClass annotationTable
setClass("annotationTable",
    representation = representation(displayData = "data.frame",
        displayCols = "list",
        hrefData = "data.frame"),
    prototype = list(displayData = data.frame(character()),
        displayCols = list(),
        hrefData = data.frame(character())))

### validity
.annotationTable.valid <- function(object){
  
  dimD <- dim(object@displayData)
  dimH <- dim(object@hrefData)
  
  if (!all.equal(dimD, dimH)) {
    warning("The displayData and hrefData should have the same dimensions")
    return(FALSE)
  } else {
    return(TRUE)
  }
}

setValidity("annotationTable", .annotationTable.valid)

##############################
### annotationTable object ###
##############################


# utility functions for automatic generation of hrefData

#' Transform an ENTREZ ID into a hyperlink
#' 
#' Utility function to transform an ENTREZ ID into a hyperlink on 
#' the NCBI Entrez page for the given gene
#' @param x vector of Entrez IDs
#' @return vector of hyperlinks for the corresponding Entrez IDs
#' @author Tobias Verbeke
#' @note Snippet taken from the \code{annaffy} package
#' @seealso \code{\link{generateGOIdLinks}}
#' @keywords manip
#' @examples 
#' generateEntrezIdLinks(c("5230", "18655"))
#' @export
generateEntrezIdLinks <- function(x){
  # code from annaffy package
  url <- "http://www.ncbi.nlm.nih.gov/gene/"
  if (!length(x))
    return(character(0))
  return(paste(url, x, sep = ""))
}

#' Transform a GO ID into a hyperlink
#' 
#' Utility function to transform a GO ID into a hyperlink
#' to the corresponding page on the gene ontology website
#' @param x vector of GO IDs
#' @return vector of hyperlinks
#' @author Tobias Verbeke
#' @note Snippet taken from the \code{annaffy} package
#' @seealso \code{\link{generateEntrezIdLinks}}
#' @keywords manip
#' @export
generateGOIdLinks <- function(x){
  # code from annaffy package
  url <- "http://amigo.geneontology.org/amigo/term/"
  
  if (!length(x))
    return(character(0))
  url <- paste(url, x, sep = "")
  for(i in 2:length(x))
    url <- paste(url, x, sep = "%0a")
  return(url)
}

#' Function to Create an annotationTable
#' 
#' This function takes data to be displayed as well as 
#' data containing hyperlinks corresponding to displayed
#'  data and constructs an object of class annotationTable
#' @param displayData data frame containing data that is meant to be displayed
#' in a LaTeX table
#' @param displayCols list of named character vectors (of length one) that function
#' as key-value pairs; the names (keys) correspond to columns for which
#' the hyperlinks should be generated whereas the strings (values)
#' indicate what kind of link should be produced based on the
#' corresponding column in the \code{displayData}. The values should be
#' one of \code{"EntrezId"} or \code{"GOId"}.
#' @param hrefData data frame containing hyperlink information for the columns of
#' the same name in the \code{displayData} data frame
#' @details 
#' If \code{hrefData} is given, the \code{displayCols} are not taken into account.
#' If no \code{hrefData} is given, the information in \code{displayCols} allows to 
#' automatically create the \code{hrefData}.
#' @return object of class 'annotationTable'
#' @author Tobias Verbeke
#' @examples 
#' ## some dummy data
#' dData <- data.frame(someSymbol = LETTERS[1:5], 
#'  accessionNumber = c("X83928", "V00540", "U21090", "L38487", "M34057"))
#' at <- annotationTable(displayData = dData, 
#'  displayCols = list(accessionNumber = "EntrezId"))
#' @keywords manip
#' @importFrom methods new
#' @export
annotationTable <- function(displayData, 
    displayCols = NULL, 
    hrefData = NULL){

  if (!is.null(displayCols)){
    if (any(!(unlist(displayCols) %in% c("EntrezId", "GOId")))) 
      stop("'displayCols' should be a named list of strings 'EntrezId' or 'GOId'")
  }
  if (is.null(hrefData) & length(displayCols)){
    # create and populate hrefData
    hrefData <- as.data.frame(matrix("", nrow = nrow(displayData), 
            ncol = ncol(displayData)))
    names(hrefData) <- names(displayData)
    for (iCol in seq(along = displayCols)){
      
      iLinkCol <- which(names(displayData) %in% names(displayCols)[iCol])
      iLinkColType <- displayCols[[iCol]]
      iLinkColValues <- switch(iLinkColType, 
          EntrezId = generateEntrezIdLinks(displayData[, iLinkCol]),
          GOId = generateGOIdLinks(displayData[, iLinkCol]))
      hrefData[, iLinkCol] <- iLinkColValues                           
    }
  } else {
    origHrefData <- hrefData
    hrefData <- as.data.frame(matrix("", nrow = nrow(displayData), 
            ncol = ncol(displayData)))
    names(hrefData) <- names(displayData)
    if (any(!(names(origHrefData) %in% names(hrefData)))) 
      stop("The column names of 'hrefData' should be all present in the column names of 'displayData'")
    hrefData[, names(origHrefData)] <- origHrefData
  }
  # create new annotationTable object
  res <- new("annotationTable", displayData = displayData, 
      hrefData = hrefData)
  return(res)
}                


### define a particular show method
#' @rdname annotationTable-class
#' @param object annotationTable object
setMethod("show", "annotationTable", function(object){
      cat("annotationTable with hyperlink annotation for columns:\n")
      hlinkedCols <- which(!sapply(object@hrefData, function(x) all(is.na(x))))
      cat(paste(names(object@displayData)[hlinkedCols], collapse = ", "), "\n\n")
      print(object@displayData)
    })

### TODO as.data.frame [strip off all annotation information]                

#########################
### xtable formatting ###
#########################

# turn S3 generic to S4
#' @exportMethod xtable
setGeneric(name = "xtable")

annotationTableSanitize <- function(str) {
  result <- str
  # only sanitize part that will be displayed i.e. content of second
  # curly braces \href{}{}
  isHref <- length(grep("^\\\\href\\{.*\\}\\{.*\\}$", result))
  if (isHref) displayPart <- gsub("(^\\\\href\\{(.*)\\}\\{).*\\}", "\\1", result)
  result <- gsub("^\\\\href\\{(.*)\\}\\{(.*)\\}", "\\2", result)
  result <- gsub("\\\\","SANITIZE.BACKSLASH",result)
  result <- gsub("$","\\$",result,fixed=TRUE)
  result <- gsub(">","$>$",result,fixed=TRUE)
  result <- gsub("<","$<$",result,fixed=TRUE)
  result <- gsub("|","$|$",result,fixed=TRUE)
  result <- gsub("{","\\{",result,fixed=TRUE)
  result <- gsub("}","\\}",result,fixed=TRUE)
  result <- gsub("%","\\%",result,fixed=TRUE)
  result <- gsub("&","\\&",result,fixed=TRUE)
  result <- gsub("_","\\_",result,fixed=TRUE)
  result <- gsub("#","\\#",result,fixed=TRUE)
  result <- gsub("^","\\verb|^|",result,fixed=TRUE)
  result <- gsub("~","\\~{}",result,fixed=TRUE)
  result <- gsub("SANITIZE.BACKSLASH","\\",result,fixed=TRUE)
  if (isHref) result <- paste(displayPart, result, "}", sep = "")
  return(result)
}

# TODO: add methods for other signatures

#' @rdname xtable-methods
#' @importFrom xtable xtable
#' @param x annotationTable object
#' @inheritParams xtable::xtable
setMethod("xtable",
  signature(x="annotationTable", caption = "missing", label = "missing", align = "missing",
      digits = "missing", display = "missing"), # for an object of class ...
  function(x, caption, label, align, digits, display){
    # strategy: construct new data frame with hyperlinks
    # and use classical xtable on this one
    d <- x@displayData
    h <- x@hrefData
    at <- character(prod(dim(d))) # annotationTable
    dim(at) <- dim(d)
    colnames(at) <- names(d)
    numericCols <- which(sapply(d, is.numeric))
    
    for (iCol in seq(ncol(d))){
      
      d[[iCol]] <- as.character(d[[iCol]])
      h[[iCol]] <- as.character(h[[iCol]])
      
      if (all(h[[iCol]] == "") | all(is.na(h[[iCol]]))){ # no hyperlinks
        at[, iCol] <- d[[iCol]]    # display data only, not transformed into character
      } else {
        tm <- matrix(c(h[[iCol]], d[[iCol]]), ncol = 2)
        at[, iCol] <- paste("\\", "href{", 
            apply(tm, 1, paste, collapse = "}{"), "}", sep = "")
      }
    }
    at <- as.data.frame(at, stringsAsFactors = FALSE)
    for (iCol in numericCols)
      at[, iCol] <- as.numeric(as.character(at[, iCol])) # quick and dirty
    # \href{http://www.wikibooks.org}{wikibooks home}
    xt <- xtable(at)
    class(xt) <- c("xtableAnnotationTable", class(xt))
    xt
})

#' @rdname xtable-methods
#' @importFrom xtable xtable
#' @param x annotationTable object
#' @inheritParams xtable::xtable
setMethod("xtable",
    signature(x="annotationTable", caption="ANY", label="ANY", 
        align="ANY", digits = "ANY", display = "ANY"), # for an object of class ...
    function(x, caption, label, align, digits, display){
      # strategy: construct new data frame with hyperlinks
      # and use classical xtable on this one
      d <- x@displayData
      h <- x@hrefData
      at <- character(prod(dim(d)))
      dim(at) <- dim(d)
      colnames(at) <- names(d)
      numericCols <- which(sapply(d, is.numeric))
      
      for (iCol in seq(ncol(d))){
        
        d[[iCol]] <- as.character(d[[iCol]])
        h[[iCol]] <- as.character(h[[iCol]])
        
        if (all(h[[iCol]] == "") | all(is.na(h[[iCol]]))){ # no hyperlinks
          at[, iCol] <- d[[iCol]]    # display data only, not transformed into character
        } else {
          tm <- matrix(c(h[[iCol]], d[[iCol]]), ncol = 2)
          at[, iCol] <- paste("\\", "href{", 
              apply(tm, 1, paste, collapse = "}{"), "}", sep = "")
        }
      }
      at <- as.data.frame(at, stringsAsFactors = FALSE)
      for (iCol in numericCols)
        at[, iCol] <- as.numeric(as.character(at[, iCol])) # quick and dirty
      # \href{http://www.wikibooks.org}{wikibooks home}
      xt <- xtable(at, caption = caption,
          label = label, align = align, digits = digits, display = display)
      class(xt) <- c("xtableAnnotationTable", class(xt))
      xt
})

#' @rdname xtable-methods
#' @importFrom xtable xtable
#' @param x annotationTable object
#' @inheritParams xtable::xtable
setMethod("xtable",
    signature(x="annotationTable", caption="ANY", label="ANY", 
        align="ANY", digits = "numeric", display = "ANY"), # for an object of class ...
    function(x, caption, label, align, digits, display){
      # strategy: construct new data frame with hyperlinks
      # and use classical xtable on this one
      d <- x@displayData
      h <- x@hrefData
      at <- character(prod(dim(d)))
      dim(at) <- dim(d)
      colnames(at) <- names(d)
      numericCols <- which(sapply(d, is.numeric))
      
      for (iCol in seq(ncol(d))){
        
        d[[iCol]] <- as.character(d[[iCol]])
        h[[iCol]] <- as.character(h[[iCol]])
        
        if (all(h[[iCol]] == "") | all(is.na(h[[iCol]]))){ # no hyperlinks
          at[, iCol] <- d[[iCol]]    # display data only, not transformed into character
        } else {
          tm <- matrix(c(h[[iCol]], d[[iCol]]), ncol = 2)
          at[, iCol] <- paste("\\", "href{", 
              apply(tm, 1, paste, collapse = "}{"), "}", sep = "")
        }
      }
      at <- as.data.frame(at, stringsAsFactors = FALSE)
      for (iCol in numericCols)
        at[, iCol] <- as.numeric(as.character(at[, iCol])) # quick and dirty
      # \href{http://www.wikibooks.org}{wikibooks home}
      xt <- xtable(at, caption = caption,
          label = label, align = align, digits = digits, display = display)
      class(xt) <- c("xtableAnnotationTable", class(xt))
      xt
    })
  
#' Print method for 'xtableAnnotationTable' objects
#' @param x Object of class 'xtableAnnotationTable'
#' @param ... Further arguments passed to \code{print.xtable}
#' @return No returned value, the object is printed.
#' @details
#'  Wrapper to be able to use a specific \code{sanitize.text} function
#'  in the \code{print.xtable} call
#' @author Tobias Verbeke
#' @seealso \code{\link[xtable]{print.xtable}}
#' @keywords manip
#' @export
print.xtableAnnotationTable <- function(x, ...){
   # xtable:::print.xtable(x, sanitize.text = annotationTableSanitize, ...)
   NextMethod(x, sanitize.text = annotationTableSanitize, ...)
} 

    