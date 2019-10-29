#' LDAPObject
#'
#' @docType class
#' @export
#' @keywords ldap object
#' @return Object of \code{\link{R6Class}} representing a LDAP object.
#' @format \code{\link{R6Class}} object.
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(dn)}}{
#'    This method is used to instantiate a LDAP object from its distinguished name (DN).
#'  }
#'  \item{\code{parseDN(dn)}}{
#'    Parses a distinguished name into \code{list}
#'  }
#' }
#' 
#' @note abstract class
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
LDAPObject <- R6Class("LDAPObject",
  private = list(
    attrs = NULL
  ),
  public = list(
    initialize = function(dn){
      self$parseDN(dn)
    },
    
    #parseDN
    parseDN = function(dn){
      attrs <- unlist(strsplit(gsub("\n","", dn),"\t"))
      attrnames <- sapply(attrs, function(x){unlist(strsplit(x,": ", perl = TRUE))[1]})
      attrs <- lapply(attrs, function(x){unlist(strsplit(x,": ", perl = TRUE))[2]})
      names(attrs) <- attrnames
      attrs <- attrs[names(attrs)!="DN"]
      private$attrs = attrs
    },
    
    #getAttributes
    getAttributes = function(){
      return(private$attrs)
    }
  )
)