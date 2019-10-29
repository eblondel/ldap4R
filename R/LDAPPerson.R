#' LDAPPerson
#'
#' @docType class
#' @importFrom R6 R6Class
#' @import RCurl
#' @export
#' @keywords ldap
#' @return Object of \code{\link{R6Class}} representing a LDAP person.
#' @format \code{\link{R6Class}} object.
#'
#' @field uid
#' @field cn
#' @field sn
#' @field givenName
#' @field mail
#' @field title
#' @field ou
#' @field employeeType
#' @field objectClass
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(dn)}}{
#'    This method is used to instantiate a LDAP person from its distinguished name (DN).
#'  }
#' }
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
LDAPPerson <- R6Class("LDAPPerson",
  private = list(),
  public = list(
    uid = NULL,
    cn = NULL,
    sn = NULL,
    givenName = NULL,
    mail = NULL,
    title = NULL,
    ou = NULL,
    employeeType = NULL,
    objectClass = NULL,
    initialize = function(dn){
      props <- unlist(strsplit(gsub("\n","", dn),"\t"))
      propnames <- sapply(props, function(x){unlist(strsplit(x,": ", fixed = TRUE))[1]})
      props <- lapply(props, function(x){unlist(strsplit(x,": ", fixed = TRUE))[2]})
      names(props) <- propnames
      props <- props[names(props)!="DN"]
      
      for(propname in names(props)){
        value <- props[names(props)==propname]
        if(is.list(value)) value <- unlist(value)
        self[[propname]] <- value
      }
    }
  )
)
