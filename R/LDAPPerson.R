#' LDAPPerson
#'
#' @docType class
#' @export
#' @keywords ldap person
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
  inherit = LDAPObject,
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
      super$initialize(dn)
      person_attrs <- self$getAttributes()
      for(attrname in names(person_attrs)){
        value <- person_attrs[names(person_attrs)==attrname]
        if(is.list(value)) value <- unlist(value)
        self[[attrname]] <- value
      }
    }
  )
)
