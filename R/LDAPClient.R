#' LDAPClient
#'
#' @docType class
#' @importFrom R6 R6Class
#' @import RCurl
#' @import plyr
#' @export
#' @keywords ldap
#' @return Object of \code{\link{R6Class}} with methods for communication a LDAP.
#' @format \code{\link{R6Class}} object.
#' 
#' @examples
#' \dontrun{
#'   ldap <- LDAPClient$new(hostname = "localhost", port = 389, dc = "planetexpress.com")
#' }
#'
#' @field url URL of the LDAP
#' @field root root LDAP DN
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(hostname, port, dc, logger)}}{
#'    This method is used to instantiate a LDAP client.  The logger can be either
#'    NULL, "INFO" (with minimum logs), or "DEBUG" (for complete curl calls logs)
#'  }
#'  \item{\code{getPersons(pretty)}}{
#'    Retrieves a list of persons. By default, this is returned as \code{data.frame}.
#'    Set \code{pretty = FALSE} to return a list of \link{LDAPPerson} objects.
#'  }
#' }
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
LDAPClient <- R6Class("LDAPClient",
  inherit = ldap4RLogger,
  private = list(),
  public = list(
    root_url = NULL,
    root = NULL,
    dc = NULL,
    url = NULL,
    initialize = function(hostname, port = NULL, dc, logger = NULL){
      super$initialize(logger = logger)
      if(!is.null(port)) hostname <- paste0(hostname, ":", port)
      dcurl <- paste0(lapply(unlist(strsplit(dc,"\\.")), function(x){paste0("dc=",x)}),collapse=",")
      self$root_url <- sprintf("ldap://%s", hostname)
      self$INFO(sprintf("LDAP root URL: %s", self$root_url))
      self$root <- RCurl::getURL(self$root_url)
      self$INFO(sprintf("LDAP root: %s", self$root))
      message(self$root)
      self$dc <- dcurl
      self$url <- sprintf("ldap://%s/%s", hostname, dcurl)
      self$INFO(sprintf("LDAP main URL: %s", self$url))
      
    },
    
    #getPersons
    getPersons = function(pretty = TRUE){
      people_attrs <- c("uid", "cn","sn","givenName", "mail", "title", "ou", "employeeType", "objectClass")
      ldap_person_url <- sprintf("%s?%s?sub?(objectClass=person)", self$url, paste0(people_attrs,collapse=","))
      self$INFO(sprintf("LDAP query for persons: %s", ldap_person_url))
      ldap_person <- RCurl::getURL(ldap_person_url)
      self$INFO(ldap_person)
      ldap_person_dn <- unlist(strsplit(ldap_person, "DN\\: "))
      ldap_person_dn <- ldap_person_dn[ldap_person_dn!=""]
      ldap_person_dn <- paste("DN:", ldap_person_dn)
      persons <- lapply(ldap_person_dn, LDAPPerson$new)
      if(pretty){
        persons <- do.call("rbind.fill", lapply(persons, function(person){
          person_df <- data.frame(cn = "", stringsAsFactors = F)
          for(people_attr in people_attrs){
            values <-  person[[people_attr]]
            for(i in 1:length(values)){
              if(i==1){
                person_df[,people_attr] <- values[i]
              }else{
                person_df[,paste0(people_attr,i)] <- values[i]
              }
            }
          }
          return(person_df)
        }))
        persons <- persons[,order(colnames(persons))]
      }
      return(persons)
    }
    
    
  )
)
