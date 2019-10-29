#' LDAPClient
#'
#' @docType class
#' @importFrom R6 R6Class
#' @import RCurl
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
#'  \item{\code{new(hostname, port, dc)}}{
#'    This method is used to instantiate a LDAP client.
#'  }
#'  \item{\code{getPersons(pretty)}}{
#'    Retrieves a list of persons. By default, this is returned as \code{data.frame}.
#'    Set \code{pretty = FALSE} to return a list of \link{LDAPPerson} objects.
#'  }
#' }
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
LDAPClient <- R6Class("LDAPClient",
  private = list(),
  public = list(
    root_url = NULL,
    root = NULL,
    dc = NULL,
    url = NULL,
    initialize = function(hostname, port = NULL, dc){
      if(!is.null(port)) hostname <- paste0(hostname, ":", port)
      dcurl <- paste0(lapply(unlist(strsplit(dc,"\\.")), function(x){paste0("dc=",x)}),collapse=",")
      self$root_url <- sprintf("ldap://%s", hostname)
      self$root <- RCurl::getURL(self$root_url)
      message(self$root)
      self$dc <- dcurl
      self$url <- sprintf("ldap://%s/%s", hostname, dcurl)
      
    },
    
    #getPersons
    getPersons = function(pretty = TRUE){
      people_attrs <- c("uid", "cn","sn","givenName", "mail", "title", "ou", "employeeType", "objectClass")
      ldap_person <- RCurl::getURL(sprintf("%s?%s?sub?(objectClass=person)", self$url, paste0(people_attrs,collapse=",")))
      ldap_person_dn <- unlist(strsplit(ldap_person, "DN: ", perl = TRUE))
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
