#' LDAPClient
#' @export
LDAPClient <- R6Class("LDAPClient",
  private = list(),
  public = list(
    url = NULL,
    root = NULL,
    initialize = function(hostname, port = NULL, dc){
      if(!is.null(port)) hostname <- paste0(hostname, ":", port)
      dcurl <- paste0(lapply(unlist(strsplit(dc,"\\.")), function(x){paste0("dc=",x)}),collapse=",")
      self$url <- sprintf("ldap://%s/%s", hostname, dcurl)
      print(self$url)
      self$root <- RCurl::getURL(self$url)
      message(self$root)
    },
    
    #getPersons
    getPersons = function(pretty = TRUE){
      people_attrs <- c("cn","sn","givenName", "mail", "ou", "employeeType")
      ldap_person <- RCurl::getURL(sprintf("%s?%s?sub?(objectClass=person)", self$url, paste0(people_attrs,collapse=",")))
      ldap_person_dn <- unlist(strsplit(ldap_person, "DN: "))
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
