#' LDAPPerson
#' @export
LDAPPerson <- R6Class("LDAPPerson",
  private = list(),
  public = list(
    cn = NULL,
    sn = NULL,
    givenName = NULL,
    mail = NULL,
    ou = NULL,
    employeeType = NULL,
    initialize = function(dn){
      
      props <- unlist(strsplit(gsub("\n","", dn),"\t"))
      propnames <- sapply(props, function(x){unlist(strsplit(x,": "))[1]})
      props <- lapply(props, function(x){unlist(strsplit(x,": "))[2]})
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
