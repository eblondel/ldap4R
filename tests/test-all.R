library(testthat)
library(ldap4R)

LDAP <- LDAPClient$new(host = "localhost", port = 389, dc = "planetexpress.com")

if(is(LDAP, "LDAPClient")){
  cat(sprintf("LDAP server '%s' up and running. Running integration tests...\n", LDAP$url))
  test_check("ldap4R")
}else{
  cat("LDAP server '%s' not running. Skipping integration tests...\n")
}