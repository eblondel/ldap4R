# test_LDAPClient.R
# Author: Emmanuel Blondel <emmanuel.blondel1@gmail.com>
#
# Description: Unit tests for LDAP client
#=======================
require(ldap4R, quietly = TRUE)
require(testthat)

context("LDAPClient")

test_that("client is instantiated",{
  expect_is(LDAP, "LDAPClient")
  expect_equal(LDAP$url, "ldap://localhost:389/dc=planetexpress,dc=com")
})

test_that("persons are listed (raw result)",{
  persons <- LDAP$getPersons(pretty = FALSE)
  expect_is(persons, "list")
  expect_equal(length(persons), 7L)
  expect_true(all(sapply(persons, is, "LDAPPerson")))
})

test_that("persons are listed (data.frame)",{
  persons <- LDAP$getPersons(pretty = TRUE)
  expect_is(persons, "data.frame")
  expect_equal(nrow(persons), 7L)
})