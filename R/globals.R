
### ----------------------------------------------------------------- ###
### PACKAGE GLOBAL VARIABLES ----
### ----------------------------------------------------------------- ###

#nocov start
SUPREME_REQUIRED_FIELDS <- c("type", "name")
SUPREME_OPTIONAL_FIELDS <- c("input", "output", "calling_modules", "src")

## Variables to prevent R CMD notes
utils::globalVariables(c("callModule", "moduleA_server"))

#nocov end

