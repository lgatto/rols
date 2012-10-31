.rolsEnv <- new.env(parent = emptyenv(), hash = TRUE)

assign("ontologies", NULL, envir = .rolsEnv)

lockEnvironment(.rolsEnv,bindings=TRUE)

getOntologiesFromEnv <- function() get("ontologies", .rolsEnv)

