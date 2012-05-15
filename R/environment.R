.rolsEnv <- new.env(parent=.GlobalEnv, hash=TRUE)

assign("ontologies", NULL, envir = .rolsEnv)

lockEnvironment(.rolsEnv,bindings=TRUE)

getOntologiesFromEnv <- function() get("ontologies", .rolsEnv)

