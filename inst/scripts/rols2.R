## Development script for rols version 2.

## Release plan
##  Update release package on the 1st of March, so that the release
##  version continues to work using the archive URL until the next
##  release in April. Commit a devel version, that uses the REST api,
##  before release.


## http://www.ebi.ac.uk/ols/beta/docs/api
## http://www.ebi.ac.uk/ols/beta/roadmap.html

## "ontologies"
## ontologyLoadDate -> olsLoaded
##                  -> olsUpdated
## olsVersion -> olsVersion
## allIds -> terms
## "isIdObsolete" -> isObsolete
## "rootId"       -> olsRoot
## olsQuery       -> OlsSearch, olsSearch

## CVParams
## "as.character.CVParam"
## "charIsCVParam"

## Dropping
## .rols environment
## "as.character.Map"  "key"  "CVParam"
## ontologyNames -> see ontologies
## "as.character.mapItem"

##  TODO
## > ls("package:rols")
## "term"                 "termMetadata"
## "value"                "termXrefs"

## term graph and jstree for a given term

## Properties and individuals
## Select 

## EXAMPLES

## Get all ontolgies
ol <- Ontologies()
ol

## Summarise ontologies
(go <- ol[["GO"]])
(efo <- ol[["EFO"]])

olsVersion(go)
olsVersion(efo)

## Directly initialise one ontology
go1 <- Ontology("go")
(GO <- Ontology("GO"))

stopifnot(identical(go, GO))
stopifnot(identical(go, go1))

## Queries

## (all) terms
(gotrms <- terms("go", pagesize = 10000))
## gotrms <- terms(go, pagesize = 10000)

## (one) term

(trm <- gotrms[[1]])
olsPrefix(trm)
gotrms[1:3]
gotrms[["GO:0032801"]]

term("GO", "GO:0032801")
term(go, "GO:0032801")

isObsolete(gotrms[["GO:0030533"]])
isObsolete(gotrms[["GO:0005563"]])

isRoot(gotrms[["GO:0030533"]])

i <- which(unlist(lapply(gotrms, function(x) isRoot(x) & !isObsolete(x))))
for (ii in i)
    show(gotrms[[ii]])

olsRoot(go)
identical(olsRoot("GO"), olsRoot(go))

parents(trm)
ancestors(trm)
children(trm)
descendants(trm)

## searching


OlsSearch(q = "trans-golgi")
OlsSearch(q = "cell")
OlsSearch(q = "cell", exact = TRUE)
OlsSearch(q = "cell", exact = TRUE, ontology = "go")
OlsSearch(q = "cell", exact = TRUE, ontology = "GO")
OlsSearch(q = "plasma,membrane", ontology = "go")

res <- OlsSearch(q = "trans-golgi", ontology = "go", rows = 5)
res
res <- olsSearch(res)
res
as(res, "data.frame")
as(res, "Terms")

res2 <- OlsSearch(q = "trans-golgi")
res2 <- olsSearch(res2)
res2 <- as(res2, "Terms")
res2
olsPrefix(res2)
termId(res2)
