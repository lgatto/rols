## a param is [CV label, accession, name|synonym, value]
setClass("CVParam",
         representation = representation(
           label = "character",
           accession = "character",
           name = "character",
           value = "character",
           user = "logical"),
         contains = "Versioned",
         prototype = prototype(
           user = FALSE,
           new("Versioned", versions=c(CVParam="0.2.0"))),
         validity = function(object) {
           msg <- validMsg(NULL, NULL)
           if (object@user) {
             if (!all(c(object@label, object@accession) == ""))
               msg <- "Label and accession must be empty in UserParams."
           } else {
             x <- c(object@label, object@accession,
                    object@name, object@value) == ""
             if (!all(x)) {
               .term <- term(object@accession, object@label)
               .synonyms <- termMetadata(object@accession, object@label)
               .synonyms <- .synonyms[grep("synonym", names(.synonyms))]
               if (!(object@name %in% c(.term, .synonyms)))
                 msg <- paste0("CVParam accession and name/synomyms do not match. Got [",
                               paste(c(.term, .synonyms), collapse = ", "),
                               "], expected '", object@name, "'.")
             }
           }
           if (is.null(msg)) TRUE else msg
         })


setClassUnion("NullOrChar", c("NULL", "character"))
setClassUnion("NullOrList", c("NULL", "list"))

.Ontology <- setClass("Ontology",
                      slots = c(loaded = "NullOrChar",
                                updated = "NullOrChar",
                                status = "NullOrChar",
                                message = "NullOrChar",
                                version = "NullOrChar",
                                numberOfTerms = "integer",
                                numberOfProperties = "integer",
                                numberOfIndividuals = "integer",
                                config = "list"
                                ))

Ontologies <- setClass("Ontologies", slots = c(x = "list"))

.Term <- setClass("Term",
                  slots = c(iri = "character",
                            label = "character",
                            description = "NullOrList",
                            annotation = "list",
                            synonym = "NullOrList",
                            ontology_name = "character",
                            ontology_prefix = "character",
                            ontology_iri = "character",
                            is_obsolete = "logical",
                            is_defining_ontology = "logical",
                            has_children = "logical",
                            is_root = "logical",
                            short_form = "character",
                            obo_id = "NullOrChar",
                            links = "list"))

Terms <- setClass("Terms", slots = c(x = "list"))

.OlsSearch <- setClass("OlsSearch",
                       slots = c(q = "character",
                                 ontology = "character",
                                 type = "character",
                                 slim = "character",
                                 fieldList = "character",
                                 queryFields = "character",
                                 exact = "logical",
                                 groupField = "logical",
                                 obsoletes = "logical",
                                 local = "character",
                                 childrenOf = "character",
                                 rows = "integer", 
                                 start = "integer",
                                 url = "character",
                                 numFound = "integer",
                                 response = "data.frame"))
