##' @title Controlled Vocabulary
##'
##' @name CVParam
##'
##' @aliases CVParam
##' @aliases charIsCVParam cvCharToCVPar as.character.CVParam
##'
##' @description
##'
##' `CVParam` objects instantiate controlled vocabulary entries.
##'
##' @section Methods:
##'
##' - `charIsCVParam(x)` checks if `x`, a character of the form
##'   `"[ONTO, ACCESSION, NAME, VALUE]"`, is a valid (possibly
##'   user-defined) `CVParam`. `"ONTO"` is the ontology label
##'   (prefix), `"ACCESSION"` is the term accession number, `"NAME"`
##'   is the term's name and `"VALUE"` is the value. Note that only
##'   the syntax validity is verified, not the semantics. See example
##'   below.
##'
##' - `coerce(from = "CVParam", to = "character")` coerces `CVParam`
##'   `from` to a `character` of the following form: `[label,
##'   accession, name, value]`. `as.character` is also defined.
##'
##' - `coerce(from = "character", to = "CVParam")` coerces `character`
##'   `from` to a `CVParam`. `as.CVParam` is also defined. If a
##'   `label` is absent, the `character` is converted to a User param,
##'   else, the `label` and `accession` are used to query the Ontology
##'   Lookup Service (see [OlsSearch()]). If a `name` is provided and
##'   does not match the retrieved name, a warning is thrown.
##'
##'   This function is vectorised; if the `from` character is of
##'   length greater than 1, then a list of `CVParam` is returned. The
##'   queries to the OLS are processed one-by-one, though.
##'
##' @author Laurent Gatto
##'
##' @examples
##'
##' ## User param
##' CVParam(name = "A user param", value = "the value")
##' ## CVParam ESI from PSI's Mass Spectrometry ontology
##' olsTerm("GO", "GO:0035145")
##' (eej <- CVParam(label = "GO", accession = "GO:0035145"))
##' class(eej)
##'
##' ## From a CVParam object to a character
##' cv <- as(eej, "character")
##' cv ## note the quotes
##'
##' ## From a character object to a CVParam
##' as(cv, "CVParam")
##' as("[GO, GO:0035145, , ]", "CVParam") ## no name
##' as("[GO, GO:0035145, exon-exon junction complex, ]", "CVParam")
##' as(c(cv, cv), "CVParam") ## more than 1 character
##'
##' x <- c("[MS, MS:1000073, , ]", ## valid CV param
##'        "[, , Hello, world]",   ## valid User param
##'        "[this, one is, not, valid]", ## not valid
##'        "[ , , , ]") ## not valid
##'
##' stopifnot(charIsCVParam(x) == c(TRUE, TRUE, FALSE, FALSE))
##'
##' ## A list of expected valid and non-valid entries
##' rols:::validCVchars
##' rols:::notvalidCVchars
NULL

############################################################
## A param is [CV label, accession, name|synonym, value]
.CVParam <- setClass("CVParam",
                     slots = c(
                         label = "character",
                         accession = "character",
                         name = "character",
                         value = "character",
                         user = "logical"),
                     prototype = prototype(user = FALSE))

## When fixed, set automatic validity back
validCVParam <- function(object) {
    msg <- validMsg(NULL, NULL)
    if (object@user) {
        if (!all(c(object@label, object@accession) == ""))
            msg <- "Label and accession must be empty in UserParams."
    } else {
        x <- c(object@label, object@accession,
               object@name, object@value) == ""
        if (!all(x)) {
            ## FIXME - why call Term here? Is this needed?
            ._term <- olsTerm(object@label, object@accession)
            ._label <- termLabel(._term)
            ._synonyms <- termSynonym(._term)
            if (!(object@name %in% c(._label, ._synonyms)))
                msg <- paste0("CVParam accession and name/synomyms do not match. Got [",
                              paste(c(._label, ._synonyms), collapse = ", "),
                              "], expected '", object@name, "'.")
        }
    }
    if (is.null(msg)) TRUE else msg
}

## trim leading and trailing whitespace
trim <- function (x) gsub("^\\s+|\\s+$", "", x)

##' @param label `character(1)` with the ontology label. If missing, a
##'     user-defined parameter is created.
##'
##' @param name `character(1)` with the name of the `CVParam` to be
##'     constructed. This argument can be omitted if `accession` is
##'     used and `label` is not missing.
##'
##' @param accession `character(1)` with the accession of the
##'     `CVParam` to be constructed. This argument can be omitted if
##'     `name` is used. Ignored for user-defined instances.
##'
##' @param value `character(1)` with the value of the `CVParam` o be
##'     constructed. This argument is optional.
##'
##' @param exact `logical(1)` defining whether the query to retrieve
##'     the `accession` (when `name` is used) should be an exact
##'     match.
##'
##' @export
##' @rdname CVParam
CVParam <- function(label,
                    name,
                    accession,
                    value,
                    exact = TRUE) {
    if (missing(label)) {
        ## a User param
        ans <- new("CVParam", name = name, user = TRUE)
    } else {
        ## a CV param
        if (missing(name) & missing(accession)) {
            stop("You need to provide at least one of 'name' or 'accession'")
        } else if (missing(name)) {
            name <- termLabel(olsTerm(label, accession))
        } else { ## missing(accession)
            resp <- OlsSearch(q = name, ontology = label, exact = exact)
            if (resp@numFound != 1)
                stop("Found more than one matching term: ",
                     paste(resp@response$obo_id, collapse = ", "))
            olsRows(resp) <- 1 ## only 1 response
            resp <- olsSearch(resp)
            accession <- resp@response$obo_id
        }

        ans <- new("CVParam", label = label, name = name,
                   accession = accession)
    }
    if (!missing(value))
        ans@value <- value
    if (validObject(ans))
        return(ans)
}

setAs("CVParam", "character",
      function(from, to = "character") {
        ans <- paste0("[",
                      from@label, ", ",
                      from@accession, ", ",
                      from@name, ", ",
                      from@value, "]")
        ans
      })

##' @export
as.character.CVParam <- function(x, ...) as(x, "character")

##' @export
##'
##' @param object `CVParam` object.
##' @rdname CVParam
setMethod("show","CVParam",
          function(object) {
            cat(as(object, "character"), "\n")
            invisible(NULL)
          })

##' @export
##' @rdname CVParam
##'
##' @param x `CVParam` to be repeated.
##'
##' @param times `numeric(1)` defining the number of repetitions.
setMethod("rep", "CVParam",
          function(x, times) {
            l <- vector("list", length = times)
            for (i in 1:times)
              l[[i]] <- x
            return(l)
          })

##' @export
cvCharToCVPar <- function(from) {
    stopifnot(length(from) == 1)
    if (!charIsCVParam(from))
            stop(paste("Your input character should be",
                       "'[MS, MS:1000073, ESI, ]'.",
                       "See ?CVParam for details."))
    ## from <- substr(from, 2, nchar(from)-1)
    ## from <- trim(strsplit(from, ",")[[1]])
    from <- .split_cvparam(from)

    ## Assuming correct order here!
    ## 1: "label", 2: "accession", 3: "name", 4: "value"
    from <- sapply(from, trim, USE.NAMES = FALSE)
    if (from[1] == "") { ## label is missing -> user param
        cv <- CVParam(name = from[3], value = from[4])
    } else { ## CV para
        cv <- CVParam(label = from[1], accession = from[2])
        if (from[3] != "" && cv@name != from[3])
            warning("The CV names did not match:\n  ",
                    "Yours: '", from[3], "' - OLS: '", cv@name, "'.")
    }
    cv
}

setAs("character", "CVParam",
      function(from, to = "CVParam") {
          ans <- lapply(from, cvCharToCVPar)
          if (length(ans) == 1)
              ans <- ans[[1]]
          ans
      })

.charIsCVParam <- function(x) {
    ## NO SEMANTICS IS CHECKED
    x <- x[1]
    stopifnot(is.character(x))
    x <- .split_cvparam(x)
    if (all(x == "")) return(FALSE)
    if (length(x) != 4) return(FALSE)
    ## CV param: 1 and 2 are present
    if (x[1] != "") {
        ## FIXME - no ontologies() anymore - maybe add rolsEnv again?
        ## if (x[2] == "" | !x[1] %in% ontologies()[, 1]) return(FALSE)
        ## Using a simple (simplistic) pattern instead
        if (x[2] == "" | !grepl("[A-Za-z]", x[1])) return(FALSE)
        acc <- strsplit(x[2], ":")[[1]]
        if (length(acc) != 2) return(FALSE)
        if (acc[1] != x[1]) return(FALSE)
    } else {
        if (x[2] != "") return(FALSE)
        ## User param: 3 and 4 are present
        if (x[4] != "" & x[3] == "") return(FALSE)
        if (x[3] != "" & x[4] == "") return(FALSE)
    }
    return(TRUE)
}

.split_cvparam <- function(x) {
    x <- strsplit(x, ",")[[1]]
    ## Order:
    ## 1. label (ontology)
    ## 2. accession
    ## 3. name (can contain commas!)
    ## 4. value
    if (length(x) > 4) {
        x[3] <- paste(x[3:(length(x) - 1)], collapse = ",")
        x[4] <- x[length(x)]
        x <- x[1:4]
    }
    trim(gsub("\\[|\\]", "", x))
}



##' @export
charIsCVParam <- function(object)
    sapply(object, .charIsCVParam)


## TESTING
notvalidCVchars <- c("[ , , , ]", "[, , , ]",
                     "[ , , ,]", "[,,,]",
                     "[AB, MS:123 , , ]", "[, MS:123 , , ]",
                     "[MS, AB:123, , ]",
                     "[, , foo, ]", "[, , , bar]",
                     "[foo, , , ]", "[, bar, , ]",
                     "[, foo, bar, ]",
                     "[MS, , , bar]", "[MS, , foo, ]")


validCVchars <- c("[MS, MS:123 , , ]", "[, , foo, bar]",
                  "[MS, MS:123, foo, bar]", ## this one is questionable
                  "[MS, MS:123, , foo]",    ## this one is questionable
                  "[MS, MS:123, foo, ]")
