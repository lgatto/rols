## File generate by rols/inst/makeIface.R
## Manual modifications: moved 'Map' and 'Vector'
## class definitions to the top

## library(SSOAP)
## library(XML)

setClass( 'Map' ,
	   representation(
		.Data = 'list') ,
	   contains = c( 'list' ) )

setClass( 'Vector' ,
	   representation(
		.Data = 'ANY') ,
	   contains = c( 'ANY' ) ) 

setClass( 'DataHolder' ,
	   representation(
		annotationNumberValue = 'numeric',
		annotationStringValue = 'character',
		annotationType = 'character',
		termId = 'character',
		termName = 'character') ,
	   contains = c( 'VirtualSOAPClass' ) ) 
setAs('list', 'DataHolder',


function (from, to = "DataHolder", strict = TRUE) 
coerceListToS4(from, new("DataHolder"))


)
setClass( 'exactName.ontologyName' ,
	   representation(
		exactName = 'character',
		ontologyName = 'character') ,
	   contains = c( 'VirtualSOAPClass' ) ) 
setAs('list', 'exactName.ontologyName',


function (from, to = "exactName.ontologyName", strict = TRUE) 
coerceListToS4(from, new("exactName.ontologyName"))


)
setClass( 'getAllTermsFromOntologyReturn' ,
	   representation(
		getAllTermsFromOntologyReturn = 'Map') ,
	   contains = c( 'VirtualSOAPClass' ) ) 
setAs('list', 'getAllTermsFromOntologyReturn',


function (from, to = "getAllTermsFromOntologyReturn", strict = TRUE) 
coerceListToS4(from, new("getAllTermsFromOntologyReturn"))


)
## setClass( 'getChildrenFromRootReturn' ,
## 	   representation(
## 		getChildrenFromRootReturn = 'Map') ,
## 	   contains = c( 'VirtualSOAPClass' ) ) 
## setAs('list', 'getChildrenFromRootReturn',


## function (from, to = "getChildrenFromRootReturn", strict = TRUE) 
## coerceListToS4(from, new("getChildrenFromRootReturn"))


## )
setClass( 'getOntologyLoadDateReturn' ,
	   representation(
		getOntologyLoadDateReturn = 'character') ,
	   contains = c( 'VirtualSOAPClass' ) ) 
setAs('list', 'getOntologyLoadDateReturn',


function (from, to = "getOntologyLoadDateReturn", strict = TRUE) 
coerceListToS4(from, new("getOntologyLoadDateReturn"))


)
setClass( 'getOntologyNamesReturn' ,
	   representation(
		getOntologyNamesReturn = 'Map') ,
	   contains = c( 'VirtualSOAPClass' ) ) 
setAs('list', 'getOntologyNamesReturn',


function (from, to = "getOntologyNamesReturn", strict = TRUE) 
coerceListToS4(from, new("getOntologyNamesReturn"))


)
setClass( 'getPrefixedTermsByNameReturn' ,
	   representation(
		getPrefixedTermsByNameReturn = 'Map') ,
	   contains = c( 'VirtualSOAPClass' ) ) 
setAs('list', 'getPrefixedTermsByNameReturn',


function (from, to = "getPrefixedTermsByNameReturn", strict = TRUE) 
coerceListToS4(from, new("getPrefixedTermsByNameReturn"))


)
setClass( 'getRootTermsReturn' ,
	   representation(
		getRootTermsReturn = 'Map') ,
	   contains = c( 'VirtualSOAPClass' ) ) 
setAs('list', 'getRootTermsReturn',


function (from, to = "getRootTermsReturn", strict = TRUE) 
coerceListToS4(from, new("getRootTermsReturn"))


)
setClass( 'getTermByIdReturn' ,
	   representation(
		getTermByIdReturn = 'character') ,
	   contains = c( 'VirtualSOAPClass' ) ) 
setAs('list', 'getTermByIdReturn',


function (from, to = "getTermByIdReturn", strict = TRUE) 
coerceListToS4(from, new("getTermByIdReturn"))


)
## setClass( 'getTermChildrenReturn' ,
## 	   representation(
## 		getTermChildrenReturn = 'Map') ,
## 	   contains = c( 'VirtualSOAPClass' ) ) 
## setAs('list', 'getTermChildrenReturn',
##
## function (from, to = "getTermChildrenReturn", strict = TRUE) 
## coerceListToS4(from, new("getTermChildrenReturn"))
##
## )
setClass( 'getTermMetadataReturn' ,
	   representation(
		getTermMetadataReturn = 'Map') ,
	   contains = c( 'VirtualSOAPClass' ) ) 
setAs('list', 'getTermMetadataReturn',


function (from, to = "getTermMetadataReturn", strict = TRUE) 
coerceListToS4(from, new("getTermMetadataReturn"))


)
setClass( 'getTermParentsReturn' ,
	   representation(
		getTermParentsReturn = 'Map') ,
	   contains = c( 'VirtualSOAPClass' ) ) 
setAs('list', 'getTermParentsReturn',


function (from, to = "getTermParentsReturn", strict = TRUE) 
coerceListToS4(from, new("getTermParentsReturn"))


)
setClass( 'getTermRelationsReturn' ,
	   representation(
		getTermRelationsReturn = 'Map') ,
	   contains = c( 'VirtualSOAPClass' ) ) 
setAs('list', 'getTermRelationsReturn',


function (from, to = "getTermRelationsReturn", strict = TRUE) 
coerceListToS4(from, new("getTermRelationsReturn"))


)
## setClass( 'getTermsByAnnotationDataResponse' ,
## 	   representation(
## 		.Data = 'list') ,
## 	   contains = c( 'list' ) ) 
## setAs('XMLInternalElementNode', 'getTermsByAnnotationDataResponse',
## function (from, to = "getTermsByAnnotationDataResponse", strict = TRUE) 
## xmlSApply(from, as, "DataHolder")
## )
## setClass( 'getTermsByExactNameReturn' ,
## 	   representation(
## 		getTermsByExactNameReturn = 'Map') ,
## 	   contains = c( 'VirtualSOAPClass' ) ) 
## setAs('list', 'getTermsByExactNameReturn',
##
## function (from, to = "getTermsByExactNameReturn", strict = TRUE) 
## coerceListToS4(from, new("getTermsByExactNameReturn"))
##
## )
setClass( 'getTermsByNameReturn' ,
	   representation(
		getTermsByNameReturn = 'Map') ,
	   contains = c( 'VirtualSOAPClass' ) ) 
setAs('list', 'getTermsByNameReturn',


function (from, to = "getTermsByNameReturn", strict = TRUE) 
coerceListToS4(from, new("getTermsByNameReturn"))


)
setClass( 'getTermXrefsReturn' ,
	   representation(
		getTermXrefsReturn = 'Map') ,
	   contains = c( 'VirtualSOAPClass' ) ) 
setAs('list', 'getTermXrefsReturn',


function (from, to = "getTermXrefsReturn", strict = TRUE) 
coerceListToS4(from, new("getTermXrefsReturn"))


)
setClass( 'getVersionReturn' ,
	   representation(
		getVersionReturn = 'character') ,
	   contains = c( 'VirtualSOAPClass' ) ) 
setAs('list', 'getVersionReturn',


function (from, to = "getVersionReturn", strict = TRUE) 
coerceListToS4(from, new("getVersionReturn"))


)
setClass( 'isObsoleteReturn' ,
	   representation(
		isObsoleteReturn = 'logical') ,
	   contains = c( 'VirtualSOAPClass' ) ) 
setAs('list', 'isObsoleteReturn',


function (from, to = "isObsoleteReturn", strict = TRUE) 
coerceListToS4(from, new("isObsoleteReturn"))


)

setAs('XMLInternalElementNode', 'Map',


function (from, to = "Map", strict = TRUE) 
xmlSApply(from, as, "mapItem")


)
setClass( 'mapItem' ,
	   representation(
		key = 'ANY',
		value = 'ANY') ,
	   contains = c( 'VirtualSOAPClass' ) ) 
setAs('list', 'mapItem',


function (from, to = "mapItem", strict = TRUE) 
coerceListToS4(from, new("mapItem"))


)
setClass( 'ontologyName' ,
	   representation(
		ontologyName = 'character') ,
	   contains = c( 'VirtualSOAPClass' ) ) 
setAs('list', 'ontologyName',


function (from, to = "ontologyName", strict = TRUE) 
coerceListToS4(from, new("ontologyName"))


)
setClass( 'ontologyName.annotationType.strValue.fromDblValue.toDblValue' ,
	   representation(
		ontologyName = 'character',
		annotationType = 'character',
		strValue = 'character',
		fromDblValue = 'numeric',
		toDblValue = 'numeric') ,
	   contains = c( 'VirtualSOAPClass' ) ) 
setAs('list', 'ontologyName.annotationType.strValue.fromDblValue.toDblValue',


function (from, to = "ontologyName.annotationType.strValue.fromDblValue.toDblValue", 
    strict = TRUE) 
coerceListToS4(from, new("ontologyName.annotationType.strValue.fromDblValue.toDblValue"))


)
setClass( 'partialName.ontologyName.reverseKeyOrder' ,
	   representation(
		partialName = 'character',
		ontologyName = 'character',
		reverseKeyOrder = 'logical') ,
	   contains = c( 'VirtualSOAPClass' ) ) 
setAs('list', 'partialName.ontologyName.reverseKeyOrder',


function (from, to = "partialName.ontologyName.reverseKeyOrder", 
    strict = TRUE) 
coerceListToS4(from, new("partialName.ontologyName.reverseKeyOrder"))


)
setClass( 'partialName.reverseKeyOrder' ,
	   representation(
		partialName = 'character',
		reverseKeyOrder = 'logical') ,
	   contains = c( 'VirtualSOAPClass' ) ) 
setAs('list', 'partialName.reverseKeyOrder',


function (from, to = "partialName.reverseKeyOrder", strict = TRUE) 
coerceListToS4(from, new("partialName.reverseKeyOrder"))


)
setClass( 'rootTermId.ontologyName.childrenIds' ,
	   representation(
		rootTermId = 'character',
		ontologyName = 'character',
		childrenIds = 'Vector') ,
	   contains = c( 'VirtualSOAPClass' ) ) 
setAs('list', 'rootTermId.ontologyName.childrenIds',


function (from, to = "rootTermId.ontologyName.childrenIds", strict = TRUE) 
coerceListToS4(from, new("rootTermId.ontologyName.childrenIds"))


)
setClass( 'termId.ontologyName' ,
	   representation(
		termId = 'character',
		ontologyName = 'character') ,
	   contains = c( 'VirtualSOAPClass' ) ) 
setAs('list', 'termId.ontologyName',


function (from, to = "termId.ontologyName", strict = TRUE) 
coerceListToS4(from, new("termId.ontologyName"))


)
setClass( 'termId.ontologyName.distance.relationTypes' ,
	   representation(
		termId = 'character',
		ontologyName = 'character',
		distance = 'integer',
		relationTypes = 'integer') ,
	   contains = c( 'VirtualSOAPClass' ) ) 
setAs('list', 'termId.ontologyName.distance.relationTypes',


function (from, to = "termId.ontologyName.distance.relationTypes", 
    strict = TRUE) 
coerceListToS4(from, new("termId.ontologyName.distance.relationTypes"))


)

setAs('XMLInternalElementNode', 'Vector',


function (from, to = "Vector", strict = TRUE) 
xmlSApply(from, as, "ANY")


)
getVersion = 
function (server = new("HTTPSOAPServer", host = "www.ebi.ac.uk", 
    port = NA_integer_, url = "/ontology-lookup/services/OntologyQuery"), 
    .convert = new("Element", type = new("ClassDefinition", slotTypes = list(getVersionReturn = new("LocalElement", 
        count = c(1L, 1L), type = new("PrimitiveSOAPType", fromConverter = function () 
        NULL, toConverter = .Primitive("as.character"), count = c(1L, 
        1L), abstract = logical(0), name = "string", ns = "xsd", 
            nsuri = "http://www.w3.org/2001/XMLSchema", default = NA_character_, 
            Rname = character(0), documentation = character(0)), 
        name = "getVersionReturn", attributes = list(), xmlAttrs = character(0), 
        ns = character(0), nsuri = "http://www.ebi.ac.uk/ontology-lookup/OntologyQuery", 
        default = NA_character_, Rname = character(0), documentation = character(0))), 
        isAttribute = FALSE, uris = "http://www.ebi.ac.uk/ontology-lookup/OntologyQuery", 
        fromConverter = function () 
        NULL, toConverter = function () 
        NULL, count = numeric(0), abstract = logical(0), name = "getVersionReturn", 
        ns = character(0), nsuri = "http://www.ebi.ac.uk/ontology-lookup/OntologyQuery", 
        default = NA_character_, Rname = character(0), documentation = character(0)), 
        name = "getVersionResponse", attributes = list(), xmlAttrs = character(0), 
        ns = character(0), nsuri = "http://www.ebi.ac.uk/ontology-lookup/OntologyQuery", 
        default = NULL, Rname = character(0), documentation = character(0)), 
    .opts = list(), nameSpaces = "1.2", .soapHeader = NULL) 
{
    .SOAP(server, "getVersion", action = "getVersion", xmlns = "http://www.ebi.ac.uk/ontology-lookup/OntologyQuery", 
        .types = list(), .convert = .convert, .header = c(Accept = "text/xml", 
            Accept = "multipart/*", `Content-Type` = "text/xml; charset=utf-8", 
            SOAPAction = "\"getVersion\""), .opts = .opts, .literal = TRUE, 
        nameSpaces = nameSpaces, .elementFormQualified = TRUE, 
        .returnNodeName = "getVersionResponse", .soapHeader = .soapHeader)
}

class( getVersion ) = c( 'SerializedFunction', 'WSDLGeneratedSOAPFunction' )

getTermById = 
function (parameters = list(...), ..., server = new("HTTPSOAPServer", 
    host = "www.ebi.ac.uk", port = NA_integer_, url = "/ontology-lookup/services/OntologyQuery"), 
    .convert = new("Element", type = new("ClassDefinition", slotTypes = list(getTermByIdReturn = new("LocalElement", 
        count = c(1L, 1L), type = new("PrimitiveSOAPType", fromConverter = function () 
        NULL, toConverter = .Primitive("as.character"), count = c(1L, 
        1L), abstract = logical(0), name = "string", ns = "xsd", 
            nsuri = "http://www.w3.org/2001/XMLSchema", default = NA_character_, 
            Rname = character(0), documentation = character(0)), 
        name = "getTermByIdReturn", attributes = list(), xmlAttrs = character(0), 
        ns = character(0), nsuri = "http://www.ebi.ac.uk/ontology-lookup/OntologyQuery", 
        default = NA_character_, Rname = character(0), documentation = character(0))), 
        isAttribute = FALSE, uris = "http://www.ebi.ac.uk/ontology-lookup/OntologyQuery", 
        fromConverter = function () 
        NULL, toConverter = function () 
        NULL, count = numeric(0), abstract = logical(0), name = "getTermByIdReturn", 
        ns = character(0), nsuri = "http://www.ebi.ac.uk/ontology-lookup/OntologyQuery", 
        default = NA_character_, Rname = character(0), documentation = character(0)), 
        name = "getTermByIdResponse", attributes = list(), xmlAttrs = character(0), 
        ns = character(0), nsuri = "http://www.ebi.ac.uk/ontology-lookup/OntologyQuery", 
        default = NULL, Rname = character(0), documentation = character(0)), 
    .opts = list(), nameSpaces = "1.2", .soapHeader = NULL) 
{
    .SOAP(server, "getTermById", parameters = as(parameters, 
        "termId.ontologyName"), action = "getTermById", xmlns = "http://www.ebi.ac.uk/ontology-lookup/OntologyQuery", 
        .types = list(parameters = new("Element", type = new("ClassDefinition", 
            slotTypes = list(termId = new("LocalElement", count = c(1L, 
            1L), type = new("PrimitiveSOAPType", fromConverter = function () 
            NULL, toConverter = .Primitive("as.character"), count = c(1L, 
            1L), abstract = logical(0), name = "string", ns = "xsd", 
                nsuri = "http://www.w3.org/2001/XMLSchema", default = NA_character_, 
                Rname = character(0), documentation = character(0)), 
                name = "termId", attributes = list(), xmlAttrs = character(0), 
                ns = character(0), nsuri = "http://www.ebi.ac.uk/ontology-lookup/OntologyQuery", 
                default = NA_character_, Rname = character(0), 
                documentation = character(0)), ontologyName = new("LocalElement", 
                count = c(1L, 1L), type = new("PrimitiveSOAPType", 
                  fromConverter = function () 
                  NULL, toConverter = .Primitive("as.character"), 
                  count = c(1L, 1L), abstract = logical(0), name = "string", 
                  ns = "xsd", nsuri = "http://www.w3.org/2001/XMLSchema", 
                  default = NA_character_, Rname = character(0), 
                  documentation = character(0)), name = "ontologyName", 
                attributes = list(), xmlAttrs = character(0), 
                ns = character(0), nsuri = "http://www.ebi.ac.uk/ontology-lookup/OntologyQuery", 
                default = NA_character_, Rname = character(0), 
                documentation = character(0))), isAttribute = c(FALSE, 
            FALSE), uris = c("http://www.ebi.ac.uk/ontology-lookup/OntologyQuery", 
            "http://www.ebi.ac.uk/ontology-lookup/OntologyQuery"
            ), fromConverter = function () 
            NULL, toConverter = function () 
            NULL, count = numeric(0), abstract = logical(0), 
            name = "termId.ontologyName", ns = character(0), 
            nsuri = "http://www.ebi.ac.uk/ontology-lookup/OntologyQuery", 
            default = NA_character_, Rname = character(0), documentation = character(0)), 
            name = "getTermById", attributes = list(), xmlAttrs = character(0), 
            ns = character(0), nsuri = "http://www.ebi.ac.uk/ontology-lookup/OntologyQuery", 
            default = NULL, Rname = character(0), documentation = character(0))), 
        .convert = .convert, .header = c(Accept = "text/xml", 
            Accept = "multipart/*", `Content-Type` = "text/xml; charset=utf-8", 
            SOAPAction = "\"getTermById\""), .opts = .opts, .literal = TRUE, 
        nameSpaces = nameSpaces, .elementFormQualified = TRUE, 
        .returnNodeName = "getTermByIdResponse", .soapHeader = .soapHeader)
}

class( getTermById ) = c( 'SerializedFunction', 'WSDLGeneratedSOAPFunction' )

getTermMetadata = 
function (parameters = list(...), ..., server = new("HTTPSOAPServer", 
    host = "www.ebi.ac.uk", port = NA_integer_, url = "/ontology-lookup/services/OntologyQuery"), 
    .convert = new("Element", type = new("ClassDefinition", slotTypes = list(getTermMetadataReturn = new("LocalElement", 
        count = c(1L, 1L), type = new("SimpleSequenceType", elementType = character(0), 
            elType = new("ClassDefinition", slotTypes = list(key = new("LocalElement", 
                count = c(1L, 1L), type = new("PrimitiveSOAPType", 
                  fromConverter = function () 
                  NULL, toConverter = function () 
                  NULL, count = c(1L, 1L), abstract = logical(0), 
                  name = "anyType", ns = "xsd", nsuri = "http://www.w3.org/2001/XMLSchema", 
                  default = NA_character_, Rname = character(0), 
                  documentation = character(0)), name = "key", 
                attributes = list(), xmlAttrs = character(0), 
                ns = character(0), nsuri = "http://xml.apache.org/xml-soap", 
                default = NA_character_, Rname = character(0), 
                documentation = character(0)), value = new("LocalElement", 
                count = c(1L, 1L), type = new("PrimitiveSOAPType", 
                  fromConverter = function () 
                  NULL, toConverter = function () 
                  NULL, count = c(1L, 1L), abstract = logical(0), 
                  name = "anyType", ns = "xsd", nsuri = "http://www.w3.org/2001/XMLSchema", 
                  default = NA_character_, Rname = character(0), 
                  documentation = character(0)), name = "value", 
                attributes = list(), xmlAttrs = character(0), 
                ns = character(0), nsuri = "http://xml.apache.org/xml-soap", 
                default = NA_character_, Rname = character(0), 
                documentation = character(0))), isAttribute = c(FALSE, 
            FALSE), uris = c("http://xml.apache.org/xml-soap", 
            "http://xml.apache.org/xml-soap"), fromConverter = function () 
            NULL, toConverter = function () 
            NULL, count = numeric(0), abstract = logical(0), 
                name = "mapItem", ns = character(0), nsuri = "http://xml.apache.org/xml-soap", 
                default = NA_character_, Rname = character(0), 
                documentation = character(0)), fromConverter = function () 
            NULL, toConverter = function () 
            NULL, count = c(0, Inf), abstract = logical(0), name = "Map", 
            ns = character(0), nsuri = "http://xml.apache.org/xml-soap", 
            default = NA_character_, Rname = character(0), documentation = character(0)), 
        name = "getTermMetadataReturn", attributes = list(), 
        xmlAttrs = character(0), ns = character(0), nsuri = "http://www.ebi.ac.uk/ontology-lookup/OntologyQuery", 
        default = NA_character_, Rname = character(0), documentation = character(0))), 
        isAttribute = FALSE, uris = "http://www.ebi.ac.uk/ontology-lookup/OntologyQuery", 
        fromConverter = function () 
        NULL, toConverter = function () 
        NULL, count = numeric(0), abstract = logical(0), name = "getTermMetadataReturn", 
        ns = character(0), nsuri = "http://www.ebi.ac.uk/ontology-lookup/OntologyQuery", 
        default = NA_character_, Rname = character(0), documentation = character(0)), 
        name = "getTermMetadataResponse", attributes = list(), 
        xmlAttrs = character(0), ns = character(0), nsuri = "http://www.ebi.ac.uk/ontology-lookup/OntologyQuery", 
        default = NULL, Rname = character(0), documentation = character(0)), 
    .opts = list(), nameSpaces = "1.2", .soapHeader = NULL) 
{
    .SOAP(server, "getTermMetadata", parameters = as(parameters, 
        "termId.ontologyName"), action = "getTermMetadata", xmlns = "http://www.ebi.ac.uk/ontology-lookup/OntologyQuery", 
        .types = list(parameters = new("Element", type = new("ClassDefinition", 
            slotTypes = list(termId = new("LocalElement", count = c(1L, 
            1L), type = new("PrimitiveSOAPType", fromConverter = function () 
            NULL, toConverter = .Primitive("as.character"), count = c(1L, 
            1L), abstract = logical(0), name = "string", ns = "xsd", 
                nsuri = "http://www.w3.org/2001/XMLSchema", default = NA_character_, 
                Rname = character(0), documentation = character(0)), 
                name = "termId", attributes = list(), xmlAttrs = character(0), 
                ns = character(0), nsuri = "http://www.ebi.ac.uk/ontology-lookup/OntologyQuery", 
                default = NA_character_, Rname = character(0), 
                documentation = character(0)), ontologyName = new("LocalElement", 
                count = c(1L, 1L), type = new("PrimitiveSOAPType", 
                  fromConverter = function () 
                  NULL, toConverter = .Primitive("as.character"), 
                  count = c(1L, 1L), abstract = logical(0), name = "string", 
                  ns = "xsd", nsuri = "http://www.w3.org/2001/XMLSchema", 
                  default = NA_character_, Rname = character(0), 
                  documentation = character(0)), name = "ontologyName", 
                attributes = list(), xmlAttrs = character(0), 
                ns = character(0), nsuri = "http://www.ebi.ac.uk/ontology-lookup/OntologyQuery", 
                default = NA_character_, Rname = character(0), 
                documentation = character(0))), isAttribute = c(FALSE, 
            FALSE), uris = c("http://www.ebi.ac.uk/ontology-lookup/OntologyQuery", 
            "http://www.ebi.ac.uk/ontology-lookup/OntologyQuery"
            ), fromConverter = function () 
            NULL, toConverter = function () 
            NULL, count = numeric(0), abstract = logical(0), 
            name = "termId.ontologyName", ns = character(0), 
            nsuri = "http://www.ebi.ac.uk/ontology-lookup/OntologyQuery", 
            default = NA_character_, Rname = character(0), documentation = character(0)), 
            name = "getTermMetadata", attributes = list(), xmlAttrs = character(0), 
            ns = character(0), nsuri = "http://www.ebi.ac.uk/ontology-lookup/OntologyQuery", 
            default = NULL, Rname = character(0), documentation = character(0))), 
        .convert = .convert, .header = c(Accept = "text/xml", 
            Accept = "multipart/*", `Content-Type` = "text/xml; charset=utf-8", 
            SOAPAction = "\"getTermMetadata\""), .opts = .opts, 
        .literal = TRUE, nameSpaces = nameSpaces, .elementFormQualified = TRUE, 
        .returnNodeName = "getTermMetadataResponse", .soapHeader = .soapHeader)
}

class( getTermMetadata ) = c( 'SerializedFunction', 'WSDLGeneratedSOAPFunction' )

getTermXrefs = 
function (parameters = list(...), ..., server = new("HTTPSOAPServer", 
    host = "www.ebi.ac.uk", port = NA_integer_, url = "/ontology-lookup/services/OntologyQuery"), 
    .convert = new("Element", type = new("ClassDefinition", slotTypes = list(getTermXrefsReturn = new("LocalElement", 
        count = c(1L, 1L), type = new("SimpleSequenceType", elementType = character(0), 
            elType = new("ClassDefinition", slotTypes = list(key = new("LocalElement", 
                count = c(1L, 1L), type = new("PrimitiveSOAPType", 
                  fromConverter = function () 
                  NULL, toConverter = function () 
                  NULL, count = c(1L, 1L), abstract = logical(0), 
                  name = "anyType", ns = "xsd", nsuri = "http://www.w3.org/2001/XMLSchema", 
                  default = NA_character_, Rname = character(0), 
                  documentation = character(0)), name = "key", 
                attributes = list(), xmlAttrs = character(0), 
                ns = character(0), nsuri = "http://xml.apache.org/xml-soap", 
                default = NA_character_, Rname = character(0), 
                documentation = character(0)), value = new("LocalElement", 
                count = c(1L, 1L), type = new("PrimitiveSOAPType", 
                  fromConverter = function () 
                  NULL, toConverter = function () 
                  NULL, count = c(1L, 1L), abstract = logical(0), 
                  name = "anyType", ns = "xsd", nsuri = "http://www.w3.org/2001/XMLSchema", 
                  default = NA_character_, Rname = character(0), 
                  documentation = character(0)), name = "value", 
                attributes = list(), xmlAttrs = character(0), 
                ns = character(0), nsuri = "http://xml.apache.org/xml-soap", 
                default = NA_character_, Rname = character(0), 
                documentation = character(0))), isAttribute = c(FALSE, 
            FALSE), uris = c("http://xml.apache.org/xml-soap", 
            "http://xml.apache.org/xml-soap"), fromConverter = function () 
            NULL, toConverter = function () 
            NULL, count = numeric(0), abstract = logical(0), 
                name = "mapItem", ns = character(0), nsuri = "http://xml.apache.org/xml-soap", 
                default = NA_character_, Rname = character(0), 
                documentation = character(0)), fromConverter = function () 
            NULL, toConverter = function () 
            NULL, count = c(0, Inf), abstract = logical(0), name = "Map", 
            ns = character(0), nsuri = "http://xml.apache.org/xml-soap", 
            default = NA_character_, Rname = character(0), documentation = character(0)), 
        name = "getTermXrefsReturn", attributes = list(), xmlAttrs = character(0), 
        ns = character(0), nsuri = "http://www.ebi.ac.uk/ontology-lookup/OntologyQuery", 
        default = NA_character_, Rname = character(0), documentation = character(0))), 
        isAttribute = FALSE, uris = "http://www.ebi.ac.uk/ontology-lookup/OntologyQuery", 
        fromConverter = function () 
        NULL, toConverter = function () 
        NULL, count = numeric(0), abstract = logical(0), name = "getTermXrefsReturn", 
        ns = character(0), nsuri = "http://www.ebi.ac.uk/ontology-lookup/OntologyQuery", 
        default = NA_character_, Rname = character(0), documentation = character(0)), 
        name = "getTermXrefsResponse", attributes = list(), xmlAttrs = character(0), 
        ns = character(0), nsuri = "http://www.ebi.ac.uk/ontology-lookup/OntologyQuery", 
        default = NULL, Rname = character(0), documentation = character(0)), 
    .opts = list(), nameSpaces = "1.2", .soapHeader = NULL) 
{
    .SOAP(server, "getTermXrefs", parameters = as(parameters, 
        "termId.ontologyName"), action = "getTermXrefs", xmlns = "http://www.ebi.ac.uk/ontology-lookup/OntologyQuery", 
        .types = list(parameters = new("Element", type = new("ClassDefinition", 
            slotTypes = list(termId = new("LocalElement", count = c(1L, 
            1L), type = new("PrimitiveSOAPType", fromConverter = function () 
            NULL, toConverter = .Primitive("as.character"), count = c(1L, 
            1L), abstract = logical(0), name = "string", ns = "xsd", 
                nsuri = "http://www.w3.org/2001/XMLSchema", default = NA_character_, 
                Rname = character(0), documentation = character(0)), 
                name = "termId", attributes = list(), xmlAttrs = character(0), 
                ns = character(0), nsuri = "http://www.ebi.ac.uk/ontology-lookup/OntologyQuery", 
                default = NA_character_, Rname = character(0), 
                documentation = character(0)), ontologyName = new("LocalElement", 
                count = c(1L, 1L), type = new("PrimitiveSOAPType", 
                  fromConverter = function () 
                  NULL, toConverter = .Primitive("as.character"), 
                  count = c(1L, 1L), abstract = logical(0), name = "string", 
                  ns = "xsd", nsuri = "http://www.w3.org/2001/XMLSchema", 
                  default = NA_character_, Rname = character(0), 
                  documentation = character(0)), name = "ontologyName", 
                attributes = list(), xmlAttrs = character(0), 
                ns = character(0), nsuri = "http://www.ebi.ac.uk/ontology-lookup/OntologyQuery", 
                default = NA_character_, Rname = character(0), 
                documentation = character(0))), isAttribute = c(FALSE, 
            FALSE), uris = c("http://www.ebi.ac.uk/ontology-lookup/OntologyQuery", 
            "http://www.ebi.ac.uk/ontology-lookup/OntologyQuery"
            ), fromConverter = function () 
            NULL, toConverter = function () 
            NULL, count = numeric(0), abstract = logical(0), 
            name = "termId.ontologyName", ns = character(0), 
            nsuri = "http://www.ebi.ac.uk/ontology-lookup/OntologyQuery", 
            default = NA_character_, Rname = character(0), documentation = character(0)), 
            name = "getTermXrefs", attributes = list(), xmlAttrs = character(0), 
            ns = character(0), nsuri = "http://www.ebi.ac.uk/ontology-lookup/OntologyQuery", 
            default = NULL, Rname = character(0), documentation = character(0))), 
        .convert = .convert, .header = c(Accept = "text/xml", 
            Accept = "multipart/*", `Content-Type` = "text/xml; charset=utf-8", 
            SOAPAction = "\"getTermXrefs\""), .opts = .opts, 
        .literal = TRUE, nameSpaces = nameSpaces, .elementFormQualified = TRUE, 
        .returnNodeName = "getTermXrefsResponse", .soapHeader = .soapHeader)
}

class( getTermXrefs ) = c( 'SerializedFunction', 'WSDLGeneratedSOAPFunction' )

getOntologyNames = 
function (server = new("HTTPSOAPServer", host = "www.ebi.ac.uk", 
    port = NA_integer_, url = "/ontology-lookup/services/OntologyQuery"), 
    .convert = new("Element", type = new("ClassDefinition", slotTypes = list(getOntologyNamesReturn = new("LocalElement", 
        count = c(1L, 1L), type = new("SimpleSequenceType", elementType = character(0), 
            elType = new("ClassDefinition", slotTypes = list(key = new("LocalElement", 
                count = c(1L, 1L), type = new("PrimitiveSOAPType", 
                  fromConverter = function () 
                  NULL, toConverter = function () 
                  NULL, count = c(1L, 1L), abstract = logical(0), 
                  name = "anyType", ns = "xsd", nsuri = "http://www.w3.org/2001/XMLSchema", 
                  default = NA_character_, Rname = character(0), 
                  documentation = character(0)), name = "key", 
                attributes = list(), xmlAttrs = character(0), 
                ns = character(0), nsuri = "http://xml.apache.org/xml-soap", 
                default = NA_character_, Rname = character(0), 
                documentation = character(0)), value = new("LocalElement", 
                count = c(1L, 1L), type = new("PrimitiveSOAPType", 
                  fromConverter = function () 
                  NULL, toConverter = function () 
                  NULL, count = c(1L, 1L), abstract = logical(0), 
                  name = "anyType", ns = "xsd", nsuri = "http://www.w3.org/2001/XMLSchema", 
                  default = NA_character_, Rname = character(0), 
                  documentation = character(0)), name = "value", 
                attributes = list(), xmlAttrs = character(0), 
                ns = character(0), nsuri = "http://xml.apache.org/xml-soap", 
                default = NA_character_, Rname = character(0), 
                documentation = character(0))), isAttribute = c(FALSE, 
            FALSE), uris = c("http://xml.apache.org/xml-soap", 
            "http://xml.apache.org/xml-soap"), fromConverter = function () 
            NULL, toConverter = function () 
            NULL, count = numeric(0), abstract = logical(0), 
                name = "mapItem", ns = character(0), nsuri = "http://xml.apache.org/xml-soap", 
                default = NA_character_, Rname = character(0), 
                documentation = character(0)), fromConverter = function () 
            NULL, toConverter = function () 
            NULL, count = c(0, Inf), abstract = logical(0), name = "Map", 
            ns = character(0), nsuri = "http://xml.apache.org/xml-soap", 
            default = NA_character_, Rname = character(0), documentation = character(0)), 
        name = "getOntologyNamesReturn", attributes = list(), 
        xmlAttrs = character(0), ns = character(0), nsuri = "http://www.ebi.ac.uk/ontology-lookup/OntologyQuery", 
        default = NA_character_, Rname = character(0), documentation = character(0))), 
        isAttribute = FALSE, uris = "http://www.ebi.ac.uk/ontology-lookup/OntologyQuery", 
        fromConverter = function () 
        NULL, toConverter = function () 
        NULL, count = numeric(0), abstract = logical(0), name = "getOntologyNamesReturn", 
        ns = character(0), nsuri = "http://www.ebi.ac.uk/ontology-lookup/OntologyQuery", 
        default = NA_character_, Rname = character(0), documentation = character(0)), 
        name = "getOntologyNamesResponse", attributes = list(), 
        xmlAttrs = character(0), ns = character(0), nsuri = "http://www.ebi.ac.uk/ontology-lookup/OntologyQuery", 
        default = NULL, Rname = character(0), documentation = character(0)), 
    .opts = list(), nameSpaces = "1.2", .soapHeader = NULL) 
{
    .SOAP(server, "getOntologyNames", action = "getOntologyNames", 
        xmlns = "http://www.ebi.ac.uk/ontology-lookup/OntologyQuery", 
        .types = list(), .convert = .convert, .header = c(Accept = "text/xml", 
            Accept = "multipart/*", `Content-Type` = "text/xml; charset=utf-8", 
            SOAPAction = "\"getOntologyNames\""), .opts = .opts, 
        .literal = TRUE, nameSpaces = nameSpaces, .elementFormQualified = TRUE, 
        .returnNodeName = "getOntologyNamesResponse", .soapHeader = .soapHeader)
}

class( getOntologyNames ) = c( 'SerializedFunction', 'WSDLGeneratedSOAPFunction' )

getOntologyLoadDate = 
function (parameters = list(...), ..., server = new("HTTPSOAPServer", 
    host = "www.ebi.ac.uk", port = NA_integer_, url = "/ontology-lookup/services/OntologyQuery"), 
    .convert = new("Element", type = new("ClassDefinition", slotTypes = list(getOntologyLoadDateReturn = new("LocalElement", 
        count = c(1L, 1L), type = new("PrimitiveSOAPType", fromConverter = function () 
        NULL, toConverter = .Primitive("as.character"), count = c(1L, 
        1L), abstract = logical(0), name = "string", ns = "xsd", 
            nsuri = "http://www.w3.org/2001/XMLSchema", default = NA_character_, 
            Rname = character(0), documentation = character(0)), 
        name = "getOntologyLoadDateReturn", attributes = list(), 
        xmlAttrs = character(0), ns = character(0), nsuri = "http://www.ebi.ac.uk/ontology-lookup/OntologyQuery", 
        default = NA_character_, Rname = character(0), documentation = character(0))), 
        isAttribute = FALSE, uris = "http://www.ebi.ac.uk/ontology-lookup/OntologyQuery", 
        fromConverter = function () 
        NULL, toConverter = function () 
        NULL, count = numeric(0), abstract = logical(0), name = "getOntologyLoadDateReturn", 
        ns = character(0), nsuri = "http://www.ebi.ac.uk/ontology-lookup/OntologyQuery", 
        default = NA_character_, Rname = character(0), documentation = character(0)), 
        name = "getOntologyLoadDateResponse", attributes = list(), 
        xmlAttrs = character(0), ns = character(0), nsuri = "http://www.ebi.ac.uk/ontology-lookup/OntologyQuery", 
        default = NULL, Rname = character(0), documentation = character(0)), 
    .opts = list(), nameSpaces = "1.2", .soapHeader = NULL) 
{
    .SOAP(server, "getOntologyLoadDate", parameters = as(parameters, 
        "ontologyName"), action = "getOntologyLoadDate", xmlns = "http://www.ebi.ac.uk/ontology-lookup/OntologyQuery", 
        .types = list(parameters = new("Element", type = new("ClassDefinition", 
            slotTypes = list(ontologyName = new("LocalElement", 
                count = c(1L, 1L), type = new("PrimitiveSOAPType", 
                  fromConverter = function () 
                  NULL, toConverter = .Primitive("as.character"), 
                  count = c(1L, 1L), abstract = logical(0), name = "string", 
                  ns = "xsd", nsuri = "http://www.w3.org/2001/XMLSchema", 
                  default = NA_character_, Rname = character(0), 
                  documentation = character(0)), name = "ontologyName", 
                attributes = list(), xmlAttrs = character(0), 
                ns = character(0), nsuri = "http://www.ebi.ac.uk/ontology-lookup/OntologyQuery", 
                default = NA_character_, Rname = character(0), 
                documentation = character(0))), isAttribute = FALSE, 
            uris = "http://www.ebi.ac.uk/ontology-lookup/OntologyQuery", 
            fromConverter = function () 
            NULL, toConverter = function () 
            NULL, count = numeric(0), abstract = logical(0), 
            name = "ontologyName", ns = character(0), nsuri = "http://www.ebi.ac.uk/ontology-lookup/OntologyQuery", 
            default = NA_character_, Rname = character(0), documentation = character(0)), 
            name = "getOntologyLoadDate", attributes = list(), 
            xmlAttrs = character(0), ns = character(0), nsuri = "http://www.ebi.ac.uk/ontology-lookup/OntologyQuery", 
            default = NULL, Rname = character(0), documentation = character(0))), 
        .convert = .convert, .header = c(Accept = "text/xml", 
            Accept = "multipart/*", `Content-Type` = "text/xml; charset=utf-8", 
            SOAPAction = "\"getOntologyLoadDate\""), .opts = .opts, 
        .literal = TRUE, nameSpaces = nameSpaces, .elementFormQualified = TRUE, 
        .returnNodeName = "getOntologyLoadDateResponse", .soapHeader = .soapHeader)
}

class( getOntologyLoadDate ) = c( 'SerializedFunction', 'WSDLGeneratedSOAPFunction' )

getAllTermsFromOntology = 
function (parameters = list(...), ..., server = new("HTTPSOAPServer", 
    host = "www.ebi.ac.uk", port = NA_integer_, url = "/ontology-lookup/services/OntologyQuery"), 
    .convert = new("Element", type = new("ClassDefinition", slotTypes = list(getAllTermsFromOntologyReturn = new("LocalElement", 
        count = c(1L, 1L), type = new("SimpleSequenceType", elementType = character(0), 
            elType = new("ClassDefinition", slotTypes = list(key = new("LocalElement", 
                count = c(1L, 1L), type = new("PrimitiveSOAPType", 
                  fromConverter = function () 
                  NULL, toConverter = function () 
                  NULL, count = c(1L, 1L), abstract = logical(0), 
                  name = "anyType", ns = "xsd", nsuri = "http://www.w3.org/2001/XMLSchema", 
                  default = NA_character_, Rname = character(0), 
                  documentation = character(0)), name = "key", 
                attributes = list(), xmlAttrs = character(0), 
                ns = character(0), nsuri = "http://xml.apache.org/xml-soap", 
                default = NA_character_, Rname = character(0), 
                documentation = character(0)), value = new("LocalElement", 
                count = c(1L, 1L), type = new("PrimitiveSOAPType", 
                  fromConverter = function () 
                  NULL, toConverter = function () 
                  NULL, count = c(1L, 1L), abstract = logical(0), 
                  name = "anyType", ns = "xsd", nsuri = "http://www.w3.org/2001/XMLSchema", 
                  default = NA_character_, Rname = character(0), 
                  documentation = character(0)), name = "value", 
                attributes = list(), xmlAttrs = character(0), 
                ns = character(0), nsuri = "http://xml.apache.org/xml-soap", 
                default = NA_character_, Rname = character(0), 
                documentation = character(0))), isAttribute = c(FALSE, 
            FALSE), uris = c("http://xml.apache.org/xml-soap", 
            "http://xml.apache.org/xml-soap"), fromConverter = function () 
            NULL, toConverter = function () 
            NULL, count = numeric(0), abstract = logical(0), 
                name = "mapItem", ns = character(0), nsuri = "http://xml.apache.org/xml-soap", 
                default = NA_character_, Rname = character(0), 
                documentation = character(0)), fromConverter = function () 
            NULL, toConverter = function () 
            NULL, count = c(0, Inf), abstract = logical(0), name = "Map", 
            ns = character(0), nsuri = "http://xml.apache.org/xml-soap", 
            default = NA_character_, Rname = character(0), documentation = character(0)), 
        name = "getAllTermsFromOntologyReturn", attributes = list(), 
        xmlAttrs = character(0), ns = character(0), nsuri = "http://www.ebi.ac.uk/ontology-lookup/OntologyQuery", 
        default = NA_character_, Rname = character(0), documentation = character(0))), 
        isAttribute = FALSE, uris = "http://www.ebi.ac.uk/ontology-lookup/OntologyQuery", 
        fromConverter = function () 
        NULL, toConverter = function () 
        NULL, count = numeric(0), abstract = logical(0), name = "getAllTermsFromOntologyReturn", 
        ns = character(0), nsuri = "http://www.ebi.ac.uk/ontology-lookup/OntologyQuery", 
        default = NA_character_, Rname = character(0), documentation = character(0)), 
        name = "getAllTermsFromOntologyResponse", attributes = list(), 
        xmlAttrs = character(0), ns = character(0), nsuri = "http://www.ebi.ac.uk/ontology-lookup/OntologyQuery", 
        default = NULL, Rname = character(0), documentation = character(0)), 
    .opts = list(), nameSpaces = "1.2", .soapHeader = NULL) 
{
    .SOAP(server, "getAllTermsFromOntology", parameters = as(parameters, 
        "ontologyName"), action = "getAllTermsFromOntology", 
        xmlns = "http://www.ebi.ac.uk/ontology-lookup/OntologyQuery", 
        .types = list(parameters = new("Element", type = new("ClassDefinition", 
            slotTypes = list(ontologyName = new("LocalElement", 
                count = c(1L, 1L), type = new("PrimitiveSOAPType", 
                  fromConverter = function () 
                  NULL, toConverter = .Primitive("as.character"), 
                  count = c(1L, 1L), abstract = logical(0), name = "string", 
                  ns = "xsd", nsuri = "http://www.w3.org/2001/XMLSchema", 
                  default = NA_character_, Rname = character(0), 
                  documentation = character(0)), name = "ontologyName", 
                attributes = list(), xmlAttrs = character(0), 
                ns = character(0), nsuri = "http://www.ebi.ac.uk/ontology-lookup/OntologyQuery", 
                default = NA_character_, Rname = character(0), 
                documentation = character(0))), isAttribute = FALSE, 
            uris = "http://www.ebi.ac.uk/ontology-lookup/OntologyQuery", 
            fromConverter = function () 
            NULL, toConverter = function () 
            NULL, count = numeric(0), abstract = logical(0), 
            name = "ontologyName", ns = character(0), nsuri = "http://www.ebi.ac.uk/ontology-lookup/OntologyQuery", 
            default = NA_character_, Rname = character(0), documentation = character(0)), 
            name = "getAllTermsFromOntology", attributes = list(), 
            xmlAttrs = character(0), ns = character(0), nsuri = "http://www.ebi.ac.uk/ontology-lookup/OntologyQuery", 
            default = NULL, Rname = character(0), documentation = character(0))), 
        .convert = .convert, .header = c(Accept = "text/xml", 
            Accept = "multipart/*", `Content-Type` = "text/xml; charset=utf-8", 
            SOAPAction = "\"getAllTermsFromOntology\""), .opts = .opts, 
        .literal = TRUE, nameSpaces = nameSpaces, .elementFormQualified = TRUE, 
        .returnNodeName = "getAllTermsFromOntologyResponse", 
        .soapHeader = .soapHeader)
}

class( getAllTermsFromOntology ) = c( 'SerializedFunction', 'WSDLGeneratedSOAPFunction' )

getRootTerms = 
function (parameters = list(...), ..., server = new("HTTPSOAPServer", 
    host = "www.ebi.ac.uk", port = NA_integer_, url = "/ontology-lookup/services/OntologyQuery"), 
    .convert = new("Element", type = new("ClassDefinition", slotTypes = list(getRootTermsReturn = new("LocalElement", 
        count = c(1L, 1L), type = new("SimpleSequenceType", elementType = character(0), 
            elType = new("ClassDefinition", slotTypes = list(key = new("LocalElement", 
                count = c(1L, 1L), type = new("PrimitiveSOAPType", 
                  fromConverter = function () 
                  NULL, toConverter = function () 
                  NULL, count = c(1L, 1L), abstract = logical(0), 
                  name = "anyType", ns = "xsd", nsuri = "http://www.w3.org/2001/XMLSchema", 
                  default = NA_character_, Rname = character(0), 
                  documentation = character(0)), name = "key", 
                attributes = list(), xmlAttrs = character(0), 
                ns = character(0), nsuri = "http://xml.apache.org/xml-soap", 
                default = NA_character_, Rname = character(0), 
                documentation = character(0)), value = new("LocalElement", 
                count = c(1L, 1L), type = new("PrimitiveSOAPType", 
                  fromConverter = function () 
                  NULL, toConverter = function () 
                  NULL, count = c(1L, 1L), abstract = logical(0), 
                  name = "anyType", ns = "xsd", nsuri = "http://www.w3.org/2001/XMLSchema", 
                  default = NA_character_, Rname = character(0), 
                  documentation = character(0)), name = "value", 
                attributes = list(), xmlAttrs = character(0), 
                ns = character(0), nsuri = "http://xml.apache.org/xml-soap", 
                default = NA_character_, Rname = character(0), 
                documentation = character(0))), isAttribute = c(FALSE, 
            FALSE), uris = c("http://xml.apache.org/xml-soap", 
            "http://xml.apache.org/xml-soap"), fromConverter = function () 
            NULL, toConverter = function () 
            NULL, count = numeric(0), abstract = logical(0), 
                name = "mapItem", ns = character(0), nsuri = "http://xml.apache.org/xml-soap", 
                default = NA_character_, Rname = character(0), 
                documentation = character(0)), fromConverter = function () 
            NULL, toConverter = function () 
            NULL, count = c(0, Inf), abstract = logical(0), name = "Map", 
            ns = character(0), nsuri = "http://xml.apache.org/xml-soap", 
            default = NA_character_, Rname = character(0), documentation = character(0)), 
        name = "getRootTermsReturn", attributes = list(), xmlAttrs = character(0), 
        ns = character(0), nsuri = "http://www.ebi.ac.uk/ontology-lookup/OntologyQuery", 
        default = NA_character_, Rname = character(0), documentation = character(0))), 
        isAttribute = FALSE, uris = "http://www.ebi.ac.uk/ontology-lookup/OntologyQuery", 
        fromConverter = function () 
        NULL, toConverter = function () 
        NULL, count = numeric(0), abstract = logical(0), name = "getRootTermsReturn", 
        ns = character(0), nsuri = "http://www.ebi.ac.uk/ontology-lookup/OntologyQuery", 
        default = NA_character_, Rname = character(0), documentation = character(0)), 
        name = "getRootTermsResponse", attributes = list(), xmlAttrs = character(0), 
        ns = character(0), nsuri = "http://www.ebi.ac.uk/ontology-lookup/OntologyQuery", 
        default = NULL, Rname = character(0), documentation = character(0)), 
    .opts = list(), nameSpaces = "1.2", .soapHeader = NULL) 
{
    .SOAP(server, "getRootTerms", parameters = as(parameters, 
        "ontologyName"), action = "getRootTerms", xmlns = "http://www.ebi.ac.uk/ontology-lookup/OntologyQuery", 
        .types = list(parameters = new("Element", type = new("ClassDefinition", 
            slotTypes = list(ontologyName = new("LocalElement", 
                count = c(1L, 1L), type = new("PrimitiveSOAPType", 
                  fromConverter = function () 
                  NULL, toConverter = .Primitive("as.character"), 
                  count = c(1L, 1L), abstract = logical(0), name = "string", 
                  ns = "xsd", nsuri = "http://www.w3.org/2001/XMLSchema", 
                  default = NA_character_, Rname = character(0), 
                  documentation = character(0)), name = "ontologyName", 
                attributes = list(), xmlAttrs = character(0), 
                ns = character(0), nsuri = "http://www.ebi.ac.uk/ontology-lookup/OntologyQuery", 
                default = NA_character_, Rname = character(0), 
                documentation = character(0))), isAttribute = FALSE, 
            uris = "http://www.ebi.ac.uk/ontology-lookup/OntologyQuery", 
            fromConverter = function () 
            NULL, toConverter = function () 
            NULL, count = numeric(0), abstract = logical(0), 
            name = "ontologyName", ns = character(0), nsuri = "http://www.ebi.ac.uk/ontology-lookup/OntologyQuery", 
            default = NA_character_, Rname = character(0), documentation = character(0)), 
            name = "getRootTerms", attributes = list(), xmlAttrs = character(0), 
            ns = character(0), nsuri = "http://www.ebi.ac.uk/ontology-lookup/OntologyQuery", 
            default = NULL, Rname = character(0), documentation = character(0))), 
        .convert = .convert, .header = c(Accept = "text/xml", 
            Accept = "multipart/*", `Content-Type` = "text/xml; charset=utf-8", 
            SOAPAction = "\"getRootTerms\""), .opts = .opts, 
        .literal = TRUE, nameSpaces = nameSpaces, .elementFormQualified = TRUE, 
        .returnNodeName = "getRootTermsResponse", .soapHeader = .soapHeader)
}

class( getRootTerms ) = c( 'SerializedFunction', 'WSDLGeneratedSOAPFunction' )

getTermsByName = 
function (parameters = list(...), ..., server = new("HTTPSOAPServer", 
    host = "www.ebi.ac.uk", port = NA_integer_, url = "/ontology-lookup/services/OntologyQuery"), 
    .convert = new("Element", type = new("ClassDefinition", slotTypes = list(getTermsByNameReturn = new("LocalElement", 
        count = c(1L, 1L), type = new("SimpleSequenceType", elementType = character(0), 
            elType = new("ClassDefinition", slotTypes = list(key = new("LocalElement", 
                count = c(1L, 1L), type = new("PrimitiveSOAPType", 
                  fromConverter = function () 
                  NULL, toConverter = function () 
                  NULL, count = c(1L, 1L), abstract = logical(0), 
                  name = "anyType", ns = "xsd", nsuri = "http://www.w3.org/2001/XMLSchema", 
                  default = NA_character_, Rname = character(0), 
                  documentation = character(0)), name = "key", 
                attributes = list(), xmlAttrs = character(0), 
                ns = character(0), nsuri = "http://xml.apache.org/xml-soap", 
                default = NA_character_, Rname = character(0), 
                documentation = character(0)), value = new("LocalElement", 
                count = c(1L, 1L), type = new("PrimitiveSOAPType", 
                  fromConverter = function () 
                  NULL, toConverter = function () 
                  NULL, count = c(1L, 1L), abstract = logical(0), 
                  name = "anyType", ns = "xsd", nsuri = "http://www.w3.org/2001/XMLSchema", 
                  default = NA_character_, Rname = character(0), 
                  documentation = character(0)), name = "value", 
                attributes = list(), xmlAttrs = character(0), 
                ns = character(0), nsuri = "http://xml.apache.org/xml-soap", 
                default = NA_character_, Rname = character(0), 
                documentation = character(0))), isAttribute = c(FALSE, 
            FALSE), uris = c("http://xml.apache.org/xml-soap", 
            "http://xml.apache.org/xml-soap"), fromConverter = function () 
            NULL, toConverter = function () 
            NULL, count = numeric(0), abstract = logical(0), 
                name = "mapItem", ns = character(0), nsuri = "http://xml.apache.org/xml-soap", 
                default = NA_character_, Rname = character(0), 
                documentation = character(0)), fromConverter = function () 
            NULL, toConverter = function () 
            NULL, count = c(0, Inf), abstract = logical(0), name = "Map", 
            ns = character(0), nsuri = "http://xml.apache.org/xml-soap", 
            default = NA_character_, Rname = character(0), documentation = character(0)), 
        name = "getTermsByNameReturn", attributes = list(), xmlAttrs = character(0), 
        ns = character(0), nsuri = "http://www.ebi.ac.uk/ontology-lookup/OntologyQuery", 
        default = NA_character_, Rname = character(0), documentation = character(0))), 
        isAttribute = FALSE, uris = "http://www.ebi.ac.uk/ontology-lookup/OntologyQuery", 
        fromConverter = function () 
        NULL, toConverter = function () 
        NULL, count = numeric(0), abstract = logical(0), name = "getTermsByNameReturn", 
        ns = character(0), nsuri = "http://www.ebi.ac.uk/ontology-lookup/OntologyQuery", 
        default = NA_character_, Rname = character(0), documentation = character(0)), 
        name = "getTermsByNameResponse", attributes = list(), 
        xmlAttrs = character(0), ns = character(0), nsuri = "http://www.ebi.ac.uk/ontology-lookup/OntologyQuery", 
        default = NULL, Rname = character(0), documentation = character(0)), 
    .opts = list(), nameSpaces = "1.2", .soapHeader = NULL) 
{
    .SOAP(server, "getTermsByName", parameters = as(parameters, 
        "partialName.ontologyName.reverseKeyOrder"), action = "getTermsByName", 
        xmlns = "http://www.ebi.ac.uk/ontology-lookup/OntologyQuery", 
        .types = list(parameters = new("Element", type = new("ClassDefinition", 
            slotTypes = list(partialName = new("LocalElement", 
                count = c(1L, 1L), type = new("PrimitiveSOAPType", 
                  fromConverter = function () 
                  NULL, toConverter = .Primitive("as.character"), 
                  count = c(1L, 1L), abstract = logical(0), name = "string", 
                  ns = "xsd", nsuri = "http://www.w3.org/2001/XMLSchema", 
                  default = NA_character_, Rname = character(0), 
                  documentation = character(0)), name = "partialName", 
                attributes = list(), xmlAttrs = character(0), 
                ns = character(0), nsuri = "http://www.ebi.ac.uk/ontology-lookup/OntologyQuery", 
                default = NA_character_, Rname = character(0), 
                documentation = character(0)), ontologyName = new("LocalElement", 
                count = c(1L, 1L), type = new("PrimitiveSOAPType", 
                  fromConverter = function () 
                  NULL, toConverter = .Primitive("as.character"), 
                  count = c(1L, 1L), abstract = logical(0), name = "string", 
                  ns = "xsd", nsuri = "http://www.w3.org/2001/XMLSchema", 
                  default = NA_character_, Rname = character(0), 
                  documentation = character(0)), name = "ontologyName", 
                attributes = list(), xmlAttrs = character(0), 
                ns = character(0), nsuri = "http://www.ebi.ac.uk/ontology-lookup/OntologyQuery", 
                default = NA_character_, Rname = character(0), 
                documentation = character(0)), reverseKeyOrder = new("LocalElement", 
                count = c(1L, 1L), type = new("PrimitiveSOAPType", 
                  fromConverter = function () 
                  NULL, toConverter = .Primitive("as.logical"), 
                  count = c(1L, 1L), abstract = logical(0), name = "boolean", 
                  ns = "xsd", nsuri = "http://www.w3.org/2001/XMLSchema", 
                  default = NA_character_, Rname = character(0), 
                  documentation = character(0)), name = "reverseKeyOrder", 
                attributes = list(), xmlAttrs = character(0), 
                ns = character(0), nsuri = "http://www.ebi.ac.uk/ontology-lookup/OntologyQuery", 
                default = NA_character_, Rname = character(0), 
                documentation = character(0))), isAttribute = c(FALSE, 
            FALSE, FALSE), uris = c("http://www.ebi.ac.uk/ontology-lookup/OntologyQuery", 
            "http://www.ebi.ac.uk/ontology-lookup/OntologyQuery", 
            "http://www.ebi.ac.uk/ontology-lookup/OntologyQuery"
            ), fromConverter = function () 
            NULL, toConverter = function () 
            NULL, count = numeric(0), abstract = logical(0), 
            name = "partialName.ontologyName.reverseKeyOrder", 
            ns = character(0), nsuri = "http://www.ebi.ac.uk/ontology-lookup/OntologyQuery", 
            default = NA_character_, Rname = character(0), documentation = character(0)), 
            name = "getTermsByName", attributes = list(), xmlAttrs = character(0), 
            ns = character(0), nsuri = "http://www.ebi.ac.uk/ontology-lookup/OntologyQuery", 
            default = NULL, Rname = character(0), documentation = character(0))), 
        .convert = .convert, .header = c(Accept = "text/xml", 
            Accept = "multipart/*", `Content-Type` = "text/xml; charset=utf-8", 
            SOAPAction = "\"getTermsByName\""), .opts = .opts, 
        .literal = TRUE, nameSpaces = nameSpaces, .elementFormQualified = TRUE, 
        .returnNodeName = "getTermsByNameResponse", .soapHeader = .soapHeader)
}

class( getTermsByName ) = c( 'SerializedFunction', 'WSDLGeneratedSOAPFunction' )

## getTermsByExactName = 
## function (parameters = list(...), ..., server = new("HTTPSOAPServer", 
##     host = "www.ebi.ac.uk", port = NA_integer_, url = "/ontology-lookup/services/OntologyQuery"), 
##     .convert = new("Element", type = new("ClassDefinition", slotTypes = list(getTermsByExactNameReturn = new("LocalElement", 
##         count = c(1L, 1L), type = new("SimpleSequenceType", elementType = character(0), 
##             elType = new("ClassDefinition", slotTypes = list(key = new("LocalElement", 
##                 count = c(1L, 1L), type = new("PrimitiveSOAPType", 
##                   fromConverter = function () 
##                   NULL, toConverter = function () 
##                   NULL, count = c(1L, 1L), abstract = logical(0), 
##                   name = "anyType", ns = "xsd", nsuri = "http://www.w3.org/2001/XMLSchema", 
##                   default = NA_character_, Rname = character(0), 
##                   documentation = character(0)), name = "key", 
##                 attributes = list(), xmlAttrs = character(0), 
##                 ns = character(0), nsuri = "http://xml.apache.org/xml-soap", 
##                 default = NA_character_, Rname = character(0), 
##                 documentation = character(0)), value = new("LocalElement", 
##                 count = c(1L, 1L), type = new("PrimitiveSOAPType", 
##                   fromConverter = function () 
##                   NULL, toConverter = function () 
##                   NULL, count = c(1L, 1L), abstract = logical(0), 
##                   name = "anyType", ns = "xsd", nsuri = "http://www.w3.org/2001/XMLSchema", 
##                   default = NA_character_, Rname = character(0), 
##                   documentation = character(0)), name = "value", 
##                 attributes = list(), xmlAttrs = character(0), 
##                 ns = character(0), nsuri = "http://xml.apache.org/xml-soap", 
##                 default = NA_character_, Rname = character(0), 
##                 documentation = character(0))), isAttribute = c(FALSE, 
##             FALSE), uris = c("http://xml.apache.org/xml-soap", 
##             "http://xml.apache.org/xml-soap"), fromConverter = function () 
##             NULL, toConverter = function () 
##             NULL, count = numeric(0), abstract = logical(0), 
##                 name = "mapItem", ns = character(0), nsuri = "http://xml.apache.org/xml-soap", 
##                 default = NA_character_, Rname = character(0), 
##                 documentation = character(0)), fromConverter = function () 
##             NULL, toConverter = function () 
##             NULL, count = c(0, Inf), abstract = logical(0), name = "Map", 
##             ns = character(0), nsuri = "http://xml.apache.org/xml-soap", 
##             default = NA_character_, Rname = character(0), documentation = character(0)), 
##         name = "getTermsByExactNameReturn", attributes = list(), 
##         xmlAttrs = character(0), ns = character(0), nsuri = "http://www.ebi.ac.uk/ontology-lookup/OntologyQuery", 
##         default = NA_character_, Rname = character(0), documentation = character(0))), 
##         isAttribute = FALSE, uris = "http://www.ebi.ac.uk/ontology-lookup/OntologyQuery", 
##         fromConverter = function () 
##         NULL, toConverter = function () 
##         NULL, count = numeric(0), abstract = logical(0), name = "getTermsByExactNameReturn", 
##         ns = character(0), nsuri = "http://www.ebi.ac.uk/ontology-lookup/OntologyQuery", 
##         default = NA_character_, Rname = character(0), documentation = character(0)), 
##         name = "getTermsByExactNameResponse", attributes = list(), 
##         xmlAttrs = character(0), ns = character(0), nsuri = "http://www.ebi.ac.uk/ontology-lookup/OntologyQuery", 
##         default = NULL, Rname = character(0), documentation = character(0)), 
##     .opts = list(), nameSpaces = "1.2", .soapHeader = NULL) 
## {
##     .SOAP(server, "getTermsByExactName", parameters = as(parameters, 
##         "exactName.ontologyName"), action = "getTermsByExactName", 
##         xmlns = "http://www.ebi.ac.uk/ontology-lookup/OntologyQuery", 
##         .types = list(parameters = new("Element", type = new("ClassDefinition", 
##             slotTypes = list(exactName = new("LocalElement", 
##                 count = c(1L, 1L), type = new("PrimitiveSOAPType", 
##                   fromConverter = function () 
##                   NULL, toConverter = .Primitive("as.character"), 
##                   count = c(1L, 1L), abstract = logical(0), name = "string", 
##                   ns = "xsd", nsuri = "http://www.w3.org/2001/XMLSchema", 
##                   default = NA_character_, Rname = character(0), 
##                   documentation = character(0)), name = "exactName", 
##                 attributes = list(), xmlAttrs = character(0), 
##                 ns = character(0), nsuri = "http://www.ebi.ac.uk/ontology-lookup/OntologyQuery", 
##                 default = NA_character_, Rname = character(0), 
##                 documentation = character(0)), ontologyName = new("LocalElement", 
##                 count = c(1L, 1L), type = new("PrimitiveSOAPType", 
##                   fromConverter = function () 
##                   NULL, toConverter = .Primitive("as.character"), 
##                   count = c(1L, 1L), abstract = logical(0), name = "string", 
##                   ns = "xsd", nsuri = "http://www.w3.org/2001/XMLSchema", 
##                   default = NA_character_, Rname = character(0), 
##                   documentation = character(0)), name = "ontologyName", 
##                 attributes = list(), xmlAttrs = character(0), 
##                 ns = character(0), nsuri = "http://www.ebi.ac.uk/ontology-lookup/OntologyQuery", 
##                 default = NA_character_, Rname = character(0), 
##                 documentation = character(0))), isAttribute = c(FALSE, 
##             FALSE), uris = c("http://www.ebi.ac.uk/ontology-lookup/OntologyQuery", 
##             "http://www.ebi.ac.uk/ontology-lookup/OntologyQuery"
##             ), fromConverter = function () 
##             NULL, toConverter = function () 
##             NULL, count = numeric(0), abstract = logical(0), 
##             name = "exactName.ontologyName", ns = character(0), 
##             nsuri = "http://www.ebi.ac.uk/ontology-lookup/OntologyQuery", 
##             default = NA_character_, Rname = character(0), documentation = character(0)), 
##             name = "getTermsByExactName", attributes = list(), 
##             xmlAttrs = character(0), ns = character(0), nsuri = "http://www.ebi.ac.uk/ontology-lookup/OntologyQuery", 
##             default = NULL, Rname = character(0), documentation = character(0))), 
##         .convert = .convert, .header = c(Accept = "text/xml", 
##             Accept = "multipart/*", `Content-Type` = "text/xml; charset=utf-8", 
##             SOAPAction = "\"getTermsByExactName\""), .opts = .opts, 
##         .literal = TRUE, nameSpaces = nameSpaces, .elementFormQualified = TRUE, 
##         .returnNodeName = "getTermsByExactNameResponse", .soapHeader = .soapHeader)
## }
##
## class( getTermsByExactName ) = c( 'SerializedFunction', 'WSDLGeneratedSOAPFunction' )

getPrefixedTermsByName = 
function (parameters = list(...), ..., server = new("HTTPSOAPServer", 
    host = "www.ebi.ac.uk", port = NA_integer_, url = "/ontology-lookup/services/OntologyQuery"), 
    .convert = new("Element", type = new("ClassDefinition", slotTypes = list(getPrefixedTermsByNameReturn = new("LocalElement", 
        count = c(1L, 1L), type = new("SimpleSequenceType", elementType = character(0), 
            elType = new("ClassDefinition", slotTypes = list(key = new("LocalElement", 
                count = c(1L, 1L), type = new("PrimitiveSOAPType", 
                  fromConverter = function () 
                  NULL, toConverter = function () 
                  NULL, count = c(1L, 1L), abstract = logical(0), 
                  name = "anyType", ns = "xsd", nsuri = "http://www.w3.org/2001/XMLSchema", 
                  default = NA_character_, Rname = character(0), 
                  documentation = character(0)), name = "key", 
                attributes = list(), xmlAttrs = character(0), 
                ns = character(0), nsuri = "http://xml.apache.org/xml-soap", 
                default = NA_character_, Rname = character(0), 
                documentation = character(0)), value = new("LocalElement", 
                count = c(1L, 1L), type = new("PrimitiveSOAPType", 
                  fromConverter = function () 
                  NULL, toConverter = function () 
                  NULL, count = c(1L, 1L), abstract = logical(0), 
                  name = "anyType", ns = "xsd", nsuri = "http://www.w3.org/2001/XMLSchema", 
                  default = NA_character_, Rname = character(0), 
                  documentation = character(0)), name = "value", 
                attributes = list(), xmlAttrs = character(0), 
                ns = character(0), nsuri = "http://xml.apache.org/xml-soap", 
                default = NA_character_, Rname = character(0), 
                documentation = character(0))), isAttribute = c(FALSE, 
            FALSE), uris = c("http://xml.apache.org/xml-soap", 
            "http://xml.apache.org/xml-soap"), fromConverter = function () 
            NULL, toConverter = function () 
            NULL, count = numeric(0), abstract = logical(0), 
                name = "mapItem", ns = character(0), nsuri = "http://xml.apache.org/xml-soap", 
                default = NA_character_, Rname = character(0), 
                documentation = character(0)), fromConverter = function () 
            NULL, toConverter = function () 
            NULL, count = c(0, Inf), abstract = logical(0), name = "Map", 
            ns = character(0), nsuri = "http://xml.apache.org/xml-soap", 
            default = NA_character_, Rname = character(0), documentation = character(0)), 
        name = "getPrefixedTermsByNameReturn", attributes = list(), 
        xmlAttrs = character(0), ns = character(0), nsuri = "http://www.ebi.ac.uk/ontology-lookup/OntologyQuery", 
        default = NA_character_, Rname = character(0), documentation = character(0))), 
        isAttribute = FALSE, uris = "http://www.ebi.ac.uk/ontology-lookup/OntologyQuery", 
        fromConverter = function () 
        NULL, toConverter = function () 
        NULL, count = numeric(0), abstract = logical(0), name = "getPrefixedTermsByNameReturn", 
        ns = character(0), nsuri = "http://www.ebi.ac.uk/ontology-lookup/OntologyQuery", 
        default = NA_character_, Rname = character(0), documentation = character(0)), 
        name = "getPrefixedTermsByNameResponse", attributes = list(), 
        xmlAttrs = character(0), ns = character(0), nsuri = "http://www.ebi.ac.uk/ontology-lookup/OntologyQuery", 
        default = NULL, Rname = character(0), documentation = character(0)), 
    .opts = list(), nameSpaces = "1.2", .soapHeader = NULL) 
{
    .SOAP(server, "getPrefixedTermsByName", parameters = as(parameters, 
        "partialName.reverseKeyOrder"), action = "getPrefixedTermsByName", 
        xmlns = "http://www.ebi.ac.uk/ontology-lookup/OntologyQuery", 
        .types = list(parameters = new("Element", type = new("ClassDefinition", 
            slotTypes = list(partialName = new("LocalElement", 
                count = c(1L, 1L), type = new("PrimitiveSOAPType", 
                  fromConverter = function () 
                  NULL, toConverter = .Primitive("as.character"), 
                  count = c(1L, 1L), abstract = logical(0), name = "string", 
                  ns = "xsd", nsuri = "http://www.w3.org/2001/XMLSchema", 
                  default = NA_character_, Rname = character(0), 
                  documentation = character(0)), name = "partialName", 
                attributes = list(), xmlAttrs = character(0), 
                ns = character(0), nsuri = "http://www.ebi.ac.uk/ontology-lookup/OntologyQuery", 
                default = NA_character_, Rname = character(0), 
                documentation = character(0)), reverseKeyOrder = new("LocalElement", 
                count = c(1L, 1L), type = new("PrimitiveSOAPType", 
                  fromConverter = function () 
                  NULL, toConverter = .Primitive("as.logical"), 
                  count = c(1L, 1L), abstract = logical(0), name = "boolean", 
                  ns = "xsd", nsuri = "http://www.w3.org/2001/XMLSchema", 
                  default = NA_character_, Rname = character(0), 
                  documentation = character(0)), name = "reverseKeyOrder", 
                attributes = list(), xmlAttrs = character(0), 
                ns = character(0), nsuri = "http://www.ebi.ac.uk/ontology-lookup/OntologyQuery", 
                default = NA_character_, Rname = character(0), 
                documentation = character(0))), isAttribute = c(FALSE, 
            FALSE), uris = c("http://www.ebi.ac.uk/ontology-lookup/OntologyQuery", 
            "http://www.ebi.ac.uk/ontology-lookup/OntologyQuery"
            ), fromConverter = function () 
            NULL, toConverter = function () 
            NULL, count = numeric(0), abstract = logical(0), 
            name = "partialName.reverseKeyOrder", ns = character(0), 
            nsuri = "http://www.ebi.ac.uk/ontology-lookup/OntologyQuery", 
            default = NA_character_, Rname = character(0), documentation = character(0)), 
            name = "getPrefixedTermsByName", attributes = list(), 
            xmlAttrs = character(0), ns = character(0), nsuri = "http://www.ebi.ac.uk/ontology-lookup/OntologyQuery", 
            default = NULL, Rname = character(0), documentation = character(0))), 
        .convert = .convert, .header = c(Accept = "text/xml", 
            Accept = "multipart/*", `Content-Type` = "text/xml; charset=utf-8", 
            SOAPAction = "\"getPrefixedTermsByName\""), .opts = .opts, 
        .literal = TRUE, nameSpaces = nameSpaces, .elementFormQualified = TRUE, 
        .returnNodeName = "getPrefixedTermsByNameResponse", .soapHeader = .soapHeader)
}

class( getPrefixedTermsByName ) = c( 'SerializedFunction', 'WSDLGeneratedSOAPFunction' )

getTermParents = 
function (parameters = list(...), ..., server = new("HTTPSOAPServer", 
    host = "www.ebi.ac.uk", port = NA_integer_, url = "/ontology-lookup/services/OntologyQuery"), 
    .convert = new("Element", type = new("ClassDefinition", slotTypes = list(getTermParentsReturn = new("LocalElement", 
        count = c(1L, 1L), type = new("SimpleSequenceType", elementType = character(0), 
            elType = new("ClassDefinition", slotTypes = list(key = new("LocalElement", 
                count = c(1L, 1L), type = new("PrimitiveSOAPType", 
                  fromConverter = function () 
                  NULL, toConverter = function () 
                  NULL, count = c(1L, 1L), abstract = logical(0), 
                  name = "anyType", ns = "xsd", nsuri = "http://www.w3.org/2001/XMLSchema", 
                  default = NA_character_, Rname = character(0), 
                  documentation = character(0)), name = "key", 
                attributes = list(), xmlAttrs = character(0), 
                ns = character(0), nsuri = "http://xml.apache.org/xml-soap", 
                default = NA_character_, Rname = character(0), 
                documentation = character(0)), value = new("LocalElement", 
                count = c(1L, 1L), type = new("PrimitiveSOAPType", 
                  fromConverter = function () 
                  NULL, toConverter = function () 
                  NULL, count = c(1L, 1L), abstract = logical(0), 
                  name = "anyType", ns = "xsd", nsuri = "http://www.w3.org/2001/XMLSchema", 
                  default = NA_character_, Rname = character(0), 
                  documentation = character(0)), name = "value", 
                attributes = list(), xmlAttrs = character(0), 
                ns = character(0), nsuri = "http://xml.apache.org/xml-soap", 
                default = NA_character_, Rname = character(0), 
                documentation = character(0))), isAttribute = c(FALSE, 
            FALSE), uris = c("http://xml.apache.org/xml-soap", 
            "http://xml.apache.org/xml-soap"), fromConverter = function () 
            NULL, toConverter = function () 
            NULL, count = numeric(0), abstract = logical(0), 
                name = "mapItem", ns = character(0), nsuri = "http://xml.apache.org/xml-soap", 
                default = NA_character_, Rname = character(0), 
                documentation = character(0)), fromConverter = function () 
            NULL, toConverter = function () 
            NULL, count = c(0, Inf), abstract = logical(0), name = "Map", 
            ns = character(0), nsuri = "http://xml.apache.org/xml-soap", 
            default = NA_character_, Rname = character(0), documentation = character(0)), 
        name = "getTermParentsReturn", attributes = list(), xmlAttrs = character(0), 
        ns = character(0), nsuri = "http://www.ebi.ac.uk/ontology-lookup/OntologyQuery", 
        default = NA_character_, Rname = character(0), documentation = character(0))), 
        isAttribute = FALSE, uris = "http://www.ebi.ac.uk/ontology-lookup/OntologyQuery", 
        fromConverter = function () 
        NULL, toConverter = function () 
        NULL, count = numeric(0), abstract = logical(0), name = "getTermParentsReturn", 
        ns = character(0), nsuri = "http://www.ebi.ac.uk/ontology-lookup/OntologyQuery", 
        default = NA_character_, Rname = character(0), documentation = character(0)), 
        name = "getTermParentsResponse", attributes = list(), 
        xmlAttrs = character(0), ns = character(0), nsuri = "http://www.ebi.ac.uk/ontology-lookup/OntologyQuery", 
        default = NULL, Rname = character(0), documentation = character(0)), 
    .opts = list(), nameSpaces = "1.2", .soapHeader = NULL) 
{
    .SOAP(server, "getTermParents", parameters = as(parameters, 
        "termId.ontologyName"), action = "getTermParents", xmlns = "http://www.ebi.ac.uk/ontology-lookup/OntologyQuery", 
        .types = list(parameters = new("Element", type = new("ClassDefinition", 
            slotTypes = list(termId = new("LocalElement", count = c(1L, 
            1L), type = new("PrimitiveSOAPType", fromConverter = function () 
            NULL, toConverter = .Primitive("as.character"), count = c(1L, 
            1L), abstract = logical(0), name = "string", ns = "xsd", 
                nsuri = "http://www.w3.org/2001/XMLSchema", default = NA_character_, 
                Rname = character(0), documentation = character(0)), 
                name = "termId", attributes = list(), xmlAttrs = character(0), 
                ns = character(0), nsuri = "http://www.ebi.ac.uk/ontology-lookup/OntologyQuery", 
                default = NA_character_, Rname = character(0), 
                documentation = character(0)), ontologyName = new("LocalElement", 
                count = c(1L, 1L), type = new("PrimitiveSOAPType", 
                  fromConverter = function () 
                  NULL, toConverter = .Primitive("as.character"), 
                  count = c(1L, 1L), abstract = logical(0), name = "string", 
                  ns = "xsd", nsuri = "http://www.w3.org/2001/XMLSchema", 
                  default = NA_character_, Rname = character(0), 
                  documentation = character(0)), name = "ontologyName", 
                attributes = list(), xmlAttrs = character(0), 
                ns = character(0), nsuri = "http://www.ebi.ac.uk/ontology-lookup/OntologyQuery", 
                default = NA_character_, Rname = character(0), 
                documentation = character(0))), isAttribute = c(FALSE, 
            FALSE), uris = c("http://www.ebi.ac.uk/ontology-lookup/OntologyQuery", 
            "http://www.ebi.ac.uk/ontology-lookup/OntologyQuery"
            ), fromConverter = function () 
            NULL, toConverter = function () 
            NULL, count = numeric(0), abstract = logical(0), 
            name = "termId.ontologyName", ns = character(0), 
            nsuri = "http://www.ebi.ac.uk/ontology-lookup/OntologyQuery", 
            default = NA_character_, Rname = character(0), documentation = character(0)), 
            name = "getTermParents", attributes = list(), xmlAttrs = character(0), 
            ns = character(0), nsuri = "http://www.ebi.ac.uk/ontology-lookup/OntologyQuery", 
            default = NULL, Rname = character(0), documentation = character(0))), 
        .convert = .convert, .header = c(Accept = "text/xml", 
            Accept = "multipart/*", `Content-Type` = "text/xml; charset=utf-8", 
            SOAPAction = "\"getTermParents\""), .opts = .opts, 
        .literal = TRUE, nameSpaces = nameSpaces, .elementFormQualified = TRUE, 
        .returnNodeName = "getTermParentsResponse", .soapHeader = .soapHeader)
}

class( getTermParents ) = c( 'SerializedFunction', 'WSDLGeneratedSOAPFunction' )

## getTermChildren = 
## function (parameters = list(...), ..., server = new("HTTPSOAPServer", 
##     host = "www.ebi.ac.uk", port = NA_integer_, url = "/ontology-lookup/services/OntologyQuery"), 
##     .convert = new("Element", type = new("ClassDefinition", slotTypes = list(getTermChildrenReturn = new("LocalElement", 
##         count = c(1L, 1L), type = new("SimpleSequenceType", elementType = character(0), 
##             elType = new("ClassDefinition", slotTypes = list(key = new("LocalElement", 
##                 count = c(1L, 1L), type = new("PrimitiveSOAPType", 
##                   fromConverter = function () 
##                   NULL, toConverter = function () 
##                   NULL, count = c(1L, 1L), abstract = logical(0), 
##                   name = "anyType", ns = "xsd", nsuri = "http://www.w3.org/2001/XMLSchema", 
##                   default = NA_character_, Rname = character(0), 
##                   documentation = character(0)), name = "key", 
##                 attributes = list(), xmlAttrs = character(0), 
##                 ns = character(0), nsuri = "http://xml.apache.org/xml-soap", 
##                 default = NA_character_, Rname = character(0), 
##                 documentation = character(0)), value = new("LocalElement", 
##                 count = c(1L, 1L), type = new("PrimitiveSOAPType", 
##                   fromConverter = function () 
##                   NULL, toConverter = function () 
##                   NULL, count = c(1L, 1L), abstract = logical(0), 
##                   name = "anyType", ns = "xsd", nsuri = "http://www.w3.org/2001/XMLSchema", 
##                   default = NA_character_, Rname = character(0), 
##                   documentation = character(0)), name = "value", 
##                 attributes = list(), xmlAttrs = character(0), 
##                 ns = character(0), nsuri = "http://xml.apache.org/xml-soap", 
##                 default = NA_character_, Rname = character(0), 
##                 documentation = character(0))), isAttribute = c(FALSE, 
##             FALSE), uris = c("http://xml.apache.org/xml-soap", 
##             "http://xml.apache.org/xml-soap"), fromConverter = function () 
##             NULL, toConverter = function () 
##             NULL, count = numeric(0), abstract = logical(0), 
##                 name = "mapItem", ns = character(0), nsuri = "http://xml.apache.org/xml-soap", 
##                 default = NA_character_, Rname = character(0), 
##                 documentation = character(0)), fromConverter = function () 
##             NULL, toConverter = function () 
##             NULL, count = c(0, Inf), abstract = logical(0), name = "Map", 
##             ns = character(0), nsuri = "http://xml.apache.org/xml-soap", 
##             default = NA_character_, Rname = character(0), documentation = character(0)), 
##         name = "getTermChildrenReturn", attributes = list(), 
##         xmlAttrs = character(0), ns = character(0), nsuri = "http://www.ebi.ac.uk/ontology-lookup/OntologyQuery", 
##         default = NA_character_, Rname = character(0), documentation = character(0))), 
##         isAttribute = FALSE, uris = "http://www.ebi.ac.uk/ontology-lookup/OntologyQuery", 
##         fromConverter = function () 
##         NULL, toConverter = function () 
##         NULL, count = numeric(0), abstract = logical(0), name = "getTermChildrenReturn", 
##         ns = character(0), nsuri = "http://www.ebi.ac.uk/ontology-lookup/OntologyQuery", 
##         default = NA_character_, Rname = character(0), documentation = character(0)), 
##         name = "getTermChildrenResponse", attributes = list(), 
##         xmlAttrs = character(0), ns = character(0), nsuri = "http://www.ebi.ac.uk/ontology-lookup/OntologyQuery", 
##         default = NULL, Rname = character(0), documentation = character(0)), 
##     .opts = list(), nameSpaces = "1.2", .soapHeader = NULL) 
## {
##     .SOAP(server, "getTermChildren", parameters = as(parameters, 
##         "termId.ontologyName.distance.relationTypes"), action = "getTermChildren", 
##         xmlns = "http://www.ebi.ac.uk/ontology-lookup/OntologyQuery", 
##         .types = list(parameters = new("Element", type = new("ClassDefinition", 
##             slotTypes = list(termId = new("LocalElement", count = c(1L, 
##             1L), type = new("PrimitiveSOAPType", fromConverter = function () 
##             NULL, toConverter = .Primitive("as.character"), count = c(1L, 
##             1L), abstract = logical(0), name = "string", ns = "xsd", 
##                 nsuri = "http://www.w3.org/2001/XMLSchema", default = NA_character_, 
##                 Rname = character(0), documentation = character(0)), 
##                 name = "termId", attributes = list(), xmlAttrs = character(0), 
##                 ns = character(0), nsuri = "http://www.ebi.ac.uk/ontology-lookup/OntologyQuery", 
##                 default = NA_character_, Rname = character(0), 
##                 documentation = character(0)), ontologyName = new("LocalElement", 
##                 count = c(1L, 1L), type = new("PrimitiveSOAPType", 
##                   fromConverter = function () 
##                   NULL, toConverter = .Primitive("as.character"), 
##                   count = c(1L, 1L), abstract = logical(0), name = "string", 
##                   ns = "xsd", nsuri = "http://www.w3.org/2001/XMLSchema", 
##                   default = NA_character_, Rname = character(0), 
##                   documentation = character(0)), name = "ontologyName", 
##                 attributes = list(), xmlAttrs = character(0), 
##                 ns = character(0), nsuri = "http://www.ebi.ac.uk/ontology-lookup/OntologyQuery", 
##                 default = NA_character_, Rname = character(0), 
##                 documentation = character(0)), distance = new("LocalElement", 
##                 count = c(1L, 1L), type = new("PrimitiveSOAPType", 
##                   fromConverter = function () 
##                   NULL, toConverter = .Primitive("as.integer"), 
##                   count = c(1L, 1L), abstract = logical(0), name = "int", 
##                   ns = "xsd", nsuri = "http://www.w3.org/2001/XMLSchema", 
##                   default = NA_character_, Rname = character(0), 
##                   documentation = character(0)), name = "distance", 
##                 attributes = list(), xmlAttrs = character(0), 
##                 ns = character(0), nsuri = "http://www.ebi.ac.uk/ontology-lookup/OntologyQuery", 
##                 default = NA_character_, Rname = character(0), 
##                 documentation = character(0)), relationTypes = new("LocalElement", 
##                 count = c(1, Inf), type = new("PrimitiveSOAPType", 
##                   fromConverter = function () 
##                   NULL, toConverter = .Primitive("as.integer"), 
##                   count = c(1, Inf), abstract = logical(0), name = "int", 
##                   ns = "xsd", nsuri = "http://www.w3.org/2001/XMLSchema", 
##                   default = NA_character_, Rname = character(0), 
##                   documentation = character(0)), name = "relationTypes", 
##                 attributes = list(), xmlAttrs = character(0), 
##                 ns = character(0), nsuri = "http://www.ebi.ac.uk/ontology-lookup/OntologyQuery", 
##                 default = NA_character_, Rname = character(0), 
##                 documentation = character(0))), isAttribute = c(FALSE, 
##             FALSE, FALSE, FALSE), uris = c("http://www.ebi.ac.uk/ontology-lookup/OntologyQuery", 
##             "http://www.ebi.ac.uk/ontology-lookup/OntologyQuery", 
##             "http://www.ebi.ac.uk/ontology-lookup/OntologyQuery", 
##             "http://www.ebi.ac.uk/ontology-lookup/OntologyQuery"
##             ), fromConverter = function () 
##             NULL, toConverter = function () 
##             NULL, count = numeric(0), abstract = logical(0), 
##             name = "termId.ontologyName.distance.relationTypes", 
##             ns = character(0), nsuri = "http://www.ebi.ac.uk/ontology-lookup/OntologyQuery", 
##             default = NA_character_, Rname = character(0), documentation = character(0)), 
##             name = "getTermChildren", attributes = list(), xmlAttrs = character(0), 
##             ns = character(0), nsuri = "http://www.ebi.ac.uk/ontology-lookup/OntologyQuery", 
##             default = NULL, Rname = character(0), documentation = character(0))), 
##         .convert = .convert, .header = c(Accept = "text/xml", 
##             Accept = "multipart/*", `Content-Type` = "text/xml; charset=utf-8", 
##             SOAPAction = "\"getTermChildren\""), .opts = .opts, 
##         .literal = TRUE, nameSpaces = nameSpaces, .elementFormQualified = TRUE, 
##         .returnNodeName = "getTermChildrenResponse", .soapHeader = .soapHeader)
## }
##
## class( getTermChildren ) = c( 'SerializedFunction', 'WSDLGeneratedSOAPFunction' )

getTermRelations = 
function (parameters = list(...), ..., server = new("HTTPSOAPServer", 
    host = "www.ebi.ac.uk", port = NA_integer_, url = "/ontology-lookup/services/OntologyQuery"), 
    .convert = new("Element", type = new("ClassDefinition", slotTypes = list(getTermRelationsReturn = new("LocalElement", 
        count = c(1L, 1L), type = new("SimpleSequenceType", elementType = character(0), 
            elType = new("ClassDefinition", slotTypes = list(key = new("LocalElement", 
                count = c(1L, 1L), type = new("PrimitiveSOAPType", 
                  fromConverter = function () 
                  NULL, toConverter = function () 
                  NULL, count = c(1L, 1L), abstract = logical(0), 
                  name = "anyType", ns = "xsd", nsuri = "http://www.w3.org/2001/XMLSchema", 
                  default = NA_character_, Rname = character(0), 
                  documentation = character(0)), name = "key", 
                attributes = list(), xmlAttrs = character(0), 
                ns = character(0), nsuri = "http://xml.apache.org/xml-soap", 
                default = NA_character_, Rname = character(0), 
                documentation = character(0)), value = new("LocalElement", 
                count = c(1L, 1L), type = new("PrimitiveSOAPType", 
                  fromConverter = function () 
                  NULL, toConverter = function () 
                  NULL, count = c(1L, 1L), abstract = logical(0), 
                  name = "anyType", ns = "xsd", nsuri = "http://www.w3.org/2001/XMLSchema", 
                  default = NA_character_, Rname = character(0), 
                  documentation = character(0)), name = "value", 
                attributes = list(), xmlAttrs = character(0), 
                ns = character(0), nsuri = "http://xml.apache.org/xml-soap", 
                default = NA_character_, Rname = character(0), 
                documentation = character(0))), isAttribute = c(FALSE, 
            FALSE), uris = c("http://xml.apache.org/xml-soap", 
            "http://xml.apache.org/xml-soap"), fromConverter = function () 
            NULL, toConverter = function () 
            NULL, count = numeric(0), abstract = logical(0), 
                name = "mapItem", ns = character(0), nsuri = "http://xml.apache.org/xml-soap", 
                default = NA_character_, Rname = character(0), 
                documentation = character(0)), fromConverter = function () 
            NULL, toConverter = function () 
            NULL, count = c(0, Inf), abstract = logical(0), name = "Map", 
            ns = character(0), nsuri = "http://xml.apache.org/xml-soap", 
            default = NA_character_, Rname = character(0), documentation = character(0)), 
        name = "getTermRelationsReturn", attributes = list(), 
        xmlAttrs = character(0), ns = character(0), nsuri = "http://www.ebi.ac.uk/ontology-lookup/OntologyQuery", 
        default = NA_character_, Rname = character(0), documentation = character(0))), 
        isAttribute = FALSE, uris = "http://www.ebi.ac.uk/ontology-lookup/OntologyQuery", 
        fromConverter = function () 
        NULL, toConverter = function () 
        NULL, count = numeric(0), abstract = logical(0), name = "getTermRelationsReturn", 
        ns = character(0), nsuri = "http://www.ebi.ac.uk/ontology-lookup/OntologyQuery", 
        default = NA_character_, Rname = character(0), documentation = character(0)), 
        name = "getTermRelationsResponse", attributes = list(), 
        xmlAttrs = character(0), ns = character(0), nsuri = "http://www.ebi.ac.uk/ontology-lookup/OntologyQuery", 
        default = NULL, Rname = character(0), documentation = character(0)), 
    .opts = list(), nameSpaces = "1.2", .soapHeader = NULL) 
{
    .SOAP(server, "getTermRelations", parameters = as(parameters, 
        "termId.ontologyName"), action = "getTermRelations", 
        xmlns = "http://www.ebi.ac.uk/ontology-lookup/OntologyQuery", 
        .types = list(parameters = new("Element", type = new("ClassDefinition", 
            slotTypes = list(termId = new("LocalElement", count = c(1L, 
            1L), type = new("PrimitiveSOAPType", fromConverter = function () 
            NULL, toConverter = .Primitive("as.character"), count = c(1L, 
            1L), abstract = logical(0), name = "string", ns = "xsd", 
                nsuri = "http://www.w3.org/2001/XMLSchema", default = NA_character_, 
                Rname = character(0), documentation = character(0)), 
                name = "termId", attributes = list(), xmlAttrs = character(0), 
                ns = character(0), nsuri = "http://www.ebi.ac.uk/ontology-lookup/OntologyQuery", 
                default = NA_character_, Rname = character(0), 
                documentation = character(0)), ontologyName = new("LocalElement", 
                count = c(1L, 1L), type = new("PrimitiveSOAPType", 
                  fromConverter = function () 
                  NULL, toConverter = .Primitive("as.character"), 
                  count = c(1L, 1L), abstract = logical(0), name = "string", 
                  ns = "xsd", nsuri = "http://www.w3.org/2001/XMLSchema", 
                  default = NA_character_, Rname = character(0), 
                  documentation = character(0)), name = "ontologyName", 
                attributes = list(), xmlAttrs = character(0), 
                ns = character(0), nsuri = "http://www.ebi.ac.uk/ontology-lookup/OntologyQuery", 
                default = NA_character_, Rname = character(0), 
                documentation = character(0))), isAttribute = c(FALSE, 
            FALSE), uris = c("http://www.ebi.ac.uk/ontology-lookup/OntologyQuery", 
            "http://www.ebi.ac.uk/ontology-lookup/OntologyQuery"
            ), fromConverter = function () 
            NULL, toConverter = function () 
            NULL, count = numeric(0), abstract = logical(0), 
            name = "termId.ontologyName", ns = character(0), 
            nsuri = "http://www.ebi.ac.uk/ontology-lookup/OntologyQuery", 
            default = NA_character_, Rname = character(0), documentation = character(0)), 
            name = "getTermRelations", attributes = list(), xmlAttrs = character(0), 
            ns = character(0), nsuri = "http://www.ebi.ac.uk/ontology-lookup/OntologyQuery", 
            default = NULL, Rname = character(0), documentation = character(0))), 
        .convert = .convert, .header = c(Accept = "text/xml", 
            Accept = "multipart/*", `Content-Type` = "text/xml; charset=utf-8", 
            SOAPAction = "\"getTermRelations\""), .opts = .opts, 
        .literal = TRUE, nameSpaces = nameSpaces, .elementFormQualified = TRUE, 
        .returnNodeName = "getTermRelationsResponse", .soapHeader = .soapHeader)
}

class( getTermRelations ) = c( 'SerializedFunction', 'WSDLGeneratedSOAPFunction' )

## getChildrenFromRoot = 
## function (parameters = list(...), ..., server = new("HTTPSOAPServer", 
##     host = "www.ebi.ac.uk", port = NA_integer_, url = "/ontology-lookup/services/OntologyQuery"), 
##     .convert = new("Element", type = new("ClassDefinition", slotTypes = list(getChildrenFromRootReturn = new("LocalElement", 
##         count = c(1L, 1L), type = new("SimpleSequenceType", elementType = character(0), 
##             elType = new("ClassDefinition", slotTypes = list(key = new("LocalElement", 
##                 count = c(1L, 1L), type = new("PrimitiveSOAPType", 
##                   fromConverter = function () 
##                   NULL, toConverter = function () 
##                   NULL, count = c(1L, 1L), abstract = logical(0), 
##                   name = "anyType", ns = "xsd", nsuri = "http://www.w3.org/2001/XMLSchema", 
##                   default = NA_character_, Rname = character(0), 
##                   documentation = character(0)), name = "key", 
##                 attributes = list(), xmlAttrs = character(0), 
##                 ns = character(0), nsuri = "http://xml.apache.org/xml-soap", 
##                 default = NA_character_, Rname = character(0), 
##                 documentation = character(0)), value = new("LocalElement", 
##                 count = c(1L, 1L), type = new("PrimitiveSOAPType", 
##                   fromConverter = function () 
##                   NULL, toConverter = function () 
##                   NULL, count = c(1L, 1L), abstract = logical(0), 
##                   name = "anyType", ns = "xsd", nsuri = "http://www.w3.org/2001/XMLSchema", 
##                   default = NA_character_, Rname = character(0), 
##                   documentation = character(0)), name = "value", 
##                 attributes = list(), xmlAttrs = character(0), 
##                 ns = character(0), nsuri = "http://xml.apache.org/xml-soap", 
##                 default = NA_character_, Rname = character(0), 
##                 documentation = character(0))), isAttribute = c(FALSE, 
##             FALSE), uris = c("http://xml.apache.org/xml-soap", 
##             "http://xml.apache.org/xml-soap"), fromConverter = function () 
##             NULL, toConverter = function () 
##             NULL, count = numeric(0), abstract = logical(0), 
##                 name = "mapItem", ns = character(0), nsuri = "http://xml.apache.org/xml-soap", 
##                 default = NA_character_, Rname = character(0), 
##                 documentation = character(0)), fromConverter = function () 
##             NULL, toConverter = function () 
##             NULL, count = c(0, Inf), abstract = logical(0), name = "Map", 
##             ns = character(0), nsuri = "http://xml.apache.org/xml-soap", 
##             default = NA_character_, Rname = character(0), documentation = character(0)), 
##         name = "getChildrenFromRootReturn", attributes = list(), 
##         xmlAttrs = character(0), ns = character(0), nsuri = "http://www.ebi.ac.uk/ontology-lookup/OntologyQuery", 
##         default = NA_character_, Rname = character(0), documentation = character(0))), 
##         isAttribute = FALSE, uris = "http://www.ebi.ac.uk/ontology-lookup/OntologyQuery", 
##         fromConverter = function () 
##         NULL, toConverter = function () 
##         NULL, count = numeric(0), abstract = logical(0), name = "getChildrenFromRootReturn", 
##         ns = character(0), nsuri = "http://www.ebi.ac.uk/ontology-lookup/OntologyQuery", 
##         default = NA_character_, Rname = character(0), documentation = character(0)), 
##         name = "getChildrenFromRootResponse", attributes = list(), 
##         xmlAttrs = character(0), ns = character(0), nsuri = "http://www.ebi.ac.uk/ontology-lookup/OntologyQuery", 
##         default = NULL, Rname = character(0), documentation = character(0)), 
##     .opts = list(), nameSpaces = "1.2", .soapHeader = NULL) 
## {
##     .SOAP(server, "getChildrenFromRoot", parameters = as(parameters, 
##         "rootTermId.ontologyName.childrenIds"), action = "getChildrenFromRoot", 
##         xmlns = "http://www.ebi.ac.uk/ontology-lookup/OntologyQuery", 
##         .types = list(parameters = new("Element", type = new("ClassDefinition", 
##             slotTypes = list(rootTermId = new("LocalElement", 
##                 count = c(1L, 1L), type = new("PrimitiveSOAPType", 
##                   fromConverter = function () 
##                   NULL, toConverter = .Primitive("as.character"), 
##                   count = c(1L, 1L), abstract = logical(0), name = "string", 
##                   ns = "xsd", nsuri = "http://www.w3.org/2001/XMLSchema", 
##                   default = NA_character_, Rname = character(0), 
##                   documentation = character(0)), name = "rootTermId", 
##                 attributes = list(), xmlAttrs = character(0), 
##                 ns = character(0), nsuri = "http://www.ebi.ac.uk/ontology-lookup/OntologyQuery", 
##                 default = NA_character_, Rname = character(0), 
##                 documentation = character(0)), ontologyName = new("LocalElement", 
##                 count = c(1L, 1L), type = new("PrimitiveSOAPType", 
##                   fromConverter = function () 
##                   NULL, toConverter = .Primitive("as.character"), 
##                   count = c(1L, 1L), abstract = logical(0), name = "string", 
##                   ns = "xsd", nsuri = "http://www.w3.org/2001/XMLSchema", 
##                   default = NA_character_, Rname = character(0), 
##                   documentation = character(0)), name = "ontologyName", 
##                 attributes = list(), xmlAttrs = character(0), 
##                 ns = character(0), nsuri = "http://www.ebi.ac.uk/ontology-lookup/OntologyQuery", 
##                 default = NA_character_, Rname = character(0), 
##                 documentation = character(0)), childrenIds = new("LocalElement", 
##                 count = c(1L, 1L), type = new("SimpleSequenceType", 
##                   elementType = character(0), elType = new("PrimitiveSOAPType", 
##                     fromConverter = function () 
##                     NULL, toConverter = function () 
##                     NULL, count = c(0, Inf), abstract = logical(0), 
##                     name = "anyType", ns = "xsd", nsuri = "http://www.w3.org/2001/XMLSchema", 
##                     default = NA_character_, Rname = character(0), 
##                     documentation = character(0)), fromConverter = function () 
##                   NULL, toConverter = function () 
##                   NULL, count = c(0, Inf), abstract = logical(0), 
##                   name = "Vector", ns = character(0), nsuri = "http://xml.apache.org/xml-soap", 
##                   default = NA_character_, Rname = character(0), 
##                   documentation = character(0)), name = "childrenIds", 
##                 attributes = list(), xmlAttrs = character(0), 
##                 ns = character(0), nsuri = "http://www.ebi.ac.uk/ontology-lookup/OntologyQuery", 
##                 default = NA_character_, Rname = character(0), 
##                 documentation = character(0))), isAttribute = c(FALSE, 
##             FALSE, FALSE), uris = c("http://www.ebi.ac.uk/ontology-lookup/OntologyQuery", 
##             "http://www.ebi.ac.uk/ontology-lookup/OntologyQuery", 
##             "http://www.ebi.ac.uk/ontology-lookup/OntologyQuery"
##             ), fromConverter = function () 
##             NULL, toConverter = function () 
##             NULL, count = numeric(0), abstract = logical(0), 
##             name = "rootTermId.ontologyName.childrenIds", ns = character(0), 
##             nsuri = "http://www.ebi.ac.uk/ontology-lookup/OntologyQuery", 
##             default = NA_character_, Rname = character(0), documentation = character(0)), 
##             name = "getChildrenFromRoot", attributes = list(), 
##             xmlAttrs = character(0), ns = character(0), nsuri = "http://www.ebi.ac.uk/ontology-lookup/OntologyQuery", 
##             default = NULL, Rname = character(0), documentation = character(0))), 
##         .convert = .convert, .header = c(Accept = "text/xml", 
##             Accept = "multipart/*", `Content-Type` = "text/xml; charset=utf-8", 
##             SOAPAction = "\"getChildrenFromRoot\""), .opts = .opts, 
##         .literal = TRUE, nameSpaces = nameSpaces, .elementFormQualified = TRUE, 
##         .returnNodeName = "getChildrenFromRootResponse", .soapHeader = .soapHeader)
## }

## class( getChildrenFromRoot ) = c( 'SerializedFunction', 'WSDLGeneratedSOAPFunction' )

isObsolete = 
function (parameters = list(...), ..., server = new("HTTPSOAPServer", 
    host = "www.ebi.ac.uk", port = NA_integer_, url = "/ontology-lookup/services/OntologyQuery"), 
    .convert = new("Element", type = new("ClassDefinition", slotTypes = list(isObsoleteReturn = new("LocalElement", 
        count = c(1L, 1L), type = new("PrimitiveSOAPType", fromConverter = function () 
        NULL, toConverter = .Primitive("as.logical"), count = c(1L, 
        1L), abstract = logical(0), name = "boolean", ns = "xsd", 
            nsuri = "http://www.w3.org/2001/XMLSchema", default = NA_character_, 
            Rname = character(0), documentation = character(0)), 
        name = "isObsoleteReturn", attributes = list(), xmlAttrs = character(0), 
        ns = character(0), nsuri = "http://www.ebi.ac.uk/ontology-lookup/OntologyQuery", 
        default = NA_character_, Rname = character(0), documentation = character(0))), 
        isAttribute = FALSE, uris = "http://www.ebi.ac.uk/ontology-lookup/OntologyQuery", 
        fromConverter = function () 
        NULL, toConverter = function () 
        NULL, count = numeric(0), abstract = logical(0), name = "isObsoleteReturn", 
        ns = character(0), nsuri = "http://www.ebi.ac.uk/ontology-lookup/OntologyQuery", 
        default = NA_character_, Rname = character(0), documentation = character(0)), 
        name = "isObsoleteResponse", attributes = list(), xmlAttrs = character(0), 
        ns = character(0), nsuri = "http://www.ebi.ac.uk/ontology-lookup/OntologyQuery", 
        default = NULL, Rname = character(0), documentation = character(0)), 
    .opts = list(), nameSpaces = "1.2", .soapHeader = NULL) 
{
    .SOAP(server, "isObsolete", parameters = as(parameters, "termId.ontologyName"), 
        action = "isObsolete", xmlns = "http://www.ebi.ac.uk/ontology-lookup/OntologyQuery", 
        .types = list(parameters = new("Element", type = new("ClassDefinition", 
            slotTypes = list(termId = new("LocalElement", count = c(1L, 
            1L), type = new("PrimitiveSOAPType", fromConverter = function () 
            NULL, toConverter = .Primitive("as.character"), count = c(1L, 
            1L), abstract = logical(0), name = "string", ns = "xsd", 
                nsuri = "http://www.w3.org/2001/XMLSchema", default = NA_character_, 
                Rname = character(0), documentation = character(0)), 
                name = "termId", attributes = list(), xmlAttrs = character(0), 
                ns = character(0), nsuri = "http://www.ebi.ac.uk/ontology-lookup/OntologyQuery", 
                default = NA_character_, Rname = character(0), 
                documentation = character(0)), ontologyName = new("LocalElement", 
                count = c(1L, 1L), type = new("PrimitiveSOAPType", 
                  fromConverter = function () 
                  NULL, toConverter = .Primitive("as.character"), 
                  count = c(1L, 1L), abstract = logical(0), name = "string", 
                  ns = "xsd", nsuri = "http://www.w3.org/2001/XMLSchema", 
                  default = NA_character_, Rname = character(0), 
                  documentation = character(0)), name = "ontologyName", 
                attributes = list(), xmlAttrs = character(0), 
                ns = character(0), nsuri = "http://www.ebi.ac.uk/ontology-lookup/OntologyQuery", 
                default = NA_character_, Rname = character(0), 
                documentation = character(0))), isAttribute = c(FALSE, 
            FALSE), uris = c("http://www.ebi.ac.uk/ontology-lookup/OntologyQuery", 
            "http://www.ebi.ac.uk/ontology-lookup/OntologyQuery"
            ), fromConverter = function () 
            NULL, toConverter = function () 
            NULL, count = numeric(0), abstract = logical(0), 
            name = "termId.ontologyName", ns = character(0), 
            nsuri = "http://www.ebi.ac.uk/ontology-lookup/OntologyQuery", 
            default = NA_character_, Rname = character(0), documentation = character(0)), 
            name = "isObsolete", attributes = list(), xmlAttrs = character(0), 
            ns = character(0), nsuri = "http://www.ebi.ac.uk/ontology-lookup/OntologyQuery", 
            default = NULL, Rname = character(0), documentation = character(0))), 
        .convert = .convert, .header = c(Accept = "text/xml", 
            Accept = "multipart/*", `Content-Type` = "text/xml; charset=utf-8", 
            SOAPAction = "\"isObsolete\""), .opts = .opts, .literal = TRUE, 
        nameSpaces = nameSpaces, .elementFormQualified = TRUE, 
        .returnNodeName = "isObsoleteResponse", .soapHeader = .soapHeader)
}

class( isObsolete ) = c( 'SerializedFunction', 'WSDLGeneratedSOAPFunction' )

## getTermsByAnnotationData = 
## function (parameters = list(...), ..., server = new("HTTPSOAPServer", 
##     host = "www.ebi.ac.uk", port = NA_integer_, url = "/ontology-lookup/services/OntologyQuery"), 
##     .convert = new("Element", type = new("SimpleSequenceType", 
##         elementType = character(0), elType = new("ClassDefinition", 
##             slotTypes = list(annotationNumberValue = new("LocalElement", 
##                 count = c(1L, 1L), type = new("PrimitiveSOAPType", 
##                   fromConverter = function () 
##                   NULL, toConverter = .Primitive("as.double"), 
##                   count = c(1L, 1L), abstract = logical(0), name = "double", 
##                   ns = "xsd", nsuri = "http://www.w3.org/2001/XMLSchema", 
##                   default = NA_character_, Rname = character(0), 
##                   documentation = character(0)), name = "annotationNumberValue", 
##                 attributes = list(), xmlAttrs = character(0), 
##                 ns = character(0), nsuri = "http://model.web.ook.ebi.ac.uk", 
##                 default = NA_character_, Rname = character(0), 
##                 documentation = character(0)), annotationStringValue = new("LocalElement", 
##                 count = c(1L, 1L), type = new("PrimitiveSOAPType", 
##                   fromConverter = function () 
##                   NULL, toConverter = .Primitive("as.character"), 
##                   count = c(1L, 1L), abstract = logical(0), name = "string", 
##                   ns = "xsd", nsuri = "http://www.w3.org/2001/XMLSchema", 
##                   default = NA_character_, Rname = character(0), 
##                   documentation = character(0)), name = "annotationStringValue", 
##                 attributes = list(), xmlAttrs = character(0), 
##                 ns = character(0), nsuri = "http://model.web.ook.ebi.ac.uk", 
##                 default = NA_character_, Rname = character(0), 
##                 documentation = character(0)), annotationType = new("LocalElement", 
##                 count = c(1L, 1L), type = new("PrimitiveSOAPType", 
##                   fromConverter = function () 
##                   NULL, toConverter = .Primitive("as.character"), 
##                   count = c(1L, 1L), abstract = logical(0), name = "string", 
##                   ns = "xsd", nsuri = "http://www.w3.org/2001/XMLSchema", 
##                   default = NA_character_, Rname = character(0), 
##                   documentation = character(0)), name = "annotationType", 
##                 attributes = list(), xmlAttrs = character(0), 
##                 ns = character(0), nsuri = "http://model.web.ook.ebi.ac.uk", 
##                 default = NA_character_, Rname = character(0), 
##                 documentation = character(0)), termId = new("LocalElement", 
##                 count = c(1L, 1L), type = new("PrimitiveSOAPType", 
##                   fromConverter = function () 
##                   NULL, toConverter = .Primitive("as.character"), 
##                   count = c(1L, 1L), abstract = logical(0), name = "string", 
##                   ns = "xsd", nsuri = "http://www.w3.org/2001/XMLSchema", 
##                   default = NA_character_, Rname = character(0), 
##                   documentation = character(0)), name = "termId", 
##                 attributes = list(), xmlAttrs = character(0), 
##                 ns = character(0), nsuri = "http://model.web.ook.ebi.ac.uk", 
##                 default = NA_character_, Rname = character(0), 
##                 documentation = character(0)), termName = new("LocalElement", 
##                 count = c(1L, 1L), type = new("PrimitiveSOAPType", 
##                   fromConverter = function () 
##                   NULL, toConverter = .Primitive("as.character"), 
##                   count = c(1L, 1L), abstract = logical(0), name = "string", 
##                   ns = "xsd", nsuri = "http://www.w3.org/2001/XMLSchema", 
##                   default = NA_character_, Rname = character(0), 
##                   documentation = character(0)), name = "termName", 
##                 attributes = list(), xmlAttrs = character(0), 
##                 ns = character(0), nsuri = "http://model.web.ook.ebi.ac.uk", 
##                 default = NA_character_, Rname = character(0), 
##                 documentation = character(0))), isAttribute = c(FALSE, 
##             FALSE, FALSE, FALSE, FALSE), uris = c("http://model.web.ook.ebi.ac.uk", 
##             "http://model.web.ook.ebi.ac.uk", "http://model.web.ook.ebi.ac.uk", 
##             "http://model.web.ook.ebi.ac.uk", "http://model.web.ook.ebi.ac.uk"
##             ), fromConverter = function () 
##             NULL, toConverter = function () 
##             NULL, count = numeric(0), abstract = logical(0), 
##             name = "DataHolder", ns = character(0), nsuri = "http://model.web.ook.ebi.ac.uk", 
##             default = NA_character_, Rname = character(0), documentation = character(0)), 
##         fromConverter = function () 
##         NULL, toConverter = function () 
##         NULL, count = c(1, Inf), abstract = logical(0), name = "getTermsByAnnotationDataResponse", 
##         ns = character(0), nsuri = "http://www.ebi.ac.uk/ontology-lookup/OntologyQuery", 
##         default = NA_character_, Rname = character(0), documentation = character(0)), 
##         name = "getTermsByAnnotationDataResponse", attributes = list(), 
##         xmlAttrs = character(0), ns = character(0), nsuri = "http://www.ebi.ac.uk/ontology-lookup/OntologyQuery", 
##         default = NA_character_, Rname = character(0), documentation = character(0)), 
##     .opts = list(), nameSpaces = "1.2", .soapHeader = NULL) 
## {
##     .SOAP(server, "getTermsByAnnotationData", parameters = as(parameters, 
##         "ontologyName.annotationType.strValue.fromDblValue.toDblValue"), 
##         action = "getTermsByAnnotationData", xmlns = "http://www.ebi.ac.uk/ontology-lookup/OntologyQuery", 
##         .types = list(parameters = new("Element", type = new("ClassDefinition", 
##             slotTypes = list(ontologyName = new("LocalElement", 
##                 count = c(1L, 1L), type = new("PrimitiveSOAPType", 
##                   fromConverter = function () 
##                   NULL, toConverter = .Primitive("as.character"), 
##                   count = c(1L, 1L), abstract = logical(0), name = "string", 
##                   ns = "xsd", nsuri = "http://www.w3.org/2001/XMLSchema", 
##                   default = NA_character_, Rname = character(0), 
##                   documentation = character(0)), name = "ontologyName", 
##                 attributes = list(), xmlAttrs = character(0), 
##                 ns = character(0), nsuri = "http://www.ebi.ac.uk/ontology-lookup/OntologyQuery", 
##                 default = NA_character_, Rname = character(0), 
##                 documentation = character(0)), annotationType = new("LocalElement", 
##                 count = c(1L, 1L), type = new("PrimitiveSOAPType", 
##                   fromConverter = function () 
##                   NULL, toConverter = .Primitive("as.character"), 
##                   count = c(1L, 1L), abstract = logical(0), name = "string", 
##                   ns = "xsd", nsuri = "http://www.w3.org/2001/XMLSchema", 
##                   default = NA_character_, Rname = character(0), 
##                   documentation = character(0)), name = "annotationType", 
##                 attributes = list(), xmlAttrs = character(0), 
##                 ns = character(0), nsuri = "http://www.ebi.ac.uk/ontology-lookup/OntologyQuery", 
##                 default = NA_character_, Rname = character(0), 
##                 documentation = character(0)), strValue = new("LocalElement", 
##                 count = c(1L, 1L), type = new("PrimitiveSOAPType", 
##                   fromConverter = function () 
##                   NULL, toConverter = .Primitive("as.character"), 
##                   count = c(1L, 1L), abstract = logical(0), name = "string", 
##                   ns = "xsd", nsuri = "http://www.w3.org/2001/XMLSchema", 
##                   default = NA_character_, Rname = character(0), 
##                   documentation = character(0)), name = "strValue", 
##                 attributes = list(), xmlAttrs = character(0), 
##                 ns = character(0), nsuri = "http://www.ebi.ac.uk/ontology-lookup/OntologyQuery", 
##                 default = NA_character_, Rname = character(0), 
##                 documentation = character(0)), fromDblValue = new("LocalElement", 
##                 count = c(1L, 1L), type = new("PrimitiveSOAPType", 
##                   fromConverter = function () 
##                   NULL, toConverter = .Primitive("as.double"), 
##                   count = c(1L, 1L), abstract = logical(0), name = "double", 
##                   ns = "xsd", nsuri = "http://www.w3.org/2001/XMLSchema", 
##                   default = NA_character_, Rname = character(0), 
##                   documentation = character(0)), name = "fromDblValue", 
##                 attributes = list(), xmlAttrs = character(0), 
##                 ns = character(0), nsuri = "http://www.ebi.ac.uk/ontology-lookup/OntologyQuery", 
##                 default = NA_character_, Rname = character(0), 
##                 documentation = character(0)), toDblValue = new("LocalElement", 
##                 count = c(1L, 1L), type = new("PrimitiveSOAPType", 
##                   fromConverter = function () 
##                   NULL, toConverter = .Primitive("as.double"), 
##                   count = c(1L, 1L), abstract = logical(0), name = "double", 
##                   ns = "xsd", nsuri = "http://www.w3.org/2001/XMLSchema", 
##                   default = NA_character_, Rname = character(0), 
##                   documentation = character(0)), name = "toDblValue", 
##                 attributes = list(), xmlAttrs = character(0), 
##                 ns = character(0), nsuri = "http://www.ebi.ac.uk/ontology-lookup/OntologyQuery", 
##                 default = NA_character_, Rname = character(0), 
##                 documentation = character(0))), isAttribute = c(FALSE, 
##             FALSE, FALSE, FALSE, FALSE), uris = c("http://www.ebi.ac.uk/ontology-lookup/OntologyQuery", 
##             "http://www.ebi.ac.uk/ontology-lookup/OntologyQuery", 
##             "http://www.ebi.ac.uk/ontology-lookup/OntologyQuery", 
##             "http://www.ebi.ac.uk/ontology-lookup/OntologyQuery", 
##             "http://www.ebi.ac.uk/ontology-lookup/OntologyQuery"
##             ), fromConverter = function () 
##             NULL, toConverter = function () 
##             NULL, count = numeric(0), abstract = logical(0), 
##             name = "ontologyName.annotationType.strValue.fromDblValue.toDblValue", 
##             ns = character(0), nsuri = "http://www.ebi.ac.uk/ontology-lookup/OntologyQuery", 
##             default = NA_character_, Rname = character(0), documentation = character(0)), 
##             name = "getTermsByAnnotationData", attributes = list(), 
##             xmlAttrs = character(0), ns = character(0), nsuri = "http://www.ebi.ac.uk/ontology-lookup/OntologyQuery", 
##             default = NULL, Rname = character(0), documentation = character(0))), 
##         .convert = .convert, .header = c(Accept = "text/xml", 
##             Accept = "multipart/*", `Content-Type` = "text/xml; charset=utf-8", 
##             SOAPAction = "\"getTermsByAnnotationData\""), .opts = .opts, 
##         .literal = TRUE, nameSpaces = nameSpaces, .elementFormQualified = TRUE, 
##         .returnNodeName = "getTermsByAnnotationDataResponse", 
##         .soapHeader = .soapHeader)
## }
## class( getTermsByAnnotationData ) = c( 'SerializedFunction', 'WSDLGeneratedSOAPFunction' )

