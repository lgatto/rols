library(SSOAP)
url <- "http://www.ebi.ac.uk/ontology-lookup/OntologyQuery.wsdl"
wsdl <- processWSDL(url)
iface <- genSOAPClientInterface(def = wsdl)
SSOAP:::makeTopLevelFunctions(iface@functions, where = environment())

con = file("iface.R", "w")
writeLines("## File generate by rols/inst/makeIface.R", con)
writeLines("## Manual modifications: moved 'Map' and 'Vector'", con) 
writeLines("## class definitions to the top\n\n", con)
writeInterface(iface, con)
close(con)

