##' @importFrom utils packageVersion
.onAttach <- function(libname, pkgname) {
    packageStartupMessage(paste("\nThis is 'rols' version",
                                packageVersion("rols"), "\n"))
}
