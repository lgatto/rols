# rols 2.25

## CHANGES IN VERSION 2.25.0

- New devel version (Bioc 3.16)

# rols 2.24

## CHANGES IN VERSION 2.24.0

- New release version (Bioc 3.15)

# rols 2.23

## CHANGES IN VERSION 2.23.2

- Remove failing `CVParams()` examples

## CHANGES IN VERSION 2.23.1

- Don't show empty `termDesc(trm)` in vignette.

## CHANGES IN VERSION 2.23.0

- New devel version

# rols 2.21

## CHANGES IN VERSION 2.21.1

- Fix failing unit test

## CHANGES IN VERSION 2.21.0

- New devel version for Bioc 3.14

# rols 2.19

## CHANGES IN VERSION 2.19.4

- Replace failing unit tests after GO:0032801 changed.

## CHANGES IN VERSION 2.19.3

- Fix error for empty query results
- New as.data.frame OlsSearch S3 method

## CHANGES IN VERSION 2.19.2

- Fix failing unit test (number of obsolete terms in `test_Terms.R`)

## CHANGES IN VERSION 2.19.1

- Fix pagination of ebi.ac.uk results contributed by Andrew Clugston
  (AndrewC160) - see issue #27.
- For failing unit test (number of obsolete terms in `test_Terms.R`)

# rols 2.17

## CHANGES IN VERSION 2.17.4
- Fix failing unit test <2020-08-26 Wed>

## CHANGES IN VERSION 2.17.3
- Fix failing unit test (Matched error messsage changed) <2020-07-01 Wed>

## CHANGES IN VERSION 2.17.2
- Fix failing unit test (Terms changed) <2020-06-09 Tue>

## CHANGES IN VERSION 2.17.1
- Fix failing unit test (GO description changed) <2020-05-01 Fri>

## CHANGES IN VERSION 2.17.0
- Bioconductor devel version (Bioc 3.12)

# rols 2.16

## CHANGES IN VERSION 2.16.0
- Bioconductor release version (Bioc 3.11)

# rols 2.15

## CHANGES IN VERSION 2.15.0
- Bioconductor version bump

# rols 2.13

## CHANGES IN VERSION 2.13.2
- Remove failing test - term is now obsolete <2019-10-12 sam.>

## CHANGES IN VERSION 2.13.1
- Fix failing test <2019-08-12 Mon>

# rols 2.11

## CHANGES IN VERSION 2.11.2
- Fix failing test <2019-04-16 Tue>

## CHANGES IN VERSION 2.11.1
- Add ImmunoOncology biocView

## CHANGES IN VERSION 2.11.0
- New version for Bioc 3.9 (devel)

# rols 2.10

## CHANGES IN VERSION 2.10.0
- New version for Bioc 3.8 (release)

# rols 2.9

## CHANGES IN VERSION 2.9.4
- Fix unit test <2018-10-06 Sat>

## CHANGES IN VERSION 2.9.3
- Update news and pkgdown

## CHANGES IN VERSION 2.9.2
- replace BiocInstaller biocLite mentions with BiocManager

## CHANGES IN VERSION 2.9.1
- Fix bug ancestors function, reported by Christian Holland (see
  https://github.com/lgatto/rols/issues/26 for details)
  <2018-06-01 Fri>

## CHANGES IN VERSION 2.9.0
- Bioconductor devel

# rols 2.8

## CHANGES IN VERSION 2.8.0
- Bioconductor release

# rols 2.7

## CHANGES IN VERSION 2.7.2
- Nothing yet

## CHANGES IN VERSION 2.7.1
- Fix failing partOf unit test <2017-11-27 Mon>

## CHANGES IN VERSION 2.7.0
- Bioconductor devel 3.7

# rols 2.6

## CHANGES IN VERSION 2.6.0
- Bioconductor release 3.6

# rols 2.5

## CHANGES IN VERSION 2.5.6
- bump version

## CHANGES IN VERSION 2.5.5
- Fix failing test do to different order <2017-10-23 Mon>

## CHANGES IN VERSION 2.5.4
- Fix bug in pagesize limit when reporting children, ancestors,
  ... (see issue #25) <2017-10-17 Tue>

## CHANGES IN VERSION 2.5.4
- New term(s) to data.frame coersion methods <2017-10-14 Sat>

## CHANGES IN VERSION 2.5.3
- Add import Ontology in NAMESPACE

## CHANGES IN VERSION 2.5.2
- Update to latest BiocStyle <2017-09-01 Fri>
- Quick fix for issue #24 (reported upsteams) <2017-09-02 Sat>
- Use Ontology generic from BiocGenerics <2017-09-03 Sun>

## CHANGES IN VERSION 2.5.1
- Fix unit test <2017-06-20 Tue>

## CHANGES IN VERSION 2.5.0
- Bioconductor devel 3.6

# rols 2.4

## CHANGES IN VERSION 2.4.0
- Bioconductor release 3.5

# rols 2.3

## CHANGES IN VERSION 2.3.5

- Fix failing test <2017-03-15 Wed>

## CHANGES IN VERSION 2.3.4

- Ammend failing Ontologies unit test (related to changes made in
  version 2.3.3: the GO name reverted back to Gene Ontolgy)
  <2017-01-11 Wed>

## CHANGES IN VERSION 2.3.3

- Add ctb to Authors@R <2016-12-28 Wed>
- use NEWS.md <2016-12-28 Wed>
- Ammend failing Ontologies unit test <2017-01-02 Mon>

## CHANGES IN VERSION 2.3.2

- Update test to reflect GO's new title <2016-12-21 Wed>

## CHANGES IN VERSION 2.3.1

- Fix failing unit test <2016-11-22 Tue>

## CHANGES IN VERSION 2.3.0

- Bioconductor devel 3.5

# rols 2.2

## CHANGES IN VERSION 2.2.0

- Bioconductor release 3.4

## CHANGES IN VERSION 2.1.3

- Fix failing tests <2016-09-16 Fri>

# rols 1.99

## CHANGES IN VERSION 1.99.1

- Explicitly extract raw content to fix error <2016-04-02 Sat>

## CHANGES IN VERSION 1.99.0

- Using the REST API

# rols 1.13

## CHANGES IN VERSION 1.13.1

- Package deprecation message <2015-12-30 Wed>

## CHANGES IN VERSION 1.13.0

- Bioc devel 3.3

# rols 1.12

## CHANGES IN VERSION 1.12.0

- Bio release 3.2

# rols 1.11

## CHANGES IN VERSION 1.11.6

- fix failing unit tests <2015-07-02 Thu>

## CHANGES IN VERSION 1.11.5

- add test script <2015-06-30 Tue>
- more unit tests <2015-06-30 Tue>

## CHANGES IN VERSION 1.11.4

- Fix bug in as("[MS, MS:123, ]", "CVParam") and add unit test
   <2015-06-19 Fri>

## CHANGES IN VERSION 1.11.3

- New charIsCVParam function to check if a character represents a
   valid CV param <2015-06-16 Tue>

## CHANGES IN VERSION 1.11.2

- Unit test <2015-06-15 Mon>

## CHANGES IN VERSION 1.11.1

- New char to CVParam coerce method <2015-06-15 Mon>

# rols 1.10

## CHANGES IN VERSION 1.11.0

- Bioc devel 3.2

## CHANGES IN VERSION 1.10.0

- Bioc release 3.1

# rols 1.7

## CHANGES IN VERSION 1.7.2

- Removing (temporarily) allIds("GO")) example as currently returns
   illegal message to fix testing error [2014-10-07 Tue]
- Updating roxygem inline docs to fix errors in generated Rds
   [2014-10-07 Tue]

## CHANGES IN VERSION 1.7.1

- add utils to Imports [2014-04-28 Mon]

## CHANGES IN VERSION 1.7.0

- new devel, Bioc 3.0

# rols 1.6

## CHANGES IN VERSION 1.6.0

- release for Bioc 2.14

# rols 1.5

## CHANGES IN VERSION 1.5.2

- pretty printing with strwrap(mtd[...]) in vignette and termMetadata
   S3 printing method for readable-utput (suggestion from Martin
   Morgan) [2014-02-18 Tue]
- Using BiocStyle vignette [2014-02-18 Tue]

## CHANGES IN VERSION 1.5.1

- split go terms (PRO:...) instead-f ids (PR:...) in vignette
   tgnqueryShow chunk <2013-10-19 Sat>

## CHANGES IN VERSION 1.5.0

- new devel for Bioc 2.14

# rols 1.3

## CHANGES IN VERSION 1.3.2

- fixed bug in isIdObsolete, reported by Alex Thomas <2013-07-27 Sat>

## CHANGES IN VERSION 1.3.1

--ther example in CVParam-class example, as PSI-ntology is not
   available <2013-05-02 Thu>

Changes IN VERSION 1.3.0

- Bioc 2.13 devel version bump

# rols 1.2

## CHANGES IN VERSION 1.2.0

- Bioc 2.12 stable version bump

# rols 1.1

## CHANGES IN VERSION 1.1.6

- Using knitr as VignetteEngine and visual tweaking <2013-02-17 Sun>

## CHANGES IN VERSION 1.1.5

- update vignette for knitr 1.0 compatibility,
   based-n Dan's updates in stable version <2013-01-15 Tue>

## CHANGES IN VERSION 1.1.4

- changed query to avoid failure (see issue #6) <2012-12-26 Wed>
- added missing space in `Empty query results after 3attempts` <2012-12-26 Wed>

## CHANGES IN VERSION 1.1.3

- updated-lsQuery example <2012-12-25 Tue>

## CHANGES IN VERSION 1.1.2

- .rolsEnv now has emptyenv() as parent <2012-10-31 Wed>

## CHANGES IN VERSION 1.1.1

- fixing vignette <2012-10-02 Tue>
- new iface <2012-10-02 Tue>

## CHANGES IN VERSION 1.1.0

- version bump for next devel <2012-10-01 Mon>

# rols 0.99

## CHANGES IN VERSION 0.99.12

o updated README.md (for github) and .Rbuildignore <2012-09-19 Wed>

## CHANGES IN VERSION 0.99.11

o updated biocViews <2012-09-18 Tue>

## CHANGES IN VERSION 0.99.10

o using knitr instead-f pgfSweave <2012-08-13 Mon>

## CHANGES IN VERSION 0.99.9

- fixed-lsQuery issued warning when missing(ontologyNames)
   irrespective-f extact <2012-05-22 Tue>

## CHANGES IN VERSION 0.99.8

- really using pdfSweave <2012-05-18 Fri>

## CHANGES IN VERSION 0.99.7

- using pdfSweave <2012-05-16 Wed>
- changed VignetteIndexEntry <2012-05-16 Wed>
- removed roxygen2 from Suggests <2012-05-16 Wed>
- added README file describinh Rd generation
   with roxygen2 <2012-05-16 Wed>

## CHANGES IN VERSION 0.99.6

- implementing Mark Carlson's suggestions
- (Temporarily) removing pgf  <2012-05-15 Tue>
- added Collate field <2012-05-15 Tue>
- moving generics and class definitions to
   AllGenerics.R and AllClasses.R <2012-05-15 Tue>
--lsQuery now repeats query 'n' times if reply
   is empty - see man page for a brief
   discussion <2012-05-15 Tue>
- updated vignette to discuss-ff/on-line data
   access <2012-05-15 Tue>

## CHANGES IN VERSION 0.99.5

- downgraded SSOAP dependency to (>= 0.8.0), as later
   SSOAP versions are not available through biocLite
   (due to check errors). rols works with SSOAP 0.8.0
   and XMLSchema 0.7.2. <2012-05-11 Fri>

## CHANGES IN VERSION 0.99.4

- added url to DESCRIPTION <2012-05-11 Fri>
- released CVParam validity contrains: now CVParam name
   must match term-r any synonym <2012-05-11 Fri>
- new CVParam constructor <2012-05-11 Fri>

## CHANGES IN VERSION 0.99.3

- minor vignette update <2012-05-04 Fri>
- new CVParam class <2012-05-08 Tue>
- More verbose CVParam validity error msg <2012-05-08 Tue>
- new 'rep' method for CVParam <2012-05-09 Wed>

## CHANGES IN VERSION 0.99.2

--lsQuery has a new 'exact' parameter <2012-05-04 Fri>

## CHANGES IN VERSION 0.99.1

- import(XMLSchema) to remove warning at startup <2012-05-04 Fri>

## CHANGES IN VERSION 0.99.0

- vignette update <2012-04-30 Mon>
- added GO.db to Suggests for vignette <2012-04-30 Mon>
- bumped version to 0.99 for Bioc submission <2012-04-30 Mon>

# rols 0.2

## CHANGES IN VERSION 0.2.2

- added a vignette <2011-12-18 Sun>
- added xtable in suggests for vignette <2011-12-18 Sun>
- added parents() and childrenRelations() query functions <2011-12-18 Sun>

## CHANGES IN VERSION 0.2.1

- added specific SSOAP and XMLSchema version
   requirements <2011-12-18 Sun>
- fixed a couple-f typos <2011-12-18 Sun>

## CHANGES IN VERSION 0.2.0

- initial release
