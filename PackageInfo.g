#
# aaa: implements algorithms for asynchronous transducers
#
# This file contains package meta data. For additional information on
# the meaning and correct usage of these fields, please consult the
# manual of the "Example" package as well as the comments in its
# PackageInfo.g file.
#
##  <#GAPDoc Label="PKGVERSIONDATA">
##  <!ENTITY VERSION "0.1.0">
##  <!ENTITY GAPVERS "4.8.0">
##  <!ENTITY ARCHIVENAME "aaa-0.1.0">
##  <!ENTITY COPYRIGHTYEARS "2017-2022">
##  <#/GAPDoc>
SetPackageInfo( rec(

PackageName := "aaa",
Subtitle := "Algorithms for Asynchronous Transducers",
Version := "0.1.0",
Date := "09/02/2022", # dd/mm/yyyy format
License := "GPL-3.0-or-later",

Persons := [
  rec(
    IsAuthor := true,
    IsMaintainer := false,
    FirstNames := "Collin",
    LastName := "Bleak",
    WWWHome := "http://www-groups.mcs.st-andrews.ac.uk/~collin/",
    Email := "cb211@st-andrews.ac.uk",
    PostalAddress := "Mathematical Institute, North Haugh, St Andrews, Fife, KY16 9SS, Scotland",
    Place := "St Andrews",
    Institution := "University of St Andrews",
  ),
  rec(
    IsAuthor := true,
    IsMaintainer := true,
    FirstNames := "Luke",
    LastName := "Elliott",
    WWWHome := "https://le27.github.io/Luke-Elliott/",
    PostalAddress := "Mathematical Institute, North Haugh, St Andrews, Fife, KY16 9SS, Scotland",
    Place := "St Andrews",
    Institution := "University of St Andrews",
  ),
  rec(
    IsAuthor := true,
    IsMaintainer := true,
    FirstNames := "Fernando",
    LastName := "Flores Brito",
    Email := "ffb3@st-andrews.ac.uk",
    PostalAddress := "Mathematical Institute, North Haugh, St Andrews, Fife, KY16 9SS, Scotland",
    Place := "St Andrews",
    Institution := "University of St Andrews",
  ),
  rec(
    IsAuthor := true,
    IsMaintainer := false,
    FirstNames := "Feyisayo",
    LastName := "Olukoya",
    WWWHome := "https://www.abdn.ac.uk/ncs/departments/mathematics/profiles/feyisayo.olukoya/",
    Email := "feyisayo.olukoya@abdn.ac.uk",
    PostalAddress := "Institute of Mathematics, Aberdeen, AB24 3UE, UK",
    Place := "Aberdeen",
    Institution := "University of Aberdeen",
  ),
],

SourceRepository := rec(
    Type := "git",
    URL := Concatenation( "https://github.com/gap-packages/", ~.PackageName ),
),
IssueTrackerURL := Concatenation( ~.SourceRepository.URL, "/issues" ),
PackageWWWHome  := Concatenation( "https://gap-packages.github.io/", ~.PackageName ),
README_URL      := Concatenation( ~.PackageWWWHome, "/README.md" ),
PackageInfoURL  := Concatenation( ~.PackageWWWHome, "/PackageInfo.g" ),
ArchiveURL      := Concatenation( ~.SourceRepository.URL,
                                 "/releases/download/v", ~.Version,
                                 "/", ~.PackageName, "-", ~.Version ),

ArchiveFormats := ".tar.gz",

##  Status information. Currently the following cases are recognized:
##    "accepted"      for successfully refereed packages
##    "submitted"     for packages submitted for the refereeing
##    "deposited"     for packages for which the GAP developers agreed
##                    to distribute them with the core GAP system
##    "dev"           for development versions of packages
##    "other"         for all other packages
##
Status := "dev",

AbstractHTML :=  
   "The <span class=\"pkgname\">aaa</span> package, is a package for \
   building and using transducers as described in the paper entitled \
   'Automata, Dynamical Systems, and Groups' of R. I. Grigorchuk, \
   V. V. Nekrashevich, and V. I. Sushchanskii. Previous similar packages \
   such as <span class=\"pkgname\">fr</span> and \
   <span class=\"pkgname\">AutomGrp</span> have had a focus on synchronous \
   transducers. Hence the name <span class=\"pkgname\">aaa</span> which \
   stands for \"asynchronous automata algorithms\".",

PackageDoc := rec(
  BookName  := "aaa",
  ArchiveURLSubset := ["doc"],
  HTMLStart := "doc/chap0_mj.html",
  PDFFile   := "doc/manual.pdf",
  SixFile   := "doc/manual.six",
  LongTitle := "implements algorithms for asynchronous transducers",
),

Dependencies := rec(
  GAP := ">=4.10",
  NeededOtherPackages := [["automata", ">=0.0.0"],
                          ["digraphs", ">=0.15.0"],
                          ["fr", ">=2.4.6"]],
  SuggestedOtherPackages := [],
  ExternalConditions := []),

##BannerString := Concatenation(
##  "----------------------------------------------------------------------",
##  "-------\n",
##  "Loading  aaa ", ~.Version, "\n",
##  "by ", ~.Persons[1].FirstNames, " ", ~.Persons[1].LastName,
##        " (", ~.Persons[1].WWWHome, ")\n",
##  "with contributions by:\n",
##  Concatenation(Concatenation(List(~.Persons{[2 .. Length(~.Persons) - 1]},
##       p -> ["     ", p.FirstNames, " ", p.LastName,
##       _RecogsFunnyNameFormatterFunction(
##         _RecogsFunnyWWWURLFunction(p)), ",\n"]))),
##  " and ", ~.Persons[Length(~.Persons)].FirstNames, " ",
##  ~.Persons[Length(~.Persons)].LastName,
##  _RecogsFunnyNameFormatterFunction(
##    _RecogsFunnyWWWURLFunction(~.Persons[Length(~.Persons)])), ".\n",
##  "-----------------------------------------------------------------------",
##  "------\n"),

AvailabilityTest := ReturnTrue,

TestFile := "tst/testall.g",

#Keywords := [ "TODO" ],

));


