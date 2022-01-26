#
# aaa_new: implements algorithms for asynchronous transducers
#
# This file is a script which compiles the package manual.
# To complile the doc for aaa run the command AaaMakeDoc(); in gap.

if not IsDirectoryPath("gap")
    or not "transducer.gd" in DirectoryContents("gap") then
  Print("Error: GAP must be run from the aaa package directory ",
        "when reading makedoc.g\n");
  FORCE_QUIT_GAP(1);
fi;
pkgdir := DirectoryCurrent();

PACKAGE := "aaa";
LoadPackage("GAPDoc");

_DocXMLFiles := ["main.xml",
                 "z-chap0.xml",
                 "z-chap1.xml",
                 "z-chap2.xml",
                 "z-chap3.xml",
                 "z-chap4.xml",
                 "z-chapint.xml",
                 "title.xml",
                 "toperations.xml",
                 "transducer.xml",
                 "utils.xml",
                 "woperations.xml",
                 "z-aaabib.xml",
                 "../PackageInfo.g"];


MakeGAPDocDoc(Filename(pkgdir, "doc"),
              "main.xml", _DocXMLFiles, PACKAGE, "MathJax", "../../..");
CopyHTMLStyleFiles(Filename(pkgdir, "doc"));
GAPDocManualLabFromSixFile(PACKAGE, Filename(pkgdir, "doc/manual.six"));

QUIT;
