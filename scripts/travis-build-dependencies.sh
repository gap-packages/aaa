#!/usr/bin/env bash

# If a command fails, exit this script with an error code
set -e

if [ "$SUITE" != "test" ] ; then
  echo -e "\nError, unrecognised Travis suite: $SUITE"
  exit 1
fi

GAPSH="$GAPROOT/bin/gap.sh"
mv ../aaa $HOME/aaa

################################################################################
# Install software necessary for tests and coverage: GAP and packages
################################################################################ 

################################################################################
# Install GAP
echo -e "\nInstalling GAP..."
if [ "$GAP" == "required" ]; then
  GAP=v`grep "GAPVERS" $HOME/aaa/PackageInfo.g | awk -F'"' '{print $2}'`
fi
GAPROOT="$HOME/gap"
echo -e "\nInstalling GAP $GAP into $GAPROOT..."
git clone -b $GAP --depth=1 https://github.com/gap-system/gap.git $GAPROOT
cd $GAPROOT
./autogen.sh
./configure --with-gmp=system $GAP_FLAGS
make -j4
mkdir pkg

# Common curl settings
CURL="curl --connect-timeout 5 --max-time 10 --retry 5 --retry-delay 0 --retry-max-time 40 -L"

################################################################################
# Install required GAP packages
cd $GAPROOT/pkg
echo -e "\nGetting the required GAP packages (smallgrp, transgrp, primgrp)..."
$CURL -O "https://www.gap-system.org/pub/gap/gap4pkgs/packages-required-master.tar.gz"
tar xf packages-required-master.tar.gz
rm packages-required-master.tar.gz

################################################################################
# Copy Aaa to its proper location
mv $HOME/aaa $GAPROOT/pkg/aaa

# Install PackageMan
cd $GAPROOT/pkg
git clone --depth=1 -b v1.0 https://github.com/gap-packages/PackageManager.git

cd $GAPROOT
echo -e "\nUpdating PackageManager..."
echo "LoadPackage(\"PackageManager\"); UpdatePackage(\"PackageManager\", false); InstallPackage(\"digraphs\", false); InstallPackage(\"automata\", false); QUIT;" |
  $GAPSH -A -x 80 -r -m 768m -o $MEM -T 2>&1
