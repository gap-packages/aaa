#!/usr/bin/env bash

# If a command fails, exit this script with an error code
set -e
set -o pipefail

GAPROOT=$HOME/gap
touch $GAPROOT/testlog.txt
TESTLOG="$GAPROOT/testlog.txt"
GAPSH="$GAPROOT/bin/gap.sh"

cd $GAPROOT/pkg/aaa
echo -e "\nRunning Aaa package tests..."
echo "LoadPackage(\"aaa\"); TestDirectory(\"tst/standard\"); QUIT;" |
  $GAPSH -A -x 80 -m 768m -o 1g -T 2>&1 | tee -a $TESTLOG

echo -e "\nSuite complete." # AppVeyor needs some extra command here (like this)

( ! grep -E "Diff|brk>|#E|Error|Errors detected|# WARNING|Syntax warning|Couldn't open saved workspace|insufficient|WARNING in|FAILED|Total errors found:" $TESTLOG )
