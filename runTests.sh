#!/bin/bash
set -e

function usage {
  echo "usage:"
  echo "  $0 [tests/test-file]"
  echo
  echo "If no test file is given, runs all the tests."
}

function runTest {
  local TESTFILE="$1"
  local EXPECT_SCRIPT="$(cat "$TESTFILE" | \
    sed 's/^\([^>].*\)$/expect {  "\1\\r" {}  timeout { exit 1 }}/g' | \
    sed 's/^> \(.*\)$/expect {  "> " {}  timeout { exit 1 }}send "\1\\r"/g' | tr '' '\n')"

  #echo "$EXPECT_SCRIPT"
  #echo

  ./regenPublicObjects.sh
  stack build hyzzy
  expect -c 'set timeout 3' \
         -c 'spawn stack run hyzzy' \
         -f <(echo "$EXPECT_SCRIPT")
}

if [ "$1" = "--help" -o "$1" = "-h" ]; then
  usage
elif [ -f "$1" ]; then
  runTest "$1"
  echo "** TEST PASSED **"
elif [ -z "$1" ]; then
  for x in tests/*; do
    runTest "$x"
  done
  echo "** ALL TESTS PASS **"
else
  (
    echo "unrecognized arguments: " $@
    usage
  ) 1>&2
  exit 1
fi
