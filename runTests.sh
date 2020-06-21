#!/bin/bash
set -e

function usage {
  echo "usage:"
  echo "  $0 [games/<game-name> games/<game-name>/tests/<test-name>]"
  echo
  echo "If no test file is given, runs all the tests."
}

function runTest {
  local GAME="$1"
  local TESTFILE="$2"
  local EXPECT_SCRIPT="$(cat "$TESTFILE" | \
    sed 's/^\([^>].*\)$/expect {  "\1\\r" {}  timeout { exit 1 }}/g' | \
    sed 's/^> \(.*\)$/expect {  "> " {}  timeout { exit 1 }}send "\1\\r"/g' | tr '' '\n')"

  #echo "$EXPECT_SCRIPT"
  #echo

  ./regenPublicObjects.sh "$GAME"
  stack build hyzzy
  expect -c 'set timeout 3' \
         -c "spawn stack run hyzzy $GAME" \
         -f <(echo "$EXPECT_SCRIPT")
}

if [ "$1" = "--help" -o "$1" = "-h" ]; then
  usage
elif [ -d "$1" -a -f "$2" ]; then
  runTest "$1" "$2"
  echo "** TEST PASSED **"
elif [ -z "$1" ]; then
  for GAME in games/*; do
    for TESTFILE in $GAME/tests/*; do
      runTest "$GAME" "$TESTFILE"
    done
  done
  echo "** ALL TESTS PASS **"
else
  (
    echo "unrecognized arguments: " $@
    usage
  ) 1>&2
  exit 1
fi
