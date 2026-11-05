#!/bin/sh
# Runs the 3 programs on every day in tests/scenarios and compares the
# results with tests/expected (trailing spaces ignored).
# Run from the project root:  sh tools/check-all.sh
ROOT=$(pwd)
FAIL=0
strip() { sed -e 's/[[:space:]]*$//' "$1" 2>/dev/null; }
same() { [ "$(strip "$1")" = "$(strip "$2")" ]; }

cp data/LNA.DAT /tmp/_lna_save 2>/dev/null
cp data/MASTER.DAT /tmp/_mas_save 2>/dev/null

for S in tests/scenarios/*; do
  N=$(basename "$S"); E="tests/expected/$N"
  cp "$S/LNA.DAT" data/LNA.DAT; cp "$S/MASTER.DAT" data/MASTER.DAT
  rm -f out/*
  ./bin/INTAKE >/dev/null; RC=$?
  OK=PASS
  [ "$RC" = "$(strip "$E/intake_rc.txt")" ] || OK="FAIL(rc=$RC)"
  for F in VALID.DAT REJECT.DAT INTAKE.RPT; do same "out/$F" "$E/$F" || OK="FAIL($F)"; done
  if [ "$RC" = "0" ]; then
    ./bin/UPDATE >/dev/null && ./bin/REPORTS >/dev/null
    for F in NEWMASTER.DAT ACTIVITY.DAT FIRMPROD.RPT TINCHG.RPT; do same "out/$F" "$E/$F" || OK="FAIL($F)"; done
  fi
  [ "$OK" = "PASS" ] || FAIL=1
  printf '%-26s %s\n' "$N" "$OK"
done

cp /tmp/_lna_save data/LNA.DAT 2>/dev/null
cp /tmp/_mas_save data/MASTER.DAT 2>/dev/null
rm -f out/*
[ $FAIL = 0 ] && echo "ALL SCENARIOS PASS" || echo "SOME SCENARIOS FAILED"
exit $FAIL
