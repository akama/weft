#!/bin/sh
# Generate link flags sexp for dune.
# When WEFT_STATIC is set, emit -static and library paths for musl sysroot.

if [ -z "${WEFT_STATIC:-}" ]; then
  echo "()"
  exit 0
fi

LPATHS="-ccopt -static -ccopt -L/usr/lib"

# Find musl lib dirs from specs file
SPECS=$(find /usr -name musl-gcc.specs 2>/dev/null | head -1)
if [ -n "$SPECS" ]; then
  # Add all -L paths from the specs file
  for d in $(grep -oP '(?<=-L)\S+' "$SPECS"); do
    # Strip trailing format chars like .%s
    d=$(echo "$d" | sed 's/[[:space:]]*%.*//')
    if [ -d "$d" ]; then
      LPATHS="$LPATHS -ccopt -L$d"
    fi
  done
  # Also add lib/ sibling of the include dir (some distros put libs there)
  MUSL_INC=$(grep -oP '(?<=-isystem )\S+' "$SPECS" | head -1)
  MUSL_PREFIX=$(dirname "$MUSL_INC")
  for d in "$MUSL_PREFIX/lib" "$MUSL_PREFIX/lib64"; do
    if [ -d "$d" ]; then
      LPATHS="$LPATHS -ccopt -L$d"
    fi
  done
fi

echo "($LPATHS)"
