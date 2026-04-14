#!/bin/sh
# Generate link flags sexp for dune.
# When WEFT_STATIC is set, emit -static and library paths for musl sysroot.

if [ -z "${WEFT_STATIC:-}" ]; then
  echo "()"
  exit 0
fi

LPATHS="-ccopt -static -ccopt -L/usr/lib"

# Find the musl sysroot and add its lib dirs
SPECS=$(find /usr -name musl-gcc.specs 2>/dev/null | head -1)
if [ -n "$SPECS" ]; then
  MUSL_DIR=$(dirname "$SPECS")
  MUSL_PREFIX=$(dirname "$MUSL_DIR")
  for d in "$MUSL_PREFIX/lib" "$MUSL_PREFIX/lib64" "$MUSL_DIR"; do
    if [ -d "$d" ]; then
      LPATHS="$LPATHS -ccopt -L$d"
    fi
  done
fi

echo "($LPATHS)"
