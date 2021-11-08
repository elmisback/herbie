#!/bin/bash

# lowered number of cores from 6 to 4 to avoid pagetable error
# caused by heavy use of FFI by eggmath.rkt
CORES=4

function output_error {
    DIR="$1"
    NAME="$2"
    SEED="$3"
    DATE=`date +%s`
    COMMIT=`git rev-parse HEAD`
    BRANCH=`git rev-parse --abbrev-ref HEAD`
    HOSTNAME=`hostname`
    cat >"$1" <<EOF
{ "date": $DATE, "commit": "$COMMIT", "branch": "$BRANCH",
  "hostname": "$HOSTNAME", "seed": "$SEED", "flags": [],
  "points": -1, "iterations": -1, "bit_width": -1,
  "note": "$NAME", "crash": true, "tests": [] }
EOF
}


function run {
  bench="$1"; shift
  name="$1"; shift
  seed=$(date "+%Y%j")
  
  echo "Running $name test with flags $@"
  rm -rf "reports/$name"
  racket "src/herbie.rkt" report \
      --note "$name" \
      --profile \
      --debug \
      --seed "$seed" \
      --egg-output "reports/egg-proof-examples/egg-input-$name.txt" \
      "$@" \
      "$bench" "reports/$name" \
      || output_error "reports/$name/results.json" "$name" "$seed"
}

report=$(git rev-parse --abbrev-ref HEAD)-$(date "+%Y-%m-%d")
mkdir -p reports
rm -rf "reports/egg-proof-examples" || true
mkdir "reports/egg-proof-examples"
dirs=""
for bench in bench/*; do
  name=$(basename "$bench" .fpcore)
  run "$bench" "$name" "$@" &
  if [ "$?" -eq 0 ]; then
      dirs="$dirs reports/$name";
  fi
done
wait
racket infra/nightly.rkt reports/ $dirs

