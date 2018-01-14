#!/usr/bin/env bash
#
# This script requires next environmet variables to be set
#  * SH_LIB: the lib directory to load utils from
#
# Ad-hoc and ugly. Don't try to imitate this

source $SH_LIB/prelude.sh

if [[ "$NARGS" != 1 ]]; then
    echo "uasge: $PROGNAME <reservoir-bin>"
fi

## Constants
##====================================================================
readonly TEMP_DIR="$(mktemp -d /tmp/reservoir-test.XXXXXX)"
cleanup() {
    rm -r "$TEMP_DIR"
}
trap cleanup exit

readonly NUM_PASSES=1000
readonly SAMPLES=5
readonly TEST_INPUT="$PROGDIR/test-input"
readonly NUM_ELEMENTS="$(wc -l "$TEST_INPUT" | cut -d ' ' -f 1)"

## 99.73% confidence interval (approximated as a normal distribution instead of binomial)
readonly CONFIDENCE_FORMULA="scale = 6; 3*sqrt(($SAMPLES/$NUM_ELEMENTS)*(1-$SAMPLES/$NUM_ELEMENTS)/$NUM_PASSES)"
readonly CONFIDENCE_INTERVAL="$(echo "$CONFIDENCE_FORMULA" | bc)"
readonly MIN_SAMPLES_FORMULA="scale = 6; ($SAMPLES/$NUM_ELEMENTS - $CONFIDENCE_INTERVAL) * $NUM_PASSES"
readonly MAX_SAMPLES_FORMULA="scale = 6; ($SAMPLES/$NUM_ELEMENTS + $CONFIDENCE_INTERVAL) * $NUM_PASSES"
readonly MIN_SAMPLES="$(echo "$MIN_SAMPLES_FORMULA" | bc)"
readonly MAX_SAMPLES="$(echo "$MAX_SAMPLES_FORMULA" | bc)"

readonly ROUND_MIN_SAMPLES="$(printf "%.0f" "$MIN_SAMPLES")"
readonly ROUND_MAX_SAMPLES="$(printf "%.0f" "$MAX_SAMPLES")"

readonly RESERVOIR="${ARGS[0]}"

echo "Testing to sample $SAMPLES elements out of $NUM_ELEMENTS"
echo "Running $NUM_PASSES"
echo "Each element should be sampled [$ROUND_MIN_SAMPLES, $ROUND_MAX_SAMPLES] times"

for i in $(seq 1 $NUM_PASSES); do
    "$RESERVOIR" "$SAMPLES" < "$TEST_INPUT" >> "$TEMP_DIR/outputs"
done

echo
echo "Amount of sampled elements (<samples> <element>):"
sort -n "$TEMP_DIR/outputs" | uniq -c | tee "$TEMP_DIR/result"

echo
echo "Checking if any element was sampled an unlikely amount of times:"
failed=0
while read line; do
    count="$(echo $line | cut -d ' ' -f 1)"
    element="$(echo $line | cut -d ' ' -f 2)"
    if (($count < $ROUND_MIN_SAMPLES)); then
        echo " - Too few: $element sampled $count times"
        failed=1
    fi
    if (($count > $ROUND_MAX_SAMPLES)); then
        echo " - Too many: $element sampled $count times"
        failed=1
    fi
done < "$TEMP_DIR/result"

if [[ $failed != 0 ]]; then
    echo
    echo "The results were unlikely"
    echo
    exit 1
fi
echo
echo "Everything seems within reason"
echo
