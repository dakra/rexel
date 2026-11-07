#!/bin/sh
# Run the elate scenarios against this machine's Rex server.
#
#   test/elate/run.sh [-v VARIANT[,VARIANT...]] [SCENARIO.json...]
#
# Without scenarios, every one in matrix/ runs; without -v, every variant a
# scenario declares.  The tls variant needs REXEL_ELATE_TAILNET, the
# server's https address on the tailnet.  ELATE names the elate command
# and ELATE_FORMAT its output format.

here=$(cd "$(dirname "$0")" && pwd)
elate=${ELATE:-uvx elate}
format=${ELATE_FORMAT:-human}
only=

while getopts v: opt; do
    case $opt in
        v) only=$OPTARG ;;
        *) echo "usage: $0 [-v VARIANTS] [SCENARIO.json...]" >&2; exit 2 ;;
    esac
done
shift $((OPTIND - 1))
[ $# -gt 0 ] || set -- "$here"/matrix/*.json

rex=${REX:-$(command -v rex)}
[ -n "$rex" ] || rex="/Applications/Rex Beta.app/Contents/Helpers/Rex Server.rex/Contents/MacOS/rex"

export REXEL_ELATE_LABEL="rexel-elate-$$"

# The scenarios clean up after themselves; this catches what a run that
# was cut short left behind: its sessions and its relays.
sweep() {
    "$rex" --autostart=false ls --json 2>/dev/null |
        python3 -c '
import json, sys
prefix = sys.argv[1]
for s in json.load(sys.stdin).get("sessions", []):
    if s.get("label", "").startswith(prefix):
        print(s["session_id"])' "$REXEL_ELATE_LABEL-" |
        while read -r id; do "$rex" --autostart=false kill "$id" >/dev/null 2>&1; done
    pkill -KILL -f "socat -lp $REXEL_ELATE_LABEL-" 2>/dev/null
    rm -f "/tmp/$REXEL_ELATE_LABEL-"*.sock
}
trap sweep EXIT
trap 'exit 130' INT TERM

variants() {
    python3 -c '
import json, sys
names = list(json.load(open(sys.argv[1])).get("variants", {}))
only = [v for v in sys.argv[2].split(",") if v]
print(" ".join(v for v in names if (v in only if only else True)) or ("-" if not names else ""))
' "$1" "$only"
}

status=0
for scenario; do
    for variant in $(variants "$scenario"); do
        if [ "$variant" = tls ] && [ -z "$REXEL_ELATE_TAILNET" ]; then
            echo "== $(basename "$scenario" .json) [tls]: skipped, REXEL_ELATE_TAILNET is not set"
            continue
        fi
        if [ "$variant" = - ]; then
            echo "== $(basename "$scenario" .json)"
            $elate run --keep-going --format "$format" "$scenario" || status=1
        else
            echo "== $(basename "$scenario" .json) [$variant]"
            $elate run --keep-going --format "$format" --variant "$variant" "$scenario" || status=1
        fi
    done
done
exit $status
