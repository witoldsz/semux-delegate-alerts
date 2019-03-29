# semux-delegate-alerts

This script scans the Semux blockchain to see if the configured validator did or did not not forge
a (COINBASE) block within specified time. Alerts are delivered to a webhook.

## Prerequisites

This app is written in Haskell and built using `stack`. Just install _The Haskell Tool Stack_ and you should be able to build and run with no issues.

## Build, configure and run it like this:

`./start.sh`
```sh
#!/bin/sh

touch ./db_NotForgingDelegates.txt
touch ./db_LowRankDelegates.txt

SEMUX_API="https://api.semux.online/v2.1.0/" \
ALERT_RANK="95" \
ALERT_AFTER_SECS=3600 \
DELEGATE="…delegate address" \
WEBHOOK_URL="…discord webhook url" \
stack run semux-delegate-alerts
```

## Build a binary distribution:

```sh
stack build
```

The binary you can find here:

`.stack-work/dist/x86_64-linux/Cabal-2.4.0.1/build/semux-delegate-alerts-exe/semux-delegate-alerts-exe`
(replace `x86_64-linux/Cabal-2.4.0.1` appropriately).

## Deployment

The app checks the blockchain and exits, so it should be scheduled to run periodically, e.g.:

```sh
$ crontab -e

## edit:

*/10 * * * * /path/to/app/start.sh 2>&1 | logger -t semux-delegate-alerts
```

Use `crontab -l` to verify.
