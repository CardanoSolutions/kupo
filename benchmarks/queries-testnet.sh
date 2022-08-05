#!/usr/bin/env bash

hostname=localhost:1442

echo '#### `v2.0.0`'

fragments="addr_test1wrsexavz37208qda7mwwu4k7hcpg26cz0ce86f5e9kul3hqzlh22t stake_test1uzdlw6rjechunh4u03p3eersp6sxpfdwlhkq59ea3mttgfsag8smr 6058ae2ba01b03504e0e2c418b44dd431dffe4105172332c518a94b3fe"

for fragment in $fragments; do
  echo ""
  echo "Name         | Value"
  echo "---          | ---"
  echo -n '`fragment`   | `'; echo -n $fragment; echo '`'
  echo ""

  echo '```console'
  echo '$ curl -s "http://$hostname/v1/matches/$fragment" > /dev/null'
  time curl -s "http://$hostname/v1/matches/$fragment" > /dev/null
  echo ""
  echo "$(curl -s "http://$hostname/v1/matches/$fragment" | jq length) matches."
  echo '```'
  echo ""

  echo '```console'
  echo '$ curl -s "http://$hostname/v1/matches/$fragment?unspent" > /dev/null'
  time curl -s "http://$hostname/v1/matches/$fragment?unspent" > /dev/null
  echo ""
  echo "$(curl -s "http://$hostname/v1/matches/$fragment?unspent" | jq length) matches."
  echo '```'
  echo ""

  echo '```console'
  echo '$ curl -s "http://$hostname/v1/matches/$fragment?spent" > /dev/null'
  time curl -s "http://$hostname/v1/matches/$fragment?spent" > /dev/null
  echo ""
  echo "$(curl -s "http://$hostname/v1/matches/$fragment?spent" | jq length) matches."
  echo '```'
  echo ""
done
