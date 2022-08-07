#!/usr/bin/env bash

hostname=localhost:1442

echo '#### `v2.0.0-beta`'
echo ""

fragment=stake1uxqh9rn76n8nynsnyvf4ulndjv0srcc8jtvumut3989cqmgjt49h6
echo "Name         | Value"
echo "---          | ---"
echo -n '`fragment`   | `'; echo -n $fragment; echo '`'
echo ""

echo '```console'
echo '$ curl -s "http://$hostname/matches/$fragment" > /dev/null'
time curl -s "http://$hostname/matches/$fragment" > /dev/null
echo ""
echo "$(curl -s "http://$hostname/matches/$fragment" | jq length) matches."
echo '```'
echo ""

echo '```console'
echo '$ curl -s "http://$hostname/matches/$fragment?unspent" > /dev/null'
time curl -s "http://$hostname/matches/$fragment?unspent" > /dev/null
echo ""
echo "$(curl -s "http://$hostname/matches/$fragment?unspent" | jq length) matches."
echo '```'
echo ""

echo ""

fragment=addr1zxj47sy4qxlktqzmkrw8dahe46gtv8seakrshsqz26qnvzypw288a4x0xf8pxgcntelxmyclq83s0ykeehchz2wtspksr3q9nx
policy_id=d7726f6b882c6fc2ca0cd96c51e5328e1d577d789b085ee0fbe23bf7
asset_name=55676c7942726f546865446566696e697469766536393439

echo "Name         | Value"
echo "---          | ---"
echo -n '`fragment`   | `'; echo -n $fragment; echo '`'
echo -n '`policy_id`  | `'; echo -n $policy_id; echo '`'
echo -n '`asset_name` | `'; echo -n $asset_name; echo '`'
echo ""

echo '```console'
echo '$ curl -s "http://$hostname/matches/$fragment" > /dev/null'
time curl -s "http://$hostname/matches/$fragment" > /dev/null
echo ""
echo "$(curl -s "http://$hostname/matches/$fragment" | jq length) matches."
echo '```'
echo ""

echo '```console'
echo '$ curl -s "http://$hostname/matches/$fragment?unspent" > /dev/null'
time curl -s "http://$hostname/matches/$fragment?unspent" > /dev/null
echo ""
echo "$(curl -s "http://$hostname/matches/$fragment" | jq length) matches."
echo '```'
echo ""

echo '```console'
echo '$ curl -s "http://$hostname/matches/$fragment?unspent&policy_id=$policy_id&asset_name=$asset_name" > /dev/null'
time curl -s "http://$hostname/matches/$fragment?unspent&policy_id=$policy_id&asset_name=$asset_name" > /dev/null
echo ""
echo "$(curl -s "http://$hostname/matches/$fragment?unspent&policy_id=$policy_id&asset_name=$asset_name" | jq length) matches."
echo '```'
echo ""
