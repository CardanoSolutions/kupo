#!/usr/bin/env bash

BASEURL="http://127.0.0.1:1442/matches"

PATTERNS=(
  "addr1vy4nmtfc4jfftgqg369hs2ku6kvcncgzhkemq6mh0u3zgpslf59wr"
  "2b3dad38ac9295a0088e8b782adcd59989e102bdb3b06b777f222406/*"
  "stake1uxqh9rn76n8nynsnyvf4ulndjv0srcc8jtvumut3989cqmgjt49h6"
  "1d7f33bd23d85e1a25d87d86fac4f199c3197a2f7afeb662a0f34e1e.*"
  "*@4301551ce28e83ef1082432f57a13bbbd389f4628592b73d71ca19e8833c0eb7"
)

for PATTERN in ${PATTERNS[@]}; do
  echo ""
  for QUERY in "$PATTERN" "$PATTERN?spent" "$PATTERN?unspent"; do
    ab -s 120 -l -k -c 10 -n 50 -v 0 "$BASEURL/$QUERY"
    echo -e "\nTotal results\n  $(curl -s "$BASEURL/$QUERY" | jq length)\n"
  done
  echo "----------"
done
