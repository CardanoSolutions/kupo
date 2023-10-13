#  This Source Code Form is subject to the terms of the Mozilla Public
#  License, v. 2.0. If a copy of the MPL was not distributed with this
#  file, You can obtain one at http://mozilla.org/MPL/2.0/.

FROM  --platform=${TARGETPLATFORM:-linux/amd64} alpine:3.18.4 as certs
# hadolint ignore=DL3018
RUN apk add --no-cache ca-certificates

FROM --platform=${TARGETPLATFORM:-linux/amd64} busybox:1.35 as kupo

LABEL name=kupo
LABEL description="A fast, lightweight & configurable chain-index for Cardano."

COPY ./bin/kupo /bin/kupo
RUN chmod +x /bin/kupo

COPY --from=certs /etc/ssl/certs /etc/ssl/certs

EXPOSE 1442/tcp
STOPSIGNAL SIGINT
HEALTHCHECK --interval=10s --timeout=5s --retries=1 CMD /bin/kupo health-check
ENTRYPOINT ["/bin/kupo"]
