#  This Source Code Form is subject to the terms of the Mozilla Public
#  License, v. 2.0. If a copy of the MPL was not distributed with this
#  file, You can obtain one at http://mozilla.org/MPL/2.0/.

#                                                                              #
# --------------------------------- SETUP ------------------------------------ #
#                                                                              #

FROM nixos/nix:2.3.11 as build

RUN echo "substituters = https://cache.nixos.org https://hydra.iohk.io" >> /etc/nix/nix.conf &&\
    echo "trusted-public-keys = cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY= hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ=" >> /etc/nix/nix.conf

WORKDIR /app
RUN nix-shell -p git --command "git clone --depth 1 https://github.com/input-output-hk/cardano-configurations.git"

WORKDIR /app/kupo
RUN nix-env -iA cachix -f https://cachix.org/api/v1/install && cachix use kupo
COPY . .
RUN nix-build -A kupo.components.exes.kupo -o dist
RUN cp -r dist/* . && chmod +w dist/bin && chmod +x dist/bin/kupo

#                                                                              #
# ----------------------------------- BUILD ---------------------------------- #
#                                                                              #

FROM busybox as kupo

ARG NETWORK=mainnet

LABEL name=kupo
LABEL description="A fast, lightweight & configurable chain-index for Cardano."

COPY --from=build /app/kupo/bin/kupo /bin/kupo
COPY --from=build /app/cardano-configurations/network/${NETWORK} /config

EXPOSE 1337/tcp
STOPSIGNAL SIGINT
ENTRYPOINT ["/bin/kupo"]
