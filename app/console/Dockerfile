FROM nixos/nix

ARG NIX_STORE_PATH

COPY nix-store.out /tmp/
RUN nix-store --import < /tmp/nix-store.out
RUN rm -f /tmp/nix-store.out
RUN cp $NIX_STORE_PATH/bin/console /usr/bin

CMD ["/usr/bin/console"]
