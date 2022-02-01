ARG VARIANT="focal"
FROM mcr.microsoft.com/vscode/devcontainers/base:0-${VARIANT}

RUN apt update -y && apt upgrade -y && apt -y install opam
RUN chmod u+s /usr/bin/bwrap

RUN opam init -y --disable-sandboxing
RUN eval $(opam env)
RUN opam switch create 4.13.1
RUN eval $(opam env)
RUN opam install -y dune core ppx_jane ocaml-lsp-server ocamlformat utop
