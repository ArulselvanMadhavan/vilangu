FROM ubuntu:latest as build

ARG USER_NAME
ARG UID
ARG GID
ARG OCAML_COMPILER
USER root

ENV DEBIAN_FRONTEND=noninteractive
RUN apt-get update && apt-get install -y sudo git inotify-tools build-essential libssl-dev liblapacke-dev libopenblas-dev pkg-config zlib1g-dev libffi-dev libgmp-dev libpcre3-dev tmux fish software-properties-common wget netbase

RUN add-apt-repository -y ppa:deadsnakes/ppa
RUN apt-get install -y python3 python3-pip

RUN apt-get install -y emacs cmake
RUN apt-get install -y opam

# Install dependencies
RUN apt-get -qq update; \
    apt-get install -qqy --no-install-recommends \
        gnupg2 wget ca-certificates apt-transport-https \
        autoconf automake cmake dpkg-dev file make patch libc6-dev

# Set repository key
RUN wget -nv -O - https://apt.llvm.org/llvm-snapshot.gpg.key | apt-key add -

# Install
RUN echo "deb http://apt.llvm.org/jammy/ llvm-toolchain-jammy-14 main" \
        > /etc/apt/sources.list.d/llvm.list; \
    apt-get -qq update && \
    apt-get install -qqy -t llvm-toolchain-jammy-14 clang-14 clang-tidy-14 clang-format-14 lld-14 && \
    for f in /usr/lib/llvm-14/bin/*; do ln -sf "$f" /usr/bin; done && \
    rm -rf /var/lib/apt/lists/*

# Install Protobuf
RUN apt update && apt install -y protobuf-compiler

# Install OCaml deps
ENV USER_NAME=${USER_NAME}
ENV OCAML_COMPILER=${OCAML_COMPILER}

RUN echo "${USER_NAME} ALL=(ALL:ALL) NOPASSWD:ALL" > /etc/sudoers.d/${USER_NAME}
RUN chmod 440 /etc/sudoers.d/${USER_NAME}
RUN chown root:root /etc/sudoers.d/${USER_NAME}
RUN groupadd -g ${GID} ${USER_NAME}
RUN useradd -m ${USER_NAME} --uid=${UID} --gid=${GID} -s /bin/bash
# RUN adduser --uid ${UID} --disabled-password --gecos '' ${USER_NAME}
RUN passwd -l ${USER_NAME}
RUN chown -R ${USER_NAME}:${USER_NAME} /home/${USER_NAME}
USER ${USER_NAME}
ENV HOME=/home/${USER_NAME}

WORKDIR ${HOME}
RUN opam init -a -y -c ${OCAML_COMPILER} --disable-sandboxing
ARG PROJECT_NAME=vilangu
ARG PROJECT_HOME=${HOME}/${PROJECT_NAME}
WORKDIR ${PROJECT_HOME}
RUN eval $(opam env)
# We can't install deps from .opam file because it's not mounted yet
RUN opam install -y dune ocaml-protoc ppx_deriving ppx_deriving_protobuf ppx_let ppx_sexp_conv menhir fpath
RUN eval $(opam env)