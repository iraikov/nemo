#!/bin/sh

mkdir -p $PWD/nemo 

${CHICKEN_HOME}/bin/chicken-install -init $PWD/nemo

rm $PWD/nemo/setup-download.* $PWD/nemo/tcp.* $PWD/nemo/srfi-18.*

${CHICKEN_HOME}/bin/chicken-install -deploy -prefix $PWD/nemo -t local -l $EGGS_DIR \
 make matchable iexpr sxml-transforms ssax sxpath datatype vector-lib regex unitconv \
 digraph graph-bfs graph-cycles graph-scc mathh strictly-pretty varsubst lalr \
 silex ersatz utf8 uri-generic defstruct getopt-long dyn-vector iset input-parse nemo





