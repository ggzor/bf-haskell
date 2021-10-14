#!/usr/bin/env bash

set -euo pipefail

(( $# != 1 )) && echo "USAGE: $0 PROGRAM" && exit 1

printf "\
#include <stdio.h>

int main() {
  char cells[3000] = {0};
  char *p = cells;

";

(
sed -e 's/[^]\[\+\< \>,\. -]//g' \
  | sed -e 's/\+/++*p;/g' \
  | sed -e 's/-/--*p;/g' \
  | sed -e 's/</--p;/g' \
  | sed -e 's/>/++p;/g' \
  | sed -e 's/\./putchar(*p);/g' \
  | sed -e 's/,/*p = getchar();/g' \
  | sed -e 's/\[/while(*p) {/g' \
  | sed -e 's/]/}/g'
) < "$1"

printf "
}
";

