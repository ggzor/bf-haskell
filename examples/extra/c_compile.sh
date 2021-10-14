#!/usr/bin/env bash

set -euo pipefail

(( $# != 1 )) && echo "USAGE: $0 PROGRAM" && exit 1

printf "\
#include <stdio.h>

int main() {
  char cells[3000] = {0};
  char *p = cells;

";

sed -e '
s/[^]\[\+\< \>,\. -]//g;
s/\+/++*p;/g;
s/-/--*p;/g;
s/</--p;/g;
s/>/++p;/g;
s/\./putchar(*p);/g;
s/,/*p = getchar();/g;
s/\[/while(*p) {/g;
s/]/}/g;
' < "$1"

printf "
}
";

