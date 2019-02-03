#!/usr/bin/env sh

# Generate an AUTHORS file based on the output of git shortlog. It uses ABC
# order, strips out leading spaces and numbers, then filters out specific
# authors.

git shortlog -se \
    | sed -E -e 's/[[:blank:]]+[0-9]+[[:blank:]]+//' \
    | sed 's/Peterpaul Klein Haneveld/Peterpaul Taekele Klein Haneveld/' \
    | uniq \
          > AUTHORS
