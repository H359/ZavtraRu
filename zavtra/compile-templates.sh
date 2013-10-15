#!/bin/bash

for tpl in templates/*.spt; do
    echo "Compiling $tpl"
    spitfire-compile -O4 -v --x-disable-psyco --normalize-whitespace -X collapse-adjacent-text -X directly-access-defined-variables -X omit-local-scope-search -X no-cheetah-cheats -X no-cheetah-compatibility -X no-enable-psyco -X generate-unicode -X use-dependency-analysis $tpl
done