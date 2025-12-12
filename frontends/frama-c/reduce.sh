#!/usr/bin/env bash

# Note: do not call dune exec from here, as this prevents parallel execution.
# The make creduce targets works better.

# The directory containing this script (https://stackoverflow.com/a/246128)
scriptdir=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )
# The path to codex
codexpath=$scriptdir/..

codexpath1=/home/fxdx/Desktop/codex
codexpath2=/home/fxdx/Desktop/codex2

DIR=$(pwd)

entry_func="main"
typedc="/home/fxdx/Desktop/codex/frama-c/test.types"

# Flags passed to codex
flags="-codex -machdep x86_32 -codex-analyze-functions $entry_func -cpp-extra-args \"-I/usr/include/csmith\""

# Compare two runs of codex on the file ./reduce.c (diff their output)
# This requires having multiple version of codex:
# - Option 1: have two local clones, and run "dune exec" in each
# - Option 2: "dune install" one version, and run it with "frama-c" directly,
#             and use "dune exec" for the other
# - Option 3: use the frama_c_codex executables directly
compare(){
    rm -f works.dump cur.dump
# You need to explicitely write your flags and optional deps

    # This should be the test version
    cd "$codexpath1" && nix develop --impure -c bash -c "dune exec frama-c/frama_c_codex.exe -- $flags -codex-exp-dump $DIR/works.dump $DIR/reduce.c" > "$DIR/new.log"
    cd "$codexpath2" && dune exec frama-c/frama_c_codex.exe -- $flags -codex-exp-dump "$DIR/cur.dump" "$DIR/reduce.c" > "$DIR/old.log"     # This should be the working version (expected result)
    # cd "$codexpath" && frama-c $flags -codex-exp-dump "$scriptdir"/reduce.c && mv "$codexpath"/main.cdump "$DIR"/works.dump

    # Method 1: fail on any diff
    diff -u "$DIR"/works.dump "$DIR"/cur.dump
    if [ "$?" == 1 ]; then exit 0; else exit 1; fi

    # Method 2 : fail on a specific error in the diff
    # diff -u --suppress-common-lines $DIR/works.dump $DIR/cur.dump | grep "Memory_access"
}

# Only run one version of codex, and use grep to check for an error message in the output
greperror(){
    # dune exec frama-c  -- -machdep x86_32  -codex-debug 2 -kernel-debug 2  -load-module /home/matthieu/git/libase/frama-c/top/CodexPlugin -codex -codex-print  -codex-no-print-value -codex-msg-key evaluating  -codex-verbose-terms 0 -codex-domains 6  ./reduce.c -ulevel -1 -codex-exp-dump test.dump -no-allow-duplication 2>&1 #| grep -F '"domains/constraint_domain2.ml", line 870, characters 14-20: Assertion failed).'
    cd "$codexpath" && frama-c $flags -codex-exp-dump reduce.dump "$DIR"/reduce.c 2>&1 | grep -F 'File "domains/memory/region_suffix_tree.ml", line 2094, characters 10-16: Assertion failed'
}

infiniteloop(){
    cd "$codexpath" && timeout 2s \
        frama_c_codex -no-autoload-plugins -codex-debug 0 -codex \
	-codex-use-type-domain \
        -kernel-debug 0 -ulevel -1 -no-allow-duplication -machdep gcc_x86_32 \
        -absolute-valid-range 100-200  -codex-exp-dump rbtree_RB_INSERT_COLOR.cdump "$DIR"/reduce.c \
        -cpp-extra-args=' "-D__RUNTIME_CHECK(x)=({if(!(x)){ error(\"runtime check failed\"); exit(1); }})"' \
        -codex-html-dump rbtree_RB_INSERT_COLOR.html \
        -codex-type-file "$codexpath"/benchmarks/types-benchmarks/vmcai2022/rbtree/rbtree_spec.typ -main rbtree_RB_INSERT_COLOR
    # frama-c -machdep x86_32 -codex-debug 2 -kernel-debug 2 -ulevel -1 -codex-use-type-domain -codex-type-file $codexpath/benchmarks/types-benchmarks/vmcai2022/rbtree/rbtree_spec.typ -codex -codex-exp-dump reduce.dump  $DIR/reduce.c 2>&1
    if [ "$?" == 124 ]; then exit 0; else exit 1; fi
}

compare
# greperror
# infiniteloop
