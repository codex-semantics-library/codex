#!/usr/bin/env bash

# Note: do not call dune exec from here, as this prevents parallel execution.
# The make creduce targets works better.

# The directory containing this script (https://stackoverflow.com/a/246128)
scriptdir=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )
# The path to codex
codexpath=$scriptdir/..

# Compare two runs of codex on the file ./reduce.c (diff their output)
# This requires having multiple version of codex:
# - Option 1: have two local clones, and run "dune exec" in each
# - Option 2: "dune install" one version, and run it with "frama-c" directly,
#             and use "dune exec" for the other
# - Option 3: use the frama_c_codex executables directly
compare(){
    rm -f works.dump cur.dump
    flags = -machdep x86_32 -codex $CODEX_OPTIONS -codex-no-fixpoint-use-regexp
    # frama-c -load-module libase -machdep x86_32  -codex-debug 2 -kernel-debug 2 -codex-msg-key evaluating -load-module /home/matthieu/git/libase/frama-c/top/CodexPlugin -codex -codex-print  -codex-no-print-value -codex-msg-key evaluating  -codex-verbose-terms 0 -codex-domains 6  ./reduce.c -ulevel -1 -codex-exp-dump cur.dump -no-allow-duplication 2>&1 #| grep -F '[codex] Warning: Bad hash collision or mistake'
    # frama-c -load-module libase -machdep x86_32  -codex-debug 2 -kernel-debug 2 -codex-msg-key evaluating -load-module /home/matthieu/git/libase/frama-c/top/CodexPluginNoWholify -codex -codex-print  -codex-no-print-value -codex-msg-key evaluating  -codex-verbose-terms 0 -codex-domains 6  ./reduce.c -ulevel -1 -codex-exp-dump works.dump -no-allow-duplication 2>&1 #| grep -F '[codex] Warning: Bad hash collision or mistake'
    # frama-c reduce.c -load-module /home/matthieu/i/src/lsl/codex/CodexPluginWorks -codex -ulevel -1   -codex-no-print -codex-no-print-value -kernel-verbose 0  -codex-verbose 0 -codex-verbose-terms 0 -codex-exp-dump works.dump -main EMC_step1 -lib-entry
    # frama-c reduce.c -load-module /home/matthieu/i/src/lsl/codex/CodexPluginNoWorks -codex -ulevel -1   -codex-no-print -codex-no-print-value -kernel-verbose 0  -codex-verbose 0 -codex-verbose-terms 0 -codex-exp-dump cur.dump -main EMC_step1 -lib-entry
    DIR=`pwd`
    # CODEX_OPTIONS="-lib-entry -main EMC_step5"
    # This should be the test version
    cd $codexpath && dune exec -- frama-c $flags -codex-exp-dump $DIR/cur.dump $DIR/reduce.c
    # This should be the working version (expected result)
    cd $codexpath && frama-c $flages -codex-exp-dump $DIR/works.dump $DIR/reduce.c
    diff -u $DIR/works.dump $DIR/cur.dump
    if [ "$?" == 1 ]; then exit 0; else exit 1; fi
 }

# Only run one version of codex, and use grep to check for an error message in the output
greperror(){
    # dune exec frama-c  -- -machdep x86_32  -codex-debug 2 -kernel-debug 2  -load-module /home/matthieu/git/libase/frama-c/top/CodexPlugin -codex -codex-print  -codex-no-print-value -codex-msg-key evaluating  -codex-verbose-terms 0 -codex-domains 6  ./reduce.c -ulevel -1 -codex-exp-dump test.dump -no-allow-duplication 2>&1 #| grep -F '"domains/constraint_domain2.ml", line 870, characters 14-20: Assertion failed).'
    DIR=`pwd`
    cd $codexpath && frama-c -machdep x86_32 -codex-debug 2 -kernel-debug 2 -ulevel -1 -codex-use-type-domain -codex -codex-exp-dump reduce.dump  $DIR/reduce.c 2>&1 | grep -F '"utils/tracelog/terminal.ml", line 391'
}

infiniteloop(){
    DIR=`pwd`
    cd $codexpath && timeout 2s \
        frama_c_codex -no-autoload-plugins -codex-debug 0 -codex \
        -codex-no-print-value -codex-verbose-terms 0 -codex-use-type-domain \
        -kernel-debug 0 -ulevel -1 -no-allow-duplication -machdep gcc_x86_32 \
        -absolute-valid-range 100-200  -codex-exp-dump rbtree_RB_INSERT_COLOR.cdump $DIR/reduce.c \
        -cpp-extra-args=' "-D__RUNTIME_CHECK(x)=({if(!(x)){ error(\"runtime check failed\"); exit(1); }})"' \
        -codex-html-dump rbtree_RB_INSERT_COLOR.html \
        -codex-type-file $codexpath/benchmarks/types-benchmarks/vmcai2022/rbtree/rbtree_spec.typ -main rbtree_RB_INSERT_COLOR
    # frama-c -machdep x86_32 -codex-debug 2 -kernel-debug 2 -ulevel -1 -codex-use-type-domain -codex-type-file $codexpath/benchmarks/types-benchmarks/vmcai2022/rbtree/rbtree_spec.typ -codex -codex-exp-dump reduce.dump  $DIR/reduce.c 2>&1
    if [ "$?" == 124 ]; then exit 0; else exit 1; fi
}

#compare
#greperror
infiniteloop
