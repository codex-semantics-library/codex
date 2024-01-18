#!/bin/bash

compare(){
    rm -f works.dump cur.dump
    frama-c -load-module libase -machdep x86_32  -codex-debug 2 -kernel-debug 2 -codex-msg-key evaluating -load-module /home/matthieu/git/libase/frama-c/top/CodexPlugin -codex -codex-print  -codex-no-print-value -codex-msg-key evaluating  -codex-verbose-terms 0 -codex-domains 6  ./reduce.c -ulevel -1 -codex-exp-dump cur.dump -no-allow-duplication 2>&1 #| grep -F '[codex] Warning: Bad hash collision or mistake'
    frama-c -load-module libase -machdep x86_32  -codex-debug 2 -kernel-debug 2 -codex-msg-key evaluating -load-module /home/matthieu/git/libase/frama-c/top/CodexPluginNoWholify -codex -codex-print  -codex-no-print-value -codex-msg-key evaluating  -codex-verbose-terms 0 -codex-domains 6  ./reduce.c -ulevel -1 -codex-exp-dump works.dump -no-allow-duplication 2>&1 #| grep -F '[codex] Warning: Bad hash collision or mistake'    
#   frama-c reduce.c -load-module /home/matthieu/i/src/lsl/codex/CodexPluginWorks -codex -ulevel -1   -codex-no-print -codex-no-print-value -kernel-verbose 0  -codex-verbose 0 -codex-verbose-terms 0 -codex-exp-dump works.dump -main EMC_step1 -lib-entry
#      frama-c reduce.c -load-module /home/matthieu/i/src/lsl/codex/CodexPluginNoWorks -codex -ulevel -1   -codex-no-print -codex-no-print-value -kernel-verbose 0  -codex-verbose 0 -codex-verbose-terms 0 -codex-exp-dump cur.dump -main EMC_step1 -lib-entry
      diff -u works.dump cur.dump    
      if [ "$?" == 1 ]; then exit 0; else exit 1; fi
 }


greperror(){
    #          dune exec frama-c  -- -machdep x86_32  -codex-debug 2 -kernel-debug 2  -load-module /home/matthieu/git/libase/frama-c/top/CodexPlugin -codex -codex-print  -codex-no-print-value -codex-msg-key evaluating  -codex-verbose-terms 0 -codex-domains 6  ./reduce.c -ulevel -1 -codex-exp-dump test.dump -no-allow-duplication 2>&1 #| grep -F '"domains/constraint_domain2.ml", line 870, characters 14-20: Assertion failed).'
    dune exec frama-c -- -machdep x86_32 -codex -codex-odvtk -codex-debug 2  -codex-verbose-terms 0 -codex-domains 6 -kernel-verbose 0 -codex-verbose 2 -codex-no-print -codex-no-print-value -codex-exp-dump u.dump ./reduce.c   | grep -F '"odvtk/producer/log.ml", line 169, characters 48-54'
}


#compare
greperror

