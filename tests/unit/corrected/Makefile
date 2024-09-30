CC=gcc-10
CFLAGS=-g -Wall -DBINSEC -Wno-main -Wno-attributes -m32 -fno-stack-protector # -fno-pie 

CODES = rb_tree_after_cpp \
	rb_tree_after_cpp2 \
	rb_tree_after_cpp3 \
	
CODES_OPT=$(addsuffix -O0, $(CODES)) \
#	  $(addsuffix -O1, $(CODES)) \
	  $(addsuffix -O2, $(CODES)) \
	  $(addsuffix -O3, $(CODES))
CODES_EXE=$(addsuffix .exe, $(CODES_OPT))
CODES_OBJDUMP=$(addsuffix .objdump, $(CODES_OPT))

all: $(CODES_EXE) $(CODES_OBJDUMP)

%-O0.exe: %.c binsec-stubs.o
	$(CC) -O0 -o $@ $(CFLAGS) $^

%-O1.exe: %.c binsec-stubs.o
	$(CC) -O1 -o $@ $(CFLAGS) $^

%-O2.exe: %.c binsec-stubs.o
	$(CC) -O2 -o $@ $(CFLAGS) $^

%-O3.exe: %.c binsec-stubs.o
	$(CC) -O3 -o $@ $(CFLAGS) $^

%.objdump: %.exe
	objdump -d -M intel $< > $@

binsec-stubs.o: binsec-stubs.c
	$(CC) -o $@ -c $(CFLAGS) $<

.PHONY: clean all
.PRECIOUS: %.objdump
clean:
	rm -rf *.exe *.o *.objdump

.PHONY: cleancsv
cleancsv:
	rm $(addsuffix .csv,$(TARGETS))

TARGET_NAMES = test1 test2 test3 \

TARGETS_OPT=$(addsuffix -O0, $(TARGET_NAMES)) 
#	    $(addsuffix -O1, $(TARGET_NAMES)) \
	    $(addsuffix -O2, $(TARGET_NAMES)) \
	    $(addsuffix -O3, $(TARGET_NAMES))
	    
TARGETS = $(TARGETS_OPT)

#ANALYZE = ~/binsec/src/binsec -codex #-codex-debug-level 4
#ANALYZE = dune exec binsec_codex -- -codex
ANALYZE = OCAMLRUNPARAM="b" dune exec binsec_codex -- -codex-type-file types.c -codex -codex-debug-level 4

regression: bench.alarms
	cat bench.alarms
	diff bench.alarms bench.alarms.expected

bench.alarms: $(addsuffix .alarms,$(TARGETS))
	head -n 1 $< > $@
	for file in $^; do tail -n 1 $$file >> $@; done

%.alarms:
	echo -n '$*: ' > $@
	(make $* 2>&1 | grep 'Total alarms:' | cut -d ' ' -f 3) >> $@

bench.csv: $(addsuffix .csv,$(TARGETS))
	head -n 1 $< > $@
	for file in $^; do tail -n 1 $$file >> $@; done

%.csv:
	hyperfine --ignore-failure --export-csv $@ --max-runs 2 "make $* > /tmp/$*.log 2>&1"
	sed -i "1s/$$/,nalarm/" $@
	sed -i "2s/$$/,$$(grep 'Total alarms:' /tmp/$*.log | cut -d ' ' -f 3)/" $@

.PHONY: phony_explicit
	
	
test1-%: rb_tree_after_cpp-%.exe  phony_explicit
	@echo "===== Analyzing $@ ===="
	@$(ANALYZE) $< -entrypoint main_insert -codex-fn-args 'rbtree+,rbnode+'
	
test2-%: rb_tree_after_cpp2-%.exe  phony_explicit
	@echo "===== Analyzing $@ ===="
	@$(ANALYZE) $< -entrypoint main_insert -codex-fn-args 'rbtree+,rbnode+'
	
test3-%: rb_tree_after_cpp3-%.exe  phony_explicit
	@echo "===== Analyzing $@ ===="
	@$(ANALYZE) $< -entrypoint main_insert -codex-fn-args 'rbtree+,rbnode+'
	
