# To add a benchmark named NAME:
# 1. add NAME to BENCHMARKS
# 2. add the list of files in NAME_files (default: NAME.c)
# 3. add the list of function entry points in NAME_entry_functions (default: test)
# 4. add the list of type files in string_type_files (default: NAME_types.c)
# Then you can execute all function entry points using make NAME, or a specific entry function FUNC by make NAME_FUNC

################ Benchmark definitions

BENCHMARKS =	aliasing \
		aliasing2 \
		assert \
		assert2 \
		assert3 \
		assert4 \
		assume \
		bad_load \
		bad_loop \
		chandra_point \
		chandra_radio \
		comparison \
		comparison2 \
		comparison3 \
		counter \
		divide \
		endian \
		list_same \
		load \
		nested_loop \
		null_ptr \
		pair \
		pair2 \
		ptr_arith \
		reduce2 \
		reduce3 \
		reduce4 \
		reduce5 \
		store \
		string \
		variant \
		variant2 \
		variant_ptr \
		vector \
		fun_ptr \
		global \
		exists_fun \
		flexible \
		reduce12 \
		test_cache \
		infinite_loop


# Tests that do not work for missing files, etc.
#		paireq \
		reduce1 \
		reduce6 \
		reduce7 \



aliasing2_entry_functions = test test2

assert_entry_functions = test_eq test_eq2 test_eq_to_4 test_load_eq
assert_type_files = assert_types.c assert_types2.c

assert2_entry_functions = assert_plus10 assert_eq assert_assign

assert3_entry_functions = test1 test2 test3

assert4_entry_functions = test_eq test_diff
assert4_type_files = assert4_types.c assert4_types2.c

bad_load_entry_functions = test1 test2

chandra_point_entry_functions = test1 test2 test3

comparison_entry_functions = test_ge test_lt20 test_gt test_gt20 test_le test_le20 test_lt0

counter_entry_functions = incr incr2 decr decr2

endian_entry_functions = test1 test1_inverted test2 test3 test4 test5

flexible_entry_functions = read write iter read_wrong

list_same_entry_functions = list_iter list_iter_check

load_entry_functions = test test2
#load_type_files =

pair_entry_functions = test_pair_eq test_pair_of_pair_eq test_pair_of_pair_eq2 test_pair_of_pair_eq3 write_1_x_4

pair2_entry_functions = test_const1 test_const2 test_exist1 test_exist2 test_exist3

store_type_files = store_types.c store_types2.c store_types3.c

string_entry_functions = test_access zeros2 zeros5 fill_test_wrong zeros zeros3 fill_test zeros4
string_type_files = string_types.c string_types2.c string_types3.c

variant_type_files = variant_types.c variant_types3.c # variant_types2.c

vector_type_files = vector_types.c vector_types2.c

fun_ptr_entry_functions = test1 test2 test3 test4 test5 test6 test8 test9

test_cache_entry_functions = alloc_list alloc_pair test_pair

infinite_loop_entry_functions = main

################ Makefile configuration

.PHONY: all analyze clean $(FUNCTIONS)

FRAMAC_KERNEL_OPTIONS = -kernel-debug 0 -ulevel -1 -no-allow-duplication -machdep x86_32 -absolute-valid-range 100-200
FRAMAC_CODEX_OPTIONS = -codex-verbose 2 -codex-debug 4 -codex -codex-no-print-value -codex-verbose-terms 0 -codex-use-type-domain
FRAMAC_CODEX = dune exec frama-c -- $(FRAMAC_CODEX_OPTIONS) $(FRAMAC_KERNEL_OPTIONS)


#FOCUSING = -codex-no-focusing
#CC=clang
CC=gcc
#CC=gcc-10
OPTIM=-O1
CFLAGS=-g -Wall -Wno-main -DBINSEC -Wno-main -Wno-attributes -m32 -fno-stack-protector $(OPTIM)
BINSEC_CODEX = OCAMLRUNPARAM="b" dune exec binsec_codex --  -codex $(FOCUSING)


help:
	@echo "Usage:"
	@echo "======"
	@echo "make c_BENCH_ENTRY: Launch C analysis of BENCH with entry point ENTRY."
	@echo "make bin_BENCH_ENTRY: Launch binary analysis of BENCH with entry point ENTRY."
	@echo "make BENCH_ENTRY: Launch both the C and binary analysis of BENCH with entry point ENTRY."
	@echo "make c_BENCH: Launch C analysis for all the entry points in BENCH."
	@echo "make bin_BENCH: Launch binary analysis for all the entry points in BENCH."
	@echo "make BENCH: Launch both the C and binary analysis for all the entry points in BENCH."
	@echo "make c_analysis: Launch C analysis for all the benchmarks"
	@echo "make bin_analysis: Launch the binary analysis for all the benchmarks"
	@echo "See Makefile for adding new benchmarks."
	@echo ""
	@echo "List of benchmarks and entry points:"
	@echo "===================================="
	$(foreach bench,$(BENCHMARKS),@echo $(bench): $($(bench)_entry_functions))

clean:
	rm -f */*.cdump */*.exe */cfg.dot */file.uml */*~

analysis: c_analysis bin_analysis

c_analysis: $(addprefix c_, $(BENCHMARKS))
bin_analysis: $(addprefix bin_, $(BENCHMARKS))



################ Templates





# Template for each TYPE
# Arguments:
# $(1) = name for the benchmark
# $(2) = name of the function entry point
# $(3) = name of the types.c file
# $(4) = MAYBE: short name of the types.c file (without the trailing _types.c)

define TYPEFILE_template
c_$(1)_$(2)_$(3)dump:
	@echo
	@echo "====== C Analysis of $(1)_$(2)_$(3) ======"
	time $(FRAMAC_CODEX) -codex-exp-dump $(1)_$(2)_$(3)dump $($(1)_files) \
	 -codex-type-file $(3) -main $(2) \
	 && cat $(1)_$(2)_$(3)dump

bin_$(1)_$(2)_$(3): $(1).exe
	@echo
	@echo "====== Binary Analysis of $(1)_$(2)_$(3) ======"
	echo -n '$(1)_$(2)_$(3)$(OPTIM): ' > $(1)_$(2)_$(3).alarms
	(($(BINSEC_CODEX) $(1).exe -entrypoint $(patsubst main,main_for_binsec,$(2)) -codex-type-file $(3)) \
	2>&1 | grep 'Total alarms:' | cut -d ' ' -f 3) >> $(1)_$(2)_$(3).alarms

endef


# Arguments:
# $(1) = name for the benchmark
# $(2) = name of the function entry point
define FUNCTION_template

$$(foreach typfil,$$($(1)_type_files), \
	$$(eval $$(call TYPEFILE_template,$(1),$(2),$$(typfil))))

c_$(1)_$(2): $$(addsuffix dump, $$(addprefix c_$(1)_$(2)_,$$($(1)_type_files)))
bin_$(1)_$(2): $$(addprefix bin_$(1)_$(2)_,$$($(1)_type_files))

endef

define BENCHMARK_template
$(1)_entry_functions := $$(or $$($(1)_entry_functions), test)
$(1)_files := $$(or $$($(1)_files), $(1).c)
$(1)_type_files := $$(or $$($(1)_type_files), $(1)_types.c)

$$(foreach f,$$($(1)_entry_functions),$$(eval $$(call FUNCTION_template,$(1),$$(f))))

c_$(1): $$(addprefix c_$(1)_,$$($(1)_entry_functions))
bin_$(1): $$(addprefix bin_$(1)_,$$($(1)_entry_functions))
$(1):  c_$(1) bin_$(1)

$(1).exe: $$($(1)_files) support/binsec-stubs.o
	$$(CC) -o $(1).exe $$(CFLAGS) $$($(1)_files) support/binsec-stubs.o #-Dmain=main_for_binsec
endef

support/binsec-stubs.o: support/binsec-stubs.c
	$(CC) -o support/binsec-stubs.o -c $(CFLAGS) support/binsec-stubs.c


$(foreach bench,$(BENCHMARKS),$(eval $(call BENCHMARK_template,$(bench))))
