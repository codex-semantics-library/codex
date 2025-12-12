# Common makefile to run codex whole program analysis on standalone .c or .i files
# Used for the sv-comp benchmarks or the whole-program ones

# Change to 'frama-c -codex' or frama_c_codex to allow multithreading
# This requires installing the current version of codex first
FRAMA_C_CODEX  = dune exec frama_c_codex --

# Use on the command line to add extra flags
CODEX_EXTRA_FLAGS ?=

TIMEOUT ?= timeout 2m

CODEX_COMMAND = $(FRAMA_C_CODEX) \
	-machdep gcc_x86_32 \
	-codex -codex-overflow-alarms -codex-exp-dump \
	-codex-extra-metrics \
	-absolute-valid-range 0-0xFF \
	$(CODEX_EXTRA_FLAGS) \
	-codex-analyze-functions main

# Number of parallel threads
J = 4

# Path to csmith header files
CSMITH_INCLUDE ?= /usr/include/csmith

# Number of CSmith files to generate
CSMITH_NUMBER ?= 40

# Flags used when running CSmith
# --no-argc to generate a main function without arguments (otherwise we get a
#           [Types.TypedC] Fatal Error: Creating a pointer to Name(char)*
# --no-bitfields as this isn't handled by C2Codex (assertion failed in frama-c/C2Codex.ml, line 2335)
# --max-array-dim 2 as bigger depth cause failure (assertion failed in domains/memory/region_suffix_tree.ml", line 1868)
CSMITH_FLAGS := --no-argc --no-float --no-bitfields --max-array-dim 1

# TODO: We could provide an "SVCOMP-expected-outcome", and output what
# we found/what was expected, and use that as a first oracle. exp_dump
# would be indepenent, more to see what has changed.

ISATTY := $(shell [ -t 0 ] && echo 1)

# Define USE_COLOR=OFF or ON to override default behaviour
ifdef ISATTY
	COLOR ?= ON
	COLUMN ?= | column
else
	COLOR ?= OFF
	COLUMN ?=
endif

# color_print(color, text) to display text on screen with given color
# where color is the ANSI color number (31 = red, 33 = green, 32 = blue...)
ifeq ($(COLOR),ON)
define color_print =
	echo -e "\033[$(1)m$(2)\033[0m"
endef
color_yellow = \033[93;1m
color_orange = \033[33m
color_red    = \033[31m
color_green  = \033[32m
color_blue   = \033[34;1m
color_reset  = \033[0m
else
define color_print =
	echo -e "$(2)"
endef
endif

define time_with_output
  env time -f 'user_time=%U\nmemory=%M' -o "$(1)"
endef

# make_suite(name, input_folder, output_folder) finds all .c and .i files in the
# specified folder input folder and creates a target 'name' that runs the analysis
# on all of them and stores their result in output_folder/filename
#
# Note that to add the suite to the 'all' target, it should be appended to the
# SUITES variable (e.g.: 'SUITES += name')
#
# .i files are C files obtained after preprocessing, so they are a bit faster to
# parse. Only keep the .c files which don't have a corresponding .i
define make_suite
$(1)_I_FILES := $(notdir $(wildcard $(2)/*.i))
$(1)_C_FILES := $(notdir $(wildcard $(2)/*.c))
$(1)_FILTERED_C_FILES := $$(filter-out $$(patsubst %.i,%.c,$$($(1)_I_FILES)), $$($(1)_C_FILES))
$(1)_FILES := $$(addprefix $(3)/, $$($(1)_I_FILES) $$($(1)_FILTERED_C_FILES))
$(1): $$(addsuffix .cdump, $$($(1)_FILES))

parallel-$(1):
	FRAMA_C_CODEX="frama-c -codex" dune exec make -- -j 8  $(1)
endef

# Bash is required to extract the exit code from our pipeline
# in make_target, we run "codex ... | tee logfile", but tee always succeeds
# and we want to get codex's return code instead of tee's.
SHELL = bash

# Run a analysis, with the given TIMEOUT
# This generates the following files:
# - target.log - stdout and stderr of the analysis (also printed to the terminal)
# - target.main.cdump - only if the analysis succeeds, Codex text dump
# - target.stats.txt - only if the analysis succeeds, user_time, memory, timestamp, command
#
# Note that since target.dump is not created, this target is never up to date
# and thus the analysis is always rerun when make is called
#
# Also prints a pretty blue bar before each analysis, and a red/green bar after
# depending on the return status. DO NOT MODIFY THESE BARS, the "./manage.py run"
# script uses them to see when an analysis starts/stop.
#
# Does not fail even if the return status is invalid
#
# This is put in a define to allow changing some values:
# - the first arguments is the output prefix (eg. 'whole-program' or 'svbenchmarks4codex')
# - the second is the input prefix (eg. 'whole-program' or '$(SVCOMPCPATH)')
# - the third argument is provided to add flags to the codex call
define make_target
$1/%.cdump:
	$$(eval TGT = $1/$$*)
	@mkdir -p $$(dir $$(TGT))
	@echo
	@$$(call color_print,34,"==== Running codex on $$(TGT) ===============================================")
	$$(eval COMMAND = $$(CODEX_COMMAND) $2/$$* -codex-output-prefix $$(TGT) $3)
	@$$(call color_print,34,"Command: $$(COMMAND)")
	@echo
	@rm -rf $$(TGT).stats.txt $$(TGT).main.cdump $$(TGT).log
	@set -o pipefail; { $$(call time_with_output,$$(TGT).stats.txt) $$(TIMEOUT) $$(COMMAND) || ( \
	        $$(RM) $$(TGT).stats.txt && false) } 2>&1 | tee $$(TGT).log && { \
	    $$(call color_print,32,"==== Codex analysis finished on $$(TGT) ==============================================="); \
	    { \
	      printf 'timestamp=%s\n' "$$(shell date +"%Y-%m-%d %H:%M:%S")"; \
	      printf 'cmd_args=%s\n' "$$(COMMAND)"; \
	    } >> $$(TGT).stats.txt; \
	  } || $$(call color_print,31,"==== Codex analysis FAILED on $$(TGT) ===============================================")
	@echo
endef

SUITES =

include sv-benchmarks4codex/Makefile

$(eval $(call make_suite,whole-program,whole-program,whole-program))
$(eval $(call make_target,whole-program,whole-program))

$(eval $(call make_suite,csmith,csmith,csmith))
$(eval $(call make_target,csmith,csmith,-cpp-extra-args "\"-I$(CSMITH_INCLUDE)\""))

SUITES += whole-program csmith

.PHONY: all
all: $(SUITES) ## Run all suites

HELP_PADDING = 15
.PHONY: help
help: ## Show this help
	@echo -e  "$(color_yellow)make:$(color_reset) this makefile is used to run codex benchmarks. List of useful targets:"
	@egrep -h '\s##\s' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "  $(color_blue)%-$(HELP_PADDING)s$(color_reset) %s\n", $$1, $$2}'

.PHONY: list-suites
list-suites: ## Print a list of available benchmarks suites
	@echo $(sort $(SUITES)) | sed -e 's/ /\n/g' $(COLUMN)

.PHONY: list
list: ## Print a list of available benchmarks
	@$(file > .tmp,$(foreach suite, $(SUITES), $($(suite)_FILES)))
	@cat .tmp | sed -e 's/ /\n/g' $(COLUMN)
	@rm -f .tmp

parallel: ## Run all in parrallel, although I recommand using './manage.py run -j N' for the pretty display
	FRAMA_C_CODEX="frama-c -codex" dune exec make -- -j $J all

NUMBERS := $(shell seq -f "%02g" 1 $(CSMITH_NUMBER))

csmith/c%.c:
	csmith $(CSMITH_FLAGS) -o $@

# csmith/c%.i: csmith/c%.c
# 	gcc -I /usr/include/csmith -E -o $@ $<

CSMITH_FILES = $(addprefix csmith/c,$(addsuffix .c,$(NUMBERS)))
.PHONY: csmith-forge
csmith-forge: $(CSMITH_FILES) ## Use csmith to generate example files (from 1 to N)

.PHONY: csmith-clean
csmith-clean: ## Delete all csmith generated programs
	rm -f csmith/c*.c csmith/c*.i

.PHONY: csmith
csmith: csmith-forge
