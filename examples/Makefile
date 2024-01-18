FRAMA_C_CODEX=frama_c_codex.exe
# FRAMA_C = dune exec frama-c -- -machdep x86_32 -codex
BINSEC_CODEX=binsec_codex.exe
# BINSEC_CODEX=dune exec binsec_codex -- -codex


abs.cdump abs.c.html:
	$(FRAMA_C_CODEX) abs.c -codex-exp-dump abs.cdump -codex-html-dump abs.c.html -main abs

abs.exe:
	clang -m32 -O1 abs.c -o abs.exe

abs.bdump abs.exe.html:
	$(BINSEC_CODEX) abs.exe -entrypoint abs -codex-type-file abs.types -codex-output-html abs.exe.html
