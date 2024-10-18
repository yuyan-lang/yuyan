
ALL_PY_FILES = $(shell find . -name '*.py')

run_default: ./.yybuild.nosync/py/output.yybcb
	make -C ../yybcvm
	../yybcvm/build/native/vm.exe ./.yybuild.nosync/py/output.yybcb

./.yybuild.nosync/py/output.yybcb: $(ALL_PY_FILES)
	@echo DEPENDS $^
	cd .. && python3 py-interpreter/yybc_compiler.py 豫言编译器/入口。豫 豫言编译器/入口。豫
