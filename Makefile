# Auto-generated -- do not modify!

SOURCE_DIR := $(shell dirname ${MAKEFILE_LIST})
MAKEINC := ${NTCL_ROOT}/ntcl-build/makefile_fragments

include ${MAKEINC}/standard_preample.mk

modules      += string property dictionary measurement quantum_number commandline configuration iterator iterator_builder iterator_constraints domain domain_builder interoptability assert timer readers
test_modules += string property dictionary measurement quantum_number commandline configuration iterator iterator_builder iterator_constraints domain domain_builder interoptability assert timer readers

modules      += api
test_modules += api

test_modules += unittest

library_name := libntcl-util.a

external_include := 
external_libraries := 
internal_include_dirs := 


include ${MAKEINC}/standard_defs.mk
