# Makefile for compiling and running term array implementation

# OCaml compiler and flags
OCAMLC = ocamlc
OCAMLOPT = ocamlopt
FLAGS = -w +a-4-70

# Source files
SOURCES = term_array.ml term_array_tests.ml
ADVANCED_TESTS = term_array.ml additional_tests.ml
MAIN_SOURCES = main.ml
DEMO_TC = term_array.ml tests_5.ml

# Executable names
TERM_ARRAY_EXE = term_array_tests
ADVANCED_TESTS_EXE = additional_tests
DEMO_TC_EXE = tests5

# Default target
all: $(TERM_ARRAY_EXE) $(ADVANCED_TESTS_EXE) 

# Compile term_array implementation
$(TERM_ARRAY_EXE): $(SOURCES)
	$(OCAMLC) $(FLAGS) -o $(TERM_ARRAY_EXE) $(SOURCES)

# Compile advanced tests
$(ADVANCED_TESTS_EXE): $(ADVANCED_TESTS)
	$(OCAMLC) $(FLAGS) -o $(ADVANCED_TESTS_EXE) $(ADVANCED_TESTS)

$(DEMO_TC_EXE): $(DEMO_TC)
	$(OCAMLC) $(FLAGS) -o $(DEMO_TC_EXE) $(DEMO_TC)

# Run the term_array tests
run_tc: $(TERM_ARRAY_EXE)
	./$(TERM_ARRAY_EXE)

# Run the advanced tests
run_advanced: $(ADVANCED_TESTS_EXE)
	./$(ADVANCED_TESTS_EXE)

demo_tc: $(DEMO_TC_EXE)
	./$(DEMO_TC_EXE)
# Run all tests
run_all: run_tc run_advanced

# Clean up compiled files
clean:
	rm -f *.cmi *.cmo *.cmx *.o $(TERM_ARRAY_EXE) $(ADVANCED_TESTS_EXE) $(MAIN_EXE) $(DEMO_TC_EXE)

# Aliases for running tests
run: run_tc

.PHONY: all run_tc run_advanced run_main run_all run clean
