# Makefile for compiling and running term array implementation

# OCaml compiler and flags
OCAMLC = ocamlc
OCAMLOPT = ocamlopt
FLAGS = -w +a-4-70

# Source files
SOURCES = term_array.ml term_array_tests.ml
MAIN_SOURCES = main.ml

# Executable names
TERM_ARRAY_EXE = term_array_tests
MAIN_EXE = main_exe

# Default target
all: $(TERM_ARRAY_EXE) $(MAIN_EXE)

# Compile term_array implementation
$(TERM_ARRAY_EXE): $(SOURCES)
	$(OCAMLC) $(FLAGS) -o $(TERM_ARRAY_EXE) $(SOURCES)

# Compile main implementation
$(MAIN_EXE): $(MAIN_SOURCES)
	$(OCAMLC) $(FLAGS) -o $(MAIN_EXE) $(MAIN_SOURCES)

# Run the term_array tests
run_tc: $(TERM_ARRAY_EXE)
	./$(TERM_ARRAY_EXE)

# Run the main implementation
run_main: $(MAIN_EXE)
	./$(MAIN_EXE)

# Clean up compiled files
clean:
	rm -f *.cmi *.cmo *.cmx *.o $(TERM_ARRAY_EXE) $(MAIN_EXE)

# Aliases for running tests
run: run_tc

.PHONY: all run_tc run_main run clean
