# Makefile for testing the matrix/vector language lexer

# OCaml compiler and tools
OCAMLC = ocamlc
OCAMLLEX = ocamllex

# Source files
SOURCES = token.ml lexer.ml
TEST_DRIVER = lexer_driver.ml

# Object files
OBJECTS = $(SOURCES:.ml=.cmo)

# Test input files (automatically find all test_input_*.txt files)
TEST_INPUTS = $(wildcard test_input_*.txt)
TEST_OUTPUTS = $(TEST_INPUTS:test_input_%.txt=test_output_%.txt)

# Main targets
all: lexer_driver $(TEST_OUTPUTS)

# Clean up generated files
clean:
	rm -f *.cmi *.cmo lexer.ml lexer_driver $(TEST_OUTPUTS)

# Generate lexer.ml from lexer.mll
lexer.ml: lexer.mll
	$(OCAMLLEX) $<

# Compile OCaml source files
%.cmo: %.ml
	$(OCAMLC) -c $<

# Build the lexer driver executable
lexer_driver: $(OBJECTS) $(TEST_DRIVER:.ml=.cmo)
	$(OCAMLC) -o $@ $^

# Create the lexer driver file if it doesn't exist
$(TEST_DRIVER):
	@echo "Creating lexer driver file: $(TEST_DRIVER)"
	@echo "open Token" > $(TEST_DRIVER)
	@echo "open Lexer" >> $(TEST_DRIVER)
	@echo "" >> $(TEST_DRIVER)
	@echo "let () =" >> $(TEST_DRIVER)
	@echo "  if Array.length Sys.argv < 2 then" >> $(TEST_DRIVER)
	@echo "    print_endline \"Usage: ./lexer_driver <input_file>\"" >> $(TEST_DRIVER)
	@echo "  else" >> $(TEST_DRIVER)
	@echo "    let filename = Sys.argv.(1) in" >> $(TEST_DRIVER)
	@echo "    try" >> $(TEST_DRIVER)
	@echo "      let tokens = tokenize_file filename in" >> $(TEST_DRIVER)
	@echo "      List.iter (fun t -> print_endline (token_to_string t)) tokens" >> $(TEST_DRIVER)
	@echo "    with" >> $(TEST_DRIVER)
	@echo "    | Lexical_error msg -> print_endline (\"Lexical error: \" ^ msg)" >> $(TEST_DRIVER)
	@echo "    | Sys_error msg -> print_endline (\"System error: \" ^ msg)" >> $(TEST_DRIVER)
	@echo "    | e -> print_endline (\"Error: \" ^ Printexc.to_string e)" >> $(TEST_DRIVER)

# Compile the lexer driver
$(TEST_DRIVER:.ml=.cmo): $(TEST_DRIVER) $(OBJECTS)
	$(OCAMLC) -c $(TEST_DRIVER)

# Run tests: process each input file and generate corresponding output file
test_output_%.txt: test_input_%.txt lexer_driver
	./lexer_driver $< > $@

# Run all tests
test: $(TEST_OUTPUTS)
	@echo "All tests completed."
	@for output in $(TEST_OUTPUTS); do \
		echo "\nResults for $$output:"; \
		echo "----------------------------------------"; \
		cat $$output | head -20; \
		if [ $$(wc -l < $$output) -gt 20 ]; then \
			echo "... (more tokens not shown)"; \
		fi; \
		echo "----------------------------------------"; \
	done

# Create sample test files if none exist
create_samples:
	@echo "Creating sample test input files..."
	@echo "/* Basic syntax test */\na := 5;\nb := 3.14;\nbool_var := true;" > test_input_01_basic.txt
	@echo "/* Vector test */\n3\n[1, 2, 3]\n\n/* Matrix test */\n2,2\n[[1, 2], [3, 4]]" > test_input_02_vectors.txt
	@echo "/* Special cases */\nmax := INT_MAX;\neps := epsilon;\n\n/* Error case */\n3\n[1, 2]  /* Dimension mismatch */" > test_input_03_special.txt
	@echo "Sample files created. Run 'make' to process them."

# Dependencies
token.cmo: token.ml
lexer.cmo: lexer.ml token.cmo

.PHONY: all clean test create_samples
