# Use ocamlfind to handle packages
OCAMLFIND = ocamlfind
OCAMLC = $(OCAMLFIND) ocamlc -package str
OCAMLLEX = ocamllex
OCAMLYACC = ocamlyacc
OCAMLDEP = $(OCAMLFIND) ocamldep -package str

# Source files
ML_SOURCES = ast.ml parser.ml lexer.ml ast_type_checker.ml interpreter.ml main.ml
MLI_SOURCES = parser.mli

# Generated files
LEXER_GEN = lexer.ml
PARSER_GEN = parser.ml parser.mli

# Object files
CMO_FILES = ast.cmo parser.cmo lexer.cmo ast_type_checker.cmo interpreter.cmo main.cmo

# Executable name
EXEC = interpreter

# Default target
all: $(EXEC)

# Run the executable
run: $(EXEC)
	./$(EXEC)

# Bytecode executable
$(EXEC): $(CMO_FILES)
	$(OCAMLC) -o $(EXEC) -linkpkg $(CMO_FILES)

# Generate lexer from .mll
lexer.ml: lexer.mll
	$(OCAMLLEX) lexer.mll

# Generate parser from .mly
parser.ml parser.mli: parser.mly
	$(OCAMLYACC) parser.mly

# Generic rules for OCaml compilation
%.cmo: %.ml
	$(OCAMLC) -c $<

%.cmi: %.mli
	$(OCAMLC) -c $<

# Clean target
clean:
	rm -f *.cmo *.cmi *.o $(LEXER_GEN) $(PARSER_GEN) $(EXEC) .depend

# Dependencies
depend: $(LEXER_GEN) $(PARSER_GEN) $(ML_SOURCES) $(MLI_SOURCES)
	$(OCAMLDEP) $^ > .depend

# Include dependencies
-include .depend

# Phony targets
.PHONY: all clean depend run