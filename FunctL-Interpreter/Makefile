# Makefile for Extended Lambda Calculus Interpreter

.PHONY: all clean test

all: lambda_interpreter test_extended_lambda

lambda_interpreter: lambda_interpreter.ml
	ocamlc -c lambda_interpreter.ml

test_extended_lambda: lambda_interpreter.cmo test_extended_lambda.ml
	ocamlc -o test_extended_lambda lambda_interpreter.cmo test_extended_lambda.ml

test: test_extended_lambda
	./test_extended_lambda

clean:
	rm -f *.cmo *.cmi lambda_interpreter test_extended_lambda