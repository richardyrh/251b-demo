# Makefile for 251b-demo Chisel project

.PHONY: compile clean test help all

# Default target
all: compile

# Compile the project
compile:
	./mill 251b-demo.compile

# Clean build artifacts
clean:
	./mill clean 251b-demo.compile

# Run tests
test:
	./mill 251b-demo.test

# Show help
help:
	@echo "Available targets:"
	@echo "  make compile  - Compile the Chisel modules"
	@echo "  make clean    - Clean build artifacts"
	@echo "  make test     - Run tests"
	@echo "  make all      - Compile the project (default)"
	@echo "  make help     - Show this help message"
