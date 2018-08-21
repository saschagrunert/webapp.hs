# Compiler configuration
.PHONY: \
	build-backend

ifndef VERBOSE
.SILENT:
else
GENERAL_ARGS += -v
endif

all: build-backend build-frontend

build-backend:
	stack build --stack-yaml=backend/stack.yaml $(GENERAL_ARGS)

build-frontend:
	stack build --stack-yaml=frontend/stack.yaml $(GENERAL_ARGS)
