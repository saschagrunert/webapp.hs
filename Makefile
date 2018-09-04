DIST_DIR := dist

UIKIT_DIR := $(DIST_DIR)/uikit
UIKIT_TAG := v3.0.0-rc.13
STYLE_SCSS := style.scss

.PHONY: \
	build \
	ghci \
	uikit \
	yesod

ifndef VERBOSE
.SILENT:
endif

all: build

build: uikit
	stack build

ghci: uikit
	stack ghci webapp:lib --no-load --work-dir .stack-work-devel

yesod: build
	stack exec -- yesod devel

uikit:
	if [ ! -d $(UIKIT_DIR) ]; then \
		mkdir -p $(DIST_DIR) ;\
		git clone -q https://github.com/uikit/uikit $(UIKIT_DIR) ;\
	fi ;\
	cd $(UIKIT_DIR) &&\
	git fetch -q --tags &&\
	git checkout -q $(UIKIT_TAG)
	cp config/$(STYLE_SCSS) $(DIST_DIR)
	sass -t compressed dist/$(STYLE_SCSS) > ./static/css/uikit.min.css
