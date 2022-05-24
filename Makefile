# Build configuration
# -------------------
APP_NAME = `grep -Eo 'name: .*' package.yaml | cut -d ':' -f 2 | awk '{print $1}'`
GIT_BRANCH = `git rev-parse --abbrev-ref HEAD`
GIT_REVISION = `git rev-parse HEAD`

# Introspection targets
# ---------------------

.PHONY: help
help: header targets

.PHONY: header
header:
	@printf "\n\033[34mEnvironment\033[0m\n"
	@printf "\033[34m---------------------------------------------------------------\033[0m\n"
	@printf "\033[33m%-23s\033[0m" "APP_NAME"
	@printf "\033[35m%s\033[0m" $(APP_NAME)
	@echo ""
	@printf "\033[33m%-23s\033[0m" "GIT_BRANCH"
	@printf "\033[35m%s\033[0m" $(GIT_BRANCH)
	@echo ""
	@printf "\033[33m%-23s\033[0m" "GIT_REVISION"
	@printf "\033[35m%s\033[0m" $(GIT_REVISION)
	@echo ""

.PHONY: targets
targets:
	@printf "\n\033[34mTargets\033[0m\n"
	@printf "\033[34m---------------------------------------------------------------\033[0m\n"
	@perl -nle'print $& if m{^[a-zA-Z_-]+:.*?## .*$$}' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-22s\033[0m %s\n", $$1, $$2}'

# Build targets
# -------------------

.PHONY: build
build: ## Make a production build
	stack build

.PHONY: build-watch
build-watch: ## Watch files for changes and re-build
	stack build --file-watch

.PHONY: clean
clean: ## Remove artificats
	stack clean


# Development targets
# -------------------

.PHONY: format
format: ## Format code
	echo brittany --write-mode=inplace app/**/*.hs src/**/*.hs test/**/*.hs

.PHONY: run
run: ## Run project
	stack exec -- arbiter-exe --verbose

.PHONY: test
test: ## Test code
	stack test --coverage
