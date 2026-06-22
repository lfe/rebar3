PROJECT = rebar3_lfe
ROOT_DIR = $(shell pwd)
SYS_TEST_DIR = /tmp/rebar3_lfe/_integration/_testing
GLOBAL_INSTALL_DIR = ~/.config/rebar3/plugins
GLOBAL_INSTALL = $(GLOBAL_INSTALL_DIR)/$(PROJECT)

.PHONY: all compile clean check test coverage benchmarks smoke-tests test-format-e2e ci

all: compile

compile:
	@rebar3 compile

clean:
	@rm -rf _build rebar.lock $(SYS_TEST_DIR) $(GLOBAL_INSTALL) ebin test/*.beam

check: clean
	@rebar3 compile
	@rebar3 xref
	@rebar3 dialyzer
	@rebar3 as test proper -c
	@rebar3 as test ct
	@rebar3 as test cover -v

test: clean
	@rebar3 as test ct
	-@rebar3 as test proper -c

coverage: clean
	@rebar3 as test do ct, cover -v

benchmarks: compile
	@rebar3 shell --eval "r3lfe_benchmarks:run_all(), init:stop()."

# Quality checks
xref:
	@rebar3 xref

dialyzer:
	@rebar3 dialyzer

quality: xref dialyzer

# CI/CD helper
ci: clean compile quality test coverage

# Publish to hex.pm
publish: clean
	@echo "\nPublishing to hex.pm ...\n"
	@rebar3 hex publish package

$(SYS_TEST_DIR):
	mkdir -p $(SYS_TEST_DIR)
	cp priv/testing/rebar.config $(SYS_TEST_DIR)/rebar.config

$(GLOBAL_INSTALL_DIR):
	mkdir -p $(GLOBAL_INSTALL_DIR)

setup: $(SYS_TEST_DIR)
setup: $(SYS_TEST_DIR)
	-git branch -D integration-testing
	git checkout -b integration-testing
	@git fetch origin
	@if git rev-parse --verify origin/integration-testing >/dev/null 2>&1; then \
		LOCAL=$$(git rev-parse integration-testing); \
		REMOTE=$$(git rev-parse origin/integration-testing); \
		if [ "$$LOCAL" = "$$REMOTE" ]; then \
			echo "Remote branch is already up to date"; \
		else \
			echo "Remote branch differs, force pushing..."; \
			git push origin integration-testing -f; \
		fi \
	else \
		echo "Remote branch doesn't exist, creating..."; \
		git push origin integration-testing; \
	fi
	git switch -

test-new: clean setup
	rebar3 compile
	cd $(SYS_TEST_DIR) && \
	rebar3 new

test-new-lfe-lib: clean setup
	rebar3 compile
	cd $(SYS_TEST_DIR) && \
	rebar3 compile && \
	rebar3 new lfe-lib example-lib && \
	cd example-lib && \
	rebar3 lfe compile

test-new-lfe-main: clean setup
	rebar3 compile
	cd $(SYS_TEST_DIR) && \
	rebar3 compile && \
	rebar3 new lfe-main example-main && \
	cd example-main && \
	rebar3 lfe compile && \
	rebar3 lfe run -- 42

test-new-lfe-app: clean setup
	rebar3 compile
	cd $(SYS_TEST_DIR) && \
	rebar3 compile && \
	rebar3 new lfe-app example-app && \
	cd example-app && \
	rebar3 compile

test-new-lfe-escript: clean setup
	rebar3 compile
	cd $(SYS_TEST_DIR) && \
	rebar3 compile && \
	rebar3 new lfe-escript example-escript && \
	cd example-escript && \
	rebar3 compile && \
	rebar3 lfe escriptize && \
	rebar3 lfe run-escript 1 2 5

test-new-lfe-release: clean setup
	rebar3 compile
	cd $(SYS_TEST_DIR) && \
	rebar3 compile && \
	rebar3 new lfe-release example-release && \
	cd example-release && \
	rebar3 compile && \
	rebar3 lfe release

test-versions-cmd: clean setup
	rebar3 compile
	cd $(SYS_TEST_DIR) && \
	rebar3 compile && \
	rebar3 lfe versions

test-clean-cmd: clean setup
	rebar3 compile
	cd $(SYS_TEST_DIR) && \
	rebar3 compile && \
	rebar3 lfe clean

test-format-e2e:
	@bash test/e2e/format_e2e.sh

smoke-tests: test-new test-new-lfe-lib test-new-lfe-main \
			 test-new-lfe-app test-new-lfe-escript test-new-lfe-release \
			 test-versions-cmd test-clean-cmd
