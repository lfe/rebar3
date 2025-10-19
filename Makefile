PROJECT = rebar3_lfe
ROOT_DIR = $(shell pwd)
SYS_TEST_DIR = $(ROOT_DIR)/priv/testing
SYS_TEST_DEPS = $(SYS_TEST_DIR)/_checkouts
SYS_TEST_REBAR3 = $(SYS_TEST_DEPS)/rebar3_lfe

check: clean
	@rebar3 lfe compile
	@rebar3 xref
	-@rebar3 dialyzer
	@rebar3 as test lfe ltest

clean:
	@rm -rf _build rebar.lock $(SYS_TEST_DEPS)

publish:
	@echo "\nPublishing to hex.pm ...\n"
	@rebar3 hex publish package

$(SYS_TEST_DIR):
	mkdir -p $(SYS_TEST_DIR)

$(SYS_TEST_DEPS): $(SYS_TEST_DIR)
	mkdir -p $(SYS_TEST_DEPS)

$(SYS_TEST_REBAR3): $(SYS_TEST_DEPS)
	cd $(SYS_TEST_DEPS) && \
	ln -s ../../../ rebar3_lfe

test-new: clean $(SYS_TEST_REBAR3)
	cd $(SYS_TEST_DIR) && \
	DEBUG=1 rebar3 new

test-new-lfe-lib: clean $(SYS_TEST_REBAR3)
	rebar3 compile
	cd $(SYS_TEST_DIR) && \
	rebar3 compile && \
	rebar3 new lfe-lib example-lib && \
	cd example-lib && \
	DEBUG=1 rebar3 lfe compile

test-new-lfe-main: clean $(SYS_TEST_REBAR3)
	rebar3 compile
	cd $(SYS_TEST_DIR) && \
	rebar3 compile && \
	rebar3 new lfe-main example-main && \
	cd example-main && \
	DEBUG=1 rebar3 lfe compile && \
	rebar3 lfe run -- 42

test-new-lfe-app: clean $(SYS_TEST_REBAR3)
	rebar3 compile
	cd $(SYS_TEST_DIR) && \
	rebar3 compile && \
	rebar3 new lfe-app example-app && \
	cd example-app && \
	DEBUG=1 rebar3 lfe compile

test-new-lfe-escript: clean $(SYS_TEST_REBAR3)
	rebar3 compile
	cd $(SYS_TEST_DIR) && \
	rebar3 compile && \
	rebar3 new lfe-escript example-escript && \
	cd example-escript && \
	DEBUG=1 rebar3 lfe compile && \
	rebar3 lfe escriptize && \
	rebar3 lfe run-escript 1 2 5

test-new-lfe-release: clean $(SYS_TEST_REBAR3)
	rebar3 compile
	cd $(SYS_TEST_DIR) && \
	rebar3 compile && \
	rebar3 new lfe-release example-release && \
	cd example-release && \
	DEBUG=1 rebar3 lfe compile && \
	rebar3 lfe release

test-versions-cmd: clean $(SYS_TEST_REBAR3)
	rebar3 compile
	cd $(SYS_TEST_DIR) && \
	rebar3 compile && \
	rebar3 lfe versions

test-clean-cmd: clean $(SYS_TEST_REBAR3)
	rebar3 compile
	cd $(SYS_TEST_DIR) && \
	rebar3 compile && \
	rebar3 lfe clean

test-clean-build-cmd: clean $(SYS_TEST_REBAR3)
	rebar3 compile
	cd $(SYS_TEST_DIR) && \
	rebar3 compile && \
	rebar3 lfe clean-build

smoke-tests: test-new test-new-lfe-lib test-new-lfe-main \
			 test-new-lfe-app test-new-lfe-escript test-new-lfe-release \
			 test-versions-cmd test-clean-cmd test-clean-build-cmd
