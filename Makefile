PROJECT = awre
CT_SUITES = eunit client
CT_OPTS = -cover test/cover.spec
ERLC_OPTS = +debug_info

DEPS = erwalib
dep_erwalib = git https://github.com/bwegh/erwa_lib master

TEST_DEPS = erwa

include erlang.mk
