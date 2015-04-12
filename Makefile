PROJECT = awre
CT_SUITES = eunit client
CT_OPTS = -cover test/cover.spec
ERLC_OPTS = +debug_info

DEPS = wamper
dep_wamper = git https://github.com/bwegh/wamper master

TEST_DEPS = erwa

include erlang.mk
