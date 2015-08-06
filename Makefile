PROJECT = coin
DEPS = cowboy jiffy gproc
dep_gproc = git https://github.com/uwiger/gproc master
SHELL_DEPS = tddreloader
SHELL_OPTS = -s tddreloader start -s coin_app start_all
include erlang.mk
