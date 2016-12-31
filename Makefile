PROJECT = esse
PROJECT_DESCRIPTION = Essential Erlang SSE
PROJECT_VERSION = 0.1.0
HOST := `hostname`
ROOT := $(shell pwd)

# Set to always 4 spaces on generated code
SP=4

V = 0

DEPS = eper

ERLC_OPTS := +debug_info +"{cover_enabled, true}"

include erlang.mk

CMN_EFLAGS = \
	-boot start_sasl \
	-smp enable \
	-setcookie ${COOKIE} \
	-name ${PROJECT}@${HOST} \
	+P 1000000 \
	+K true +A 160 -sbt ts

DEV_EFLAGS = \
	-pa ${ROOT}/deps/*/ebin \
	-pa ${ROOT}/ebin \
	${CMN_EFLAGS}

REL_EFLAGS = \
	-pa ${ROOT_REL}/lib/*/ebin \
	${CMN_EFLAGS}

dev:
	erl ${DEV_EFLAGS} -s esse_app
