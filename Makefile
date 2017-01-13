### Copyright (c) 2016-2017, DuoMark International, Inc. All Rights Reserved.
### Author Jay Nelson <jay@duomark.com>

### Project name and version are defined by relx.config line:
### {release, {esse, 0.2.0}, [esse]}
PROJECT_AWK := awk '/^{release,/ {sub(/^{release, {/,       ""); sub(/,.*/,  ""); print}'
VERSION_AWK := awk '/^{release,/ {sub(/^{release, [^ ]+ "/, ""); sub(/\".*/, ""); print}'

PROJECT         := $(shell ${PROJECT_AWK} <relx.config)
PROJECT_VERSION := $(shell ${VERSION_AWK} <relx.config)
PROJECT_DESCRIPTION = Essential Erlang Server Sent Events (SSE)

HOST := `hostname`
ROOT := $(shell pwd)

# Set to always 4 spaces on generated code
SP=4

V = 0

DEPS = eper jsone uuid epocxy

dep_jsone_commit = 1.4.0
dep_uuid_commit  = v1.5.4

PLT_APPS = crypto

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
