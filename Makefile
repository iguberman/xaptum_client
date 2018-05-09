BASEDIR = $(shell pwd)
REBAR = rebar3
APPNAME = enfddsc
RELPATH = _build/default/rel/$(APPNAME)
BUILDDIR = _build
APPVSN = 1.0
SHELL = /bin/bash
PRIVDIR = $(BASEDIR)/priv
IDF = $(PRIVDIR)/build_id
ID_CMD = echo "`date +'%y-%m-%d %H:%M:%S'`" > $(IDF)
DEBUG=1

compile:
	$(REBAR) compile

privdir:
	mkdir -p $(PRIVDIR)

id: privdir
	$(ID_CMD)

release: id
	$(REBAR) release

console: release
	$(BASEDIR)/_build/default/rel/$(APPNAME)/bin/$(APPNAME) console

device-release: id
	$(REBAR) as device release

device-tar: device-release
	$(REBAR) as device tar

device-console: device-release
	$(BASEDIR)/_build/device/rel/$(APPNAME)/bin/$(APPNAME) console

subscriber-release: id
	$(REBAR) as subscriber release

subscriber-console: subscriber-release
	$(BASEDIR)/_build/subscriber/rel/$(APPNAME)/bin/$(APPNAME) console

subscriber-tar: subscriber-release
	$(REBAR) as subscriber tar

testrel:
	$(REBAR) as test release

test:	compile testrel
	$(REBAR) ct --dir $(BASEDIR)/ct --include $(BASEDIR)/include --logdir $(BASEDIR)/ct/log

dialyzer: test
	$(REBAR) dialyzer

xref:
	$(REBAR) as device xref

clean:
	$(REBAR) clean
	rm -rf _build
