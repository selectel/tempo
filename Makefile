.PHONY: deps skiptest tempo eunit test doc clean distclean

REBAR=rebar

all: tempo

dev: dev-tempo eunit

deps:
	$(REBAR) get-deps

tempo: deps
	$(REBAR) compile

eunit: dev-tempo
	$(REBAR) -C rebar_dev.config eunit skip_deps=true

test: eunit

doc: dev-tempo
	$(REBAR) -C rebar_dev.config doc skip_deps=true

clean:
	$(REBAR) clean
	rm -rf priv ebin

distclean: clean
	rm -rf deps

# dev

dev-deps:
	$(REBAR) -C rebar_dev.config get-deps

dev-tempo: dev-deps
	$(REBAR) -C rebar_dev.config compile

dev-clean:
	$(REBAR) -C rebar_dev.config clean
	rm -rf priv ebin

dev-distclean: dev-clean
	rm -rf deps
