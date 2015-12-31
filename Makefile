.PHONY : compile test clean

compile:
	rm -f maxwell_protocol
	./rebar get-deps
	./rebar compile eu
	erl -pa deps/*/ebin -pa ebin  -name chan@127.0.0.1 -setcookie maxwell_backend  -s start_channel

test: compile
	./rebar eu

clean:
	rm -f ./ebin/* .eunit/* maxwell_protocol
