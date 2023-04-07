all:
	rm -rf apps/kube/src/*~ *~;
	rm -rf apps/kube/src/*.beam;
	rm -rf test/*.beam test/*~;
	rm -rf config/*~;
	rm -rf  _build;
	rebar3 release;
	rm -rf _build*;
	git add -f *;
	git commit -m $(m);
	git push;
	echo Ok there you go!make
build:
	rm -rf apps/kube/src/*~ *~;
	rm -rf apps/kube/src/*.beam;
	rm -rf test/*.beam test/*~;
	rm -rf  _build/test; # A bugfix in rebar3 or OTP
	rm -rf  _build;
	rebar3 release;
	rm -rf _build*

clean:
	rm -rf  *~ */*~ src/*.beam tests/*.beam
	rm -rf erl_cra*;
	rm -rf spec.*;
	rm -rf tests_ebin
	rm -rf ebin;
	rm -rf Mnesia.*;
	rm -rf *.dir;
	rm -rf common;
	rm -rf sd;
	rm -rf nodelog;
	rm -rf etcd;
prod:
	rm -rf config/*~;
	rm -rf test/*.beam test/*~;	
	rebar3 as prod release;
	rebar3 as prod tar;
	rm /home/joq62/erlang/infra/api_repo/kube.api;
	cp api/kube.api /home/joq62/erlang/infra/api_repo;
	mv _build/prod/rel/kube/*.tar.gz ../release 

dev:
	rm -rf config/*~;
	rm -rf rebar.lock;
	rm -rf test/*.beam test/*~;
	rm -rf apps/kube/src/*~ *~;
	rm -rf apps/kube/src/*.beam;
	rm -rf  _build;
	rebar3 release;
	rebar3 ct
eunit:
	rm -rf config/*~;
	rm -rf rebar.lock;
	rm -rf test/*.beam test/*~;
	rm -rf apps/kube/src/*~ *~;
	rm -rf apps/kube/src/*.beam;
	rm -rf  _build;
	rm -rf api/*;
	rebar3 release;
	rm -rf test_ebin;
	mkdir test_ebin;
	cp apps/kube/src/*.api api;
	erlc -I api -I /home/joq62/erlang/infra/api_repo -o test_ebin test/*.erl;
	erl -pa _build/default/lib/*/* -pa test_ebin -sname do_test -run $(m) start $(a) $(b) -setcookie $(c)
