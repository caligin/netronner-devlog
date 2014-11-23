.PHONY: clean target/site template run

all: template

clean:
	rm -rf target

target:
	mkdir target

target/site: site | target
	cp -r site target

template: target/site vars.properties templatize.rb
	ruby templatize.rb

run: template
	cd target/site && python -m SimpleHTTPServer 8888
