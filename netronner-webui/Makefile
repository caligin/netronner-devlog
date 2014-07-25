.PHONY: clean template

all: template

target:
	mkdir target

target/site: site | target
	cp -r site target

template: target/site vars.properties templatize.rb
	ruby templatize.rb