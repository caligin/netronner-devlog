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

tarball: template
	GIT_COMMIT=$$(git log --pretty=format:'%H' -n 1); \
	IS_SNAPSHOT=$$(git st --porcelain . | grep -q '' && echo "-SNAPSHOT" || echo ""); \
	cd target; \
	tar cvzf netronner-ui-$$GIT_COMMIT$$IS_SNAPSHOT.tar.gz site
