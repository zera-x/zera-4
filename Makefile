CC = tsc

default: all

.PHONY: all
all: dist/core.js dist/reader.js dist/util.js dist/zera.js

dist/core.js:
	cp src/zera/core.js dist/core.js

dist/core-opt.js:
	closure-compiler -O ADVANCED dist/core.js > dist/core-opt.js

dist/util.js:
	($(CC) src/zera/util.ts --outDir dist >> /dev/null) || exit 0

dist/reader.js:
	($(CC) src/zera/reader.ts --outDir dist >> /dev/null) || exit 0

dist/zera.js: dist/core.js dist/reader.js dist/util.js dist/core.zera.js dist/js.zera.js dist/html.zera.js
	cat dist/mori.js dist/util.js dist/reader.js dist/core.zera.js dist/js.zera.js dist/html.zera.js dist/core.js > dist/zera.js

dist/core.zera.js:
	./bin/zera scripts/zera-to-js.zera src/zera/core.zera > dist/core.zera.js

dist/js.zera.js:
	./bin/zera scripts/zera-to-js.zera src/zera/js.zera > dist/js.zera.js

dist/html.zera.js:
	./bin/zera scripts/zera-to-js.zera src/zera/core/html.zera > dist/html.zera.js

.PHONY: install
install: all
	npm install -g .

.PHONY: clean
clean:
	rm dist/core.js
	rm dist/reader.js
	rm dist/util.js
	rm dist/core.zera.js
	rm dist/zera.js
	rm dist/html.zera.js
	rm dist/js.zera.js