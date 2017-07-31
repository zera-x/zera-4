CC = tsc

default: all

.PHONY: all
all: dist/core.js dist/reader.js dist/util.js

dist/core.js:
	($(CC) src/zera/core.ts --outDir dist >> /dev/null) || exit 0

dist/util.js:
	($(CC) src/zera/util.ts --outDir dist >> /dev/null) || exit 0

dist/reader.js:
	($(CC) src/zera/reader.ts --outDir dist >> /dev/null) || exit 0

.PHONY: install
install: all
	npm install -g .

.PHONY: clean
clean:
	rm dist/core.js
	rm dist/reader.js
	rm dist/util.js
