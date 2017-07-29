.PHONY: clean deps all browser node clean-browser clean-node

CC = tsc

default: all

all: browser node

browser: dist/pbnj.js

node: dist/core.js dist/reader.js dist/wonderscript.js

dist/core.js:
	$(CC) --outDir dist src/pbnj/core.ts

dist/reader.js:
	$(CC) --outDir dist src/pbnj/reader.ts

dist/wonderscript.js:
	$(CC) --outDir dist src/pbnj/wonderscript.ts

dist/pbnj.js:
	$(CC) -p . --m system --outFile dist/pbnj.js

clean-node:
	rm dist/core.js
	rm dist/reader.js
	rm dist/wonderscript.js

clean-browser:
	rm dist/pbnj.js
	rm dist/pbnj.js.map

clean: clean-node clean-browser
