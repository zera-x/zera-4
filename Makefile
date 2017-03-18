.PHONY: clean deps all browser node clean-browser clean-node

CC=tsc

default: all

all: browser node

browser: dist/pbnj.js

node: dist/pbnj.core.js dist/pbnj.env.js dist/pbnj.reader.js dist/pbnj.wonderscript.js

dist/pbnj.core.js:
	$(CC) src/pbnj/core.ts

dist/pbnj.env.js:
	$(CC) src/pbnj/env.ts

dist/pbnj.reader.js:
	$(CC) src/pbnj/reader.ts

dist/pbnj.wonderscript.js:
	$(CC) src/pbnj/wonderscript.ts

dist/pbnj.js:
	$(CC) -p .

clean-node:
	rm dist/pbnj.core.js
	rm dist/pbnj.env.js
	rm dist/pbnj.reader.js
	rm dist/pbnj.wonderscript.js

clean-browser:
	rm dist/pbnj.js
	rm dist/pbnj.js.map

clean: clean-node clean-browser
