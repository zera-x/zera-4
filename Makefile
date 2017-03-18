.PHONY: clean deps

default: dist/pbnj.js

dist/pbnj.core.js:
	tsc --out dist/pbnj.core.js src/pbnj/core.ts

dist/pbnj.env.js:
	tsc --out dist/pbnj.env.js src/pbnj/env.ts

dist/pbnj.reader.js:
	tsc --out dist/pbnj.reader.js src/pbnj/reader.ts

dist/pbnj.wonderscript.js:
	tsc --out dist/pbnj.wonderscript.js src/pbnj/wonderscript.ts

dist/pbnj.js: dist/pbnj.core.js dist/pbnj.env.js dist/pbnj.reader.js dist/pbnj.wonderscript.js
	cat dist/pbnj.core.js dist/pbnj.env.js dist/pbnj.reader.js dist/pbnj.wonderscript.js > dist/pbnj.js

dist/pbnj-deps.js:
	cat dist/mori.js dist/pbnj.js > dist/pbnj-deps.js

clean:
	rm dist/pbnj.js
	rm dist/pbnj.core.js
	rm dist/pbnj.env.js
	rm dist/pbnj.reader.js
	rm dist/pbnj.wonderscript.js
	rm dist/pbnj.js.map
