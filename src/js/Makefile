default: dist/pbnj.js

dist/pbnj.js:
	closure-compiler --create_source_map dist/pbnj.js.map --output_wrapper_file pbnj.out src/ > dist/pbnj.js

clean:
	rm dist/pbnj.js
	rm dist/pbnj.js.map
