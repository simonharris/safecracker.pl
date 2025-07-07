test: testprolog

testprolog:
	@swipl -g "load_files([parser/tests/test_safe_cracker,\
						  parser/tests/test_grammar,\
						  parser/tests/test_parser,\
						  parser/tests/test_solutions]), run_tests" -t halt


.PHONY: web
web:
	cd web/site/ && npm run serve -- --host 0.0.0.0

buildweb:
	cd web/site/ && npm run build

constraints:
	@grep -h "% [1-5]. "parser/solutions/safe202* | cut -d' ' -f7-


update:
	git stash
	git pull
	git stash apply
	touch wsgi.py
.PHONY: api


api:
	cd web/api2 && uvicorn main:app --reload

