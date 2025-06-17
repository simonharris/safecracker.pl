test:
	@swipl -g "load_files([tests/test_safe_cracker,\
						  tests/test_grammar,\
						  tests/test_parser,\
						  tests/test_solutions]), run_tests" -t halt

constraints:
	@grep -h "% [1-5]. " solutions/safe202* | cut -d' ' -f7-
