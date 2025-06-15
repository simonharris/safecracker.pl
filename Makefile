test:
	@swipl -g "load_files([tests/test_safe_cracker,\
						  tests/test_parser,\
						  tests/test_solutions_safe]), run_tests" -t halt

constraints:
	@grep -h "% [1-5]. " solutions/safe202* | cut -d' ' -f7-
