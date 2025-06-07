test:
	@swipl -g "load_files([tests/test_safe_cracker,\
						  tests/test_solutions_safe,\
						  tests/test_solutions_teaser]), run_tests" -t halt
