test:
	swipl -g "load_files([tests/test_safe_cracker, tests/test_solutions]), run_tests" -t halt
