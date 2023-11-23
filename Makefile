.PHONY: test clean

test:
	cask
	TESTING=1 cask exec buttercup -L . -l "tests/setup.el" --traceback pretty

clean:
	rm -rf .cask
	rm -rf .test-grammars
