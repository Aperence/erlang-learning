compile:
	find . -name "*.erl" | xargs erlc

clean:
	find . -name "*.beam" | xargs rm -f

.PHONY:
	clean
	compile