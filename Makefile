.PHONY: docs
docs:
	@find . -name *.lhs -print \
	| sed -e 's/\./docs/' \
	| sed -e 's/\.lhs/.html/' \
	| xargs -n 1 make

docs/%.html: %.lhs
	pandoc --css "./styles.css" --metadata pagetitle="$<" -s -o "$@" "$<"

clean:
	rm docs/*.html
