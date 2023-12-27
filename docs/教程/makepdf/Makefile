MARKDOWN_FILES := $(wildcard ../*.md)
combined_tutorial.pdf: $(MARKDOWN_FILES)
	pandoc $(MARKDOWN_FILES) -o combined_tutorial.pdf --pdf-engine=xelatex -V CJKmainfont="PingFang SC" --highlight-style=pygments -H template.tex --listings


clean:
	rm combined_tutorial.pdf