
all:
	pandoc --self-contained --template=src/template.html --css src/pandoc.css --standalone --toc --toc-depth=3 src/index.md > artifact.html
	pandoc --self-contained --template=src/template.html --css src/pandoc-pdf.css --standalone --toc --toc-depth=3 src/index.md > artifact-to-print.html
	pandoc --self-contained --css src/pandoc.css --standalone src/install.md > install.html

