
code:
	mkdir -p bin
	javac -d bin -classpath ".:tools/*" `find src -name "*.java"` 
	fsc -d bin -classpath ".:tools/*" `find src -name "*.java"` `find src -name "*.scala"`

tests: code
	scala -J-Xmx12g -J-Xss40m  -cp "bin:tools/*" org.scalatest.tools.Runner -R bin/ -s anchor.unit.SpecTests -o

strict-tests: code
	scala -J-Xmx12g -J-Xss40m  -cp "bin:tools/*" org.scalatest.tools.Runner -R bin/ -s anchor.unit.StrictSpecTests -o

wc:
	find src -name "*.scala" | xargs wc -l | tail -1

clean:
	fsc -shutdown
	rm -rf bin/*

all:	clean code wc
