Java-build:
	javac -d dist javaVersion/javaFa.java

Java-run:
	java -cp dist javaVersion.javaFa

clean:
	rm -rf dist/*