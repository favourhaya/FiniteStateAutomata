Java-build:
	javac -d dist JavaFa/javaFa.java

Java-run:
	java -cp dist JavaFa/javaFa
clean:
	rm -rf dist/*