FROM eclipse-temurin:27-jre-alpine
WORKDIR /app
COPY target/plauna-standalone.jar /app/plauna-standalone.jar
RUN mkdir -p /var/lib/plauna
CMD ["sh", "-c", "java -jar /app/plauna-standalone.jar $PLAUNA_ARGS"]
