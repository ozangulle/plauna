FROM clojure:temurin-23-tools-deps-1.12.0.1479-bookworm-slim as build
COPY . /usr/src/app/
WORKDIR /usr/src/app
RUN clojure -T:build uber

FROM eclipse-temurin:23-jre-noble
COPY --from=build /usr/src/app/target/plauna-0.0.1-standalone.jar /app/
EXPOSE 8080
WORKDIR /app
CMD ["sh", "-c", "java -jar plauna-0.0.1-standalone.jar $PLAUNA_ARGS"]
