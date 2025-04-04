FROM clojure:temurin-23-tools-deps-bookworm-slim as build
COPY . /usr/src/app/
WORKDIR /usr/src/app
RUN clojure -T:build uber

FROM eclipse-temurin:23.0.2_7-jre-ubi9-minimal
COPY --from=build /usr/src/app/target/plauna-standalone.jar /app/
EXPOSE 8080
WORKDIR /app
CMD ["sh", "-c", "java -jar plauna-standalone.jar $PLAUNA_ARGS"]
