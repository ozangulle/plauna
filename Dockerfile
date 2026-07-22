FROM clojure:temurin-25-tools-deps-bookworm-slim AS build
WORKDIR /usr/src/app
RUN apt-get update \
  && apt-get install -y --no-install-recommends nodejs npm \
  && rm -rf /var/lib/apt/lists/*
COPY package.json ./
COPY package-lock.json* ./
RUN npm ci
COPY . .
RUN clojure -M:cljs release app
RUN clojure -T:build uber


FROM eclipse-temurin:25-alpine
WORKDIR /app
COPY --from=build /usr/src/app/target/plauna-standalone.jar /app/plauna-standalone.jar
EXPOSE 8080
RUN mkdir -p /var/lib/plauna
CMD ["sh", "-c", "java -jar /app/plauna-standalone.jar $PLAUNA_ARGS"]