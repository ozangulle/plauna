FROM eclipse-temurin:25-alpine
WORKDIR /app
COPY --from=build /usr/src/app/target/plauna-standalone.jar /app/plauna-standalone.jar
#EXPOSE 8080
RUN mkdir -p /var/lib/plauna
CMD ["sh", "-c", "java -jar /app/plauna-standalone.jar $PLAUNA_ARGS"]
