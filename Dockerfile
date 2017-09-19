FROM node:7.8.0

RUN mkdir -p /app
WORKDIR /app

COPY "node_modules/" "node_modules"
COPY "static/" "static/"
COPY "dist/" "dist"

# RUN gzip static/dist/bundle.js

ENV NODE_ENV production

EXPOSE 3000

CMD NODE_ENV=production node dist/server.js
