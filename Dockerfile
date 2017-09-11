FROM node:7.8.0

COPY "node_modules/" "./node_modules"
COPY "static/" "./static/"
COPY "dist/" "./dist"

ENV NODE_ENV production

EXPOSE 3000

CMD node ./dist/server.js