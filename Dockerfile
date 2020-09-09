FROM node:8.0.0

RUN mkdir -p /code
WORKDIR /code
ADD . /code

ENV NODE_ENV production

RUN npm install yarn && \
    yarn && \
    yarn run postinstall && \
    NODE_ENV=production yarn run build && \
    yarn cache clean

EXPOSE 3000

CMD NODE_ENV=production node ./dist/server.js
