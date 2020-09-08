FROM node:8.0.0

RUN mkdir -p /code
WORKDIR /code
ADD . /code

RUN npm install yarn && \
    yarn && \
    yarn run postinstall && \
    yarn run build && \
    yarn cache clean


ENV NODE_ENV production

EXPOSE 3000

CMD NODE_ENV=production node dist/server.js
