'use strict';

exports.config = {
  title: 'CodeSaga',
  public_path: process.env.NODE_ENV === 'production' ? '/dist' : 'http://localhost:8080/static/dist'
};
