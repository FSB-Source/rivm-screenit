module.exports = {
  ...require("../../../frontend-base/config/jest.config"),
  testPathIgnorePatterns: ['/__fixtures__/', '/__tests__/'],
  reporters: [ "default", "jest-junit" ],
};
