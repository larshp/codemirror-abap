const path = require('path');

module.exports = {
  transform: {
    '^.+\.(t|j)sx?$': [
      'ts-jest',
      {
        tsconfig: path.resolve(__dirname, '../config/tsconfig.json'),
      },
    ],
  },
  rootDir: path.resolve(__dirname, '../'),
  testRegex: '(/__tests__/.*|(\.|/)(test|spec))\.(jsx?|tsx?)$',
  moduleFileExtensions: ['ts', 'tsx', 'js', 'jsx', 'json', 'node'],
  coverageDirectory: './coverage/',
  testEnvironment: "jsdom",
  collectCoverage: true,
  collectCoverageFrom: ['src/**/*.{ts,tsx}'],
};
