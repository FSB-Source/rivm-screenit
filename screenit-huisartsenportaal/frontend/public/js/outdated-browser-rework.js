/*-
 * ========================LICENSE_START=================================
 * screenit-huisartsenportaal
 * %%
 * Copyright (C) 2012 - 2022 Facilitaire Samenwerking Bevolkingsonderzoek
 * %%
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 * =========================LICENSE_END==================================
 */
(function (f) {
	if (typeof exports === 'object' && typeof module !== 'undefined') {
		module.exports = f();
	} else if (typeof define === 'function' && define.amd) {
		define([], f);
	} else {
		var g;
		if (typeof window !== 'undefined') {
			g = window;
		} else if (typeof global !== 'undefined') {
			g = global;
		} else if (typeof self !== 'undefined') {
			g = self;
		} else {
			g = this;
		}
		g.outdatedBrowserRework = f();
	}
})(function () {
	var define, module, exports;
	return (function () {
		function r(e, n, t) {
			function o(i, f) {
				if (!n[i]) {
					if (!e[i]) {
						var c = 'function' == typeof require && require;
						if (!f && c) {
							return c(i, !0);
						}
						if (u) {
							return u(i, !0);
						}
						var a = new Error('Cannot find module \'' + i + '\'');
						throw a.code = 'MODULE_NOT_FOUND', a;
					}
					var p = n[i] = {exports: {}};
					e[i][0].call(p.exports, function (r) {
						var n = e[i][1][r];
						return o(n || r);
					}, p, p.exports, r, e, n, t);
				}
				return n[i].exports;
			}

			for (var u = 'function' == typeof require && require, i = 0; i < t.length; i++) o(t[i]);
			return o;
		}

		return r;
	})()({
		1: [
			function (require, module, exports) {
				var DEFAULTS = {
					Chrome: 57, 
					Edge: 39,
					Safari: 10,
					'Mobile Safari': 10,
					Opera: 50,
					Firefox: 50,
					Vivaldi: 1,
					IE: false,
				};

				var EDGEHTML_VS_EDGE_VERSIONS = {
					12: 0.1,
					13: 21,
					14: 31,
					15: 39,
					16: 41,
					17: 42,
					18: 44,
				};

				var updateDefaults = function (defaults, updatedValues) {
					for (var key in updatedValues) {
						defaults[key] = updatedValues[key];
					}

					return defaults;
				};

				module.exports = function (parsedUserAgent, options) {

					var browserSupport = options.browserSupport ? updateDefaults(DEFAULTS, options.browserSupport) : DEFAULTS;
					var requiredCssProperty = options.requiredCssProperty || false;

					var browserName = parsedUserAgent.browser.name;

					var isAndroidButNotChrome;
					if (options.requireChromeOnAndroid) {
						isAndroidButNotChrome = parsedUserAgent.os.name === 'Android' && parsedUserAgent.browser.name !== 'Chrome';
					}

					var parseMinorVersion = function (version) {
						return version.replace(/[^\d.]/g, '').split('.')[1];
					};

					var isBrowserUnsupported = function () {
						var isUnsupported = false;
						if (!(browserName in browserSupport)) {
							if (!options.isUnknownBrowserOK) {
								isUnsupported = true;
							}
						} else if (!browserSupport[browserName]) {
							isUnsupported = true;
						}
						return isUnsupported;
					};

					var isBrowserUnsupportedResult = isBrowserUnsupported();

					var isBrowserOutOfDate = function () {
						var browserVersion = parsedUserAgent.browser.version;
						var browserMajorVersion = parsedUserAgent.browser.major;
						var osName = parsedUserAgent.os.name;
						var osVersion = parsedUserAgent.os.version;

						if (browserName === 'Edge' && browserMajorVersion <= 18) {
							browserMajorVersion = EDGEHTML_VS_EDGE_VERSIONS[browserMajorVersion];
						}

						if (browserName === 'Firefox' && osName === 'iOS') {
							browserName = 'Mobile Safari';
							browserVersion = osVersion;
							browserMajorVersion = osVersion.substring(0, osVersion.indexOf('.'));
						}

						var isOutOfDate = false;
						if (isBrowserUnsupportedResult) {
							isOutOfDate = true;
						} else if (browserName in browserSupport) {
							var minVersion = browserSupport[browserName];
							if (typeof minVersion == 'object') {
								var minMajorVersion = minVersion.major;
								var minMinorVersion = minVersion.minor;

								if (browserMajorVersion < minMajorVersion) {
									isOutOfDate = true;
								} else if (browserMajorVersion == minMajorVersion) {
									var browserMinorVersion = parseMinorVersion(browserVersion);

									if (browserMinorVersion < minMinorVersion) {
										isOutOfDate = true;
									}
								}
							} else if (browserMajorVersion < minVersion) {
								isOutOfDate = true;
							}
						}
						return isOutOfDate;
					};

					var isPropertySupported = function (property) {
						if (!property) {
							return true;
						}
						var div = document.createElement('div');
						var vendorPrefixes = ['khtml', 'ms', 'o', 'moz', 'webkit'];
						var count = vendorPrefixes.length;

						if (property in div.style) {
							return true;
						}

						property = property.replace(/^[a-z]/, function (val) {
							return val.toUpperCase();
						});

						while (count--) {
							var prefixedProperty = vendorPrefixes[count] + property;

							if (prefixedProperty in div.style) {
								return true;
							}
						}
						return false;
					};

					return {
						isAndroidButNotChrome: isAndroidButNotChrome,
						isBrowserOutOfDate: isBrowserOutOfDate(),
						isBrowserUnsupported: isBrowserUnsupportedResult,
						isPropertySupported: isPropertySupported(requiredCssProperty),
					};
				};

			}, {}], 2: [
			function (require, module, exports) {

				module.exports = function deepExtend() {
					if (arguments.length < 1 || typeof arguments[0] !== 'object') {
						return false;
					}

					if (arguments.length < 2) {
						return arguments[0];
					}

					var target = arguments[0];

					for (var i = 1; i < arguments.length; i++) {
						var obj = arguments[i];

						for (var key in obj) {
							var src = target[key];
							var val = obj[key];

							if (typeof val !== 'object' || val === null) {
								target[key] = val;

							} else if (typeof src !== 'object' || src === null) {
								target[key] = deepExtend({}, val);

							} else {
								target[key] = deepExtend(src, val);
							}
						}
					}

					return target;
				};

			}, {}], 3: [
			function (require, module, exports) {
				var evaluateBrowser = require('./evaluateBrowser');
				var languageMessages = require('./languages.json');
				var deepExtend = require('./extend');
				var UserAgentParser = require('ua-parser-js');

				var COLORS = {
					salmon: '#f25648',
					white: 'white',
				};

				module.exports = function (options) {
					var main = function () {

						var parsedUserAgent = new UserAgentParser(navigator.userAgent).getResult();

						var outdatedUI = document.getElementById('outdated');

						options = options || {};

						var browserLocale = window.navigator.language || window.navigator.userLanguage; 

						var backgroundColor = options.backgroundColor || COLORS.salmon;
						var textColor = options.textColor || COLORS.white;
						var fullscreen = options.fullscreen || false;
						var language = options.language || browserLocale.slice(0, 2); 

						var updateSource = 'web'; 

						var isAndroid = parsedUserAgent.os.name === 'Android';
						if (isAndroid) {
							updateSource = 'googlePlay';
						} else if (parsedUserAgent.os.name === 'iOS') {
							updateSource = 'appStore';
						}

						var isBrowserUnsupported = false; 

						var done = true;

						var changeOpacity = function (opacityValue) {
							outdatedUI.style.opacity = opacityValue / 100;
							outdatedUI.style.filter = 'alpha(opacity=' + opacityValue + ')';
						};

						var fadeIn = function (opacityValue) {
							changeOpacity(opacityValue);
							if (opacityValue === 1) {
								outdatedUI.style.display = 'table';
							}
							if (opacityValue === 100) {
								done = true;
							}
						};

						var makeFadeInFunction = function (opacityValue) {
							return function () {
								fadeIn(opacityValue);
							};
						};

						var getMessage = function (lang) {
							var defaultMessages = languageMessages[lang] || languageMessages.en;
							var customMessages = options.messages && options.messages[lang];
							var messages = deepExtend({}, defaultMessages, customMessages);

							var updateMessages = {
								web:
									'<p>' +
									messages.update.web +
									(messages.url ? (
										'<a id="buttonUpdateBrowser" rel="nofollow" href="' +
										messages.url +
										'">' +
										messages.callToAction +
										'</a>'
									) : '') +
									'</p>',
								googlePlay:
									'<p>' +
									messages.update.googlePlay +
									'<a id="buttonUpdateBrowser" rel="nofollow" href="https://play.google.com/store/apps/details?id=com.android.chrome">' +
									messages.callToAction +
									'</a></p>',
								appStore: '<p>' + messages.update[updateSource] + '</p>',
							};

							var updateMessage = updateMessages[updateSource];

							var browserSupportMessage = messages.outOfDate;
							if (isBrowserUnsupported && messages.unsupported) {
								browserSupportMessage = messages.unsupported;
							}

							return (
								'<div class="vertical-center">' +
								'<svg xmlns="http://www.w3.org/2000/svg" width="222" height="60" viewBox="0 0 222 60"><style>.a{fill:#739DD2;}.b{fill:#9FA617;}.c{fill:#ED174F;}.d{fill:#603359;}.e{fill:#82CEC1;}.f{fill:#992D4C;}.g{fill:#F9A350;}.h{fill:#ED1B2D;}.i{fill:#968B51;}.j{fill:#992E38;}</style><title>  Logo - aangepast</title><desc>  Created with Sketch.</desc><g fill="none"><g class="svgtest" transform="translate(-20 -10)translate(20 10)translate(69.13846 21.323077)" fill="#231F20"><path d="M6.3 7.3C6.3 6.5 6.1 6 5.5 6 4.5 6 3.3 7.6 2.7 10.8L2.6 11.3C2.7 11.3 3 11.4 3.5 11.4 5 11.4 6.3 9.9 6.3 7.3M0 12.7L2.3 0 4.6 0 3.8 4.2C3.7 5 3.6 5.5 3.4 6.2L3.4 6.2C4.2 5 5 4 6.5 4 7.9 4 8.8 5.1 8.8 6.9 8.8 10.7 6.6 13.1 3.3 13.1 1.4 13.1 0.3 12.8 0 12.7"/><path d="M15.2 6.5C15.2 6 14.9 5.8 14.4 5.8 13.5 5.8 12.6 6.5 12.2 8.2 14 7.9 15.2 7.4 15.2 6.5M13.5 11.3C14.4 11.3 15.1 10.8 15.7 10.3L16.6 11.9C15.9 12.5 14.8 13.1 13.1 13.1 10.9 13.1 9.7 11.7 9.7 9.6 9.7 6.4 12 4 14.8 4 16.4 4 17.4 4.8 17.4 6.2 17.4 8.4 15 9.3 12.1 9.7 12.1 10.7 12.6 11.3 13.5 11.3"/><path d="M18.3 4.2L20.8 4.2 21.2 8.6C21.3 9.3 21.3 10 21.3 10.2L21.3 10.2 24.3 4.2 26.7 4.2 21.9 12.9 19.7 12.9 18.3 4.2"/><path d="M32.4 7.6C32.4 6.4 31.9 5.7 31 5.7 29.6 5.7 28.7 7.8 28.7 9.4 28.7 10.6 29.1 11.4 30 11.4 31.5 11.4 32.4 9.2 32.4 7.6M26.3 9.5C26.3 6.5 28.4 4 31.2 4 33.4 4 34.7 5.5 34.7 7.6 34.7 10.8 32.7 13.1 29.8 13.1 27.5 13.1 26.3 11.5 26.3 9.5"/><polygon points="37.9 0 40.2 0 37.9 12.9 35.5 12.9"/><polygon points="42.5 12.9 40.2 12.9 42.4 0 44.8 0 43.4 7.7 46.7 4.2 49.3 4.2 45.6 8.1 47.9 12.9 45.4 12.9 43.3 8.5"/><path d="M51 1.6C51 0.8 51.5 0.2 52.4 0.2 53.2 0.2 53.8 0.8 53.8 1.6 53.8 2.3 53.2 3 52.4 3 51.5 3 51 2.3 51 1.6M50.8 4.2L53.1 4.2 51.5 12.9 49.2 12.9 50.8 4.2"/><path d="M55.4 4.2L57.7 4.2C57.6 4.7 57.5 5.6 57.3 6.2L57.4 6.2C57.9 5.2 58.8 3.9 60.5 3.9 62.1 3.9 62.7 5.1 62.3 7.3L61.3 12.9 59 12.9 60 7.4C60.1 6.4 60 6 59.4 6 58.4 6 57.2 7.3 56.3 12.2L56.2 12.9 53.9 12.9 55.4 4.2"/><path d="M69.6 6.3L69.7 5.8C69.5 5.7 69.2 5.7 68.7 5.7 67.2 5.7 66 7.3 66 9.8 66 10.6 66.2 11.1 66.8 11.1 67.8 11.1 69.1 9.6 69.6 6.3M62.9 16L63.9 14.3C64.4 14.6 65.1 15.1 66.2 15.1 67.5 15.1 68.1 14.4 68.4 12.9L68.5 12.5C68.6 11.8 68.7 11.4 68.9 10.9L68.8 10.9C68.1 12.1 67.3 13.1 65.9 13.1 64.5 13.1 63.5 11.9 63.5 10.2 63.5 6.4 65.7 3.9 69 3.9 70.8 3.9 71.9 4.3 72.3 4.4L70.7 12.8C70.2 15.8 69 17 66.1 17 64.7 17 63.6 16.5 62.9 16"/><path d="M91.7 12.9L91.7 7.1C91.7 6.1 91.4 5.4 90.3 5.4 89 5.4 88.1 6.6 87.9 6.9L87.9 12.9 86.4 12.9 86.4 4.3 87.9 4.3 87.9 5.5C88.3 5 89.2 4.1 90.7 4.1 92.5 4.1 93.3 5.3 93.3 6.8L93.3 12.9 91.7 12.9"/><path d="M98.7 11.8C99.8 11.8 100.5 11 100.8 10.7L100.8 6.1C100.4 5.7 99.8 5.4 98.8 5.4 97.3 5.4 96.4 6.7 96.4 8.6 96.4 10.5 97.1 11.8 98.7 11.8M98.6 4.1C99.8 4.1 100.5 4.5 100.8 4.8L100.8 0 102.3 0 102.3 11C102.3 11.9 102.4 12.6 102.5 12.9L101 12.9C100.9 12.8 100.8 12.3 100.8 11.9 100.3 12.6 99.5 13.1 98.4 13.1 96.3 13.1 94.8 11.5 94.8 8.6 94.8 5.9 96.4 4.1 98.6 4.1"/><path d="M109.8 7.5C109.8 6.4 109.3 5.3 107.9 5.3 106.6 5.3 105.8 6.2 105.6 7.5L109.8 7.5ZM108.2 11.9C109.4 11.9 110.2 11.4 110.5 11.2L111.1 12.3C110.7 12.6 109.7 13.1 108.1 13.1 105.4 13.1 103.9 11.2 103.9 8.5 103.9 6 105.6 4.1 107.9 4.1 110.3 4.1 111.6 5.9 111.3 8.7L105.5 8.7C105.6 10.5 106.3 11.9 108.2 11.9Z"/><path d="M117.7 5.7C117.6 5.6 117.4 5.5 116.9 5.5 115.7 5.5 115 6.9 114.9 7.2L114.9 12.9 113.4 12.9 113.4 4.3 114.9 4.3 114.9 5.6C115.2 5 116 4.1 117.1 4.1 117.7 4.1 118 4.2 118.1 4.3L117.7 5.7"/><polygon points="119.3 12.9 119.3 12 123.5 5.5 119.5 5.5 119.5 4.3 125.4 4.3 125.4 5.3 121.1 11.7 125.5 11.7 125.5 12.9"/><path d="M130.6 5.3C128.9 5.3 128.3 6.8 128.3 8.6 128.3 10.4 129 11.9 130.7 11.9 132.4 11.9 133.2 10.4 133.2 8.6 133.2 6.8 132.4 5.3 130.6 5.3M130.7 13.1C127.8 13.1 126.6 10.7 126.6 8.6 126.6 6.6 127.7 4.1 130.7 4.1 133.6 4.1 134.8 6.5 134.8 8.6 134.8 10.6 133.6 13.1 130.7 13.1"/><path d="M142 7.5C142 6.4 141.4 5.3 140 5.3 138.7 5.3 138 6.2 137.8 7.5L142 7.5ZM140.3 11.9C141.5 11.9 142.3 11.4 142.7 11.2L143.3 12.3C142.8 12.6 141.9 13.1 140.3 13.1 137.5 13.1 136.1 11.2 136.1 8.5 136.1 6 137.7 4.1 140.1 4.1 142.4 4.1 143.7 5.9 143.5 8.7L137.7 8.7C137.8 10.5 138.5 11.9 140.3 11.9Z"/><path d="M150.2 4.3L152 4.3 148.5 8.1 152.1 12.9 150.3 12.9 146.7 8.1 150.2 4.3ZM145.2 12.9L146.7 12.9 146.7 0 145.2 0 145.2 12.9Z"/><path d="M78.8 12.8C79.3 13 79.9 13.1 80.6 13.1 83.5 13.1 84.7 10.6 84.7 8.6 84.7 6.5 83.5 4.1 80.5 4.1 79.8 4.1 79.2 4.2 78.7 4.4 78.1 4.1 77.5 3.9 76.6 3.9 74.6 3.9 73.5 5 73.5 6.4 73.5 7.5 74 8.1 74.7 8.8 75.4 9.6 76 10 76 10.6 76 11 75.7 11.4 74.9 11.4 74.1 11.4 73.5 11 73.1 10.7L72.1 12.3C72.5 12.6 73.4 13.1 75 13.1 77.2 13.1 78.4 12.2 78.4 10.6 78.4 9.4 77.7 8.8 76.9 7.9 76.2 7.3 75.8 6.8 75.8 6.4 75.8 5.9 76.1 5.5 76.7 5.5 77 5.4 77.3 5.4 77.5 5.5L77.4 5.5C78.1 5.7 78.5 6.6 78.5 6.6 78.9 5.8 79.5 5.3 80.5 5.3 82.2 5.3 83.1 6.8 83.1 8.6 83.1 10.4 82.3 11.9 80.6 11.9 80.1 11.9 79.7 11.8 79.4 11.6 79.4 11.6 79.4 12.4 78.8 12.8"/></g><path d="M31.4 28.8C31.6 27.8 31.7 26.8 31.7 25.7 31.7 24.3 31.5 22.9 31.1 21.6 32.5 21.3 33.7 20.6 34.7 19.6 36 18.3 36.7 16.6 36.7 14.6 36.7 12.6 36 10.9 34.7 9.6 33.4 8.3 31.6 7.5 29.6 7.5 27.7 7.5 25.9 8.3 24.6 9.6 23.8 10.4 23.2 11.4 22.9 12.5 21.2 11.8 19.3 11.4 17.4 11.4L17.4 11.4C16.8 11.4 16.2 11.5 15.7 11.5 16.3 8.8 17.6 6.4 19.5 4.5 22.1 1.9 25.7 0.3 29.6 0.3L29.6 0.3C33.6 0.3 37.2 1.9 39.8 4.5 42.3 7.1 43.9 10.7 43.9 14.6 43.9 15.6 43.8 16.6 43.6 17.6 41.4 18.3 39.4 19.5 37.8 21.1 35.9 23 34.5 25.5 33.9 28.2 33.1 28.5 32.2 28.7 31.4 28.8M24.2 27.8C23.1 27.4 22 26.8 21.1 26 21.1 26 21.1 25.9 21.1 25.8 21.1 23.8 19.5 22.2 17.5 22.2 16.9 21.1 16.3 20 16 18.8 16.4 18.7 16.9 18.6 17.4 18.6 19.4 18.6 21.1 19.4 22.4 20.7 23.7 22 24.5 23.7 24.5 25.7 24.5 26.5 24.4 27.2 24.2 27.8" class="a"/><path d="M29.7 18.3L29.6 18.3C29 17.3 28.3 16.4 27.5 15.6 27.1 15.1 26.6 14.7 26 14.3 26.2 12.5 27.8 11 29.7 11 31.7 11 33.3 12.7 33.3 14.7 33.3 16.7 31.7 18.3 29.7 18.3" class="a"/><path d="M47.9 45.5L47.9 45.5C46.6 45.5 45.3 45.4 44.1 45 44 41.2 42.4 37.6 39.9 35.1 38.2 33.4 36 32.1 33.6 31.4 33.6 31.4 33.6 31.3 33.6 31.2 33.6 30.2 33.7 29.2 33.9 28.2 36.1 27.5 38.1 26.3 39.8 24.7 41.7 22.8 43 20.3 43.6 17.6 45 17.2 46.4 16.9 47.9 16.9L47.9 16.9C51.8 16.9 55.4 18.5 58 21.1 60.6 23.7 62.2 27.3 62.2 31.2 62.2 35.2 60.6 38.8 58 41.4 55.5 43.9 51.9 45.5 47.9 45.5M47.9 24.1C45.9 24.1 44.2 24.9 42.9 26.2 41.6 27.5 40.8 29.3 40.8 31.2 40.8 33.2 41.6 35 42.9 36.3 44.2 37.6 45.9 38.3 47.9 38.3 49.9 38.3 51.6 37.6 52.9 36.3 54.2 35 55 33.2 55 31.2 55 29.3 54.2 27.5 52.9 26.2 51.6 24.9 49.9 24.1 47.9 24.1" class="b"/><path d="M33.9 28.2C34.5 25.5 35.9 23 37.8 21.1 39.4 19.5 41.4 18.3 43.6 17.6 43 20.3 41.7 22.8 39.8 24.7 38.1 26.3 36.1 27.5 33.9 28.2" fill="#585929"/><path d="M47.8 34.9C45.8 34.9 44.2 33.3 44.2 31.3 44.2 29.3 45.8 27.6 47.8 27.6 49.8 27.6 51.5 29.3 51.5 31.3 51.5 33.3 49.8 34.9 47.8 34.9" class="b"/><path d="M11 38.5C9.9 37.9 8.9 37.3 8 36.5 8.3 35.7 8.8 35 9.4 34.4 10.7 33.1 12.5 32.3 14.5 32.3 14.6 32.3 14.7 32.3 14.8 32.3 15.5 32.6 16.3 32.8 17.1 32.8 18 33.2 18.8 33.7 19.5 34.4 19.6 34.5 19.8 34.7 19.9 34.9 19.8 35 19.8 35 19.7 35.1 18.9 35.9 18.2 36.7 17.6 37.7 17 36.6 15.8 35.8 14.5 35.8 12.8 35.8 11.4 37 11 38.5M26.3 31.4C25.8 30.6 25.2 29.9 24.6 29.3 24.3 29.1 24.1 28.9 23.9 28.7 24 28.4 24.1 28.1 24.2 27.8 25.9 28.5 27.7 28.9 29.6 28.9L29.7 28.9C30.2 28.9 30.8 28.9 31.4 28.8 31.2 29.5 31 30.3 30.7 30.9 30.4 30.9 30.1 30.9 29.8 30.9L29.8 30.9C28.6 30.9 27.4 31.1 26.3 31.4M3.7 29.9C3.3 28.6 3.1 27.2 3.1 25.7 3.1 21.8 4.7 18.2 7.3 15.6 9.5 13.4 12.4 11.9 15.7 11.5 15.5 12.5 15.3 13.5 15.3 14.6 15.3 16 15.6 17.4 16 18.8 14.6 19 13.4 19.7 12.4 20.7 11.1 22 10.3 23.7 10.3 25.7L10.3 25.7C8 26.4 6 27.7 4.3 29.3 4.1 29.5 3.9 29.7 3.7 29.9M29.6 21.7C27.7 21.7 25.9 20.9 24.6 19.6 23.3 18.3 22.5 16.6 22.5 14.6 22.5 13.9 22.7 13.2 22.9 12.5 24 13 25.1 13.6 26 14.3 26 14.4 26 14.6 26 14.7 26 16.7 27.6 18.3 29.6 18.3 30.2 19.3 30.7 20.4 31.1 21.6 30.6 21.6 30.1 21.7 29.6 21.7" class="c"/><path d="M29.6 28.9L29.6 28.9C27.7 28.9 25.9 28.5 24.2 27.8 24.4 27.2 24.5 26.5 24.5 25.7 24.5 23.7 23.7 22 22.4 20.7 21.1 19.4 19.4 18.6 17.4 18.6 16.9 18.6 16.4 18.7 16 18.8 15.6 17.4 15.3 16 15.3 14.6 15.3 13.5 15.5 12.5 15.7 11.5 16.2 11.5 16.8 11.4 17.4 11.4L17.4 11.4C19.3 11.4 21.2 11.8 22.9 12.5 22.7 13.2 22.5 13.9 22.5 14.6 22.5 16.6 23.3 18.3 24.6 19.6 25.9 20.9 27.7 21.7 29.6 21.7 30.1 21.7 30.6 21.6 31.1 21.6 31.5 22.9 31.7 24.3 31.7 25.7 31.7 26.8 31.6 27.8 31.4 28.8 30.8 28.9 30.2 28.9 29.6 28.9" class="d"/><path d="M29.6 18.3C27.6 18.3 26 16.7 26 14.7 26 14.6 26 14.4 26 14.3 26.6 14.7 27.1 15.1 27.5 15.6 28.3 16.4 29 17.3 29.6 18.3" class="d"/><path d="M21 26.7C19 25.7 16.8 25.1 14.5 25.1L14.4 25.1C14.2 25.1 14 25.1 13.9 25.1 14.2 23.4 15.6 22.2 17.4 22.2 17.5 22.2 17.5 22.2 17.5 22.2 18.1 23.1 18.8 23.9 19.5 24.7 20 25.2 20.5 25.6 21.1 26 21.1 26.3 21 26.5 21 26.7" class="c"/><path d="M21.1 26C20.5 25.6 20 25.2 19.5 24.7 18.8 23.9 18.1 23.1 17.5 22.2 19.5 22.2 21.1 23.8 21.1 25.8 21.1 25.9 21.1 26 21.1 26" fill="#8C2E57"/><path d="M15.6 46.4C15.5 46 15.5 45.6 15.5 45.2 15.5 44.4 15.6 43.7 15.7 42.9 16.9 42.5 17.9 41.4 18.1 40 19.3 39.9 20.4 39.7 21.6 39.4L21.6 39.4C21.6 41.4 20.8 43.1 19.5 44.4 18.4 45.5 17.1 46.2 15.6 46.4" class="e"/><path d="M18.1 40C18.1 39.8 18.1 39.7 18.1 39.5 18.1 38.8 17.9 38.2 17.6 37.7 18.2 36.7 18.9 35.9 19.7 35.1 19.8 35 19.8 35 19.9 34.9 20.9 36.1 21.5 37.7 21.6 39.4 20.4 39.7 19.3 39.9 18.1 40" class="f"/><path d="M29.8 59.5L29.8 59.5C25.9 59.5 22.3 57.9 19.7 55.3 19.1 54.7 18.5 54 18 53.3 20.4 52.6 22.6 51.4 24.3 49.7 24.5 49.9 24.6 50.1 24.8 50.2 26.1 51.5 27.8 52.3 29.8 52.3 31.8 52.3 33.5 51.5 34.8 50.2 36.1 48.9 36.9 47.2 36.9 45.2 36.9 43.2 36.1 41.5 34.8 40.2 33.5 38.9 31.8 38.1 29.8 38.1 29.4 38.1 29.1 38.1 28.7 38.2 28.6 37.2 28.4 36.2 28.1 35.2 29.2 34 30.1 32.5 30.7 30.9 31.7 31 32.7 31.2 33.6 31.4 33.6 35.3 35.2 38.8 37.8 41.4 39.5 43.1 41.7 44.4 44.1 45 44.1 45.1 44.1 45.2 44.1 45.2 44.1 49.1 42.5 52.7 39.9 55.3 37.3 57.9 33.7 59.5 29.8 59.5" class="e"/><path d="M44.1 45C41.7 44.4 39.5 43.1 37.8 41.4 35.2 38.8 33.6 35.3 33.6 31.4 36 32.1 38.2 33.4 39.9 35.1 42.4 37.6 44 41.2 44.1 45" fill="#3D6F21"/><path d="M28.1 35.2C27.7 33.8 27.1 32.5 26.3 31.4 27.4 31.1 28.6 30.9 29.8 30.9L29.8 30.9C30.1 30.9 30.4 30.9 30.7 30.9 30.1 32.5 29.2 34 28.1 35.2" class="f"/><path d="M29.8 48.9C28.4 48.9 27.2 48.1 26.6 47 27.5 45.4 28.2 43.7 28.5 41.9 28.9 41.7 29.4 41.6 29.8 41.6 31.8 41.6 33.5 43.3 33.5 45.3 33.5 47.3 31.8 48.9 29.8 48.9" class="e"/><path d="M14.5 53.7L14.5 53.7C10.5 53.7 6.9 52.1 4.3 49.5 1.8 46.9 0.2 43.3 0.2 39.4 0.2 35.8 1.5 32.5 3.7 29.9 4.4 32.2 5.7 34.2 7.3 35.8 7.5 36 7.7 36.3 8 36.5 7.6 37.4 7.4 38.4 7.4 39.4 7.4 41.4 8.1 43.1 9.4 44.4 10.7 45.7 12.5 46.5 14.5 46.5 14.8 46.5 15.2 46.5 15.6 46.4 15.8 48.9 16.6 51.3 18 53.3 16.9 53.6 15.7 53.7 14.5 53.7M17.4 32.8C17.3 32.8 17.2 32.8 17.1 32.8 16.4 32.5 15.6 32.4 14.8 32.3 13.9 32 13.1 31.4 12.4 30.7 11.1 29.4 10.3 27.7 10.3 25.7 11.4 25.4 12.6 25.2 13.9 25.1 13.8 25.3 13.8 25.6 13.8 25.8 13.8 27.8 15.4 29.4 17.4 29.4 19.1 29.4 20.6 28.3 21 26.7 22 27.2 23 27.9 23.9 28.7 23.5 29.4 23 30.1 22.4 30.7 21.1 32 19.4 32.8 17.4 32.8" class="g"/><path d="M8 36.5C7.7 36.3 7.5 36 7.3 35.8 5.7 34.2 4.4 32.2 3.7 29.9 3.9 29.7 4.1 29.5 4.3 29.3 6 27.7 8 26.4 10.3 25.7 10.3 27.7 11.1 29.4 12.4 30.7 13.1 31.4 13.9 32 14.8 32.3 14.7 32.3 14.6 32.3 14.5 32.3 12.5 32.3 10.7 33.1 9.4 34.4 8.8 35 8.3 35.7 8 36.5M19.9 34.9C19.8 34.7 19.6 34.5 19.5 34.4 18.8 33.7 18 33.2 17.1 32.8 17.2 32.8 17.3 32.8 17.4 32.8 19.4 32.8 21.1 32 22.4 30.7 23 30.1 23.5 29.4 23.9 28.7 24.1 28.9 24.3 29.1 24.6 29.3 25.2 29.9 25.8 30.6 26.3 31.4 23.8 32 21.7 33.2 19.9 34.9" class="h"/><path d="M17.4 29.4C15.4 29.4 13.8 27.8 13.8 25.8 13.8 25.6 13.8 25.3 13.9 25.1 14 25.1 14.2 25.1 14.4 25.1L14.5 25.1C16.8 25.1 19 25.7 21 26.7 20.6 28.3 19.1 29.4 17.4 29.4" class="h"/><path d="M24.3 49.7C23.3 48.5 22.7 46.9 22.7 45.2 22.7 43.2 23.5 41.5 24.8 40.2 25.8 39.1 27.2 38.4 28.7 38.2 28.7 38.6 28.7 39 28.7 39.4 28.7 40.2 28.7 41.1 28.5 41.9 27.2 42.4 26.2 43.7 26.2 45.3 26.2 45.9 26.3 46.5 26.6 47 26 47.9 25.3 48.8 24.6 49.5 24.5 49.6 24.4 49.7 24.3 49.7" class="g"/><path d="M18 53.3C16.6 51.3 15.8 48.9 15.6 46.4 17.1 46.2 18.4 45.5 19.5 44.4 20.8 43.1 21.6 41.4 21.6 39.4L21.6 39.4C23.8 38.7 25.9 37.5 27.5 35.8 27.7 35.6 27.9 35.4 28.1 35.2 28.4 36.2 28.6 37.2 28.7 38.2 27.2 38.4 25.8 39.1 24.8 40.2 23.5 41.5 22.7 43.2 22.7 45.2 22.7 46.9 23.3 48.5 24.3 49.7 22.6 51.4 20.4 52.6 18 53.3" class="i"/><path d="M21.6 39.4C21.6 37.7 20.9 36.1 19.9 34.9 21.7 33.2 23.8 32 26.3 31.4 27.1 32.5 27.7 33.8 28.1 35.2 27.9 35.4 27.7 35.6 27.5 35.8 25.9 37.5 23.8 38.7 21.6 39.4" class="j"/><path d="M26.6 47C26.3 46.5 26.2 45.9 26.2 45.3 26.2 43.7 27.2 42.4 28.5 41.9 28.2 43.7 27.5 45.4 26.6 47" class="i"/><path d="M14.5 43.1C12.5 43.1 10.8 41.5 10.8 39.5 10.8 39.1 10.9 38.8 11 38.5 12.7 39.3 14.5 39.9 16.5 40 16.1 40.9 15.8 41.9 15.7 42.9 15.3 43.1 14.9 43.1 14.5 43.1" class="g"/><path d="M16.5 40C14.5 39.9 12.7 39.3 11 38.5 11.4 37 12.8 35.8 14.5 35.8 15.8 35.8 17 36.6 17.6 37.7 17.2 38.4 16.8 39.2 16.5 40" class="h"/><path d="M15.7 42.9C15.8 41.9 16.1 40.9 16.5 40 16.8 40 17.1 40 17.4 40L17.4 40C17.6 40 17.9 40 18.1 40 17.9 41.4 17 42.5 15.7 42.9" class="i"/><path d="M17.4 40L17.4 40C17.1 40 16.8 40 16.5 40 16.8 39.2 17.2 38.4 17.6 37.7 17.9 38.2 18.1 38.8 18.1 39.5 18.1 39.7 18.1 39.8 18.1 40 17.9 40 17.6 40 17.4 40" class="j"/></g></svg>' +
								'<h6>' +
								browserSupportMessage +
								'</h6>' +
								updateMessage +
								'<p class="last"><a href="https://www.bevolkingsonderzoeknederland.nl" id="buttonCloseUpdateBrowser" title="' +
								messages.close +
								'">&times;</a></p>' +
								'</div>'
							);
						};

						var result = evaluateBrowser(parsedUserAgent, options);
						if (result.isAndroidButNotChrome || result.isBrowserOutOfDate || !result.isPropertySupported) {

							isBrowserUnsupported = result.isBrowserUnsupported;

							if (done && outdatedUI.style.opacity !== '1') {
								done = false;

								for (var opacity = 1; opacity <= 100; opacity++) {
									setTimeout(makeFadeInFunction(opacity), opacity * 8);
								}
							}

							var insertContentHere = document.getElementById('outdated');
							if (fullscreen) {
								insertContentHere.classList.add('fullscreen');
							}
							insertContentHere.innerHTML = getMessage(language);
						}
					};

					var oldOnload = window.onload;
					if (typeof window.onload !== 'function') {
						window.onload = main;
					} else {
						window.onload = function () {
							if (oldOnload) {
								oldOnload();
							}
							main();
						};
					}
				};

			}, {'./evaluateBrowser': 1, './extend': 2, './languages.json': 4, 'ua-parser-js': 5}], 4: [
			function (require, module, exports) {
				module.exports = {
					'ko': {
						'outOfDate': '최신 브라우저가 아닙니다!',
						'update': {
							'web': '웹사이트를 제대로 보려면 브라우저를 업데이트하세요.',
							'googlePlay': 'Google Play에서 Chrome을 설치하세요',
							'appStore': '설정 앱에서 iOS를 업데이트하세요',
						},
						'url': 'https://browser-update.org/update-browser.html',
						'callToAction': '지금 브라우저 업데이트하기',
						'close': '닫기',
					},
					'ja': {
						'outOfDate': '古いブラウザをお使いのようです。',
						'update': {
							'web': 'ウェブサイトを正しく表示できるように、ブラウザをアップデートしてください。',
							'googlePlay': 'Google PlayからChromeをインストールしてください',
							'appStore': '設定からiOSをアップデートしてください',
						},
						'url': 'https://browser-update.org/update-browser.html',
						'callToAction': '今すぐブラウザをアップデートする',
						'close': '閉じる',
					},
					'br': {
						'outOfDate': 'O seu navegador est&aacute; desatualizado!',
						'update': {
							'web': 'Atualize o seu navegador para ter uma melhor experi&ecirc;ncia e visualiza&ccedil;&atilde;o deste site. ',
							'googlePlay': 'Please install Chrome from Google Play',
							'appStore': 'Please update iOS from the Settings App',
						},
						'url': 'https://browser-update.org/update-browser.html',
						'callToAction': 'Atualize o seu navegador agora',
						'close': 'Fechar',
					},
					'ca': {
						'outOfDate': 'El vostre navegador no està actualitzat!',
						'update': {
							'web': 'Actualitzeu el vostre navegador per veure correctament aquest lloc web. ',
							'googlePlay': 'Instal·leu Chrome des de Google Play',
							'appStore': 'Actualitzeu iOS des de l\'aplicació Configuració',
						},
						'url': 'https://browser-update.org/update-browser.html',
						'callToAction': 'Actualitzar el meu navegador ara',
						'close': 'Tancar',
					},
					'zh': {
						'outOfDate': '您的浏览器已过时',
						'update': {
							'web': '要正常浏览本网站请升级您的浏览器。',
							'googlePlay': 'Please install Chrome from Google Play',
							'appStore': 'Please update iOS from the Settings App',
						},
						'url': 'https://browser-update.org/update-browser.html',
						'callToAction': '现在升级',
						'close': '关闭',
					},
					'cz': {
						'outOfDate': 'Váš prohlížeč je zastaralý!',
						'update': {
							'web': 'Pro správné zobrazení těchto stránek aktualizujte svůj prohlížeč. ',
							'googlePlay': 'Nainstalujte si Chrome z Google Play',
							'appStore': 'Aktualizujte si systém iOS',
						},
						'url': 'https://browser-update.org/update-browser.html',
						'callToAction': 'Aktualizovat nyní svůj prohlížeč',
						'close': 'Zavřít',
					},
					'da': {
						'outOfDate': 'Din browser er forældet!',
						'update': {
							'web': 'Opdatér din browser for at få vist denne hjemmeside korrekt. ',
							'googlePlay': 'Installér venligst Chrome fra Google Play',
							'appStore': 'Opdatér venligst iOS',
						},
						'url': 'https://browser-update.org/update-browser.html',
						'callToAction': 'Opdatér din browser nu',
						'close': 'Luk',
					},
					'de': {
						'outOfDate': 'Ihr Browser ist veraltet!',
						'update': {
							'web': 'Bitte aktualisieren Sie Ihren Browser, um diese Website korrekt darzustellen. ',
							'googlePlay': 'Please install Chrome from Google Play',
							'appStore': 'Please update iOS from the Settings App',
						},
						'url': 'https://browser-update.org/update-browser.html',
						'callToAction': 'Den Browser jetzt aktualisieren ',
						'close': 'Schließen',
					},
					'ee': {
						'outOfDate': 'Sinu veebilehitseja on vananenud!',
						'update': {
							'web': 'Palun uuenda oma veebilehitsejat, et näha lehekülge korrektselt. ',
							'googlePlay': 'Please install Chrome from Google Play',
							'appStore': 'Please update iOS from the Settings App',
						},
						'url': 'https://browser-update.org/update-browser.html',
						'callToAction': 'Uuenda oma veebilehitsejat kohe',
						'close': 'Sulge',
					},
					'en': {
						'outOfDate': 'Your browser is out-of-date!',
						'update': {
							'web': 'Update your browser to view this website correctly. ',
							'googlePlay': 'Please install Chrome from Google Play',
							'appStore': 'Please update iOS from the Settings App',
						},
						'url': 'https://browser-update.org/update-browser.html',
						'callToAction': 'Update my browser now',
						'close': 'Close',
					},
					'es': {
						'outOfDate': '¡Tu navegador está anticuado!',
						'update': {
							'web': 'Actualiza tu navegador para ver esta página correctamente. ',
							'googlePlay': 'Please install Chrome from Google Play',
							'appStore': 'Please update iOS from the Settings App',
						},
						'url': 'https://browser-update.org/update-browser.html',
						'callToAction': 'Actualizar mi navegador ahora',
						'close': 'Cerrar',
					},
					'fa': {
						'rightToLeft': true,
						'outOfDate': 'مرورگر شما منسوخ شده است!',
						'update': {
							'web': 'جهت مشاهده صحیح این وبسایت، مرورگرتان را بروز رسانی نمایید. ',
							'googlePlay': 'Please install Chrome from Google Play',
							'appStore': 'Please update iOS from the Settings App',
						},
						'url': 'https://browser-update.org/update-browser.html',
						'callToAction': 'همین حالا مرورگرم را بروز کن',
						'close': 'Close',
					},
					'fi': {
						'outOfDate': 'Selaimesi on vanhentunut!',
						'update': {
							'web': 'Lataa ajantasainen selain n&auml;hd&auml;ksesi t&auml;m&auml;n sivun oikein. ',
							'googlePlay': 'Asenna uusin Chrome Google Play -kaupasta',
							'appStore': 'Päivitä iOS puhelimesi asetuksista',
						},
						'url': 'https://browser-update.org/update-browser.html',
						'callToAction': 'P&auml;ivit&auml; selaimeni nyt ',
						'close': 'Sulje',
					},
					'fr': {
						'outOfDate': 'Votre navigateur n\'est plus compatible !',
						'update': {
							'web': 'Mettez à jour votre navigateur pour afficher correctement ce site Web. ',
							'googlePlay': 'Merci d\'installer Chrome depuis le Google Play Store',
							'appStore': 'Merci de mettre à jour iOS depuis l\'application Réglages',
						},
						'url': 'https://browser-update.org/update-browser.html',
						'callToAction': 'Mettre à jour maintenant ',
						'close': 'Fermer',
					},
					'hu': {
						'outOfDate': 'A böngészője elavult!',
						'update': {
							'web': 'Firssítse vagy cserélje le a böngészőjét. ',
							'googlePlay': 'Please install Chrome from Google Play',
							'appStore': 'Please update iOS from the Settings App',
						},
						'url': 'https://browser-update.org/update-browser.html',
						'callToAction': 'A böngészőm frissítése ',
						'close': 'Close',
					},
					'id': {
						'outOfDate': 'Browser yang Anda gunakan sudah ketinggalan zaman!',
						'update': {
							'web': 'Perbaharuilah browser Anda agar bisa menjelajahi website ini dengan nyaman. ',
							'googlePlay': 'Please install Chrome from Google Play',
							'appStore': 'Please update iOS from the Settings App',
						},
						'url': 'https://browser-update.org/update-browser.html',
						'callToAction': 'Perbaharui browser sekarang ',
						'close': 'Close',
					},
					'it': {
						'outOfDate': 'Il tuo browser non &egrave; aggiornato!',
						'update': {
							'web': 'Aggiornalo per vedere questo sito correttamente. ',
							'googlePlay': 'Please install Chrome from Google Play',
							'appStore': 'Please update iOS from the Settings App',
						},
						'url': 'https://browser-update.org/update-browser.html',
						'callToAction': 'Aggiorna ora',
						'close': 'Chiudi',
					},
					'lt': {
						'outOfDate': 'Jūsų naršyklės versija yra pasenusi!',
						'update': {
							'web': 'Atnaujinkite savo naršyklę, kad galėtumėte peržiūrėti šią svetainę tinkamai. ',
							'googlePlay': 'Please install Chrome from Google Play',
							'appStore': 'Please update iOS from the Settings App',
						},
						'url': 'https://browser-update.org/update-browser.html',
						'callToAction': 'Atnaujinti naršyklę ',
						'close': 'Close',
					},
					'nl': {
						'outOfDate': 'Je gebruikt een oude browser!',
						'update': {
							'web': 'Update je browser om deze website correct te bekijken. ',
							'googlePlay': 'Please install Chrome from Google Play',
							'appStore': 'Please update iOS from the Settings App',
						},
						'url': 'https://browser-update.org/update-browser.html',
						'callToAction': 'Update mijn browser nu ',
						'close': 'Sluiten',
					},
					'pl': {
						'outOfDate': 'Twoja przeglądarka jest przestarzała!',
						'update': {
							'web': 'Zaktualizuj swoją przeglądarkę, aby poprawnie wyświetlić tę stronę. ',
							'googlePlay': 'Proszę zainstalować przeglądarkę Chrome ze sklepu Google Play',
							'appStore': 'Proszę zaktualizować iOS z Ustawień',
						},
						'url': 'https://browser-update.org/update-browser.html',
						'callToAction': 'Zaktualizuj przeglądarkę już teraz',
						'close': 'Zamknij',
					},
					'pt': {
						'outOfDate': 'O seu browser est&aacute; desatualizado!',
						'update': {
							'web': 'Atualize o seu browser para ter uma melhor experi&ecirc;ncia e visualiza&ccedil;&atilde;o deste site. ',
							'googlePlay': 'Please install Chrome from Google Play',
							'appStore': 'Please update iOS from the Settings App',
						},
						'url': 'https://browser-update.org/update-browser.html',
						'callToAction': 'Atualize o seu browser agora',
						'close': 'Fechar',
					},
					'ro': {
						'outOfDate': 'Browserul este învechit!',
						'update': {
							'web': 'Actualizați browserul pentru a vizualiza corect acest site. ',
							'googlePlay': 'Please install Chrome from Google Play',
							'appStore': 'Please update iOS from the Settings App',
						},
						'url': 'https://browser-update.org/update-browser.html',
						'callToAction': 'Actualizați browserul acum!',
						'close': 'Close',
					},
					'ru': {
						'outOfDate': 'Ваш браузер устарел!',
						'update': {
							'web': 'Обновите ваш браузер для правильного отображения этого сайта. ',
							'googlePlay': 'Please install Chrome from Google Play',
							'appStore': 'Please update iOS from the Settings App',
						},
						'url': 'https://browser-update.org/update-browser.html',
						'callToAction': 'Обновить мой браузер ',
						'close': 'Закрыть',
					},
					'si': {
						'outOfDate': 'Vaš brskalnik je zastarel!',
						'update': {
							'web': 'Za pravilen prikaz spletne strani posodobite vaš brskalnik. ',
							'googlePlay': 'Please install Chrome from Google Play',
							'appStore': 'Please update iOS from the Settings App',
						},
						'url': 'https://browser-update.org/update-browser.html',
						'callToAction': 'Posodobi brskalnik ',
						'close': 'Zapri',
					},
					'sv': {
						'outOfDate': 'Din webbläsare stödjs ej längre!',
						'update': {
							'web': 'Uppdatera din webbläsare för att webbplatsen ska visas korrekt. ',
							'googlePlay': 'Please install Chrome from Google Play',
							'appStore': 'Please update iOS from the Settings App',
						},
						'url': 'https://browser-update.org/update-browser.html',
						'callToAction': 'Uppdatera min webbläsare nu',
						'close': 'Stäng',
					},
					'ua': {
						'outOfDate': 'Ваш браузер застарів!',
						'update': {
							'web': 'Оновіть ваш браузер для правильного відображення цього сайта. ',
							'googlePlay': 'Please install Chrome from Google Play',
							'appStore': 'Please update iOS from the Settings App',
						},
						'url': 'https://browser-update.org/update-browser.html',
						'callToAction': 'Оновити мій браузер ',
						'close': 'Закрити',
					},
				};

			}, {}], 5: [
			function (require, module, exports) {

				(function (window, undefined) {

					'use strict';

					var LIBVERSION = '0.7.22',
						EMPTY = '',
						UNKNOWN = '?',
						FUNC_TYPE = 'function',
						UNDEF_TYPE = 'undefined',
						OBJ_TYPE = 'object',
						STR_TYPE = 'string',
						MAJOR = 'major', 
						MODEL = 'model',
						NAME = 'name',
						TYPE = 'type',
						VENDOR = 'vendor',
						VERSION = 'version',
						ARCHITECTURE = 'architecture',
						CONSOLE = 'console',
						MOBILE = 'mobile',
						TABLET = 'tablet',
						SMARTTV = 'smarttv',
						WEARABLE = 'wearable',
						EMBEDDED = 'embedded';

					var util = {
						extend: function (regexes, extensions) {
							var mergedRegexes = {};
							for (var i in regexes) {
								if (extensions[i] && extensions[i].length % 2 === 0) {
									mergedRegexes[i] = extensions[i].concat(regexes[i]);
								} else {
									mergedRegexes[i] = regexes[i];
								}
							}
							return mergedRegexes;
						},
						has: function (str1, str2) {
							if (typeof str1 === 'string') {
								return str2.toLowerCase().indexOf(str1.toLowerCase()) !== -1;
							} else {
								return false;
							}
						},
						lowerize: function (str) {
							return str.toLowerCase();
						},
						major: function (version) {
							return typeof (version) === STR_TYPE ? version.replace(/[^\d\.]/g, '').split('.')[0] : undefined;
						},
						trim: function (str) {
							return str.replace(/^[\s\uFEFF\xA0]+|[\s\uFEFF\xA0]+$/g, '');
						},
					};

					var mapper = {

						rgx: function (ua, arrays) {

							var i = 0, j, k, p, q, matches, match;

							while (i < arrays.length && !matches) {

								var regex = arrays[i],       
									props = arrays[i + 1];   
								j = k = 0;

								while (j < regex.length && !matches) {

									matches = regex[j++].exec(ua);

									if (!!matches) {
										for (p = 0; p < props.length; p++) {
											match = matches[++k];
											q = props[p];

											if (typeof q === OBJ_TYPE && q.length > 0) {
												if (q.length == 2) {
													if (typeof q[1] == FUNC_TYPE) {

														this[q[0]] = q[1].call(this, match);
													} else {

														this[q[0]] = q[1];
													}
												} else if (q.length == 3) {

													if (typeof q[1] === FUNC_TYPE && !(q[1].exec && q[1].test)) {

														this[q[0]] = match ? q[1].call(this, match, q[2]) : undefined;
													} else {

														this[q[0]] = match ? match.replace(q[1], q[2]) : undefined;
													}
												} else if (q.length == 4) {
													this[q[0]] = match ? q[3].call(this, match.replace(q[1], q[2])) : undefined;
												}
											} else {
												this[q] = match ? match : undefined;
											}
										}
									}
								}
								i += 2;
							}
						},

						str: function (str, map) {

							for (var i in map) {

								if (typeof map[i] === OBJ_TYPE && map[i].length > 0) {
									for (var j = 0; j < map[i].length; j++) {
										if (util.has(map[i][j], str)) {
											return (i === UNKNOWN) ? undefined : i;
										}
									}
								} else if (util.has(map[i], str)) {
									return (i === UNKNOWN) ? undefined : i;
								}
							}
							return str;
						},
					};

					var maps = {

						browser: {
							oldsafari: {
								version: {
									'1.0': '/8',
									'1.2': '/1',
									'1.3': '/3',
									'2.0': '/412',
									'2.0.2': '/416',
									'2.0.3': '/417',
									'2.0.4': '/419',
									'?': '/',
								},
							},
						},

						device: {
							amazon: {
								model: {
									'Fire Phone': ['SD', 'KF'],
								},
							},
							sprint: {
								model: {
									'Evo Shift 4G': '7373KT',
								},
								vendor: {
									'HTC': 'APA',
									'Sprint': 'Sprint',
								},
							},
						},

						os: {
							windows: {
								version: {
									'ME': '4.90',
									'NT 3.11': 'NT3.51',
									'NT 4.0': 'NT4.0',
									'2000': 'NT 5.0',
									'XP': ['NT 5.1', 'NT 5.2'],
									'Vista': 'NT 6.0',
									'7': 'NT 6.1',
									'8': 'NT 6.2',
									'8.1': 'NT 6.3',
									'10': ['NT 6.4', 'NT 10.0'],
									'RT': 'ARM',
								},
							},
						},
					};

					var regexes = {

						browser: [
							[

								/(opera\smini)\/([\w\.-]+)/i,                                       
								/(opera\s[mobiletab]+).+version\/([\w\.-]+)/i,                      
								/(opera).+version\/([\w\.]+)/i,                                     
								/(opera)[\/\s]+([\w\.]+)/i,                                          
							], [NAME, VERSION], [

								/(opios)[\/\s]+([\w\.]+)/i,                                          
							], [[NAME, 'Opera Mini'], VERSION], [

								/\s(opr)\/([\w\.]+)/i,                                               
							], [[NAME, 'Opera'], VERSION], [

								/(kindle)\/([\w\.]+)/i,                                             
								/(lunascape|maxthon|netfront|jasmine|blazer)[\/\s]?([\w\.]*)/i,

								/(avant\s|iemobile|slim)(?:browser)?[\/\s]?([\w\.]*)/i,

								/(bidubrowser|baidubrowser)[\/\s]?([\w\.]+)/i,                      
								/(?:ms|\()(ie)\s([\w\.]+)/i,                                        

								/(rekonq)\/([\w\.]*)/i,                                             
								/(chromium|flock|rockmelt|midori|epiphany|silk|skyfire|ovibrowser|bolt|iron|vivaldi|iridium|phantomjs|bowser|quark|qupzilla|falkon)\/([\w\.-]+)/i,

							], [NAME, VERSION], [

								/(konqueror)\/([\w\.]+)/i,                                           
							], [[NAME, 'Konqueror'], VERSION], [

								/(trident).+rv[:\s]([\w\.]+).+like\sgecko/i,                         
							], [[NAME, 'IE'], VERSION], [

								/(edge|edgios|edga|edg)\/((\d+)?[\w\.]+)/i,                          
							], [[NAME, 'Edge'], VERSION], [

								/(yabrowser)\/([\w\.]+)/i,                                           
							], [[NAME, 'Yandex'], VERSION], [

								/(Avast)\/([\w\.]+)/i,                                               
							], [[NAME, 'Avast Secure Browser'], VERSION], [

								/(AVG)\/([\w\.]+)/i,                                                 
							], [[NAME, 'AVG Secure Browser'], VERSION], [

								/(puffin)\/([\w\.]+)/i,                                              
							], [[NAME, 'Puffin'], VERSION], [

								/(focus)\/([\w\.]+)/i,                                               
							], [[NAME, 'Firefox Focus'], VERSION], [

								/(opt)\/([\w\.]+)/i,                                                 
							], [[NAME, 'Opera Touch'], VERSION], [

								/((?:[\s\/])uc?\s?browser|(?:juc.+)ucweb)[\/\s]?([\w\.]+)/i,         
							], [[NAME, 'UCBrowser'], VERSION], [

								/(comodo_dragon)\/([\w\.]+)/i,                                       
							], [[NAME, /_/g, ' '], VERSION], [

								/(windowswechat qbcore)\/([\w\.]+)/i,                                
							], [[NAME, 'WeChat(Win) Desktop'], VERSION], [

								/(micromessenger)\/([\w\.]+)/i,                                      
							], [[NAME, 'WeChat'], VERSION], [

								/(brave)\/([\w\.]+)/i,                                               
							], [[NAME, 'Brave'], VERSION], [

								/(qqbrowserlite)\/([\w\.]+)/i,                                       
							], [NAME, VERSION], [

								/(QQ)\/([\d\.]+)/i,                                                  
							], [NAME, VERSION], [

								/m?(qqbrowser)[\/\s]?([\w\.]+)/i,                                    
							], [NAME, VERSION], [

								/(baiduboxapp)[\/\s]?([\w\.]+)/i,                                    
							], [NAME, VERSION], [

								/(2345Explorer)[\/\s]?([\w\.]+)/i,                                   
							], [NAME, VERSION], [

								/(MetaSr)[\/\s]?([\w\.]+)/i,                                         
							], [NAME], [

								/(LBBROWSER)/i,                                                      
							], [NAME], [

								/xiaomi\/miuibrowser\/([\w\.]+)/i,                                   
							], [VERSION, [NAME, 'MIUI Browser']], [

								/;fbav\/([\w\.]+);/i,                                                
							], [VERSION, [NAME, 'Facebook']], [

								/safari\s(line)\/([\w\.]+)/i,                                       
								/android.+(line)\/([\w\.]+)\/iab/i,                                  
							], [NAME, VERSION], [

								/headlesschrome(?:\/([\w\.]+)|\s)/i,                                 
							], [VERSION, [NAME, 'Chrome Headless']], [

								/\swv\).+(chrome)\/([\w\.]+)/i,                                      
							], [[NAME, /(.+)/, '$1 WebView'], VERSION], [

								/((?:oculus|samsung)browser)\/([\w\.]+)/i,
							], [[NAME, /(.+(?:g|us))(.+)/, '$1 $2'], VERSION], [                

								/android.+version\/([\w\.]+)\s+(?:mobile\s?safari|safari)*/i,        
							], [VERSION, [NAME, 'Android Browser']], [

								/(sailfishbrowser)\/([\w\.]+)/i,                                     
							], [[NAME, 'Sailfish Browser'], VERSION], [

								/(chrome|omniweb|arora|[tizenoka]{5}\s?browser)\/v?([\w\.]+)/i,

							], [NAME, VERSION], [

								/(dolfin)\/([\w\.]+)/i,                                              
							], [[NAME, 'Dolphin'], VERSION], [

								/(qihu|qhbrowser|qihoobrowser|360browser)/i,                         
							], [[NAME, '360 Browser']], [

								/((?:android.+)crmo|crios)\/([\w\.]+)/i,                             
							], [[NAME, 'Chrome'], VERSION], [

								/(coast)\/([\w\.]+)/i,                                               
							], [[NAME, 'Opera Coast'], VERSION], [

								/fxios\/([\w\.-]+)/i,                                                
							], [VERSION, [NAME, 'Firefox']], [

								/version\/([\w\.]+).+?mobile\/\w+\s(safari)/i,                       
							], [VERSION, [NAME, 'Mobile Safari']], [

								/version\/([\w\.]+).+?(mobile\s?safari|safari)/i,                    
							], [VERSION, NAME], [

								/webkit.+?(gsa)\/([\w\.]+).+?(mobile\s?safari|safari)(\/[\w\.]+)/i,  
							], [[NAME, 'GSA'], VERSION], [

								/webkit.+?(mobile\s?safari|safari)(\/[\w\.]+)/i,                     
							], [NAME, [VERSION, mapper.str, maps.browser.oldsafari.version]], [

								/(webkit|khtml)\/([\w\.]+)/i,
							], [NAME, VERSION], [

								/(navigator|netscape)\/([\w\.-]+)/i,                                 
							], [[NAME, 'Netscape'], VERSION], [
								/(swiftfox)/i,                                                      
								/(icedragon|iceweasel|camino|chimera|fennec|maemo\sbrowser|minimo|conkeror)[\/\s]?([\w\.\+]+)/i,

								/(firefox|seamonkey|k-meleon|icecat|iceape|firebird|phoenix|palemoon|basilisk|waterfox)\/([\w\.-]+)$/i,

								/(mozilla)\/([\w\.]+).+rv\:.+gecko\/\d+/i,                          

								/(polaris|lynx|dillo|icab|doris|amaya|w3m|netsurf|sleipnir)[\/\s]?([\w\.]+)/i,

								/(links)\s\(([\w\.]+)/i,                                            
								/(gobrowser)\/?([\w\.]*)/i,                                         
								/(ice\s?browser)\/v?([\w\._]+)/i,                                   
								/(mosaic)[\/\s]([\w\.]+)/i,                                          
							], [NAME, VERSION],
						],

						cpu: [
							[

								/(?:(amd|x(?:(?:86|64)[_-])?|wow|win)64)[;\)]/i,                     
							], [[ARCHITECTURE, 'amd64']], [

								/(ia32(?=;))/i,                                                      
							], [[ARCHITECTURE, util.lowerize]], [

								/((?:i[346]|x)86)[;\)]/i,                                            
							], [[ARCHITECTURE, 'ia32']], [

								/windows\s(ce|mobile);\sppc;/i,
							], [[ARCHITECTURE, 'arm']], [

								/((?:ppc|powerpc)(?:64)?)(?:\smac|;|\))/i,                           
							], [[ARCHITECTURE, /ower/, '', util.lowerize]], [

								/(sun4\w)[;\)]/i,                                                    
							], [[ARCHITECTURE, 'sparc']], [

								/((?:avr32|ia64(?=;))|68k(?=\))|arm(?:64|(?=v\d+[;l]))|(?=atmel\s)avr|(?:irix|mips|sparc)(?:64)?(?=;)|pa-risc)/i,

							], [[ARCHITECTURE, util.lowerize]],
						],

						device: [
							[

								/\((ipad|playbook);[\w\s\),;-]+(rim|apple)/i,                        
							], [MODEL, VENDOR, [TYPE, TABLET]], [

								/applecoremedia\/[\w\.]+ \((ipad)/,                                  
							], [MODEL, [VENDOR, 'Apple'], [TYPE, TABLET]], [

								/(apple\s{0,1}tv)/i,                                                 
							], [[MODEL, 'Apple TV'], [VENDOR, 'Apple'], [TYPE, SMARTTV]], [

								/(archos)\s(gamepad2?)/i,                                           
								/(hp).+(touchpad)/i,                                                
								/(hp).+(tablet)/i,                                                  
								/(kindle)\/([\w\.]+)/i,                                             
								/\s(nook)[\w\s]+build\/(\w+)/i,                                     
								/(dell)\s(strea[kpr\s\d]*[\dko])/i,                                  
							], [VENDOR, MODEL, [TYPE, TABLET]], [

								/(kf[A-z]+)\sbuild\/.+silk\
							], [MODEL, [VENDOR, 'Amazon'], [TYPE, TABLET]], [
								/(sd|kf)[0349hijorstuw]+\sbuild\/.+silk\
							], [[MODEL, mapper.str, maps.device.amazon.model], [VENDOR, 'Amazon'], [TYPE, MOBILE]], [
								/android.+aft([bms])\sbuild/i,                                       
							], [MODEL, [VENDOR, 'Amazon'], [TYPE, SMARTTV]], [

								/\((ip[honed|\s\w*]+);.+(apple)/i,                                   
							], [MODEL, VENDOR, [TYPE, MOBILE]], [
								/\((ip[honed|\s\w*]+);/i,                                            
							], [MODEL, [VENDOR, 'Apple'], [TYPE, MOBILE]], [

								/(blackberry)[\s-]?(\w+)/i,                                         
								/(blackberry|benq|palm(?=\-)|sonyericsson|acer|asus|dell|meizu|motorola|polytron)[\s_-]?([\w-]*)/i,

								/(hp)\s([\w\s]+\w)/i,                                               
								/(asus)-?(\w+)/i,                                                    
							], [VENDOR, MODEL, [TYPE, MOBILE]], [
								/\(bb10;\s(\w+)/i,                                                   
							], [MODEL, [VENDOR, 'BlackBerry'], [TYPE, MOBILE]], [

								/android.+(transfo[prime\s]{4,10}\s\w+|eeepc|slider\s\w+|nexus 7|padfone|p00c)/i,
							], [MODEL, [VENDOR, 'Asus'], [TYPE, TABLET]], [

								/(sony)\s(tablet\s[ps])\sbuild\
								/(sony)?(?:sgp.+)\sbuild\
							], [[VENDOR, 'Sony'], [MODEL, 'Xperia Tablet'], [TYPE, TABLET]], [
								/android.+\s([c-g]\d{4}|so[-l]\w+)(?=\sbuild\/|\).+chrome\/(?![1-6]{0,1}\d\.))/i,
							], [MODEL, [VENDOR, 'Sony'], [TYPE, MOBILE]], [

								/\s(ouya)\s/i,                                                      
								/(nintendo)\s([wids3u]+)/i,                                          
							], [VENDOR, MODEL, [TYPE, CONSOLE]], [

								/android.+;\s(shield)\sbuild/i,                                      
							], [MODEL, [VENDOR, 'Nvidia'], [TYPE, CONSOLE]], [

								/(playstation\s[34portablevi]+)/i,                                   
							], [MODEL, [VENDOR, 'Sony'], [TYPE, CONSOLE]], [

								/(sprint\s(\w+))/i,                                                  
							], [[VENDOR, mapper.str, maps.device.sprint.vendor], [MODEL, mapper.str, maps.device.sprint.model], [TYPE, MOBILE]], [

								/(htc)[;_\s-]+([\w\s]+(?=\)|\sbuild)|\w+)/i,                        
								/(zte)-(\w*)/i,                                                     
								/(alcatel|geeksphone|nexian|panasonic|(?=;\s)sony)[_\s-]?([\w-]*)/i,

							], [VENDOR, [MODEL, /_/g, ' '], [TYPE, MOBILE]], [

								/(nexus\s9)/i,                                                       
							], [MODEL, [VENDOR, 'HTC'], [TYPE, TABLET]], [

								/d\/huawei([\w\s-]+)[;\)]/i,
								/(nexus\s6p|vog-l29|ane-lx1|eml-l29|ele-l29)/i,                              
							], [MODEL, [VENDOR, 'Huawei'], [TYPE, MOBILE]], [

								/android.+(bah2?-a?[lw]\d{2})/i,                                     
							], [MODEL, [VENDOR, 'Huawei'], [TYPE, TABLET]], [

								/(microsoft);\s(lumia[\s\w]+)/i,                                     
							], [VENDOR, MODEL, [TYPE, MOBILE]], [

								/[\s\(;](xbox(?:\sone)?)[\s\);]/i,                                   
							], [MODEL, [VENDOR, 'Microsoft'], [TYPE, CONSOLE]], [
								/(kin\.[onetw]{3})/i,                                                
							], [[MODEL, /\./g, ' '], [VENDOR, 'Microsoft'], [TYPE, MOBILE]], [

								/\s(milestone|droid(?:[2-4x]|\s(?:bionic|x2|pro|razr))?:?(\s4g)?)[\w\s]+build\
								/mot[\s-]?(\w*)/i,
								/(XT\d{3,4}) build\
								/(nexus\s6)/i,
							], [MODEL, [VENDOR, 'Motorola'], [TYPE, MOBILE]], [
								/android.+\s(mz60\d|xoom[\s2]{0,2})\sbuild\
							], [MODEL, [VENDOR, 'Motorola'], [TYPE, TABLET]], [

								/hbbtv\/\d+\.\d+\.\d+\s+\([\w\s]*;\s*(\w[^;]*);([^;]*)/i,            
							], [[VENDOR, util.trim], [MODEL, util.trim], [TYPE, SMARTTV]], [

								/hbbtv.+maple;(\d+)/i,
							], [[MODEL, /^/, 'SmartTV'], [VENDOR, 'Samsung'], [TYPE, SMARTTV]], [

								/\(dtv[\);].+(aquos)/i,                                              
							], [MODEL, [VENDOR, 'Sharp'], [TYPE, SMARTTV]], [

								/android.+((sch-i[89]0\d|shw-m380s|gt-p\d{4}|gt-n\d+|sgh-t8[56]9|nexus 10))/i,
								/((SM-T\w+))/i,
							], [[VENDOR, 'Samsung'], MODEL, [TYPE, TABLET]], [                  
								/smart-tv.+(samsung)/i,
							], [VENDOR, [TYPE, SMARTTV], MODEL], [
								/((s[cgp]h-\w+|gt-\w+|galaxy\snexus|sm-\w[\w\d]+))/i,
								/(sam[sung]*)[\s-]*(\w+-?[\w-]*)/i,
								/sec-((sgh\w+))/i,
							], [[VENDOR, 'Samsung'], MODEL, [TYPE, MOBILE]], [

								/sie-(\w*)/i,                                                        
							], [MODEL, [VENDOR, 'Siemens'], [TYPE, MOBILE]], [

								/(maemo|nokia).*(n900|lumia\s\d+)/i,                                
								/(nokia)[\s_-]?([\w-]*)/i,
							], [[VENDOR, 'Nokia'], MODEL, [TYPE, MOBILE]], [

								/android[x\d\.\s;]+\s([ab][1-7]\-?[0178a]\d\d?)/i,                   
							], [MODEL, [VENDOR, 'Acer'], [TYPE, TABLET]], [

								/android.+([vl]k\-?\d{3})\s+build/i,                                 
							], [MODEL, [VENDOR, 'LG'], [TYPE, TABLET]], [
								/android\s3\.[\s\w;-]{10}(lg?)-([06cv9]{3,4})/i,                     
							], [[VENDOR, 'LG'], MODEL, [TYPE, TABLET]], [
								/(lg) netcast\.tv/i,                                                 
							], [VENDOR, MODEL, [TYPE, SMARTTV]], [
								/(nexus\s[45])/i,                                                   
								/lg[e;\s\/-]+(\w*)/i,
								/android.+lg(\-?[\d\w]+)\s+build/i,
							], [MODEL, [VENDOR, 'LG'], [TYPE, MOBILE]], [

								/(lenovo)\s?(s(?:5000|6000)(?:[\w-]+)|tab(?:[\s\w]+))/i,             
							], [VENDOR, MODEL, [TYPE, TABLET]], [
								/android.+(ideatab[a-z0-9\-\s]+)/i,                                  
							], [MODEL, [VENDOR, 'Lenovo'], [TYPE, TABLET]], [
								/(lenovo)[_\s-]?([\w-]+)/i,
							], [VENDOR, MODEL, [TYPE, MOBILE]], [

								/linux;.+((jolla));/i,                                               
							], [VENDOR, MODEL, [TYPE, MOBILE]], [

								/((pebble))app\/[\d\.]+\s/i,                                         
							], [VENDOR, MODEL, [TYPE, WEARABLE]], [

								/android.+;\s(oppo)\s?([\w\s]+)\sbuild/i,                            
							], [VENDOR, MODEL, [TYPE, MOBILE]], [

								/crkey/i,                                                            
							], [[MODEL, 'Chromecast'], [VENDOR, 'Google'], [TYPE, SMARTTV]], [

								/android.+;\s(glass)\s\d/i,                                          
							], [MODEL, [VENDOR, 'Google'], [TYPE, WEARABLE]], [

								/android.+;\s(pixel c)[\s)]/i,                                       
							], [MODEL, [VENDOR, 'Google'], [TYPE, TABLET]], [

								/android.+;\s(pixel( [23])?( xl)?)[\s)]/i,                              
							], [MODEL, [VENDOR, 'Google'], [TYPE, MOBILE]], [

								/android.+;\s(\w+)\s+build\/hm\1/i,                                 
								/android.+(hm[\s\-_]*note?[\s_]*(?:\d\w)?)\s+build/i,               
								/android.+(mi[\s\-_]*(?:a\d|one|one[\s_]plus|note lte)?[\s_]*(?:\d?\w?)[\s_]*(?:plus)?)\s+build/i,

								/android.+(redmi[\s\-_]*(?:note)?(?:[\s_]?[\w\s]+))\s+build/i,       
							], [[MODEL, /_/g, ' '], [VENDOR, 'Xiaomi'], [TYPE, MOBILE]], [
								/android.+(mi[\s\-_]*(?:pad)(?:[\s_]?[\w\s]+))\s+build/i,            
							], [[MODEL, /_/g, ' '], [VENDOR, 'Xiaomi'], [TYPE, TABLET]], [
								/android.+;\s(m[1-5]\snote)\sbuild/i,                                
							], [MODEL, [VENDOR, 'Meizu'], [TYPE, MOBILE]], [
								/(mz)-([\w-]{2,})/i,
							], [[VENDOR, 'Meizu'], MODEL, [TYPE, MOBILE]], [

								/android.+a000(1)\s+build/i,                                        
								/android.+oneplus\s(a\d{4})[\s)]/i,
							], [MODEL, [VENDOR, 'OnePlus'], [TYPE, MOBILE]], [

								/android.+[;\/]\s*(RCT[\d\w]+)\s+build/i,                            
							], [MODEL, [VENDOR, 'RCA'], [TYPE, TABLET]], [

								/android.+[;\/\s]+(Venue[\d\s]{2,7})\s+build/i,                      
							], [MODEL, [VENDOR, 'Dell'], [TYPE, TABLET]], [

								/android.+[;\/]\s*(Q[T|M][\d\w]+)\s+build/i,                         
							], [MODEL, [VENDOR, 'Verizon'], [TYPE, TABLET]], [

								/android.+[;\/]\s+(Barnes[&\s]+Noble\s+|BN[RT])(V?.*)\s+build/i,     
							], [[VENDOR, 'Barnes & Noble'], MODEL, [TYPE, TABLET]], [

								/android.+[;\/]\s+(TM\d{3}.*\b)\s+build/i,                           
							], [MODEL, [VENDOR, 'NuVision'], [TYPE, TABLET]], [

								/android.+;\s(k88)\sbuild/i,                                         
							], [MODEL, [VENDOR, 'ZTE'], [TYPE, TABLET]], [

								/android.+[;\/]\s*(gen\d{3})\s+build.*49h/i,                         
							], [MODEL, [VENDOR, 'Swiss'], [TYPE, MOBILE]], [

								/android.+[;\/]\s*(zur\d{3})\s+build/i,                              
							], [MODEL, [VENDOR, 'Swiss'], [TYPE, TABLET]], [

								/android.+[;\/]\s*((Zeki)?TB.*\b)\s+build/i,                         
							], [MODEL, [VENDOR, 'Zeki'], [TYPE, TABLET]], [

								/(android).+[;\/]\s+([YR]\d{2})\s+build/i,
								/android.+[;\/]\s+(Dragon[\-\s]+Touch\s+|DT)(\w{5})\sbuild/i,        
							], [[VENDOR, 'Dragon Touch'], MODEL, [TYPE, TABLET]], [

								/android.+[;\/]\s*(NS-?\w{0,9})\sbuild/i,                            
							], [MODEL, [VENDOR, 'Insignia'], [TYPE, TABLET]], [

								/android.+[;\/]\s*((NX|Next)-?\w{0,9})\s+build/i,                    
							], [MODEL, [VENDOR, 'NextBook'], [TYPE, TABLET]], [

								/android.+[;\/]\s*(Xtreme\_)?(V(1[045]|2[015]|30|40|60|7[05]|90))\s+build/i,
							], [[VENDOR, 'Voice'], MODEL, [TYPE, MOBILE]], [                    

								/android.+[;\/]\s*(LVTEL\-)?(V1[12])\s+build/i,                     
							], [[VENDOR, 'LvTel'], MODEL, [TYPE, MOBILE]], [

								/android.+;\s(PH-1)\s/i,
							], [MODEL, [VENDOR, 'Essential'], [TYPE, MOBILE]], [                

								/android.+[;\/]\s*(V(100MD|700NA|7011|917G).*\b)\s+build/i,          
							], [MODEL, [VENDOR, 'Envizen'], [TYPE, TABLET]], [

								/android.+[;\/]\s*(Le[\s\-]+Pan)[\s\-]+(\w{1,9})\s+build/i,          
							], [VENDOR, MODEL, [TYPE, TABLET]], [

								/android.+[;\/]\s*(Trio[\s\-]*.*)\s+build/i,                         
							], [MODEL, [VENDOR, 'MachSpeed'], [TYPE, TABLET]], [

								/android.+[;\/]\s*(Trinity)[\-\s]*(T\d{3})\s+build/i,                
							], [VENDOR, MODEL, [TYPE, TABLET]], [

								/android.+[;\/]\s*TU_(1491)\s+build/i,                               
							], [MODEL, [VENDOR, 'Rotor'], [TYPE, TABLET]], [

								/android.+(KS(.+))\s+build/i,                                        
							], [MODEL, [VENDOR, 'Amazon'], [TYPE, TABLET]], [

								/android.+(Gigaset)[\s\-]+(Q\w{1,9})\s+build/i,                      
							], [VENDOR, MODEL, [TYPE, TABLET]], [

								/\s(tablet|tab)[;\/]/i,                                             
								/\s(mobile)(?:[;\/]|\ssafari)/i,                                     
							], [[TYPE, util.lowerize], VENDOR, MODEL], [

								/[\s\/\(](smart-?tv)[;\)]/i,                                         
							], [[TYPE, SMARTTV]], [

								/(android[\w\.\s\-]{0,9});.+build/i,                                 
							], [MODEL, [VENDOR, 'Generic']],
						],

						engine: [
							[

								/windows.+\sedge\/([\w\.]+)/i,                                       
							], [VERSION, [NAME, 'EdgeHTML']], [

								/webkit\/537\.36.+chrome\/(?!27)([\w\.]+)/i,                         
							], [VERSION, [NAME, 'Blink']], [

								/(presto)\/([\w\.]+)/i,                                             
								/(webkit|trident|netfront|netsurf|amaya|lynx|w3m|goanna)\/([\w\.]+)/i,

								/(khtml|tasman|links)[\/\s]\(?([\w\.]+)/i,                          
								/(icab)[\/\s]([23]\.[\d\.]+)/i,                                      
							], [NAME, VERSION], [

								/rv\:([\w\.]{1,9}).+(gecko)/i,                                       
							], [VERSION, NAME],
						],

						os: [
							[

								/microsoft\s(windows)\s(vista|xp)/i,                                 
							], [NAME, VERSION], [
								/(windows)\snt\s6\.2;\s(arm)/i,                                     
								/(windows\sphone(?:\sos)*)[\s\/]?([\d\.\s\w]*)/i,                   
								/(windows\smobile|windows)[\s\/]?([ntce\d\.\s]+\w)/i,
							], [NAME, [VERSION, mapper.str, maps.os.windows.version]], [
								/(win(?=3|9|n)|win\s9x\s)([nt\d\.]+)/i,
							], [[NAME, 'Windows'], [VERSION, mapper.str, maps.os.windows.version]], [

								/\((bb)(10);/i,                                                      
							], [[NAME, 'BlackBerry'], VERSION], [
								/(blackberry)\w*\/?([\w\.]*)/i,                                     
								/(tizen|kaios)[\/\s]([\w\.]+)/i,                                    
								/(android|webos|palm\sos|qnx|bada|rim\stablet\sos|meego|sailfish|contiki)[\/\s-]?([\w\.]*)/i,

							], [NAME, VERSION], [
								/(symbian\s?os|symbos|s60(?=;))[\/\s-]?([\w\.]*)/i,                  
							], [[NAME, 'Symbian'], VERSION], [
								/\((series40);/i,                                                    
							], [NAME], [
								/mozilla.+\(mobile;.+gecko.+firefox/i,                               
							], [[NAME, 'Firefox OS'], VERSION], [

								/(nintendo|playstation)\s([wids34portablevu]+)/i,                   

								/(mint)[\/\s\(]?(\w*)/i,                                            
								/(mageia|vectorlinux)[;\s]/i,                                       
								/(joli|[kxln]?ubuntu|debian|suse|opensuse|gentoo|(?=\s)arch|slackware|fedora|mandriva|centos|pclinuxos|redhat|zenwalk|linpus)[\/\s-]?(?!chrom)([\w\.-]*)/i,

								/(hurd|linux)\s?([\w\.]*)/i,                                        
								/(gnu)\s?([\w\.]*)/i,                                                
							], [NAME, VERSION], [

								/(cros)\s[\w]+\s([\w\.]+\w)/i,                                       
							], [[NAME, 'Chromium OS'], VERSION], [

								/(sunos)\s?([\w\.\d]*)/i,                                            
							], [[NAME, 'Solaris'], VERSION], [

								/\s([frentopc-]{0,4}bsd|dragonfly)\s?([\w\.]*)/i,                    
							], [NAME, VERSION], [

								/(haiku)\s(\w+)/i,                                                   
							], [NAME, VERSION], [

								/cfnetwork\/.+darwin/i,
								/ip[honead]{2,4}(?:.*os\s([\w]+)\slike\smac|;\sopera)/i,             
							], [[VERSION, /_/g, '.'], [NAME, 'iOS']], [

								/(mac\sos\sx)\s?([\w\s\.]*)/i,
								/(macintosh|mac(?=_powerpc)\s)/i,                                    
							], [[NAME, 'Mac OS'], [VERSION, /_/g, '.']], [

								/((?:open)?solaris)[\/\s-]?([\w\.]*)/i,                             
								/(aix)\s((\d)(?=\.|\)|\s)[\w\.])*/i,                                
								/(plan\s9|minix|beos|os\/2|amigaos|morphos|risc\sos|openvms|fuchsia)/i,

								/(unix)\s?([\w\.]*)/i,                                               
							], [NAME, VERSION],
						],
					};

					var UAParser = function (uastring, extensions) {

						if (typeof uastring === 'object') {
							extensions = uastring;
							uastring = undefined;
						}

						if (!(this instanceof UAParser)) {
							return new UAParser(uastring, extensions).getResult();
						}

						var ua = uastring || ((window && window.navigator && window.navigator.userAgent) ? window.navigator.userAgent : EMPTY);
						var rgxmap = extensions ? util.extend(regexes, extensions) : regexes;

						this.getBrowser = function () {
							var browser = {name: undefined, version: undefined};
							mapper.rgx.call(browser, ua, rgxmap.browser);
							browser.major = util.major(browser.version); 
							return browser;
						};
						this.getCPU = function () {
							var cpu = {architecture: undefined};
							mapper.rgx.call(cpu, ua, rgxmap.cpu);
							return cpu;
						};
						this.getDevice = function () {
							var device = {vendor: undefined, model: undefined, type: undefined};
							mapper.rgx.call(device, ua, rgxmap.device);
							return device;
						};
						this.getEngine = function () {
							var engine = {name: undefined, version: undefined};
							mapper.rgx.call(engine, ua, rgxmap.engine);
							return engine;
						};
						this.getOS = function () {
							var os = {name: undefined, version: undefined};
							mapper.rgx.call(os, ua, rgxmap.os);
							return os;
						};
						this.getResult = function () {
							return {
								ua: this.getUA(),
								browser: this.getBrowser(),
								engine: this.getEngine(),
								os: this.getOS(),
								device: this.getDevice(),
								cpu: this.getCPU(),
							};
						};
						this.getUA = function () {
							return ua;
						};
						this.setUA = function (uastring) {
							ua = uastring;
							return this;
						};
						return this;
					};

					UAParser.VERSION = LIBVERSION;
					UAParser.BROWSER = {
						NAME: NAME,
						MAJOR: MAJOR, 
						VERSION: VERSION,
					};
					UAParser.CPU = {
						ARCHITECTURE: ARCHITECTURE,
					};
					UAParser.DEVICE = {
						MODEL: MODEL,
						VENDOR: VENDOR,
						TYPE: TYPE,
						CONSOLE: CONSOLE,
						MOBILE: MOBILE,
						SMARTTV: SMARTTV,
						TABLET: TABLET,
						WEARABLE: WEARABLE,
						EMBEDDED: EMBEDDED,
					};
					UAParser.ENGINE = {
						NAME: NAME,
						VERSION: VERSION,
					};
					UAParser.OS = {
						NAME: NAME,
						VERSION: VERSION,
					};

					if (typeof (exports) !== UNDEF_TYPE) {

						if (typeof module !== UNDEF_TYPE && module.exports) {
							exports = module.exports = UAParser;
						}
						exports.UAParser = UAParser;
					} else {

						if (typeof (define) === 'function' && define.amd) {
							define(function () {
								return UAParser;
							});
						} else if (window) {

							window.UAParser = UAParser;
						}
					}

					var $ = window && (window.jQuery || window.Zepto);
					if ($ && !$.ua) {
						var parser = new UAParser();
						$.ua = parser.getResult();
						$.ua.get = function () {
							return parser.getUA();
						};
						$.ua.set = function (uastring) {
							parser.setUA(uastring);
							var result = parser.getResult();
							for (var prop in result) {
								$.ua[prop] = result[prop];
							}
						};
					}

				})(typeof window === 'object' ? window : this);

			}, {}],
	}, {}, [3])(3);
});
