/*-
 * ========================LICENSE_START=================================
 * screenit-clientportaal
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
(function(f) {
    if (typeof exports === 'object' && typeof module !== 'undefined') {module.exports = f();} else if (typeof define === 'function' && define.amd) {
        define([], f);
    } else {
        var g;
        if (typeof window !== 'undefined') {g = window;} else if (typeof global !== 'undefined') {g = global;} else if (typeof self !== 'undefined') {g = self;} else {g = this;}
        g.outdatedBrowserRework = f();
    }
})(function() {
    var define, module, exports;
    return (function() {
        function r(e, n, t) {
            function o(i, f) {
                if (!n[i]) {
                    if (!e[i]) {
                        var c = 'function' == typeof require && require;
                        if (!f && c) return c(i, !0);
                        if (u) return u(i, !0);
                        var a = new Error('Cannot find module \'' + i + '\'');
                        throw a.code = 'MODULE_NOT_FOUND', a;
                    }
                    var p = n[i] = {exports: {}};
                    e[i][0].call(p.exports, function(r) {
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
            function(require, module, exports) {
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

                var updateDefaults = function(defaults, updatedValues) {
                    for (var key in updatedValues) {
                        defaults[key] = updatedValues[key];
                    }

                    return defaults;
                };

                module.exports = function(parsedUserAgent, options) {

                    var browserSupport = options.browserSupport ? updateDefaults(DEFAULTS, options.browserSupport) : DEFAULTS;
                    var requiredCssProperty = options.requiredCssProperty || false;

                    var browserName = parsedUserAgent.browser.name;

                    var isAndroidButNotChrome;
                    if (options.requireChromeOnAndroid) {
                        isAndroidButNotChrome = parsedUserAgent.os.name === 'Android' && parsedUserAgent.browser.name !== 'Chrome';
                    }

                    var parseMinorVersion = function(version) {
                        return version.replace(/[^\d.]/g, '').split('.')[1];
                    };

                    var isBrowserUnsupported = function() {
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

                    var isBrowserOutOfDate = function() {
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

                    var isPropertySupported = function(property) {
                        if (!property) {
                            return true;
                        }
                        var div = document.createElement('div');
                        var vendorPrefixes = ['khtml', 'ms', 'o', 'moz', 'webkit'];
                        var count = vendorPrefixes.length;

                        if (property in div.style) {
                            return true;
                        }

                        property = property.replace(/^[a-z]/, function(val) {
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
            function(require, module, exports) {

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
            function(require, module, exports) {
                var evaluateBrowser = require('./evaluateBrowser');
                var languageMessages = require('./languages.json');
                var deepExtend = require('./extend');
                var UserAgentParser = require('ua-parser-js');

                var COLORS = {
                    salmon: '#f25648',
                    white: 'white',
                };

                module.exports = function(options) {
                    var main = function() {

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

                        var changeOpacity = function(opacityValue) {
                            outdatedUI.style.opacity = opacityValue / 100;
                            outdatedUI.style.filter = 'alpha(opacity=' + opacityValue + ')';
                        };

                        var fadeIn = function(opacityValue) {
                            changeOpacity(opacityValue);
                            if (opacityValue === 1) {
                                outdatedUI.style.display = 'table';
                            }
                            if (opacityValue === 100) {
                                done = true;
                            }
                        };

                        var makeFadeInFunction = function(opacityValue) {
                            return function() {
                                fadeIn(opacityValue);
                            };
                        };

                        var getMessage = function(lang) {
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
                                '<svg id="logo" ' +
                                'class="MijnBevolkingsOnderzoekLogo_style__2MBoS" ' +
                                'xmlns="http://www.w3.org/2000/svg" ' +
                                'width="154.727" ' +
                                'height="41.382" ' +
                                'viewBox="0 0 154.727 41.382">' +
                                '<path id="Fill_1" d="M2.31,9.158A7.715,7.715,0,0,1,0,8.878L1.61,0H3.22L2.66,2.936a8.271,8.271,0,0,1-.28,1.4C2.942,3.492,3.495,2.8,4.55,2.8c.993,0,1.61.777,1.61,2.028C6.16,7.456,4.649,9.158,2.31,9.158ZM3.85,4.194c-.662,0-1.525,1.037-1.96,3.355l-.07.35a.7.7,0,0,1,.1.016,2.641,2.641,0,0,0,.525.054c.944,0,1.96-.9,1.96-2.866C4.41,4.483,4.232,4.194,3.85,4.194Z" transform="translate(48.257 21.686)" fill="#1c1b19"></path><path id="Fill_3" d="M2.38,6.361A2.218,2.218,0,0,1,0,3.914,3.714,3.714,0,0,1,3.57,0C4.71,0,5.39.575,5.39,1.538A1.9,1.9,0,0,1,4.3,3.233a7.393,7.393,0,0,1-2.616.751,1.221,1.221,0,0,0,.256.826A.914.914,0,0,0,2.66,5.1,2.432,2.432,0,0,0,4.2,4.4l.63,1.119A3.654,3.654,0,0,1,2.38,6.361Zm.91-5.1c-.728,0-1.289.612-1.54,1.678,1.413-.236,2.1-.624,2.1-1.188C3.85,1.418,3.667,1.258,3.29,1.258Z" transform="translate(55.047 24.482)" fill="#1c1b19"></path><path id="Fill_5" d="M0,0H1.75l.28,3.076A8.238,8.238,0,0,1,2.1,4.194L4.2,0H5.88L2.52,6.082H.98L0,0" transform="translate(61.067 24.622)" fill="#1c1b19"></path><path id="Fill_7" d="M2.45,6.361A2.328,2.328,0,0,1,0,3.845,3.616,3.616,0,0,1,3.43,0,2.328,2.328,0,0,1,5.88,2.517C5.88,4.78,4.469,6.361,2.45,6.361Zm.84-5.173c-1,0-1.61,1.506-1.61,2.586,0,.9.323,1.4.91,1.4,1.041,0,1.68-1.547,1.68-2.656C4.27,1.672,3.913,1.188,3.29,1.188Z" transform="translate(66.667 24.482)" fill="#1c1b19"></path><path id="Fill_9" d="M1.68,0H3.29L1.68,9.017H0Z" transform="translate(73.107 21.686)" fill="#1c1b19"></path><path id="Fill_11" d="M1.61,9.017H0L1.54,0H3.22L2.24,5.383,4.55,2.936H6.37L3.78,5.662,5.39,9.017H3.64L2.17,5.942Z" transform="translate(76.397 21.686)" fill="#1c1b19"></path><path id="Fill_13" d="M1.61,8.878H0L1.12,2.8H2.73L1.61,8.877Zm.63-6.92A.959.959,0,0,1,1.26.979.918.918,0,0,1,2.24,0a.958.958,0,0,1,.98.979A1.006,1.006,0,0,1,2.24,1.957Z" transform="translate(82.697 21.826)" fill="#1c1b19"></path><path id="Fill_15" d="M1.05.21H2.66a12.329,12.329,0,0,1-.28,1.4h.07A2.479,2.479,0,0,1,4.62,0C5.74,0,6.16.839,5.88,2.377l-.7,3.915H3.57l.7-3.845c.07-.7,0-.979-.42-.979-.7,0-1.54.909-2.17,4.334l-.07.489H0L1.05.21" transform="translate(85.987 24.412)" fill="#1c1b19"></path><path id="Fill_17" d="M2.24,9.158A3.872,3.872,0,0,1,0,8.458L.7,7.27l.1.063a2.721,2.721,0,0,0,1.507.5c.875,0,1.321-.445,1.54-1.538l.07-.279A4.329,4.329,0,0,1,4.2,4.893H4.13c-.467.8-1.036,1.538-2.03,1.538A1.8,1.8,0,0,1,.42,4.4a4.762,4.762,0,0,1,1.059-3.2A3.591,3.591,0,0,1,4.27,0,7.414,7.414,0,0,1,6.433.309L6.58.35,5.46,6.221C5.1,8.362,4.23,9.158,2.24,9.158Zm1.819-7.9c-1.1,0-1.89,1.205-1.89,2.866,0,.62.178.908.56.908.7,0,1.61-1.05,1.96-3.355l.071-.35A1.766,1.766,0,0,0,4.06,1.258Z" transform="translate(92.287 24.412)" fill="#1c1b19"></path><path id="Fill_19" d="M3.71,6.151V2.1c0-.7-.21-1.188-.98-1.188A2.257,2.257,0,0,0,1.05,1.957V6.151H0V.14H1.05V.979A2.486,2.486,0,0,1,3.01,0,1.707,1.707,0,0,1,4.83,1.887V6.151H3.71" transform="translate(108.737 24.552)" fill="#1c1b19"></path><path id="Fill_21" d="M2.52,9.158C.966,9.158,0,7.952,0,6.012,0,4.16,1.094,2.866,2.66,2.866a2.137,2.137,0,0,1,1.54.489V0H5.25V7.689a4.964,4.964,0,0,0,.14,1.328H4.34a1.516,1.516,0,0,1-.14-.7A1.979,1.979,0,0,1,2.52,9.158ZM2.8,3.775c-1.02,0-1.68.878-1.68,2.237,0,1.442.572,2.237,1.61,2.237A2.013,2.013,0,0,0,4.15,7.53l0,0L4.2,7.48V4.264A1.88,1.88,0,0,0,2.8,3.775Z" transform="translate(114.617 21.686)" fill="#1c1b19"></path><path id="Fill_23" d="M2.94,6.291C1.127,6.291,0,5.059,0,3.076A2.883,2.883,0,0,1,2.8,0,2.3,2.3,0,0,1,4.568.713a3.149,3.149,0,0,1,.612,2.5H1.12C1.2,4.7,1.838,5.453,3.01,5.453a2.858,2.858,0,0,0,1.61-.489l.42.769A3.657,3.657,0,0,1,2.94,6.291ZM2.8.839A1.611,1.611,0,0,0,1.19,2.377H4.13a1.761,1.761,0,0,0-.418-1.2A1.238,1.238,0,0,0,2.8.839Z" transform="translate(120.987 24.552)" fill="#1c1b19"></path><path id="Fill_25" d="M3.01,1.118c-.07-.07-.21-.14-.56-.14-.84,0-1.33.979-1.4,1.188V6.151H0V.14H1.05v.909A1.86,1.86,0,0,1,2.59,0a1.123,1.123,0,0,1,.7.14l-.28.979" transform="translate(127.637 24.552)" fill="#1c1b19"></path><path id="Fill_27" d="M0,6.012V5.383L2.94.839H.14V0H4.27V.7L1.26,5.173H4.34v.839Z" transform="translate(131.767 24.692)" fill="#1c1b19"></path><path id="Fill_29" d="M2.87,6.291a2.658,2.658,0,0,1-2.2-1.022A3.536,3.536,0,0,1,0,3.146a3.573,3.573,0,0,1,.647-2.1A2.649,2.649,0,0,1,2.87,0a2.657,2.657,0,0,1,2.2,1.022A3.536,3.536,0,0,1,5.74,3.146a3.529,3.529,0,0,1-.674,2.1A2.646,2.646,0,0,1,2.87,6.291ZM2.8.839c-1.4,0-1.61,1.445-1.61,2.307,0,1.445.628,2.307,1.68,2.307,1.29,0,1.75-1.192,1.75-2.307S4.142.839,2.8.839Z" transform="translate(136.877 24.552)" fill="#1c1b19"></path><path id="Fill_31" d="M2.94,6.291C1.1,6.291,0,5.089,0,3.076A3.276,3.276,0,0,1,.77.883,2.637,2.637,0,0,1,2.8,0,2.232,2.232,0,0,1,4.485.679a3.288,3.288,0,0,1,.7,2.537H1.12c.057,1.02.418,2.237,1.82,2.237a3.218,3.218,0,0,0,1.574-.435l.105-.054.42.769A3.762,3.762,0,0,1,2.94,6.291ZM2.73.839c-.84,0-1.387.546-1.54,1.538H4.13a1.739,1.739,0,0,0-.313-1.035A1.3,1.3,0,0,0,2.73.839Z" transform="translate(143.527 24.552)" fill="#1c1b19"></path><path id="Fill_33" d="M4.83,9.017H3.57L1.05,5.662V9.017H0V0H1.05V5.662L3.5,3.006H4.76L2.31,5.662,4.829,9.016Z" transform="translate(149.897 21.686)" fill="#1c1b19"></path><path id="Fill_35" d="M4.69,6.221a3.4,3.4,0,0,0,1.26.21A2.927,2.927,0,0,0,8.82,3.285,2.905,2.905,0,0,0,5.88.14,3.4,3.4,0,0,0,4.62.35,3.043,3.043,0,0,0,3.15,0C1.75,0,.98.769.98,1.748a2.188,2.188,0,0,0,.84,1.678c.49.559.91.839.91,1.258,0,.28-.21.559-.77.559A2.087,2.087,0,0,1,.7,4.753L0,5.872a3.316,3.316,0,0,0,2.03.559c1.54,0,2.38-.629,2.38-1.748,0-.839-.49-1.258-1.05-1.887-.49-.419-.77-.769-.77-1.049a.587.587,0,0,1,.63-.629.768.768,0,0,1,.56,0H3.71a1.5,1.5,0,0,1,.77.769A1.461,1.461,0,0,1,5.88.979C7.07.979,7.7,2.027,7.7,3.285S7.14,5.592,5.95,5.592a1.494,1.494,0,0,1-.84-.21,1.087,1.087,0,0,1-.42.839" transform="translate(98.727 24.412)" fill="#1c1b19"></path><text id="Mijn" transform="translate(47.645 13.194)"><tspan x="0" y="0">Mijn</tspan></text><path id="Fill_36" d="M10.99,19.923h0a10.872,10.872,0,0,0,.21-2.166,9.846,9.846,0,0,0-.42-2.866,5.014,5.014,0,0,0,2.52-1.4A4.819,4.819,0,0,0,14.7,10a4.822,4.822,0,0,0-1.4-3.5,4.99,4.99,0,0,0-7.07,0A4.582,4.582,0,0,0,5.04,8.528a10.218,10.218,0,0,0-3.85-.769c-.22,0-.438.019-.649.037-.193.017-.374.033-.542.033A10.025,10.025,0,0,1,16.87,2.936a10.276,10.276,0,0,1,2.091,3.189A10.018,10.018,0,0,1,19.74,10a10.713,10.713,0,0,1-.21,2.1,10.072,10.072,0,0,0-4.06,2.447A10.245,10.245,0,0,0,12.74,19.5a7.957,7.957,0,0,1-1.749.42Zm-5.04-.7h0A6.783,6.783,0,0,1,3.78,17.965v-.14a2.521,2.521,0,0,0-2.52-2.517l0,0A10.3,10.3,0,0,1,.21,12.932a4.052,4.052,0,0,1,.98-.14,4.864,4.864,0,0,1,3.5,1.468,4.851,4.851,0,0,1,1.47,3.495,4.653,4.653,0,0,1-.21,1.468Z" transform="translate(10.85 0)" fill="#6e93c0"></path><path id="Fill_37" d="M2.59,5.1H2.52A9.752,9.752,0,0,0,1.05,3.216,4.471,4.471,0,0,0,0,2.307,2.641,2.641,0,0,1,2.59,0,2.568,2.568,0,0,1,5.11,2.586,2.508,2.508,0,0,1,2.59,5.1" transform="translate(18.06 7.48)" fill="#6e93c0"></path><path id="Fill_38" d="M10.01,19.992a8.234,8.234,0,0,1-2.66-.35,10.236,10.236,0,0,0-2.94-6.92A10.338,10.338,0,0,0,0,10.136V10A10.713,10.713,0,0,1,.21,7.9,10.759,10.759,0,0,0,4.34,5.453,10.059,10.059,0,0,0,7,.489,11.016,11.016,0,0,1,10.01,0a9.9,9.9,0,0,1,3.876.786,10.068,10.068,0,0,1,5.346,5.339A9.863,9.863,0,0,1,20.02,10a10,10,0,0,1-2.94,7.13,9.666,9.666,0,0,1-3.141,2.089A10.184,10.184,0,0,1,10.01,19.992Zm0-14.959A4.869,4.869,0,0,0,6.51,6.5a4.974,4.974,0,0,0,0,7.061,4.838,4.838,0,0,0,3.5,1.4,4.835,4.835,0,0,0,3.5-1.4,4.973,4.973,0,0,0,0-7.061A4.866,4.866,0,0,0,10.01,5.033Z" transform="translate(23.38 11.604)" fill="#97a03b"></path><path id="Fill_39" d="M0,7.41A10.224,10.224,0,0,1,2.73,2.447,10.053,10.053,0,0,1,6.79,0,10.112,10.112,0,0,1,4.13,4.963,10.808,10.808,0,0,1,0,7.41" transform="translate(23.59 12.093)" fill="#4f512b"></path><path id="Fill_40" d="M2.52,5.1A2.508,2.508,0,0,1,0,2.586,2.568,2.568,0,0,1,2.52,0,2.628,2.628,0,0,1,5.11,2.586,2.568,2.568,0,0,1,2.52,5.1" transform="translate(30.8 19.083)" fill="#97a03b"></path><path id="Fill_41" d="M5.53,18.874h0a10.882,10.882,0,0,1-2.1-1.4,4.162,4.162,0,0,1,.98-1.468A5.015,5.015,0,0,1,7.98,14.54h.21a4.152,4.152,0,0,0,1.61.35,5.384,5.384,0,0,1,1.68,1.118l.005.005a1.512,1.512,0,0,1,.275.344l-.14.14a9.218,9.218,0,0,0-1.47,1.818,2.526,2.526,0,0,0-2.17-1.328,2.574,2.574,0,0,0-2.45,1.887Zm10.71-4.963h0a8.375,8.375,0,0,0-1.19-1.467,3.061,3.061,0,0,1-.49-.42l.1-.315.1-.315a9.9,9.9,0,0,0,3.78.769h.071a7.249,7.249,0,0,0,1.189-.07c-.021.074-.043.15-.064.227a7.28,7.28,0,0,1-.426,1.241h-.63a9.374,9.374,0,0,0-2.45.349ZM.42,12.862h0A10.027,10.027,0,0,1,2.94,2.866,9.935,9.935,0,0,1,8.82,0a10.822,10.822,0,0,0-.28,2.167A10.559,10.559,0,0,0,9.03,5.1,4.428,4.428,0,0,0,6.51,6.431a4.855,4.855,0,0,0-1.47,3.5,10.7,10.7,0,0,0-4.2,2.517l-.419.418ZM18.55,7.13a5,5,0,0,1-3.5-1.468,4.851,4.851,0,0,1-1.47-3.5A5.477,5.477,0,0,1,13.86.7a10.949,10.949,0,0,1,2.17,1.258v.28a2.521,2.521,0,0,0,2.52,2.517A11.608,11.608,0,0,1,19.6,7.06h-.014c-.17,0-.343.017-.511.034S18.725,7.13,18.55,7.13Z" transform="translate(2.03 7.829)" fill="#d8234a"></path><path id="Fill_42" d="M10.01,12.233h0a9.87,9.87,0,0,1-3.78-.769A4.641,4.641,0,0,0,6.44,10,4.851,4.851,0,0,0,4.97,6.5a4.864,4.864,0,0,0-3.5-1.468,4.137,4.137,0,0,0-.98.14A10.744,10.744,0,0,1,0,2.237,10.791,10.791,0,0,1,.28.07C.63.07,1.05,0,1.47,0A10.223,10.223,0,0,1,5.32.769a5.438,5.438,0,0,0-.28,1.468,4.851,4.851,0,0,0,1.47,3.5A4.987,4.987,0,0,0,10.01,7.2c.35,0,.7-.07,1.05-.07A9.792,9.792,0,0,1,11.48,10a10.839,10.839,0,0,1-.21,2.167,7.618,7.618,0,0,1-1.26.07" transform="translate(10.57 7.759)" fill="#522f49"></path><path id="Fill_43" d="M2.52,2.8A2.508,2.508,0,0,1,0,.28V0A4.471,4.471,0,0,1,1.05.909,9.752,9.752,0,0,1,2.52,2.8" transform="translate(18.06 9.786)" fill="#522f49"></path><path id="Fill_44" d="M4.97,3.146A10.175,10.175,0,0,0,.42,2.027H0A2.424,2.424,0,0,1,2.45,0h.07a17.014,17.014,0,0,0,1.4,1.748,7.066,7.066,0,0,0,1.12.909c0,.21-.07.35-.07.489" transform="translate(9.59 15.309)" fill="#d8234a"></path><path id="Fill_45" d="M2.52,2.656A7.066,7.066,0,0,1,1.4,1.748,17.014,17.014,0,0,1,0,0,2.508,2.508,0,0,1,2.52,2.517v.14" transform="translate(12.11 15.309)" fill="#7b2e4c"></path><path id="Fill_46" d="M.07,4.893A3.4,3.4,0,0,1,0,4.054,12.462,12.462,0,0,1,.14,2.447,2.529,2.529,0,0,0,1.82.419,14.97,14.97,0,0,0,4.27,0,4.851,4.851,0,0,1,2.8,3.5a4.689,4.689,0,0,1-2.73,1.4" transform="translate(10.71 27.332)" fill="#81c7b4"></path><path id="Fill_47" d="M.35,3.565v-.35A2.411,2.411,0,0,0,0,1.957,9.293,9.293,0,0,1,1.47.14L1.61,0A5.42,5.42,0,0,1,2.8,3.146a14.97,14.97,0,0,1-2.45.419" transform="translate(12.18 24.186)" fill="#882e45"></path><path id="Fill_48" d="M8.26,19.992h0a10.013,10.013,0,0,1-7.07-2.936A11.412,11.412,0,0,1,0,15.658a10.268,10.268,0,0,0,4.41-2.517c.14.14.21.28.35.35a4.906,4.906,0,0,0,7,0,4.89,4.89,0,0,0,0-6.99,4.864,4.864,0,0,0-3.5-1.468,2.875,2.875,0,0,0-.77.07,11.132,11.132,0,0,0-.42-2.1A9.347,9.347,0,0,0,8.89,0a17.519,17.519,0,0,1,2.03.35,9.822,9.822,0,0,0,2.94,6.99,9.607,9.607,0,0,0,4.41,2.517V10a10.031,10.031,0,0,1-10.01,10" transform="translate(12.46 21.39)" fill="#81c7b4"></path><path id="Fill_49" d="M7.35,9.507A9.607,9.607,0,0,1,2.94,6.99,9.822,9.822,0,0,1,0,0,10.4,10.4,0,0,1,4.41,2.586a10.264,10.264,0,0,1,2.94,6.92" transform="translate(23.38 21.74)" fill="#3c6834"></path><path id="Fill_50" d="M1.26,3.006A8.465,8.465,0,0,0,0,.35,9.426,9.426,0,0,1,2.45,0h.63A9.347,9.347,0,0,1,1.26,3.006" transform="translate(18.27 21.39)" fill="#882e45"></path><path id="Fill_51" d="M2.24,5.1A2.563,2.563,0,0,1,0,3.775,10.992,10.992,0,0,0,1.33.21,2.158,2.158,0,0,1,2.24,0,2.628,2.628,0,0,1,4.83,2.586,2.568,2.568,0,0,1,2.24,5.1" transform="translate(18.48 28.87)" fill="#81c7b4"></path><path id="Fill_52" d="M10.01,19.992a10.028,10.028,0,0,1-7.14-2.936A10.279,10.279,0,0,1,.779,13.867,10.015,10.015,0,0,1,0,10,10.153,10.153,0,0,1,2.45,3.355,10,10,0,0,0,4.97,7.48c.044.044.088.1.135.149a1.82,1.82,0,0,0,.355.34A5.014,5.014,0,0,0,5.04,10a4.821,4.821,0,0,0,1.4,3.495,5.015,5.015,0,0,0,3.57,1.468,3.157,3.157,0,0,0,.77-.07,9.738,9.738,0,0,0,1.68,4.823A9.3,9.3,0,0,1,10.01,19.992Zm2.03-14.61h-.21a6.017,6.017,0,0,0-1.61-.35A4.358,4.358,0,0,1,8.54,3.914,4.851,4.851,0,0,1,7.07.42,13.557,13.557,0,0,1,9.59,0a1.191,1.191,0,0,0-.07.489,2.521,2.521,0,0,0,2.52,2.517,2.556,2.556,0,0,0,2.52-1.887,9.554,9.554,0,0,1,2.03,1.4,6.59,6.59,0,0,1-1.05,1.4A4.867,4.867,0,0,1,12.04,5.383Z" transform="translate(0 17.336)" fill="#ee9d55"></path><path id="Fill_53" d="M3.01,7.55h0a1.805,1.805,0,0,1-.354-.34c-.047-.054-.091-.105-.135-.149A10,10,0,0,1,0,2.936l.42-.42A10.7,10.7,0,0,1,4.62,0,4.853,4.853,0,0,0,6.09,3.5,4.364,4.364,0,0,0,7.77,4.613H7.56A5.015,5.015,0,0,0,3.99,6.082,4.152,4.152,0,0,0,3.01,7.549Zm8.33-1.119h0a1.536,1.536,0,0,0-.279-.348A5.378,5.378,0,0,0,9.38,4.963h.21A4.869,4.869,0,0,0,13.09,3.5a6.611,6.611,0,0,0,1.05-1.4,3.076,3.076,0,0,0,.49.42,8.263,8.263,0,0,1,1.19,1.468A9.662,9.662,0,0,0,11.341,6.43Z" transform="translate(2.45 17.755)" fill="#d72633"></path><path id="Fill_54" d="M2.52,3.006A2.508,2.508,0,0,1,0,.489,1.188,1.188,0,0,1,.07,0H.49A10.175,10.175,0,0,1,5.04,1.118,2.563,2.563,0,0,1,2.52,3.006" transform="translate(9.52 17.336)" fill="#d72633"></path><path id="Fill_55" d="M1.12,8.039A4.929,4.929,0,0,1,0,4.893,4.851,4.851,0,0,1,1.47,1.4,4.493,4.493,0,0,1,4.2,0V.839a7.368,7.368,0,0,1-.14,1.748A2.539,2.539,0,0,0,2.45,4.963a2.3,2.3,0,0,0,.28,1.188A11.788,11.788,0,0,1,1.33,7.9c-.07.07-.14.14-.21.14" transform="translate(15.75 26.493)" fill="#ee9d55"></path><path id="Fill_56" d="M1.68,12.652A9.744,9.744,0,0,1,0,7.829a4.689,4.689,0,0,0,2.73-1.4,4.851,4.851,0,0,0,1.47-3.5A9.714,9.714,0,0,0,8.33.419L8.75,0a11.132,11.132,0,0,1,.42,2.1A4.493,4.493,0,0,0,6.44,3.5a4.851,4.851,0,0,0-1.47,3.5,4.929,4.929,0,0,0,1.12,3.146,10.268,10.268,0,0,1-4.41,2.517" transform="translate(10.78 24.396)" fill="#8c844f"></path><path id="Fill_57" d="M1.19,5.592A4.988,4.988,0,0,0,0,2.447,9.717,9.717,0,0,1,4.48,0,8.465,8.465,0,0,1,5.74,2.656l-.42.419A9.714,9.714,0,0,1,1.19,5.592" transform="translate(13.79 21.74)" fill="#882e36"></path><path id="Fill_58" d="M.28,3.565A2.3,2.3,0,0,1,0,2.377,2.539,2.539,0,0,1,1.61,0,10.992,10.992,0,0,1,.28,3.565" transform="translate(18.2 29.079)" fill="#8c844f"></path><path id="Fill_59" d="M2.59,3.216A2.568,2.568,0,0,1,0,.7,2.066,2.066,0,0,1,.14,0,10.162,10.162,0,0,0,3.99,1.049a6.712,6.712,0,0,0-.56,2.027,1.778,1.778,0,0,1-.84.14" transform="translate(7.42 26.703)" fill="#ee9d55"></path><path id="Fill_60" d="M3.85,2.936A10.162,10.162,0,0,1,0,1.887,2.559,2.559,0,0,1,2.45,0,2.538,2.538,0,0,1,4.62,1.328a11.365,11.365,0,0,0-.77,1.608" transform="translate(7.56 24.815)" fill="#d72633"></path><path id="Fill_61" d="M0,2.027A6.712,6.712,0,0,1,.56,0H1.68A2.419,2.419,0,0,1,0,2.027" transform="translate(10.85 27.751)" fill="#8c844f"></path><path id="Fill_62" d="M.63,1.608H0A11.365,11.365,0,0,1,.77,0a2.411,2.411,0,0,1,.35,1.258v.35H.63" transform="translate(11.41 26.143)" fill="#882e36"> </path>' +
                                '</svg>' +
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
                        window.onload = function() {
                            if (oldOnload) {
                                oldOnload();
                            }
                            main();
                        };
                    }
                };

            }, {'./evaluateBrowser': 1, './extend': 2, './languages.json': 4, 'ua-parser-js': 5}], 4: [
            function(require, module, exports) {
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
            function(require, module, exports) {

                (function(window, undefined) {

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
                        extend: function(regexes, extensions) {
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
                        has: function(str1, str2) {
                            if (typeof str1 === 'string') {
                                return str2.toLowerCase().indexOf(str1.toLowerCase()) !== -1;
                            } else {
                                return false;
                            }
                        },
                        lowerize: function(str) {
                            return str.toLowerCase();
                        },
                        major: function(version) {
                            return typeof (version) === STR_TYPE ? version.replace(/[^\d\.]/g, '').split('.')[0] : undefined;
                        },
                        trim: function(str) {
                            return str.replace(/^[\s\uFEFF\xA0]+|[\s\uFEFF\xA0]+$/g, '');
                        },
                    };

                    var mapper = {

                        rgx: function(ua, arrays) {

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

                        str: function(str, map) {

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

                    var UAParser = function(uastring, extensions) {

                        if (typeof uastring === 'object') {
                            extensions = uastring;
                            uastring = undefined;
                        }

                        if (!(this instanceof UAParser)) {
                            return new UAParser(uastring, extensions).getResult();
                        }

                        var ua = uastring || ((window && window.navigator && window.navigator.userAgent) ? window.navigator.userAgent : EMPTY);
                        var rgxmap = extensions ? util.extend(regexes, extensions) : regexes;

                        this.getBrowser = function() {
                            var browser = {name: undefined, version: undefined};
                            mapper.rgx.call(browser, ua, rgxmap.browser);
                            browser.major = util.major(browser.version); 
                            return browser;
                        };
                        this.getCPU = function() {
                            var cpu = {architecture: undefined};
                            mapper.rgx.call(cpu, ua, rgxmap.cpu);
                            return cpu;
                        };
                        this.getDevice = function() {
                            var device = {vendor: undefined, model: undefined, type: undefined};
                            mapper.rgx.call(device, ua, rgxmap.device);
                            return device;
                        };
                        this.getEngine = function() {
                            var engine = {name: undefined, version: undefined};
                            mapper.rgx.call(engine, ua, rgxmap.engine);
                            return engine;
                        };
                        this.getOS = function() {
                            var os = {name: undefined, version: undefined};
                            mapper.rgx.call(os, ua, rgxmap.os);
                            return os;
                        };
                        this.getResult = function() {
                            return {
                                ua: this.getUA(),
                                browser: this.getBrowser(),
                                engine: this.getEngine(),
                                os: this.getOS(),
                                device: this.getDevice(),
                                cpu: this.getCPU(),
                            };
                        };
                        this.getUA = function() {
                            return ua;
                        };
                        this.setUA = function(uastring) {
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
                            define(function() {
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
                        $.ua.get = function() {
                            return parser.getUA();
                        };
                        $.ua.set = function(uastring) {
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
