/*-
 * ========================LICENSE_START=================================
 * screenit-huisartsenportaal
 * %%
 * Copyright (C) 2016 - 2022 Facilitaire Samenwerking Bevolkingsonderzoek
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

(function(root, factory)
{
	if (typeof define === "function" && define.amd)
	{
		define(["angular",
				"angular-cookies",
				"query-string"], factory);
	}
	else if (typeof exports === "object")
	{
		module.exports = factory(require("angular"), require("angular-cookies"), require("query-string"));
	}
	else
	{
		root.angularOAuth2 = factory(root.angular, "ngCookies", root.queryString);
	}
})(this, function(angular, ngCookies, queryString)
{
	var ngModule = angular.module("angular-oauth2", [ngCookies]).config(oauthConfig).factory("oauthInterceptor", oauthInterceptor).provider("OAuth", OAuthProvider)
	.provider("OAuthToken", OAuthTokenProvider);
	function oauthConfig($httpProvider)
	{
		$httpProvider.interceptors.push("oauthInterceptor");
	}
	oauthConfig.$inject = ["$httpProvider"];
	function oauthInterceptor($q, $rootScope, OAuthToken)
	{
		return {
			request: function request(config)
			{
				config.headers = config.headers || {};
				if (!config.headers.hasOwnProperty("Authorization") && OAuthToken.getAuthorizationHeader())
				{
					config.headers.Authorization = OAuthToken.getAuthorizationHeader();
				}
				return config;
			},
			responseError: function responseError(rejection)
			{
				if (400 === rejection.status && rejection.data && ("invalid_request" === rejection.data.error || "invalid_grant" === rejection.data.error))
				{
					OAuthToken.removeToken();
					$rootScope.$emit("oauth:error", rejection);
				}
				if (401 === rejection.status && rejection.data && "invalid_token" === rejection.data.error || rejection.headers("www-authenticate")
					&& 0 === rejection.headers("www-authenticate").indexOf("Bearer"))
				{
					$rootScope.$emit("oauth:error", rejection);
				}
				return $q.reject(rejection);
			}
		};
	}
	oauthInterceptor.$inject = ["$q",
								"$rootScope",
								"OAuthToken"];
	var _createClass = function()
	{
		function defineProperties(target, props)
		{
			for (var i = 0; i < props.length; i++)
			{
				var descriptor = props[i];
				descriptor.enumerable = descriptor.enumerable || false;
				descriptor.configurable = true;
				if ("value" in descriptor)
					descriptor.writable = true;
				Object.defineProperty(target, descriptor.key, descriptor);
			}
		}
		return function(Constructor, protoProps, staticProps)
		{
			if (protoProps)
				defineProperties(Constructor.prototype, protoProps);
			if (staticProps)
				defineProperties(Constructor, staticProps);
			return Constructor;
		};
	}();
	function _classCallCheck(instance, Constructor)
	{
		if (!(instance instanceof Constructor))
		{
			throw new TypeError("Cannot call a class as a function");
		}
	}
	var defaults = {
		baseUrl: null,
		clientId: null,
		clientSecret: null,
		grantPath: "/oauth2/token",
		revokePath: "/oauth2/revoke"
	};
	var requiredKeys = ["baseUrl",
						"clientId",
						"grantPath",
						"revokePath"];
	function OAuthProvider()
	{
		var config;
		this.configure = function(params)
		{
			if (config)
			{
				throw new Error("Already configured.");
			}
			if (!(params instanceof Object))
			{
				throw new TypeError("Invalid argument: `config` must be an `Object`.");
			}
			config = angular.extend({}, defaults, params);
			angular.forEach(requiredKeys, function(key)
			{
				if (!config[key])
				{
					throw new Error("Missing parameter: " + key + ".");
				}
			});
			if ("/" === config.baseUrl.substr(-1))
			{
				config.baseUrl = config.baseUrl.slice(0, -1);
			}
			if ("/" !== config.grantPath[0])
			{
				config.grantPath = "/" + config.grantPath;
			}
			if ("/" !== config.revokePath[0])
			{
				config.revokePath = "/" + config.revokePath;
			}
			return config;
		};
		this.$get = function($http, OAuthToken)
		{
			var OAuth = function()
			{
				function OAuth()
				{
					_classCallCheck(this, OAuth);
					if (!config)
					{
						throw new Error("`OAuthProvider` must be configured first.");
					}
				}
				_createClass(OAuth, [	{
											key: "isAuthenticated",
											value: function isAuthenticated()
											{
												return !!OAuthToken.getToken();
											}
										},
										{
											key: "getAccessToken",
											value: function getAccessToken(data, options)
											{
												data = angular.extend({
													client_id: config.clientId,
													grant_type: "password"
												}, data);
												if (null !== config.clientSecret)
												{
													data.client_secret = config.clientSecret;
												}
												data = queryString.stringify(data);
												options = angular.extend({
													headers: {
														Authorization: undefined,
														"Content-Type": "application/x-www-form-urlencoded"
													}
												}, options);
												return $http.post("" + config.baseUrl + config.grantPath, data, options).then(function(response)
												{
													OAuthToken.setToken(response.data);
													return response;
												});
											}
										},
										{
											key: "getRefreshToken",
											value: function getRefreshToken(data, options)
											{
												data = angular.extend({
													client_id: config.clientId,
													grant_type: "refresh_token",
													refresh_token: OAuthToken.getRefreshToken()
												}, data);
												if (null !== config.clientSecret)
												{
													data.client_secret = config.clientSecret;
												}
												data = queryString.stringify(data);
												options = angular.extend({
													headers: {
														Authorization: undefined,
														"Content-Type": "application/x-www-form-urlencoded"
													}
												}, options);
												return $http.post("" + config.baseUrl + config.grantPath, data, options).then(function(response)
												{
													OAuthToken.setToken(response.data);
													return response;
												});
											}
										},
										{
											key: "revokeToken",
											value: function revokeToken(data, options)
											{
												var refreshToken = OAuthToken.getRefreshToken();
												data = angular.extend({
													client_id: config.clientId,
													token: refreshToken ? refreshToken : OAuthToken.getAccessToken(),
													token_type_hint: refreshToken ? "refresh_token" : "access_token"
												}, data);
												if (null !== config.clientSecret)
												{
													data.client_secret = config.clientSecret;
												}
												data = queryString.stringify(data);
												options = angular.extend({
													headers: {
														"Content-Type": "application/x-www-form-urlencoded"
													}
												}, options);
												return $http.post("" + config.baseUrl + config.revokePath, data, options).then(function(response)
												{
													OAuthToken.removeToken();
													return response;
												});
											}
										}]);
				return OAuth;
			}();
			return new OAuth();
		};
		this.$get.$inject = [	"$http",
								"OAuthToken"];
	}
	var _createClass = function()
	{
		function defineProperties(target, props)
		{
			for (var i = 0; i < props.length; i++)
			{
				var descriptor = props[i];
				descriptor.enumerable = descriptor.enumerable || false;
				descriptor.configurable = true;
				if ("value" in descriptor)
					descriptor.writable = true;
				Object.defineProperty(target, descriptor.key, descriptor);
			}
		}
		return function(Constructor, protoProps, staticProps)
		{
			if (protoProps)
				defineProperties(Constructor.prototype, protoProps);
			if (staticProps)
				defineProperties(Constructor, staticProps);
			return Constructor;
		};
	}();
	function _classCallCheck(instance, Constructor)
	{
		if (!(instance instanceof Constructor))
		{
			throw new TypeError("Cannot call a class as a function");
		}
	}
	function OAuthTokenProvider()
	{
		var config = {
			name: "token",
			options: {
				secure: true
			}
		};
		this.configure = function(params)
		{
			if (!(params instanceof Object))
			{
				throw new TypeError("Invalid argument: `config` must be an `Object`.");
			}
			angular.extend(config, params);
			return config;
		};
		this.$get = function($cookies)
		{
			var OAuthToken = function()
			{
				function OAuthToken()
				{
					_classCallCheck(this, OAuthToken);
				}
				_createClass(OAuthToken, [	{
												key: "setToken",
												value: function setToken(data)
												{
													if (hasStorage)
													{
														return sessionStorage.setItem(config.name, JSON.stringify(data));
													}
												}
											},
											{
												key: "getToken",
												value: function getToken()
												{
													if (hasStorage)
													{
														return $.parseJSON(sessionStorage.getItem(config.name));
													}
												}
											},
											{
												key: "getAccessToken",
												value: function getAccessToken()
												{
													return this.getToken() ? this.getToken().access_token : undefined;
												}
											},
											{
												key: "getAuthorizationHeader",
												value: function getAuthorizationHeader()
												{
													if (!(this.getTokenType() && this.getAccessToken()))
													{
														return;
													}
													return this.getTokenType().charAt(0).toUpperCase() + this.getTokenType().substr(1) + " " + this.getAccessToken();
												}
											},
											{
												key: "getRefreshToken",
												value: function getRefreshToken()
												{
													return this.getToken() ? this.getToken().refresh_token : undefined;
												}
											},
											{
												key: "getTokenType",
												value: function getTokenType()
												{
													return this.getToken() ? this.getToken().token_type : undefined;
												}
											},
											{
												key: "removeToken",
												value: function removeToken()
												{
													if (hasStorage)
													{
														return sessionStorage.removeItem(config.name);
													}
													else
													{
														return $cookies.remove(config.name, config.options);
													}
												}
											}]);
				return OAuthToken;
			}();
			return new OAuthToken();
		};
		this.$get.$inject = ["$cookies"];
	}

	var hasStorage = (function()
	{
		var mod = "test";
		try
		{
			localStorage.setItem(mod, mod);
			localStorage.removeItem(mod);
			return true;
		}
		catch (exception)
		{
			return false;
		}
	}());
	return ngModule;
});
