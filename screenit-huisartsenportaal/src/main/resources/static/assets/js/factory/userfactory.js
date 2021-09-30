/*-
 * ========================LICENSE_START=================================
 * screenit-huisartsenportaal
 * %%
 * Copyright (C) 2016 - 2021 Facilitaire Samenwerking Bevolkingsonderzoek
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
'use strict';

angular.module('rivmUistrijkendarts').factory('userfactory', function(OAuth, OAuthToken, $resource, $location, authService, $rootScope, verificatiefactory)
{
	var base, UserApi, loginData, disableMenu, userdatapromise;

	disableMenu = false;

	base = {};
	base.login = login;
	base.clearUserData = clearUserData;
	base.logout = logout;
	base.userdata = getInlogData;
	base.userdatapromise = userdatapromise;
	base.afterLogin = afterLogin;
	base.getId = getId;
	base.currentUser = currentUser;
	base.isIngelogd = isIngelogd;
	base.getInlogData = getInlogData;
	base.getHuisarts = getHuisarts;
	base.controleHuisarts = controleHuisarts;
	base.saveHuisarts = saveHuisarts;
	base.heeftRecht = heeftRecht;
	base.wachtwoordVergeten = wachtwoordVergeten;
	base.wachtwoordWijzigen = wachtwoordWijzigen;
	base.setLoginData = setLoginData;
	base.getWoonplaatsen = getWoonplaatsen;
	base.getDisableMenu = getDisableMenu;
	base.setDisableMenu = setDisableMenu;

	UserApi = $resource('./api/v1/huisarts', {
		huisartsId: '@_id'
	}, {
		get: {
			method: 'GET'
		},
		put: {
			method: 'PUT'
		},
		putControle: {
			method: 'PUT',
			url: './api/v1/huisarts/controle'
		},
		getCurrentUser: {
			method: 'GET',
			url: './api/v1/huisarts/currentuser'
		},
		postWachtwoordVergeten: {
			method: 'POST',
			url: './api/v1/wachtwoord/vergeten'
		},
		postWachtwoordWijzigen: {
			method: 'POST',
			url: './api/v1/wachtwoord/wijzigen'
		},
		getWoonplaatsen: {
			method: 'GET',
			isArray: true,
			url: './api/v1/woonplaatsen/:waarde',
			waarde: '@_value'
		}
	});

	function getHuisarts(id)
	{
		return UserApi.get();
	}

	function setDisableMenu(waarde)
	{
		disableMenu = waarde;
	}

	function getDisableMenu()
	{
		return disableMenu;
	}

	function getWoonplaatsen(string)
	{
		return UserApi.getWoonplaatsen({
			waarde: string
		})
	}

	function setLoginData(data)
	{
		loginData = data;
	}

	function wachtwoordVergeten(dto)
	{
		return UserApi.postWachtwoordVergeten({}, dto);
	}

	function wachtwoordWijzigen(wachtwoord)
	{
		return UserApi.postWachtwoordWijzigen({}, wachtwoord);
	}

	function saveHuisarts(huisartsData)
	{
		return UserApi.put(huisartsData);
	}

	function controleHuisarts(huisartsData)
	{
		return UserApi.putControle(huisartsData);
	}

	function currentUser()
	{
		return UserApi.getCurrentUser();
	}

	function isIngelogd()
	{
		return OAuth.isAuthenticated();
	}

	function getId()
	{
		return loginData.id;
	}

	function getInlogData(ignoreNieuweOvereenkomst)
	{

		var promise = currentUser().$promise;
        verificatiefactory.resetMelding();
		promise.then(function(data)
		{
			setLoginData(data);
			console.log("Rollen zijn gezet: " + loginData.rollen)
			$rootScope.$broadcast('userChange');
			if (!data.overeenkomstGetekend && (!ignoreNieuweOvereenkomst || !heeftRecht("ROLE_REGISTEREN")))
			{
				$location.path('/inner/nieuweovereenkomst/');
			}
		}, function()
		{
            verificatiefactory.resetMelding();
			logout();
		});

		userdatapromise = promise;

		return promise;
	}

	function login(username, password, scope)
	{
		var promise;

		var data = {
			username: username,
			password: password,
			scope: scope,
		};

		var config = {
			headers: {
				'Content-Type': 'application/x-www-form-urlencoded',
				'Accept': 'application/json'
			}
		};

		promise = OAuth.getAccessToken(data);

		promise.then(function(data)
		{
			var token = data.data.access_token;
			huisartsportaal.token = token.substr(token.length - 8);
		})

		return promise;
	}

	function heeftRecht(recht)
	{
		if (loginData == undefined || !isIngelogd())
		{
			return false;
		}
		return $.inArray(recht, loginData.rollen) >= 0
	}

	function afterLogin(data)
	{
		window.location.href = '#/inner/';
	}

	function logout()
	{
		loginData = {};
		var promise = OAuth.revokeToken();

		promise.then(function()
		{
			setLoginData(undefined);
			$rootScope.$broadcast('userChange');
			$location.path('/');
			huisartsportaal.token = undefined;
		});

		return promise;
	}

    function clearUserData()
    {
        loginData = {};
        var promise = OAuth.revokeToken();

        promise.then(function()
        {
            setLoginData(undefined);
            $rootScope.$broadcast('userChange');
            huisartsportaal.token = undefined;
        });

        return promise;
    }

	if (isIngelogd() && loginData == undefined)
	{
		getInlogData();
	}

	return base;
});
