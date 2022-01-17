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
'use strict';

angular.module('rivmUistrijkendarts').factory('locatiefactory', function(OAuth, OAuthToken, $resource)
{
	var base, locatieApi;

	base = {};
	base.all = all;
	base.save = saveLocatie;
	base.put = putLocatie;
	base.getLocaties = getLocaties;

	locatieApi = $resource('./api/v1/locatie', {}, {
		getLocatie: {
			method: 'GET',
			isArray: true
		},
		saveLocatie: {
			method: 'POST',
			isArray: true
		},
		putLocatie: {
			method: 'PUT'
		},
		getLocaties: {
			url: './api/v1/locaties',
			method: 'POST'
		}
	});

	function all()
	{
		return locatieApi.getLocatie();
	}

	function saveLocatie(locatie)
	{
		return locatieApi.saveLocatie({}, locatie);
	}

	function putLocatie(locatie)
	{
		return locatieApi.putLocatie({}, locatie);
	}

	function getLocaties(locatieZoekObject)
	{
		return locatieApi.getLocaties({}, locatieZoekObject)
	}

	return base;
});
