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

angular.module('rivmUistrijkendarts').factory('verrichtingenfactory', function($resource)
{

	var base = {}, VerrichtingenApi;
	base.getVerrichtingen = getVerrichtingen;
	base.getVerrichtingenCsv = getVerrichtingenCsv;

	VerrichtingenApi = $resource('./api/v1/verrichting/', {}, {
		getAll: {
			method: 'POST',
			url: './api/v1/verrichting/all'
		},
		getCsv: {
			method: 'POST',
			url: './api/v1/verrichting/csv',
			isArray: true
		}
	});

	function getVerrichtingen(verrichting)
	{
		return VerrichtingenApi.getAll({}, verrichting);
	}
	function getVerrichtingenCsv(verrichting)
	{
		return VerrichtingenApi.getCsv({}, verrichting);
	}

	return base;
});
