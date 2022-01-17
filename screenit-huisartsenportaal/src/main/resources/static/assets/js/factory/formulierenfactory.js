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

angular.module('rivmUistrijkendarts').factory('formulierenfactory', function($resource)
{
	var base, AanvragenApi;

	base = {};
	base.aanvragen = aanvragen;
	base.saveAanvraag = saveAanvraag;
	base.statsLocatie = statsLocatie;

	AanvragenApi = $resource('./api/v1/aanvragen/huisarts/:huisartsId', {
		huisartsId: '@_id'
	}, {
		get: {
			method: 'POST',
			url: './api/v1/aanvragen/huisarts',
		},
		save: {
			method: 'POST'
		},
		stats: {
			method: 'GET',
			url: './api/v1/aanvragen/locatie/:waarde/stats',
			waarde: '@_value'
		}
	});

	function aanvragen(filter)
	{
		return AanvragenApi.get({}, filter);
	}

	function saveAanvraag(id, aanvraag)
	{
		return AanvragenApi.save({
			huisartsId: id
		}, aanvraag);
	}

	function statsLocatie(id)
	{
		return AanvragenApi.stats({
			waarde: id
		})
	}

	return base;
});
