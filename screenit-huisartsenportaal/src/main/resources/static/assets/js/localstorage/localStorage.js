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

angular.module('rivmUistrijkendarts').controller('storageCtrl', function($rootScope, userfactory)
{
	var ctrl
	ctrl = this;
	ctrl.browser = get_browser();

	function get_browser()
	{
		var ua = navigator.userAgent, tem, M = ua.match(/(opera|chrome|safari|firefox|msie|trident(?=\/))\/?\s*(\d+)/i) || [];
		if (/trident/i.test(M[1]))
		{
			tem = /\brv[ :]+(\d+)/g.exec(ua) || [];
			return {
				name: 'IE',
				version: (tem[1] || '')
			};
		}
		if (M[1] === 'Chrome')
		{
			tem = ua.match(/\bOPR\/(\d+)/)
			if (tem != null)
			{
				return {
					name: 'Opera',
					version: tem[1]
				};
			}
		}
		M = M[2] ? [M[1],
					M[2]] : [	navigator.appName,
								navigator.appVersion,
								'-?'];
		if ((tem = ua.match(/version\/(\d+)/i)) != null)
		{
			M.splice(1, 1, tem[1]);
		}
		return {
			name: M[0],
			version: M[1]
		};
	}

	userfactory.setDisableMenu(true);
	$rootScope.$broadcast('menuChange');

});
