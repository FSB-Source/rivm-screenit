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

angular.module('rivmUistrijkendarts').controller('wachtwoordCtrl', function($rootScope, userfactory, $location, toaster)
{
	this.user = {};

	this.vergeten = function(user)
	{

		userfactory.wachtwoordVergeten(user).$promise.then(function(data)
		{
			toaster.success("Uw wachtwoord is succesvol gereset.");
			$location.path("/")
		}, function(data)
		{
			if (data.status == 400)
			{
				toaster.error(data.data[0].defaultMessage);
			}
		});
	}

	userfactory.setDisableMenu(false);
	$rootScope.$broadcast('menuChange');
});
