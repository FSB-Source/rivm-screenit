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

angular.module('rivmUistrijkendarts').controller('wachtwoordVergetenRegistrerenCtrl', function($rootScope, $routeParams, userfactory, toaster)
{
	var ctrl, user;

	ctrl = this;
	this.user = {};

	this.login = function(user)
	{

		userfactory.login(user.username, user.password, "wachtwoordvergeten").then(function(data)
		{
			console.log(data);
			toaster.success("U kunt nu een nieuwe wachtwoord aanmaken.");
			ctrl.codeSubmit();
		}, function(data)
		{
			if (data.status = 400)
			{
				toaster.error(data.data.error_description)
			}
			else
			{
				toaster.error("Er is een probleem opgetreden, neem contact op met de helpdesk.")
			}
		})
	}

	this.codeSubmit = function()
	{
		window.location.href = '#/wachtwoordvergeten/voltooien/';
	}

	userfactory.setDisableMenu(false);
	$rootScope.$broadcast('menuChange');
});
