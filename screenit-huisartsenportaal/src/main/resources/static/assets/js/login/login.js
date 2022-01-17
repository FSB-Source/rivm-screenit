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

angular.module('rivmUistrijkendarts').controller('loginCtrl', function($rootScope, userfactory, toaster)
{
	this.user = {};

	this.login = function(user)
	{
		userfactory.login(user.username, user.password, "login").then(function(data)
		{
			console.log(data);
			userfactory.userdata().then(function(data2)
			{
				if (!userfactory.heeftRecht("ROLE_AANVRAGEN") && !userfactory.heeftRecht("ROLE_OVEREENKOMST"))
				{
					toaster.error("U moet zich eerst registreren om te kunnen inloggen.");
					userfactory.logout();
				}
				else
				{
					toaster.success("Login succesvol!");
					userfactory.afterLogin();
				}
			}, function(data2)
			{
				console.log(data2);
				toaster.error("Er is een onbekende fout opgetreden neem contact op met de helpdesk.")
			});

		}, function(data)
		{
			if (data.status == 400 && data.data.error_description !== "Bad credentials")
			{
				toaster.error(data.data.error_description)
			}
			else if (data.status == 400 && data.data.error_description === "Bad credentials")
			{
				toaster.error("Er is op dit moment geen account gevonden met deze informatie. Probeer het opnieuw.")
			}
			else
			{
				toaster.error("Er is een onbekende fout opgetreden neem contact op met de helpdesk.");
			}
		});
	}

	userfactory.setDisableMenu(false);
	$rootScope.$broadcast('menuChange');

});
