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

angular.module('rivmUistrijkendarts').controller('labformulierenCtrl', function(OAuth, NgTableParams, userfactory, formulierenfactory, toaster)
{
	var vm, medewerker;
	vm = this;

	this.maxSize = 5;
	this.totalItems = 10;
	this.currentPage = 1;

	this.huisarts = {};
	this.aanvraag = {};

	this.resultOptions = {
		first: 0,
		count: 10,
		sortOptions: {
			aanvraagDatum: 'desc'
		}
	};

	this.totalen = {
		aantalAanvragen: 0
	};

	var $labformulierInput = $("#labformulier-aantal-input");
	$labformulierInput.on('keydown keyup', function(evt)
	{

		if (evt.key === ',' || evt.key === '.' || (/^[a-zA-Z]/.test(evt.key) && evt.key.length === 1)
			|| ($labformulierInput.val().length == 2 && evt.keyCode != 8 && evt.keyCode != 46))
		{
			evt.preventDefault();
		}
	});

	this.opslaan = function(aanvraag)
	{
		formulierenfactory.saveAanvraag(userfactory.getId(), aanvraag).$promise.then(function(data)
		{
			toaster.success("Aanvraag is succesvol verwerkt");
            $('#confirmActiveren').modal('toggle');
			vm.tableParams.reload();
			vm.resetForm();
		}, function(data)
		{
			if (data.status == 400)
			{
				if (data.data[0] != undefined && data.data[0].defaultMessage != undefined)
				{
					toaster.error(data.data[0].defaultMessage);
				}
				else
				{
					toaster.error("Aanvragen van labformulieren niet gelukt.");
				}
			}
			else
			{
				toaster.error("Wegens onbekende oorzaak konden de labformulieren niet worden aangevraagd. Neem contact op met de helpdesk.");
			}
		});
	}

	this.getHuisarts = function()
	{
		userfactory.getHuisarts(userfactory.getId()).$promise.then(function(data)
		{
			vm.huisarts = data;

			for (var i = data.locaties.length - 1; i > -1; i--)
			{
			    if (data.locaties[i].status === "INACTIEF")
			    {
			        data.locaties.splice(i, 1);
                }
            }
			if (data.locaties.length === 1)
			{
				vm.aanvraag.locatie = data.locaties[0];
			}

			vm.tableParams = new NgTableParams({
				page: 1,
				count: 10,
				sorting: {
					aanvraagDatum: 'desc'
				}
			}, {
				counts: [],
				getData: function(params)
				{
					vm.resultOptions = {
						first: (params.page() - 1) * params.count(),
						count: params.count(),
						sortOptions: params.sorting()
					};

				return formulierenfactory.aanvragen(vm.resultOptions).$promise.then(function(data)
				{
					params.total(data.aantalAanvragen); 
						vm.totalen.aantalAanvragen= data.aantalAanvragen;
						return data.aanvragen;
					}, function(data)
					{
						if (data.status == 400)
						{
							if (data.data[0] != undefined && data.data[0].defaultMessage != undefined)
							{
								toaster.error(data.data[0].defaultMessage);
							}
						}
					});
				}
			});
		});
	}

	this.resetForm = function()
	{
		vm.aanvraag = {};
	}

	userfactory.userdata().then(function(data)
	{
		userfactory.setLoginData(data);

		vm.getHuisarts();
	});

});
