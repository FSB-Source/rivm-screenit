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

angular.module('rivmUistrijkendarts').controller('verrichtingenCtrl', function(OAuth, ngTableDefaults, NgTableParams, userfactory, verrichtingenfactory, toaster)
{
	var vm, medewerker;
	vm = this;

	ngTableDefaults.params.count = 10;

	this.verrichtingenZoekObject = {};

	this.verrichting = {
		verrichtingenZoekObject: {},
		resultOptions: {
			first: 0,
			count: 10,
			sortOptions: {
				verrichtingsDatum: 'desc'
			}
		}
	};
	this.huisarts = {};
	this.totalen = {
		aantalVerrichtingen: 0
	};

	init();

	function init()
	{
		userfactory.userdata().then(function(data)
		{
			userfactory.setLoginData(data);
			userfactory.getHuisarts(userfactory.getId()).$promise.then(function(data)
			{
				vm.huisarts = data;
				if (data.locaties.length === 1)
				{
					vm.verrichtingenZoekObject.locatie = data.locaties[0];
				}
			});
			vm.tableParams = new NgTableParams({
				page: 1,
				count: 10,
				sorting: {
					verrichtingsDatum: 'desc'
				}
			}, {
				counts: [],
				getData: function(params)
				{
					vm.verrichting.resultOptions = {
						first: (params.page() - 1) * params.count(),
						count: params.count(),
						sortOptions: params.sorting()
					};

					return verrichtingenfactory.getVerrichtingen(vm.verrichting).$promise.then(function(data)
					{
						params.total(data.aantalVerrichtingen); 
						vm.totalen.aantalVerrichtingen = data.aantalVerrichtingen;
						return data.verrichtingen;
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

	this.dateformat = 'd!-M!-yyyy';

	this.verrichtingDatupPopupVanafOpen = false;
	this.verrichtingDatupPopuptmOpen = false;

	this.open1 = function()
	{
		this.verrichtingDatumPopupVanafOpen = true;
	};

	this.open2 = function()
	{
		this.verrichtingDatumPopuptmOpen = true;
	};

	this.open3 = function()
	{
		this.datumUitstrijkjePopupOpen = true;
	};

	this.dateOptions = {
		dateDisabled: false,
		maxDate: new Date(),
		minDate: new Date(2016, 1, 1),
		startingDay: 1
	};

	this.zoeken = function(verrichtingenZoekObject, verrichtingform)
	{
		if (verrichtingform.$valid)
		{
			vm.verrichting.verrichtingenZoekObject = $.extend(true, {}, verrichtingenZoekObject);
			vm.verrichting.resultOptions.first = 0;
			vm.verrichting.resultOptions.count = 10;
			vm.tableParams.reload();
			vm.tableParams.page(1);
		}
	};

	this.csvHeader = [	"Huisartslocatie",
						"Naam cliënt",
						"Monster-id",
						"Datum uitstrijkje",
						"Datum verrichting",
						"Ontvangst formulier",
						"Screeningsorganisatie"];

	this.getCsvExportData = function()
	{
		var verrichtingenZoekObject = vm.verrichting;
		verrichtingenZoekObject.resultOptions.first = 0;
		verrichtingenZoekObject.resultOptions.count = -1;
		return verrichtingenfactory.getVerrichtingenCsv(verrichtingenZoekObject).$promise.then(function(data)
		{
			return data;
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
	};

});
