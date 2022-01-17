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

angular.module('rivmUistrijkendarts').controller('betalingenCtrl', function(OAuth, ngTableDefaults, NgTableParams, userfactory, betalingenfactory, toaster)
{
	var vm, medewerker;
	vm = this;

	ngTableDefaults.params.count = 10;

	this.betalingenZoekObject = {};

	this.betaling = {
		betalingenZoekObject: {},
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
		aantalBetalingen: 0,
		totaalBedrag: "€ 0,00"
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
					vm.betalingenZoekObject.locatie = data.locaties[0];
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
					vm.betaling.resultOptions = {
						first: (params.page() - 1) * params.count(),
						count: params.count(),
						sortOptions: params.sorting()
					};

					return betalingenfactory.getBetalingen(vm.betaling).$promise.then(function(data)
					{
						params.total(data.aantalBetalingen); 
						vm.totalen.totaalBedrag = data.totaalBedrag;
						vm.totalen.aantalBetalingen = data.aantalBetalingen;
						return data.betalingen;
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

	this.betalingDatupPopupVanafOpen = false;
	this.betalingDatupPopuptmOpen = false;

	this.open1 = function()
	{
		this.betalingDatumPopupVanafOpen = true;
	};

	this.open2 = function()
	{
		this.betalingDatumPopuptmOpen = true;
	};

	this.dateOptions = {
		dateDisabled: false,
		maxDate: new Date(),
		minDate: new Date(2016, 1, 1),
		startingDay: 1
	};

	this.zoeken = function(betalingenZoekObject, betalingform)
	{
		if (betalingform.$valid)
		{
			vm.betaling.betalingenZoekObject = $.extend(true, {}, betalingenZoekObject);
			vm.betaling.resultOptions.first = 0;
			vm.betaling.resultOptions.count = 10;
			vm.tableParams.reload();
			vm.tableParams.page(1);
		}
	};

	this.csvHeader = [	"Huisartslocatie",
						"Naam cliënt",
						"Monster-id",
						"Bedrag",
						"Datum betaling",
						"Betalingskenmerk",
						"Screeningsorganisatie"];

	this.getCsvExportData = function()
	{
		var betalingenZoekObject = vm.betaling;
		betalingenZoekObject.resultOptions.first = 0;
		betalingenZoekObject.resultOptions.count = -1;
		return betalingenfactory.getBetalingenCsv(betalingenZoekObject).$promise.then(function(data)
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
