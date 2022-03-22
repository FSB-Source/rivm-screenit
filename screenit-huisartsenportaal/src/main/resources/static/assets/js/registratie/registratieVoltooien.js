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

angular.module('rivmUistrijkendarts').controller('registrerenVoltooienCtrl', function ($scope, $q, $routeParams, $http, $window, $location, $sce, ngTableDefaults, NgTableParams,
																					   userfactory, locatiefactory, toaster, registratiefactory, overeenkomstfactory, formulierenfactory) {
	$scope.showPasswordForFields = new Map();
	$scope.showPassword = function (field) {
		return $scope.showPasswordForFields.has(field) ? $scope.showPasswordForFields.get(field) : false
	}
	$scope.handleTogglePasswordVisible = function (field, mouseLeave) {
		if (!mouseLeave || ($scope.showPasswordForFields.has(field) && $scope.showPasswordForFields.get(field) === true)) {
			$scope.showPasswordForFields.set(field, $scope.showPasswordForFields.has(field) ? !$scope.showPasswordForFields.get(field) : true);
		}
	}

	var ctrl, huisarts, locatie, memoryLocatie, hasLocatie;

	ctrl = this;
	this.huisarts = {};
	this.woonplaatsen = [];
	this.locaties = [];
	this.locatieWoonplaatsen = [];
	this.locatie
	this.locatieZoekObject = {
		status: "ACTIEF",
		resultOptions: {
			first: 0,
			count: 10,
			sortOptions: {}
		}
	}
	this.optionsLocatieStatus = ["ACTIEF",
		"KLANTNUMMER_NIET_GEVERIFIEERD",
		"INACTIEF"];

	this.validationFields = ['email',
		'achternaam',
		'gebruikersnaam',
		'wachtwoord',
		'wachtwoordcontrole',
		'straat',
		'huisnummer',
		'postcode',
		'plaats',
		'overeenkomst'];
	this.validationFieldsLocation = ['locatie',
		'klantnummer',
		'iban',
		'tenaamstelling',
		'locatieAdresStraat',
		'locatieAdresHuisnummer',
		'locatieAdresPostcode',
		'locatieAdresPlaats'];

	userfactory.userdata().then(function () {
		if (registratiefactory.getHuisarts() != undefined) {
			ctrl.huisarts = registratiefactory.getHuisarts();
		} else {
			ctrl.init();
		}

		locatiefactory.all().$promise.then(function (response) {
			var aantalActief = 0;
			response.forEach(function (locatie, index) {
				if (locatie.status !== "INACTIEF") {
					aantalActief++;
				}
			});
			if (aantalActief === 0 && response.length !== 0) {
				ctrl.locatieZoekObject.status = "INACTIEF";
			}
			ctrl.settingUpTable();
		});
	});

	ctrl.getLocatieStatusLabelClass = function (locatieStatus) {
		if (locatieStatus === "ACTIEF") {
			return "label-success";
		} else if (locatieStatus === "KLANTNUMMER_NIET_GEVERIFIEERD") {
			return "label-warning";
		} else {
			return "label-danger";
		}
	};

	ctrl.getLocatieStatusText = function (locatieStatus) {
		if (locatieStatus === "ACTIEF") {
			return "actief";
		} else if (locatieStatus === "KLANTNUMMER_NIET_GEVERIFIEERD") {
			return "verifieren";
		} else {
			return "verwijderd";
		}
	};

	ctrl.getLocatieChoiceStatusLabelClass = function (locatieStatus) {
		if (locatieStatus === "ACTIEF") {
			return "label-success";
		} else if (locatieStatus === "KLANTNUMMER_NIET_GEVERIFIEERD") {
			return "label-warning";
		} else {
			return "label-danger";
		}
	};

	ctrl.getLocatieChoiceStatusText = function (locatieStatus) {
		if (locatieStatus === "ACTIEF") {
			return "actief";
		} else if (locatieStatus === "KLANTNUMMER_NIET_GEVERIFIEERD") {
			return "verifiëren";
		} else {
			return "verwijderd";
		}
	};

	this.settingUpTable = function () {
		ctrl.tableParams = new NgTableParams({
			page: 1,
			count: 10
		}, {
			counts: [],
			getData: function (params) {
				ctrl.locatieZoekObject.resultOptions = {
					first: (params.page() - 1) * params.count(),
					count: params.count(),
					sortOptions: params.sorting()
				};
				return locatiefactory.getLocaties(ctrl.locatieZoekObject).$promise.then(function (data) {
					params.total(data.aantalLocaties);
					return data.locaties;
				}, function data() {
					if (data.status == 400) {
						if (data.data[0] != undefined && data.data[0].defaultMessage != undefined) {
							toaster.error(data.data[0].defaultMessage);
						}
					}
				})
			}
		})
	}

	this.validateFields = function (form, fields) {
		angular.forEach(fields, function (field) {
			form[field].$dirty = true;
		});
	};

	this.resetFormFields = function (form, fields) {
		angular.forEach(fields, function (field) {
			var formField = form[field];
			if (formField) {
				formField.$dirty = false;
			}
		});
	};

	this.init = function () {
		userfactory.getHuisarts(userfactory.getId()).$promise.then(function (data) {
			ctrl.huisarts = data;
		});
		ctrl.hasLocatie = false
	};

	this.controle = function controle(form, huisarts) {
		if (form.$valid) {
			userfactory.controleHuisarts(huisarts).$promise.then(function (data) {
				registratiefactory.setHuisarts(data);
				$location.path('/registreren/controle/');
			}, function (data) {
				if (data.status == 400) {
					if (data.data[0] != undefined && data.data[0].defaultMessage != undefined) {
						toaster.error(data.data[0].defaultMessage);
					} else {
						toaster.error("Wegens onbekende oorzaak kon de huisarts niet worden opgeslagen. Neem contact op met de helpdesk.");
					}
				} else {
					toaster.error("Wegens onbekende oorzaak kon de huisarts niet worden opgeslagen. Neem contact op met de helpdesk.");
				}
			});
		} else {
			ctrl.validateFields(form, validationFields)
		}
	};

	this.addLocatie = function (form) {
		this.resetFormFields(form, ctrl.validationFieldsLocation);
		ctrl.locatie = {
			status: "ACTIEF"
		}
		ctrl.hasLocatie = true
	};

	this.openEditLocatie = function (locatie) {
		if (locatie.status !== "INACTIEF") {
			ctrl.memoryLocatie = locatie
			ctrl.locatie = angular.copy(locatie);
			$('#locatieToevoegen').modal('show');
		} else {
			toaster.error("U kunt alleen actieve locaties aanpassen.");
		}
	};

	this.saveLocatie = function (form, locatie) {
		if (form.$valid) {
			locatie.status = "KLANTNUMMER_NIET_GEVERIFIEERD";
			locatiefactory.save(locatie).$promise
				.then(function (data) {
						angular.copy(locatie, memoryLocatie)
						ctrl.locaties = data;
						ctrl.locatie = undefined;
						ctrl.huisarts.locaties = data;
						ctrl.locatieZoekObject.status = "ACTIEF";
						ctrl.tableParams.reload();
						$('#locatieToevoegen').modal('hide');
						toaster.success("Locatie is succesvol opgeslagen");
					},
					function (data) {
						if (data.status == 400) {
							if (data.data[0] != undefined && data.data[0].defaultMessage != undefined) {
								toaster.error(data.data[0].defaultMessage);
							} else {
								toaster.error("Locatie kon niet worden opgeslagen.");
							}
						} else {
							toaster.error("Wegens onbekende oorzaak kon de locatie niet worden opgeslagen. Neem contact op met de helpdesk.");
						}
					});
		} else {
			this.validateFields(form, validationFieldsLocation)
		}
	};

	this.confirmActiverenLocatie = function (locatie) {
		this.locatie = angular.copy(locatie);
		$('#confirmActiveren').modal('show');
	}

	this.activeerLocatie = function (locatie) {
		$('#confirmActiveren').modal('hide');
		locatie.status = "ACTIEF";
		locatiefactory.put(locatie).$promise.then(function (data) {
			ctrl.locatieZoekObject.status = "ACTIEF";
			ctrl.tableParams.reload();
			ctrl.locatie = undefined;
		}, function (data) {
			if (data.status == 400) {
				if (data.data[0] != undefined && data.data[0].defaultMessage != undefined) {
					toaster.error(data.data[0].defaultMessage);
				} else {
					toaster.error("Locatie kon niet worden geactivereerd.");
				}
			} else {
				toaster.error("Wegens onbekende oorzaak kon de locatie niet worden geactiveerd. Neem contact op met de helpdesk.");
			}
		})
	}

	this.confirmVerwijderenLocatie = function (locatie) {
		this.locatie = angular.copy(locatie);
		formulierenfactory.statsLocatie(locatie.huisartsportaalId).$promise.then(function (data) {
			ctrl.locatieVerwijderenAanvragen = false;
			ctrl.locatieVerwijderenVerzonden = false;
			if (data.aantalNogTeVersturen > 0) {
				ctrl.locatieVerwijderenAanvragen = true;
			}
			if (data.aantalVerstuurd > 0) {
				ctrl.locatieVerwijderenVerzonden = true;
			}
			$('#confirmVerwijderen').modal('show');

		}, function (data) {
			if (data.status == 400) {
				if (data.data[0] != undefined && data.data[0].defaultMessage != undefined) {
					toaster.error(data.data[0].defaultMessage);
				} else {
					toaster.error("Locatie kon niet worden verwijderd.");
				}
			} else {
				toaster.error("Wegens onbekende oorzaak kon de locatie niet worden verwijderd. Neem contact op met de helpdesk.");
			}
		})
	}

	this.deleteLocatie = function (locatie) {
		$('#confirmVerwijderen').modal('hide');
		locatie.status = "INACTIEF";
		locatiefactory.put(locatie).$promise.then(function (data) {
			ctrl.tableParams.reload();
			ctrl.locatie = undefined;
		}, function (data) {
			if (data.status == 400) {
				if (data.data[0] != undefined && data.data[0].defaultMessage != undefined) {
					toaster.error(data.data[0].defaultMessage);
				} else {
					toaster.error("Locatie kon niet worden verwijderd.");
				}
			} else {
				toaster.error("Wegens onbekende oorzaak kon de locatie niet worden verwijderd. Neem contact op met de helpdesk.");
			}
		})
	};

	this.changeLocatie = function (index) {
		ctrl.locatieWoonplaatsen = [];
		ctrl.locatie = ctrl.huisarts.locaties[index];
	};

	this.gelijkCheck = function () {
		if (ctrl.locatie.check) {
			var adres = ctrl.huisarts.postadres;
			ctrl.locatie.locatieAdres = {};
			ctrl.locatie.locatieAdres.straat = adres.straat;
			ctrl.locatie.locatieAdres.huisnummer = adres.huisnummer;
			ctrl.locatie.locatieAdres.huisnummertoevoeging = adres.huisnummertoevoeging;
			ctrl.locatie.locatieAdres.postcode = adres.postcode;
			ctrl.locatie.locatieAdres.woonplaats = adres.woonplaats;
		}
	};

	this.getWoonplaatsen = function (waarde) {
		userfactory.getWoonplaatsen(waarde).$promise.then(function (data) {
			ctrl.woonplaatsen = data;
		}, function (data) {
			console.log(data);
		})
	};

	this.getLocatieWoonplaatsen = function (waarde) {
		userfactory.getWoonplaatsen(waarde).$promise.then(function (data) {
			ctrl.locatieWoonplaatsen = data;
		}, function (data) {
			console.log(data);
		})
	};

	this.getOvereenkomst = function () {
		overeenkomstfactory.openOvereenkomst();
	};

});
