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

angular.module('rivmUistrijkendarts')
.controller('wijzigenGegevensCtrl',
			function(	$http,
						$routeParams,
						$rootScope,
						$location,
						NgTableParams,
						userfactory,
						locatiefactory,
						toaster,
						registratiefactory,
						overeenkomstfactory,
						formulierenfactory,
						verificatiefactory)
			{
				var ctrl, wijzigGegevens, huisarts, locatie, memoryLocatie, locaties, wachtwoord;

				ctrl = this;
				this.wijzigGegevens = $location.path() === "/inner/gegevens/";

				ctrl.wachtwoord = {};
				this.woonplaatsen = [];
				this.locatieWoonplaatsen = [];
				this.locatieVerwijderenAanvragen = false;
				this.locatieVerwijderenVerzonden = false;
				this.locatieZoekObject = {
					status: "ACTIEF",
					resultOptions: {
						first: 0,
						count: 10,
						sortOptions: {}
					}
				};
				this.optionsLocatieStatus = [	"ACTIEF",
												"KLANTNUMMER_NIET_GEVERIFIEERD",
												"INACTIEF"];

				var validationFields = ['email',
										'achternaam',
										'straat',
										'huisnummer',
										'postcode',
										'plaats',
										'overeenkomst'];
				var validationFieldsLocation = ['locatie',
												'klantnummer',
												'iban',
												'tenaamstelling',
												'locatieAdresStraat',
												'locatieAdresHuisnummer',
												'locatieAdresPostcode',
												'locatieAdresPlaats'];

				this.validateFields = function(form, fields)
				{
					angular.forEach(fields, function(field)
					{
						form[field].$dirty = true;
					});
				};

				ctrl.resetTable = function()
                {
                    ctrl.locatieZoekObject.resultOptions.first = 0;
				    ctrl.tableParams.reload();
                    ctrl.tableParams.page(1);
                };

				userfactory.userdata().then(function()
				{
					ctrl.init();
				});

				ctrl.toonHerzendKnop = function()
				{
					if (ctrl.locatie && ctrl.locatie.status === "KLANTNUMMER_NIET_GEVERIFIEERD")
					{
						return true;
					}
					return false;
				};

				ctrl.getLocatieStatusLabelClass = function(locatieStatus)
				{
					if (locatieStatus === "ACTIEF")
					{
						return "label-success";
					}
					else if (locatieStatus === "KLANTNUMMER_NIET_GEVERIFIEERD")
					{
						return "label-warning";
					}
					else
					{
						return "label-danger";
					}
				};

				ctrl.getLocatieStatusText = function(locatieStatus)
				{
					if (locatieStatus === "ACTIEF")
					{
						return "actief";
					}
					else if (locatieStatus === "KLANTNUMMER_NIET_GEVERIFIEERD")
					{
						return "verifieren";
					}
					else
					{
						return "verwijderd";
					}
				};

				ctrl.getLocatieChoiceStatusLabelClass = function(locatieStatus)
				{
					if (locatieStatus === "ACTIEF")
					{
						return "label-success";
					}
					else if (locatieStatus === "KLANTNUMMER_NIET_GEVERIFIEERD")
					{
						return "label-warning";
					}
					else
					{
						return "label-danger";
					}
				};

				ctrl.getLocatieChoiceStatusText = function(locatieStatus)
				{
					if (locatieStatus === "ACTIEF")
					{
						return "actief";
					}
					else if (locatieStatus === "KLANTNUMMER_NIET_GEVERIFIEERD")
					{
						return "verifiëren";
					}
					else
					{
						return "verwijderd";
					}
				};

				ctrl.locatiePopupLabel = "Locatie wijzigen";

				this.init = function()
				{
					userfactory.getHuisarts(userfactory.getId()).$promise.then(function(data)
					{
						ctrl.huisarts = data;
						if (ctrl.wijzigGegevens)
						{
							ctrl.huisarts.overeenkomst = true;
						}
					});
					ctrl.tableParams = new NgTableParams({
						page: 1,
						count: 10
					}, {
						counts: [],
						getData: function(params)
						{
							ctrl.locatieZoekObject.resultOptions = {
								first: (params.page() - 1) * params.count(),
								count: params.count(),
								sortOptions: params.sorting()
							};
							return locatiefactory.getLocaties(ctrl.locatieZoekObject).$promise.then(function(data)
							{
								params.total(data.aantalLocaties);
								return data.locaties;
							}, function data()
							{
								if (data.status == 400)
								{
									if (data.data[0] != undefined && data.data[0].defaultMessage != undefined)
									{
										toaster.error(data.data[0].defaultMessage);
									}
								}
							})
						}
					})

				};

				this.addLocatie = function()
				{
					ctrl.locatie = {
						status: null
					}
                    ctrl.locatiePopupLabel = "Locatie toevoegen";
				};

				function clearLocatie()
				{
					ctrl.locatie = {
						zorgmailklantnummer: null,
						iban: null,
						ibanTenaamstelling: null,
						locatieAdres: {
							straat: null,
							huisnummer: null,
							huisnummertoevoeging: null,
							postcode: null,
							woonplaats: null
						}
					}
				}

				this.resetFieldsLocatie = function(form, locatie)
				{
					form.$setPristine();
					$rootScope.$broadcast("resetErrors");
					clearLocatie();
				};

				this.openEditLocatie = function(form, locatie)
				{
				    ctrl.locatiePopupLabel = "Locatie wijzigen";
					if (locatie.status !== "INACTIEF")
					{
						form.$setPristine();
						ctrl.memoryLocatie = locatie;
						ctrl.locatie = angular.copy(locatie);
						$('#locatieToevoegen').modal('show');
					}
					else
					{
						toaster.error("U kunt alleen actieve locaties aanpassen. Maak deze locatie actief en pas de informatie over deze locatie aan.");
					}
				};

				this.saveLocatie = function(form, locatie)
				{
					if (form.$valid)
					{
						locatie.status = "ACTIEF";
						locatiefactory.save(locatie).$promise.then(function(data)
						{
							angular.copy(locatie, locatie.memoryLocatie)
							ctrl.locaties = data;
							ctrl.huisarts.locaties = data;
							ctrl.locatieZoekObject.status = "ACTIEF";
							var klantnrIsDirty = form.klantnummer.$dirty;
							form.$setPristine();
							clearLocatie();
							ctrl.tableParams.reload();
							$('#locatieToevoegen').modal('hide');
							if (klantnrIsDirty)
							{
								verificatiefactory.resetMelding();
								toaster.warning("Om uw Zorgmail klantnummer te bevestigen is een bericht met een verificatiecode naar uw zorgmailadres gestuurd. Volg de instructies in de mail om uw Zorgmail klantnummer te bevestigen.", "", {
									timeOut: 0
								});
							}
							else
							{
								toaster.success("Locatie is succesvol opgeslagen");
							}
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
									toaster.error("Locatie kon niet worden opgeslagen.");
								}
							}
							else
							{
								toaster.error("Wegens onbekende oorzaak kon de locatie niet worden opgeslagen. Neem contact op met de helpdesk.");
							}
						});
					}
				};

				this.confirmActiverenLocatie = function(locatie)
				{
					this.locatie = angular.copy(locatie);
					$('#confirmActiveren').modal('toggle');
				}

				this.activeerLocatie = function(locatie)
				{
					$('#confirmActiveren').modal('hide');
					locatie.status = "ACTIEF";
					locatiefactory.put(locatie).$promise.then(function(data)
					{
						ctrl.locatieZoekObject.status = "ACTIEF";
						ctrl.tableParams.reload();
						ctrl.locatie = undefined;
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
								toaster.error("Locatie kon niet worden geactivereerd.");
							}
						}
						else
						{
							toaster.error("Wegens onbekende oorzaak kon de locatie niet worden geactiveerd. Neem contact op met de helpdesk.");
						}
					})
				}

				this.confirmVerwijderenLocatie = function(locatie)
				{
					this.locatie = angular.copy(locatie);
					formulierenfactory.statsLocatie(locatie.huisartsportaalId).$promise.then(function(data)
					{
						ctrl.locatieVerwijderenAanvragen = false;
						ctrl.locatieVerwijderenVerzonden = false;
						if (data.aantalNogTeVersturen > 0)
						{
							ctrl.locatieVerwijderenAanvragen = true;
						}
						if (data.aantalVerstuurd > 0)
						{
							ctrl.locatieVerwijderenVerzonden = true;
						}
						$('#confirmVerwijderen').modal('toggle');

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
								toaster.error("Locatie kon niet worden verwijderd.");
							}
						}
						else
						{
							toaster.error("Wegens onbekende oorzaak kon de locatie niet worden verwijderd. Neem contact op met de helpdesk.");
						}
					})
				}

				this.deleteLocatie = function(locatie)
				{
					$('#confirmVerwijderen').modal('hide');
					locatie.status = "INACTIEF";
					locatiefactory.put(locatie).$promise.then(function(data)
					{
						ctrl.tableParams.reload();
						ctrl.locatie = undefined;
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
								toaster.error("Locatie kon niet worden verwijderd.");
							}
						}
						else
						{
							toaster.error("Wegens onbekende oorzaak kon de locatie niet worden verwijderd. Neem contact op met de helpdesk.");
						}
					})
				};

				this.changeLocatie = function(index)
				{
					ctrl.locatieWoonplaatsen = [];
					ctrl.locatie = ctrl.huisarts.locaties[index];
				}

				this.opslaan = function opslaan(form, huisarts)
				{
					if (form.$valid)
					{
						userfactory.saveHuisarts(huisarts).$promise.then(function(data)
						{
							toaster.success("Huisarts succesvol opgeslagen!")
							if (!ctrl.wijzigGegevens)
							{
								$location.path("/inner/");
							}
							else
							{
								ctrl.huisarts = data;
								ctrl.huisarts.overeenkomst = true;
							}
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
									toaster.error("Opslaan huisarts niet gelukt!");
								}
							}
							else
							{
								toaster.error("Wegens onbekende oorzaak kon de huisarts niet worden opgeslagen. Neem contact op met de helpdesk.");
							}
						})
					}
					else
					{
						this.validateFields(form, validationFields)
					}
				}

				this.verwijderen = function(index)
				{
					ctrl.huisarts.locaties.splice(index, 1);
				}

				this.gelijkCheck = function()
				{
					if (ctrl.locatie.check)
					{
						var adres = ctrl.huisarts.postadres;
						ctrl.locatie.locatieAdres = {};
						ctrl.locatie.locatieAdres.straat = adres.straat;
						ctrl.locatie.locatieAdres.huisnummer = adres.huisnummer;
						ctrl.locatie.locatieAdres.huisnummertoevoeging = adres.huisnummertoevoeging;
						ctrl.locatie.locatieAdres.postcode = adres.postcode;
						ctrl.locatie.locatieAdres.woonplaats = adres.woonplaats;
					}
				}

				this.getWoonplaatsen = function(waarde)
				{
					userfactory.getWoonplaatsen(waarde).$promise.then(function(data)
					{
						ctrl.woonplaatsen = data;
					}, function(data)
					{
						console.log(data);
					})
				}

				this.getLocatieWoonplaatsen = function(waarde)
				{
					userfactory.getWoonplaatsen(waarde).$promise.then(function(data)
					{
						ctrl.locatieWoonplaatsen = data;
					}, function(data)
					{
						console.log(data);
					})
				}

				this.getOvereenkomst = function()
				{
					overeenkomstfactory.openOvereenkomst();
				}

				function clearWachtwoord()
				{
					ctrl.wachtwoord = {
						oudeWachtwoord: null,
						nieuweWachtwoord: null,
						nieuweWachtwoordControle: null
					};
				}

				this.resetFieldsWijzigWachtwoord = function(form)
				{
					form.$setPristine();
					$rootScope.$broadcast("resetErrors");
					clearWachtwoord();
				};

				this.wijzigWachtwoord = function(form, wachtwoord)
				{
					if (wachtwoord === undefined)
					{
						toaster.error("Er zijn geen wachtwoorden ingevoerd.");
					}
					else
					{
						if (form.$valid)
						{
							userfactory.wachtwoordWijzigen(wachtwoord).$promise.then(function(data)
							{
								$('#wachtwoordwijzigen').modal('hide');
								toaster.success("Wachtwoord succesvol gewijzigd.");
								clearWachtwoord();

							}, function(data)
							{
								if (data.status == 400)
								{
									toaster.error(data.data[0].defaultMessage);
								}
							});
						}
					}
				}

			});
