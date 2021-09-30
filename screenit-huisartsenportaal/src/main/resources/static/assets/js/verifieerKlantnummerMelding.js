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

angular.module('rivmUistrijkendarts').controller('verifieerMeldingCtrl',
function($location, $uibModal, userfactory, verificatiefactory)
{
    var ctrl = this;

    ctrl.locaties = [];

    ctrl.isShowMeldingUrl = function()
    {
        return !ctrl.urlEndsWith($location.path(), "#/") && $location.path().indexOf("wachtwoordvergeten") === -1 && $location.path().indexOf("registreren") === -1;
    };

    ctrl.tonen = function()
    {
        if (verificatiefactory.getToonMelding() && ctrl.isShowMeldingUrl() && userfactory.isIngelogd())
        {
            verificatiefactory.meldingShown();
            verificatiefactory.getLocaties().$promise.then(function(response)
            {
                ctrl.locaties = response;
            }, function(data)
            {
                ctrl.locaties = [];
                verificatiefactory.doOnce = true;
                console.log(data);
            });
        }
        return userfactory.isIngelogd() && userfactory.heeftRecht("ROLE_AANVRAGEN") && ctrl.locaties.length > 0;
    };

    ctrl.urlEndsWith = function(theUrl, suffix)
    {
        return theUrl.indexOf(suffix, this.length - suffix.length) !== -1;
    };

    ctrl.openPincodePopup = function(locatie)
    {
        var modalInstance = $uibModal.open({
            templateUrl: './assets/js/intern/verifieren/pincodePopup.tmp.html',
            controller: 'pincodePopupCtrl',
            controllerAs: 'ctrl',
            backdrop: 'static',
            ariaLabelledBy: 'pincodeVerificatieLabel',
            size: 'lg',
            resolve: {
                huidigeLocatie: function()
                {
                    return locatie;
                }
            }
        });
    };

    ctrl.locatieLabel = "Locatie:";
    ctrl.zorgmailLabel = "Zorgmail klantnummer:";
    ctrl.pincodeLabel = "Locatie verifiëren";

    ctrl.meldingtekstpt1 = "U heeft het Zorgmail klantnummer nog niet geverifieerd voor de volgende huisartslocatie(s):<br /><br />";
    ctrl.meldingtekstpt2 = "Via de Zorgmail heeft u per locatie een bericht ontvangen met een verificatiecode. Klik op de knop 'Locatie verifi&euml;ren' en bevestig uw zorgmailklantnummer door de verificatiecode in te voeren.<br/><br/>" +
        "Na verificatie van uw Zorgmail klantnummer kunt u laboratoriumformulieren aanvragen en huisartsberichten ontvangen. <br/>  " +
        "Let op: U ontvangt geen uitslagberichten zolang uw klantnummer niet is bevestigd.";

});
