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

angular.module('rivmUistrijkendarts').controller(	'registrerenControleCtrl',
function($http, $routeParams, $location, locatiefactory, userfactory, toaster, registratiefactory, overeenkomstfactory)
{
    var ctrl, huisarts;

    ctrl = this;
    this.huisarts = {}
    this.locaties;
    this.locatieZoekObject = {
        actief: true,
        resultOptions: {
            first: -1,
            count: -1,
            sortOptions: {}
        }
    }

    if (registratiefactory.getHuisarts() != undefined)
    {
        ctrl.huisarts = registratiefactory.getHuisarts();
        locatiefactory.getLocaties(ctrl.locatieZoekObject).$promise.then(function(data)
        {
            ctrl.locaties = data.locaties;
        });
    }
    else
    {
        ctrl.locatieTerug();
    }

    this.opslaan = function opslaan(huisarts)
    {
        userfactory.saveHuisarts(huisarts).$promise.then(function(data)
        {
            var moetVerifieren = false;
            for (var i = 0; i < ctrl.locaties.length; i++)
            {
                if (ctrl.locaties[i].status === "KLANTNUMMER_NIET_GEVERIFIEERD")
                {
                    moetVerifieren = true;
                }
            }
            if (moetVerifieren)
            {
                toaster.warning("Om uw Zorgmail klantnummer te bevestigen is een bericht met een verificatiecode naar uw zorgmailadres gestuurd. Volg de instructies in de mail om uw Zorgmail klantnummer te bevestigen.", "", {
                    timeOut: 0
                });
            }
            registratiefactory.setHuisarts(undefined);
            userfactory.logout();
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
                    toaster.error("Huisarts kon niet worden opgeslagen. Neem contact op met de helpdesk.");
                }
            }
            else
            {
                toaster.error("Er is een onbekende fout opgetreden neem contact op met de helpdesk.");
            }
        })
    }

    this.locatieTerug = function()
    {
        $location.path('/registreren/voltooien/');
    }

    this.terug = function()
    {
        registratiefactory.setHuisarts(ctrl.huisarts);
        ctrl.locatieTerug();
    }

    this.getOvereenkomst = function()
    {
        overeenkomstfactory.openOvereekomst();
    }

});
