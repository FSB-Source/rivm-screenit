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

angular.module('rivmUistrijkendarts').controller('pincodePopupCtrl', function($scope, $rootScope, huidigeLocatie, $uibModalInstance, userfactory, verificatiefactory, $route, toaster)
{
	var ctrl = this;

	ctrl.locatie = huidigeLocatie;

	ctrl.pincode = "";
	ctrl.zorgmailklantnummer = ctrl.locatie.zorgmailKlantnummer;
	ctrl.locatienaam = ctrl.locatie.locatieNaam;

	this.resetFields = function(form)
	{
		form.$setPristine();
		$rootScope.$broadcast("resetErrors");
		ctrl.pincode = "";
		$uibModalInstance.close();
	};

	ctrl.herzendVerificatieMail = function()
	{
		verificatiefactory.herzendVerificatieMail(ctrl.locatie).$promise.then(function(data)
		{
			toaster.info("Zorgmail klantnummer verificatiemail is opnieuw verzonden.");
			$uibModalInstance.close();
		});
	};

	this.verifieerLocatie = function verifieerLocatie(form, pincode)
	{
		if (form.$valid)
		{
			ctrl.locatie.verificatieCode = pincode;
			verificatiefactory.verifieerLocatie(ctrl.locatie).$promise.then(function(data)
			{
                verificatiefactory.resetMelding();
                $route.reload();
				toaster.info("Bedankt voor het verifiëren van uw Zorgmail klantnummer. U kunt nu voor de betreffende locatie labformulieren aanvragen en huisartsberichten ontvangen.");
                $uibModalInstance.close();
			}, function(data)
			{
				toaster.error("Verificatie mislukt! Controleer of u de juiste verificatiecode bij de juiste locatie heeft ingevuld. U kunt de verificatiecode opnieuw aanvragen door op de knop 'Herzend verificatiemail' te klikken.");
			});
		}

	};
});
