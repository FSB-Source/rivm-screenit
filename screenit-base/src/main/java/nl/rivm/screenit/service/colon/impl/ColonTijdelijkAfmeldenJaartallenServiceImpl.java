package nl.rivm.screenit.service.colon.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
 * %%
 * Copyright (C) 2012 - 2024 Facilitaire Samenwerking Bevolkingsonderzoek
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

import java.util.ArrayList;
import java.util.List;

import lombok.AllArgsConstructor;

import nl.rivm.screenit.PreferenceKey;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.colon.ColonDossier;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.colon.ColonDossierBaseService;
import nl.rivm.screenit.service.colon.ColonTijdelijkAfmeldenJaartallenService;
import nl.rivm.screenit.util.ColonScreeningRondeUtil;
import nl.rivm.screenit.util.DateUtil;
import nl.topicuszorg.preferencemodule.service.SimplePreferenceService;

import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Service
@Transactional(propagation = Propagation.SUPPORTS, readOnly = true)
@AllArgsConstructor
public class ColonTijdelijkAfmeldenJaartallenServiceImpl implements ColonTijdelijkAfmeldenJaartallenService
{

	private SimplePreferenceService preferenceService;

	private ICurrentDateSupplier currentDateSupplier;

	private ColonDossierBaseService colonDossierBaseService;

	@Override
	public List<Integer> bepaalMogelijkeAfmeldJaren(Client client)
	{
		List<Integer> afmeldbaarTotMetJaartallen = new ArrayList<>();
		var dossier = client.getColonDossier();
		boolean laatsteScreeningRondeAanwezig = dossier != null && dossier.getLaatsteScreeningRonde() != null;

		if (laatsteScreeningRondeAanwezig && heeftGeenOngunstigeUitslagInLaatsteRonde(dossier))
		{
			var begindatumLaatsteScreeningRonde = DateUtil.toLocalDate(client.getColonDossier().getLaatsteScreeningRonde().getCreatieDatum());
			var geboortedatumClient = DateUtil.toLocalDate(client.getPersoon().getGeboortedatum());
			var datumVandaag = currentDateSupplier.getLocalDate();
			var volgendeUitnodigingsDatum = colonDossierBaseService.getDatumVolgendeUitnodiging(dossier);

			Integer maximumLeeftijd = preferenceService.getInteger(PreferenceKey.MAXIMALE_LEEFTIJD_COLON.name());
			long leeftijdGeenUitnodigingMeer = maximumLeeftijd + 1L;

			for (int toeTeVoegenJaren = 2; toeTeVoegenJaren <= 5; toeTeVoegenJaren++)
			{
				var teKiezenJaartal = begindatumLaatsteScreeningRonde.plusYears(toeTeVoegenJaren).getYear();

				boolean nieuweUitnodigingNietInDitJaar = teKiezenJaartal > datumVandaag.getYear();
				boolean nieuweDatumVoorDatumDatClientTeOudIs = datumVandaag.plusYears(toeTeVoegenJaren).minusYears(leeftijdGeenUitnodigingMeer).isBefore(geboortedatumClient);
				boolean nieuweDatumNietVoorUitnodigingsdatum =
					volgendeUitnodigingsDatum == null || !begindatumLaatsteScreeningRonde.plusYears(toeTeVoegenJaren).isBefore(volgendeUitnodigingsDatum);

				if (nieuweUitnodigingNietInDitJaar && nieuweDatumVoorDatumDatClientTeOudIs && nieuweDatumNietVoorUitnodigingsdatum)
				{
					afmeldbaarTotMetJaartallen.add(teKiezenJaartal);
				}
			}
		}
		return afmeldbaarTotMetJaartallen;
	}

	private boolean heeftGeenOngunstigeUitslagInLaatsteRonde(ColonDossier dossier)
	{
		var laatsteRonde = dossier.getLaatsteScreeningRonde();
		return ColonScreeningRondeUtil.getEersteOngunstigeTest(laatsteRonde) == null;
	}
}
