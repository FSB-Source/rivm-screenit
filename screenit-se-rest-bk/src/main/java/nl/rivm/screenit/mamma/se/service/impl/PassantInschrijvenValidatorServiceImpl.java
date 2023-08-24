package nl.rivm.screenit.mamma.se.service.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-se-rest-bk
 * %%
 * Copyright (C) 2012 - 2023 Facilitaire Samenwerking Bevolkingsonderzoek
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

import java.time.LocalDate;

import nl.rivm.screenit.mamma.se.service.PassantInschrijvenValidatorService;
import nl.rivm.screenit.mamma.se.service.PassantValidatorResult;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.mamma.MammaAfspraak;
import nl.rivm.screenit.model.mamma.MammaScreeningRonde;
import nl.rivm.screenit.model.mamma.MammaScreeningsEenheid;
import nl.rivm.screenit.model.mamma.MammaUitnodiging;
import nl.rivm.screenit.model.mamma.enums.MammaAfspraakStatus;
import nl.rivm.screenit.service.mamma.MammaBaseDossierService;
import nl.rivm.screenit.util.DateUtil;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Service
@Transactional(propagation = Propagation.SUPPORTS)
public class PassantInschrijvenValidatorServiceImpl implements PassantInschrijvenValidatorService
{
	@Autowired
	private MammaBaseDossierService baseDossierService;

	public PassantValidatorResult isGeldigPassantScenario(Client client, LocalDate currentDate, MammaScreeningsEenheid se)
	{
		MammaScreeningRonde laatsteScreeningRonde = client.getMammaDossier().getLaatsteScreeningRonde();
		if (laatsteScreeningRonde != null && laatsteScreeningRonde.getLaatsteUitnodiging() != null)
		{
			MammaAfspraak laatsteAfspraak = laatsteScreeningRonde.getLaatsteUitnodiging().getLaatsteAfspraak();
			if (heeftAfspraakOpHuidigeDagOpSe(currentDate, se, laatsteAfspraak))
			{
				return PassantValidatorResult.ONGELDIG_ZELFDE_DAG;
			}
			if (heeftOnderzoekInLaatsteRonde(laatsteScreeningRonde))
			{
				return PassantValidatorResult.ONGELDIG;
			}
		}
		boolean isAfspraakMogelijk = baseDossierService.isAfspraakMakenMogelijk(client.getMammaDossier(), false, true) || baseDossierService.isVerzettenMogelijk(client.getMammaDossier());
		return isAfspraakMogelijk ? PassantValidatorResult.OK : PassantValidatorResult.ONGELDIG;
	}

	private boolean heeftAfspraakOpHuidigeDagOpSe(LocalDate currentDate, MammaScreeningsEenheid se, MammaAfspraak laatsteAfspraak)
	{
		return laatsteAfspraak != null && DateUtil.isZelfdeDag(currentDate, laatsteAfspraak.getVanaf())
			&& laatsteAfspraak.getStandplaatsPeriode().getScreeningsEenheid().equals(se) && MammaAfspraakStatus.NIET_GEANNULEERD.contains(laatsteAfspraak.getStatus());
	}

	private boolean heeftOnderzoekInLaatsteRonde(MammaScreeningRonde laatsteScreeningRonde)
	{
		return laatsteScreeningRonde
			.getUitnodigingen().stream()
			.map(MammaUitnodiging::getAfspraken)
			.anyMatch(afspraken -> afspraken.stream()
				.anyMatch(afspraak -> afspraak.getOnderzoek() != null));
	}
}
