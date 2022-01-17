package nl.rivm.screenit.service.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
 * %%
 * Copyright (C) 2012 - 2022 Facilitaire Samenwerking Bevolkingsonderzoek
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
import java.util.ArrayList;
import java.util.List;

import nl.rivm.screenit.PreferenceKey;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.Dossier;
import nl.rivm.screenit.model.cervix.enums.CervixLeeftijdcategorie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.service.ClientDoelgroepService;
import nl.rivm.screenit.service.ClientService;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.util.DateUtil;
import nl.topicuszorg.patientregistratie.persoonsgegevens.model.Geslacht;
import nl.topicuszorg.preferencemodule.service.SimplePreferenceService;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Component
@Transactional(propagation = Propagation.SUPPORTS, readOnly = true)
public class ClientDoelgroepServiceImpl implements ClientDoelgroepService
{
	@Autowired
	private ClientService clientService;

	@Autowired
	private SimplePreferenceService simplePreferenceService;

	@Autowired
	private ICurrentDateSupplier currentDateSupplier;

	@Override
	public boolean behoortTotDoelgroep(Client client, Bevolkingsonderzoek bevolkingsonderzoek)
	{
		if (clientService.isClientOverleden(client))
		{
			return false;
		}
		else
		{
			switch (bevolkingsonderzoek)
			{
			case COLON:
				return behoortTotColonDoelgroep(client);
			case CERVIX:
				return behoortTotCervixDoelgroep(client);
			case MAMMA:
				return behoortTotMammaDoelgroep(client);
			default:
				return false;
			}
		}
	}

	private boolean heeftDossierActiviteit(Dossier<?, ?> dossier)
	{
		return dossier != null && (dossier.getLaatsteScreeningRonde() != null || dossier.getLaatsteAfmelding() != null);
	}

	private boolean behoortTotColonDoelgroep(Client client)
	{
		LocalDate geboortedatum = DateUtil.toLocalDate(client.getPersoon().getGeboortedatum());

		return clientIsOuderDanMinimaleLeeftijd(geboortedatum, Bevolkingsonderzoek.COLON,
			simplePreferenceService.getInteger(PreferenceKey.MINIMALE_LEEFTIJD_COLON.name()))
			&& clientIsJongerDanMaximaleLeeftijd(geboortedatum, Bevolkingsonderzoek.COLON,
			simplePreferenceService.getInteger(PreferenceKey.MAXIMALE_LEEFTIJD_COLON.name()) + 1)
			|| heeftDossierActiviteit(client.getColonDossier());
	}

	private boolean behoortTotCervixDoelgroep(Client client)
	{
		LocalDate geboortedatum = DateUtil.toLocalDate(client.getPersoon().getGeboortedatum());

		boolean isVrouw = Geslacht.VROUW.equals(client.getPersoon().getGeslacht());
		return isVrouw && clientIsJongerDanMaximaleLeeftijd(geboortedatum, Bevolkingsonderzoek.CERVIX, CervixLeeftijdcategorie._70.getLeeftijd())
			&& clientIsOuderDanMinimaleLeeftijd(geboortedatum, Bevolkingsonderzoek.CERVIX, CervixLeeftijdcategorie._30.getLeeftijd())
			|| heeftDossierActiviteit(client.getCervixDossier());
	}

	private boolean behoortTotMammaDoelgroep(Client client)
	{
		return behoortTotMammaLeeftijdDoelgroep(client) || heeftDossierActiviteit(client.getMammaDossier());
	}

	@Override
	public boolean behoortTotMammaLeeftijdDoelgroep(Client client)
	{
		LocalDate geboortedatum = DateUtil.toLocalDate(client.getPersoon().getGeboortedatum());

		boolean isVrouw = Geslacht.VROUW.equals(client.getPersoon().getGeslacht());
		return isVrouw
			&& clientIsJongerDanMaximaleLeeftijd(geboortedatum, Bevolkingsonderzoek.MAMMA,
			simplePreferenceService.getInteger(PreferenceKey.MAMMA_MAXIMALE_LEEFTIJD.name()))
			&& clientIsOuderDanMinimaleLeeftijd(geboortedatum, Bevolkingsonderzoek.MAMMA,
			simplePreferenceService.getInteger(PreferenceKey.MAMMA_MINIMALE_LEEFTIJD.name()));
	}

	private boolean clientIsOuderDanMinimaleLeeftijd(LocalDate geboortedatum, Bevolkingsonderzoek bevolkingsonderzoek, int minimaleLeeftijdParameter)
	{
		LocalDate nu = currentDateSupplier.getLocalDate();

		if (Bevolkingsonderzoek.MAMMA.equals(bevolkingsonderzoek))
		{
			int geboortejaar = geboortedatum.getYear();

			int geboortejaarBijMinimaleLeeftijd = nu.minusYears(minimaleLeeftijdParameter).getYear();
			return geboortejaar <= geboortejaarBijMinimaleLeeftijd;
		}

		LocalDate geboortedatumBijMinimaleLeeftijd = nu.minusYears(minimaleLeeftijdParameter);

		return geboortedatum.isBefore(geboortedatumBijMinimaleLeeftijd);
	}

	private boolean clientIsJongerDanMaximaleLeeftijd(LocalDate geboortedatum, Bevolkingsonderzoek bevolkingsonderzoek, int maximaleLeeftijdParameter)
	{
		LocalDate nu = currentDateSupplier.getLocalDate();

		if (Bevolkingsonderzoek.MAMMA.equals(bevolkingsonderzoek))
		{
			int geboortejaar = geboortedatum.getYear();
			int geboortejaarBijMaximaleLeeftijd = nu.minusYears(maximaleLeeftijdParameter).getYear() - 1;
			return geboortejaar >= geboortejaarBijMaximaleLeeftijd;
		}

		LocalDate geboortedatumBijMaximaleLeeftijd = nu.minusYears(maximaleLeeftijdParameter);

		return geboortedatum.isAfter(geboortedatumBijMaximaleLeeftijd);
	}

	@Override
	public List<Bevolkingsonderzoek> totWelkeBevolkingsonderzoekenHoortDezeClient(Client client)
	{
		List<Bevolkingsonderzoek> onderzoeken = new ArrayList<>();
		for (Bevolkingsonderzoek onderzoek : Bevolkingsonderzoek.values())
		{
			if (client == null || behoortTotDoelgroep(client, onderzoek))
			{
				onderzoeken.add(onderzoek);
			}
		}
		return onderzoeken;
	}
}
