package nl.rivm.screenit.service.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
 * %%
 * Copyright (C) 2012 - 2025 Facilitaire Samenwerking Bevolkingsonderzoek
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

import nl.rivm.screenit.PreferenceKey;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.DeelnamemodusDossier;
import nl.rivm.screenit.model.GbaPersoon;
import nl.rivm.screenit.model.enums.Deelnamemodus;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.TransgenderService;
import nl.rivm.screenit.util.DateUtil;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.patientregistratie.persoonsgegevens.model.Geslacht;
import nl.topicuszorg.preferencemodule.service.SimplePreferenceService;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Service
@Transactional(propagation = Propagation.REQUIRED)
public class TransgenderServiceImpl implements TransgenderService
{
	@Autowired
	private HibernateService hibernateService;

	@Autowired
	private ICurrentDateSupplier currentDateSupplier;

	@Autowired
	private SimplePreferenceService preferenceService;

	@Override
	public void bijwerkenDeelnamemodus(DeelnamemodusDossier dossier)
	{
		var geslacht = dossier.getClient().getPersoon().getGeslacht();
		if (geslacht == Geslacht.MAN || geslacht == Geslacht.ONBEKEND)
		{
			geefSelectieblokkade(dossier);
		}
		else if (geslacht == Geslacht.VROUW)
		{
			verwijderSelectieblokkade(dossier);
		}
	}

	@Override
	public void bijwerkenDeelnamemodus(Client client)
	{
		bijwerkenDeelnamemodus(client.getMammaDossier());
		bijwerkenDeelnamemodus(client.getCervixDossier());
	}

	private void geefSelectieblokkade(DeelnamemodusDossier dossier)
	{
		if (dossier.getDeelnamemodus() == Deelnamemodus.STANDAARD)
		{
			dossier.setDeelnamemodus(Deelnamemodus.SELECTIEBLOKKADE);
			hibernateService.saveOrUpdate(dossier);
		}
	}

	private void verwijderSelectieblokkade(DeelnamemodusDossier dossier)
	{
		if (dossier.getDeelnamemodus() == Deelnamemodus.SELECTIEBLOKKADE)
		{
			dossier.setDeelnamemodus(Deelnamemodus.STANDAARD);
			hibernateService.saveOrUpdate(dossier);
		}
	}

	@Override
	@Transactional(propagation = Propagation.SUPPORTS, readOnly = true)
	public boolean isNieuweClientAdhocPlaatsingTransgender(GbaPersoon persoon)
	{
		if (persoon.getGeslacht() != Geslacht.MAN || persoon.getGeboortedatum() == null)
		{
			return false;
		}

		var huidigeJaar = currentDateSupplier.getLocalDate().getYear();
		var geboortejaar = DateUtil.toLocalDate(persoon.getGeboortedatum()).getYear();
		var leeftijd = huidigeJaar - geboortejaar;
		var minimaleLeeftijdDk = preferenceService.getInteger(PreferenceKey.MINIMALE_LEEFTIJD_COLON.name());
		return leeftijd < minimaleLeeftijdDk - 1; 
	}
}
