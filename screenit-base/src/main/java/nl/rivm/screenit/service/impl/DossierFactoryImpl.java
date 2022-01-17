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

import java.util.HashSet;
import java.util.Set;

import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.DossierStatus;
import nl.rivm.screenit.model.cervix.CervixDossier;
import nl.rivm.screenit.model.colon.ColonDossier;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.mamma.MammaDeelnamekans;
import nl.rivm.screenit.model.mamma.MammaDossier;
import nl.rivm.screenit.model.mamma.berichten.xds.XdsStatus;
import nl.rivm.screenit.model.mamma.enums.MammaDoelgroep;
import nl.rivm.screenit.service.DossierFactory;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.patientregistratie.persoonsgegevens.model.Geslacht;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Service
@Transactional(propagation = Propagation.REQUIRED)
public class DossierFactoryImpl implements DossierFactory
{
	private static final Logger LOG = LoggerFactory.getLogger(DossierFactoryImpl.class);

	@Autowired
	private HibernateService hibernateService;

	@Override
	public Set<Bevolkingsonderzoek> maakDossiers(Client client)
	{
		Set<Bevolkingsonderzoek> bevolkingsonderzoeken = new HashSet<>();
		if (client.getColonDossier() == null)
		{
			maakColonDossier(client);
			bevolkingsonderzoeken.add(Bevolkingsonderzoek.COLON);
		}

		if (client.getPersoon().getGeslacht() == Geslacht.VROUW)
		{
			if (client.getCervixDossier() == null)
			{
				maakCervixDossier(client);
				bevolkingsonderzoeken.add(Bevolkingsonderzoek.CERVIX);
			}
			if (client.getMammaDossier() == null)
			{
				maakMammaDossier(client);
				bevolkingsonderzoeken.add(Bevolkingsonderzoek.MAMMA);
			}
		}

		return bevolkingsonderzoeken;
	}

	private void maakColonDossier(Client client)
	{
		ColonDossier colonDossier = new ColonDossier();
		colonDossier.setStatus(DossierStatus.ACTIEF);
		colonDossier.setAangemeld(true);
		colonDossier.setClient(client);
		client.setColonDossier(colonDossier);
		hibernateService.saveOrUpdateAll(colonDossier, client);
	}

	private void maakCervixDossier(Client client)
	{
		LOG.info("CervixDossier aanmaken voor client(id: " + client.getId() + ")");

		CervixDossier cervixDossier = new CervixDossier();
		cervixDossier.setStatus(DossierStatus.ACTIEF);
		cervixDossier.setAangemeld(true);
		cervixDossier.setClient(client);
		client.setCervixDossier(cervixDossier);

		hibernateService.saveOrUpdateAll(cervixDossier, client);
	}

	private void maakMammaDossier(Client client)
	{
		LOG.info("MammaDossier aanmaken voor client(id: " + client.getId() + ")");

		MammaDossier mammaDossier = new MammaDossier();
		mammaDossier.setStatus(DossierStatus.ACTIEF);
		mammaDossier.setDoelgroep(MammaDoelgroep.REGULIER);
		mammaDossier.setAangemeld(true);
		mammaDossier.setXdsStatus(XdsStatus.NIET_AANGEMELD);
		mammaDossier.setClient(client);
		client.setMammaDossier(mammaDossier);

		MammaDeelnamekans mammaDeelnamekans = new MammaDeelnamekans();
		mammaDeelnamekans.setDossier(mammaDossier);
		mammaDossier.setDeelnamekans(mammaDeelnamekans);

		hibernateService.saveOrUpdateAll(mammaDossier, mammaDeelnamekans, client);
	}
}
