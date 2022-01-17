package nl.rivm.screenit.service.cervix.impl;

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

import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.GbaPersoon;
import nl.rivm.screenit.model.Gemeente;
import nl.rivm.screenit.model.cervix.CervixHuisarts;
import nl.rivm.screenit.model.cervix.CervixHuisartsBericht;
import nl.rivm.screenit.model.cervix.CervixHuisartsLocatie;
import nl.rivm.screenit.model.cervix.CervixUitnodiging;
import nl.rivm.screenit.model.cervix.CervixUitstrijkje;
import nl.rivm.screenit.model.cervix.enums.CervixHuisartsBerichtStatus;
import nl.rivm.screenit.model.cervix.enums.CervixOmissieType;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.HuisartsBerichtType;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.model.logging.LogEvent;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.LogService;
import nl.rivm.screenit.service.cervix.CervixEdiService;
import nl.rivm.screenit.service.cervix.CervixHuisartsBerichtService;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Service
@Transactional(propagation = Propagation.SUPPORTS)
public class CervixHuisartsBerichtServiceImpl implements CervixHuisartsBerichtService
{

	private static final Logger LOG = LoggerFactory.getLogger(CervixHuisartsBerichtServiceImpl.class);

	@Autowired
	private LogService logService;

	@Autowired
	private CervixEdiService ediService;

	@Autowired
	private HibernateService hibernateService;

	@Autowired
	private ICurrentDateSupplier currentDateSupplier;

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public void sendKlantnummerVerificatieMail(CervixHuisarts huisarts, CervixHuisartsLocatie locatie)
	{
		try
		{
			LOG.info("Verstuur Zorgmail klantnummer verificatiebericht voor huisartslocatie hp-id:" + locatie.getHuisartsportaalId());

			CervixHuisartsBericht huisartsBericht = new CervixHuisartsBericht();
			huisartsBericht.setBerichtType(HuisartsBerichtType.CERVIX_ZORGMAIL_VERIFICATIE);
			huisartsBericht.setHuisartsLocatie(locatie);
			huisartsBericht.setScreeningsOrganisatie(locatie.getLocatieAdres().getGbaGemeente().getScreeningOrganisatie());
			huisartsBericht.setStatus(CervixHuisartsBerichtStatus.AANGEMAAKT);
			huisartsBericht.setStatusDatum(currentDateSupplier.getDate());
			huisartsBericht.setAanmaakDatum(currentDateSupplier.getDate());
			ediService.verstuurKlantnummerVerificatieMedVry(huisartsBericht);
		}
		catch (IllegalStateException e)
		{
			logService.logGebeurtenis(LogGebeurtenis.CERVIX_HUISARTSBERICHT_VERZENDEN_MISLUKT, new LogEvent(e.getMessage()), null,
				(Client) null, Bevolkingsonderzoek.CERVIX);
			LOG.error(e.getMessage(), e);
		}
	}

	@Override
	public CervixHuisartsBericht maakCervixHuisartsBericht(HuisartsBerichtType berichtType, Client client, CervixUitstrijkje uitstrijkje, CervixOmissieType omissieType)
	{
		CervixHuisartsBericht huisartsBericht = maakCervixHuisartsBericht(berichtType, client, uitstrijkje.getUitnodiging());

		huisartsBericht.setOmissieType(omissieType);

		huisartsBericht.setUitstrijkje(uitstrijkje);
		uitstrijkje.setHuisartsBericht(huisartsBericht);

		hibernateService.saveOrUpdate(huisartsBericht);
		hibernateService.saveOrUpdate(uitstrijkje);

		return huisartsBericht;
	}

	@Override
	public CervixHuisartsBericht maakCervixHuisartsBericht(HuisartsBerichtType berichtType, Client client, CervixHuisartsLocatie huisartsLocatie, CervixUitnodiging uitnodiging)
	{
		CervixHuisartsBericht huisartsBericht = maakCervixHuisartsBericht(berichtType, client, uitnodiging);

		huisartsBericht.setHuisartsLocatie(huisartsLocatie);
		huisartsLocatie.getHuisartsberichten().add(huisartsBericht);

		hibernateService.saveOrUpdate(huisartsBericht);
		hibernateService.saveOrUpdate(huisartsLocatie);

		return huisartsBericht;
	}

	private CervixHuisartsBericht maakCervixHuisartsBericht(HuisartsBerichtType berichtType, Client client, CervixUitnodiging uitnodiging)
	{
		CervixHuisartsBericht huisartsBericht = new CervixHuisartsBericht();
		huisartsBericht.setStatus(CervixHuisartsBerichtStatus.AANGEMAAKT);
		huisartsBericht.setStatusDatum(currentDateSupplier.getDate());
		huisartsBericht.setAanmaakDatum(currentDateSupplier.getDate());
		huisartsBericht.setClient(client);
		huisartsBericht.setBerichtType(berichtType);
		huisartsBericht.setScreeningRonde(client.getCervixDossier().getLaatsteScreeningRonde());

		GbaPersoon persoon = client.getPersoon();
		Gemeente gemeente = persoon.getGbaAdres().getGbaGemeente();
		if (gemeente == null)
		{
			throw new IllegalStateException("De gemeente is onbekend voor cliënt met id " + client.getId());
		}
		else if (gemeente.getCode().equals(Gemeente.RNI_CODE))
		{
			huisartsBericht.setScreeningsOrganisatie(uitnodiging.getBrief().getMergedBrieven().getScreeningOrganisatie());
		}
		else
		{
			huisartsBericht.setScreeningsOrganisatie(gemeente.getScreeningOrganisatie());
		}

		client.getHuisartsBerichten().add(huisartsBericht);

		hibernateService.saveOrUpdate(huisartsBericht);
		hibernateService.saveOrUpdate(client);

		return huisartsBericht;
	}
}
