package nl.rivm.screenit.service.mamma.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
 * %%
 * Copyright (C) 2012 - 2020 Facilitaire Samenwerking Bevolkingsonderzoek
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
import nl.rivm.screenit.model.EnovationHuisarts;
import nl.rivm.screenit.model.MailMergeContext;
import nl.rivm.screenit.model.ScreeningOrganisatie;
import nl.rivm.screenit.model.enums.HuisartsBerichtType;
import nl.rivm.screenit.model.mamma.MammaBeoordeling;
import nl.rivm.screenit.model.mamma.berichten.MammaHuisartsBericht;
import nl.rivm.screenit.model.mamma.enums.MammaHuisartsBerichtStatus;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.mamma.MammaBaseBeoordelingService;
import nl.rivm.screenit.service.mamma.MammaEdiService;
import nl.rivm.screenit.service.mamma.MammaHuisartsBerichtService;
import nl.topicuszorg.hibernate.object.helper.HibernateHelper;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Service
@Transactional(propagation = Propagation.SUPPORTS)
public class MammaHuisartsBerichtServiceImpl implements MammaHuisartsBerichtService
{
	private static final Logger LOG = LoggerFactory.getLogger(MammaHuisartsBerichtServiceImpl.class);

	@Autowired
	private MammaEdiService ediService;

	@Autowired
	private ICurrentDateSupplier currentDateSupplier;

	@Autowired
	private MammaBaseBeoordelingService baseBeoordelingService;

	@Autowired
	private HibernateService hibernateService;

	@Override
	public void verstuurHuisartsBericht(MammaBeoordeling beoordeling, EnovationHuisarts huisarts, HuisartsBerichtType huisartsBerichtType)
	{
		Client client = (Client) HibernateHelper.deproxy(baseBeoordelingService.getClientVanBeoordeling(beoordeling));
		MailMergeContext context = new MailMergeContext();
		context.setClient(client);

		if (client.getPersoon().getOverlijdensdatum() != null)
		{
			LOG.debug("Er wordt geen HuisartsBericht gemaakt voor het HuisartsBerichtType: " + huisartsBerichtType + ", voor Client: "
				+ client.getId() + ". Client is overleden.");
			return;
		}
		LOG.debug("Er wordt een HuisartsBericht gemaakt voor het HuisartsBerichtType: " + huisartsBerichtType + ", voor Client: "
			+ client.getId());

		final MammaHuisartsBericht huisartsBericht = maakHuisartsbericht(beoordeling, huisarts, huisartsBerichtType);

		ediService.maakHuisartsBericht(client, context, huisartsBericht);
		hibernateService.saveOrUpdate(huisartsBericht);
		hibernateService.saveOrUpdate(beoordeling);
		ediService.verstuurMedVry(huisartsBericht, true);

	}

	private MammaHuisartsBericht maakHuisartsbericht(MammaBeoordeling beoordeling, EnovationHuisarts huisarts, HuisartsBerichtType huisartsBerichtType)
	{
		ScreeningOrganisatie screeningOrganisatie = (ScreeningOrganisatie) HibernateHelper
			.deproxy(beoordeling.getOnderzoek().getScreeningsEenheid().getBeoordelingsEenheid().getParent().getRegio());
		final MammaHuisartsBericht huisartsBericht = new MammaHuisartsBericht();
		huisartsBericht.setClient(baseBeoordelingService.getClientVanBeoordeling(beoordeling));
		huisartsBericht.setBerichtType(huisartsBerichtType);
		huisartsBericht.setScreeningsOrganisatie(screeningOrganisatie);
		huisartsBericht.setAanmaakDatum(currentDateSupplier.getDate());
		huisartsBericht.setBeoordeling(beoordeling);
		beoordeling.getHuisartsBerichten().add(huisartsBericht);

		huisartsBericht.setHuisarts(huisarts);
		setStatus(huisartsBericht, MammaHuisartsBerichtStatus.AANGEMAAKT);
		return huisartsBericht;
	}

	private void setStatus(MammaHuisartsBericht huisartsBericht, MammaHuisartsBerichtStatus status)
	{
		huisartsBericht.setStatus(status);
		huisartsBericht.setStatusDatum(currentDateSupplier.getDate());
	}
}
