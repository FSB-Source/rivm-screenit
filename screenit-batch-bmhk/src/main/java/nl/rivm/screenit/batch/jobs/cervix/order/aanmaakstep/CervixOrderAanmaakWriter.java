package nl.rivm.screenit.batch.jobs.cervix.order.aanmaakstep;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-bmhk
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

import java.util.ArrayList;
import java.util.List;

import nl.rivm.screenit.batch.jobs.cervix.order.CervixOrderConstants;
import nl.rivm.screenit.batch.jobs.helpers.BaseWriter;
import nl.rivm.screenit.batch.service.CervixMaakOrderBerichtService;
import nl.rivm.screenit.dao.cervix.CervixBepaalVervolgDao;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.Instelling;
import nl.rivm.screenit.model.cervix.CervixUitstrijkje;
import nl.rivm.screenit.model.cervix.enums.CervixCytologieReden;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.service.LogService;
import nl.rivm.screenit.service.cervix.CervixFactory;
import nl.rivm.screenit.service.cervix.impl.CervixBepaalVervolgContext;
import nl.topicuszorg.hibernate.object.model.HibernateObject;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;

public class CervixOrderAanmaakWriter extends BaseWriter<CervixUitstrijkje>
{

	private static final Logger LOG = LoggerFactory.getLogger(CervixOrderAanmaakWriter.class);

	@Autowired
	private LogService logService;

	@Autowired
	private CervixMaakOrderBerichtService maakOrderBerichtService;

	@Autowired
	private CervixFactory factory;

	@Autowired
	private HibernateService hibernateService;

	@Autowired
	private CervixBepaalVervolgDao bepaalVervolgDao;

	@Override
	protected void write(CervixUitstrijkje uitstrijkje) throws Exception
	{
		try
		{
			CervixCytologieReden cytologieReden = getCytologieReden(uitstrijkje);
			factory.maakCytologieOrder(uitstrijkje, cytologieReden, maakHl7v2Bericht(uitstrijkje, cytologieReden));

			aantalContextOphogen(CervixOrderConstants.KEY_ORDER_AANGEMAAKT);
		}
		catch (IllegalStateException e)
		{
			logging(uitstrijkje, e.getMessage());
		}
	}

	private String maakHl7v2Bericht(CervixUitstrijkje uitstrijkje, CervixCytologieReden cytologieReden)
	{
		return maakOrderBerichtService.maakOrderTextBericht(uitstrijkje, cytologieReden);
	}

	private CervixCytologieReden getCytologieReden(CervixUitstrijkje uitstrijkje) throws IllegalStateException
	{
		try
		{
			CervixBepaalVervolgContext vervolgContext = new CervixBepaalVervolgContext(uitstrijkje);

			if (vervolgContext.inVervolgonderzoekDatum != null)
			{
				if (bepaalVervolgDao.anderUitstrijkjeOnbeoordeelbaarCytologie(vervolgContext.huidigUitstrijkje))
				{
					return CervixCytologieReden.HERHALING_VERVOLGONDERZOEK;
				}
				return CervixCytologieReden.VERVOLGONDERZOEK;
			}

			if (bepaalVervolgDao.anderUitstrijkjeOnbeoordeelbaarCytologie(vervolgContext.huidigUitstrijkje))
			{
				return CervixCytologieReden.HERHALING_INITIEEL_NA_ONBEOORDEELBAARHEID;
			}

			if (vervolgContext.monsterHpvUitslag instanceof CervixUitstrijkje)
			{
				return CervixCytologieReden.INITIEEL_ZONDER_ZAS;
			}
			else
			{
				return CervixCytologieReden.INITIEEL_NA_ZAS;
			}
		}
		catch (Exception e)
		{
			LOG.error("Er is een probleem opgetreden met het bepalen van de cytologie reden.", e);
			throw new IllegalStateException("Er kon voor deze client geen cytologiereden worden bepaald.");
		}
	}

	private void logging(CervixUitstrijkje uitstrijkje, String melding)
	{
		List<Instelling> instellingen = new ArrayList<>();
		instellingen.add(uitstrijkje.getLaboratorium());
		Client client = uitstrijkje.getOntvangstScreeningRonde().getDossier().getClient();
		logService.logGebeurtenis(LogGebeurtenis.CERVIX_ORDER_AANMAKEN_MISLUKT, instellingen, client, melding, Bevolkingsonderzoek.CERVIX);
	}
}
