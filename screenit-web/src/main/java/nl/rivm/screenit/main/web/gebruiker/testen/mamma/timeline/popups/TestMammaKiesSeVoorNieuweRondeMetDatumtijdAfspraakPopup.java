package nl.rivm.screenit.main.web.gebruiker.testen.mamma.timeline.popups;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
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

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import nl.rivm.screenit.main.service.mamma.MammaTestTimelineService;
import nl.rivm.screenit.main.web.gebruiker.testen.mamma.timeline.MammaTestTimelinePage;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.mamma.MammaScreeningRonde;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;

import org.apache.commons.lang.StringUtils;
import org.apache.wicket.model.IModel;
import org.apache.wicket.spring.injection.annot.SpringBean;

public class TestMammaKiesSeVoorNieuweRondeMetDatumtijdAfspraakPopup extends TestMammaKiesSeEnUitnodigingsNrVoorAfspraakPopup
{
	@SpringBean
	private MammaTestTimelineService testTimelineService;

	@SpringBean
	private HibernateService hibernateService;

	public TestMammaKiesSeVoorNieuweRondeMetDatumtijdAfspraakPopup(String id, IModel<List<Client>> clientModel)
	{
		super(id, clientModel);
	}

	@Override
	protected void opslaan()
	{
		boolean verstuurHl7Berichten = ((MammaTestTimelinePage) getPage()).getVerstuurHl7Berichten().getObject();

		for (Client client : getModelObject())
		{
			if (getScreeningsEenheid() != null)
			{

				if (StringUtils.isBlank(getUitnodigingsNr()))
				{
					mammaBaseTestTimelineService.nieuweRondeAfspraakUitnodiging(client, getScreeningsEenheid(), verstuurHl7Berichten);
				}
				else
				{
					try
					{
						Map<String, Long> uitnodigingsNrParam = new HashMap<>();
						uitnodigingsNrParam.put("uitnodigingsNr", Long.parseLong(getUitnodigingsNr()));
						MammaScreeningRonde screeningRonde = hibernateService.getUniqueByParameters(MammaScreeningRonde.class, uitnodigingsNrParam);
						if (screeningRonde == null)
						{
							mammaBaseTestTimelineService.nieuweRondeAfspraakUitnodiging(client, getScreeningsEenheid(), verstuurHl7Berichten,
								Long.parseLong(getUitnodigingsNr()));
						}
						else
						{

							error("Het uitnodigingsnummer is al bekend in ScreenIT voor client met BSN: " + screeningRonde.getDossier().getClient().getPersoon().getBsn());
						}
					}
					catch (NumberFormatException e)
					{
						error("Vul a.u.b. een geldig getal in.");
					}
				}
			}
		}
	}
}
