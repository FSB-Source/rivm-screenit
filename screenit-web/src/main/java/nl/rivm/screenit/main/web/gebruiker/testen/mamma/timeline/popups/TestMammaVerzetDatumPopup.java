package nl.rivm.screenit.main.web.gebruiker.testen.mamma.timeline.popups;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
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

import java.util.ArrayList;
import java.util.List;

import nl.rivm.screenit.main.web.gebruiker.testen.gedeeld.timeline.popups.TestAbstractVerzetDatumPopup;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.service.mamma.MammaBaseTestTimelineTimeService;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;

import org.apache.wicket.model.IModel;
import org.apache.wicket.spring.injection.annot.SpringBean;

public class TestMammaVerzetDatumPopup extends TestAbstractVerzetDatumPopup
{

	@SpringBean
	private MammaBaseTestTimelineTimeService baseTestTimelineTimeService;

	@SpringBean
	private HibernateService hibernateService;

	public TestMammaVerzetDatumPopup(String id, IModel<List<Client>> clientModel)
	{
		super(id, clientModel);
	}

	private List<Client> getReloadClienten()
	{
		List<Client> reloadedClienten = new ArrayList<>();
		for (Client client : getModelObject())
		{
			reloadedClienten.add(hibernateService.load(Client.class, client.getId()));
		}
		return reloadedClienten;
	}

	@Override
	protected void opslaan()
	{
		for (Client client : getReloadClienten())
		{
			baseTestTimelineTimeService.rekenDossierTerug(client.getMammaDossier(), getAantalDagen());
		}
	}
}
