
package nl.rivm.screenit.main.web.gebruiker.screening.mamma.planning;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
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
import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.ScreeningOrganisatie;
import nl.rivm.screenit.service.mamma.MammaBaseConceptPlanningsApplicatie;
import nl.rivm.screenit.util.NaamUtil;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;

import org.apache.wicket.behavior.AttributeAppender;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.panel.Panel;
import org.apache.wicket.spring.injection.annot.SpringBean;

public class MammaPlanningTabExtensiePanel extends Panel
{

	private static final long serialVersionUID = 1L;

	@SpringBean
	private MammaBaseConceptPlanningsApplicatie conceptPlanningsApplicatie;

	@SpringBean
	private HibernateService hibernateService;

	public MammaPlanningTabExtensiePanel(String id)
	{
		super(id);

		ScreeningOrganisatie screeningOrganisatie = ScreenitSession.get().getScreeningOrganisatie();
		if (screeningOrganisatie != null)
		{
			List<Long> conceptGewijzigdDoor = new ArrayList<>(Arrays.asList(conceptPlanningsApplicatie.getConceptGewijzigdDoor(screeningOrganisatie)));
			if (conceptGewijzigdDoor.remove(ScreenitSession.get().getLoggedInInstellingGebruiker().getId()))
			{
				conceptGewijzigdDoor.add(ScreenitSession.get().getLoggedInInstellingGebruiker().getId());
			}
			List<InstellingGebruiker> conceptGewijzigdDoorInstellingGebruikers = new ArrayList<>();
			for (Long conceptGewijzigdDoorId : conceptGewijzigdDoor)
			{
				conceptGewijzigdDoorInstellingGebruikers.add(hibernateService.load(InstellingGebruiker.class, conceptGewijzigdDoorId));
			}
			if (!conceptGewijzigdDoorInstellingGebruikers.isEmpty())
			{
				InstellingGebruiker eersteInstellingGebruiker = conceptGewijzigdDoorInstellingGebruikers.get(0);
				String naamGebruiker = NaamUtil.getNaamGebruiker(eersteInstellingGebruiker.getMedewerker());
				if (conceptGewijzigdDoorInstellingGebruikers.size() > 1)
				{
					naamGebruiker += " ...";
				}
				Label label = new Label("gewijzigdDoor", naamGebruiker);
				add(label);
				conceptGewijzigdDoorInstellingGebruikers.remove(eersteInstellingGebruiker);
				if (!conceptGewijzigdDoorInstellingGebruikers.isEmpty())
				{
					label.add(new AttributeAppender("title", "... " + conceptGewijzigdDoorInstellingGebruikers.stream()
						.map(ig -> NaamUtil.getNaamGebruiker(ig.getMedewerker())).collect(Collectors.joining(", "))));
				}
				return;
			}
		}
		add(new Label("gewijzigdDoor", "").setVisible(false));
	}

}
