
package nl.rivm.screenit.main.web.gebruiker.algemeen.medewerker;

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

import java.util.List;

import nl.rivm.screenit.model.Gebruiker;
import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.service.InstellingService;

import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.panel.GenericPanel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.spring.injection.annot.SpringBean;

public class MedewerkerPaspoortPanel extends GenericPanel<Gebruiker>
{

	private static final long serialVersionUID = 1L;

	@SpringBean
	private InstellingService instellingService;

	public MedewerkerPaspoortPanel(String id, IModel<Gebruiker> model)
	{
		super(id, model);
		add(new Label("voornaam"));
		add(new Label("achternaamVolledig"));
		add(new Label("functie.naam"));
		String instellingen = "";
		Gebruiker medewerker = model.getObject();
		List<InstellingGebruiker> instellingMedewerkers = instellingService.getActieveInstellingGebruikers(medewerker);
		boolean first = true;
		for (InstellingGebruiker instellingMedewerker : instellingMedewerkers)
		{
			if (!first)
			{
				instellingen += ", ";
			}
			instellingen += instellingMedewerker.getOrganisatie().getNaam();
			first = false;
		}

		add(new Label("organisaties", instellingen));
	}
}
