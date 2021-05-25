package nl.rivm.screenit.main.web.gebruiker.algemeen.organisatie.screeningorganisatie;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
 * %%
 * Copyright (C) 2012 - 2021 Facilitaire Samenwerking Bevolkingsonderzoek
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

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

import nl.rivm.screenit.main.service.mamma.MammaScreeningsEenheidService;
import nl.rivm.screenit.model.BeoordelingsEenheid;
import nl.rivm.screenit.model.CentraleEenheid;
import nl.rivm.screenit.model.Instelling;
import nl.rivm.screenit.model.ScreeningOrganisatie;
import nl.rivm.screenit.service.InstellingService;

import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.list.ListItem;
import org.apache.wicket.markup.html.list.ListView;
import org.apache.wicket.markup.html.panel.GenericPanel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.spring.injection.annot.SpringBean;

public class GekoppeldeCeBeSePanel extends GenericPanel<Instelling>
{
	private static final long serialVersionUID = 1L;

	@SpringBean
	private InstellingService instellingService;

	@SpringBean
	private MammaScreeningsEenheidService screeningsEenheidService;

	private List<GekoppeldeCeBeSeRij> rijen = new ArrayList<>();

	GekoppeldeCeBeSePanel(String id, IModel<Instelling> model)
	{
		super(id, model);

		ScreeningOrganisatie regio = (ScreeningOrganisatie) model.getObject();
		vulRijen(regio);

		add(new ListView<GekoppeldeCeBeSeRij>("ceLijst", rijen)
		{
			protected void populateItem(ListItem<GekoppeldeCeBeSeRij> item)
			{
				GekoppeldeCeBeSeRij rij = item.getModelObject();
				item.add(new Label("centraleEenheid", rij.centraleEenheid));
				item.add(new Label("beoordelingseenheid", rij.beoordelingsEenheid));
				item.add(new Label("seLijst", rij.screeningsEenheden));
			}
		});
	}

	private void vulRijen(ScreeningOrganisatie regio)
	{
		List<CentraleEenheid> centraleEenheden = instellingService.getActieveCentraleEenhedenBinnenRegio(regio);
		List<BeoordelingsEenheid> regioBeoordelingseenheden = instellingService.getActieveBeoordelingseenhedenBinnenRegio(regio);

		for (CentraleEenheid centraleEenheid : centraleEenheden)
		{
			addRijenVoorCentraleEenheid(regioBeoordelingseenheden, centraleEenheid);
		}
	}

	private void addRijenVoorCentraleEenheid(List<BeoordelingsEenheid> regioBeoordelingseenheden, CentraleEenheid centraleEenheid)
	{
		List<BeoordelingsEenheid> ceBeoordelingseenheden = filterCeBeoordelingseenheden(regioBeoordelingseenheden, centraleEenheid);

		if (ceBeoordelingseenheden.isEmpty())
		{
			addRijZonderBeoordelingsEenheden(centraleEenheid);
		}
		else
		{
			addRijPerBeoordelingseenheid(centraleEenheid, ceBeoordelingseenheden);
		}
	}

	private List<BeoordelingsEenheid> filterCeBeoordelingseenheden(List<BeoordelingsEenheid> regioBeoordelingsEenheden, CentraleEenheid centraleEenheid)
	{
		return regioBeoordelingsEenheden.stream().filter(be -> centraleEenheid.equals(be.getParent())).collect(Collectors.toList());
	}

	private void addRijZonderBeoordelingsEenheden(CentraleEenheid centraleEenheid)
	{
		GekoppeldeCeBeSeRij rij = new GekoppeldeCeBeSeRij();
		rij.centraleEenheid = centraleEenheid.getNaam();
		rij.beoordelingsEenheid = "Geen beoordelingseenheden gekoppeld";
		rijen.add(rij);
	}

	private void addRijPerBeoordelingseenheid(CentraleEenheid centraleEenheid, List<BeoordelingsEenheid> ceBeoordelingsEenheden)
	{
		for (BeoordelingsEenheid beoordelingsEenheid : ceBeoordelingsEenheden)
		{
			GekoppeldeCeBeSeRij rij = new GekoppeldeCeBeSeRij();
			rij.centraleEenheid = centraleEenheid.getNaam();
			rij.beoordelingsEenheid = beoordelingsEenheid.getNaam();
			rij.screeningsEenheden = screeningsEenheidService.getScreeningsEenhedenNamen(beoordelingsEenheid);
			rijen.add(rij);
		}
	}

	private static class GekoppeldeCeBeSeRij implements Serializable
	{
		private static final long serialVersionUID = 1L;

		public String centraleEenheid;

		public String beoordelingsEenheid;

		public String screeningsEenheden;
	}
}
