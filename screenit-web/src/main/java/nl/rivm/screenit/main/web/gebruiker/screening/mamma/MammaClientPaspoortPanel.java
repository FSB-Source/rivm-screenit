package nl.rivm.screenit.main.web.gebruiker.screening.mamma;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
 * %%
 * Copyright (C) 2012 - 2024 Facilitaire Samenwerking Bevolkingsonderzoek
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
import nl.rivm.screenit.model.mamma.MammaDossier;
import nl.rivm.screenit.model.mamma.MammaScreeningRonde;
import nl.rivm.screenit.model.mamma.enums.MammaDoelgroep;
import nl.rivm.screenit.service.mamma.MammaBaseDossierService;
import nl.rivm.screenit.util.AdresUtil;
import nl.rivm.screenit.util.DateUtil;
import nl.rivm.screenit.util.NaamUtil;

import org.apache.wicket.markup.head.CssHeaderItem;
import org.apache.wicket.markup.head.IHeaderResponse;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.panel.EmptyPanel;
import org.apache.wicket.markup.html.panel.GenericPanel;
import org.apache.wicket.model.CompoundPropertyModel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.spring.injection.annot.SpringBean;

public class MammaClientPaspoortPanel extends GenericPanel<MammaScreeningRonde>
{

	@SpringBean
	private MammaBaseDossierService dossierService;

	@Override
	public void renderHead(IHeaderResponse response)
	{
		super.renderHead(response);
		response.render(CssHeaderItem.forUrl("assets/font-awesome/css/font-awesome.min.css"));
	}

	public MammaClientPaspoortPanel(String id, IModel<MammaScreeningRonde> screeningRondeModel, boolean anoniem)
	{
		super(id, new CompoundPropertyModel<>(screeningRondeModel));

		MammaScreeningRonde screeningRonde = getModelObject();
		MammaDossier dossier = screeningRonde.getDossier();
		Client client = dossier.getClient();
		add(new Label("persoon.naam", anoniem ? "Anoniem" : NaamUtil.titelVoorlettersTussenvoegselEnAanspreekAchternaam(client)));
		add(new Label("uitnodigingsNr"));
		add(new Label("dossier.client.persoon.bsn").setVisible(!anoniem));
		add(new EmptyPanel("rolstoelClient").setVisible(dossier.getDoelgroep().equals(MammaDoelgroep.MINDER_VALIDE)));
		add(new Label("mammaDossier.uitgenodigd", dossierService.aantalOproepen(dossier)));
		add(new Label("mammaDossier.onderzocht", dossierService.aantalOpgekomenBE(dossier)));

		GbaPersoon persoon = client.getPersoon();
		add(new Label("dossier.client.persoon.geboortedatum", DateUtil.getGeboortedatum(persoon)));
		add(new Label("gbaLocatiebeschrijving", AdresUtil.getAdres(persoon.getGbaAdres())));
		add(new Label("dossier.client.persoon.gbaAdres.postcode"));
		add(new Label("dossier.client.persoon.gbaAdres.plaats"));
	}
}
