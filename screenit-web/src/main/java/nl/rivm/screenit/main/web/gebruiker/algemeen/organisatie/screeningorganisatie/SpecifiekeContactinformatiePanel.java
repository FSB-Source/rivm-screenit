package nl.rivm.screenit.main.web.gebruiker.algemeen.organisatie.screeningorganisatie;

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

import java.util.Arrays;
import java.util.List;

import nl.rivm.screenit.main.web.component.ComponentHelper;
import nl.rivm.screenit.model.Instelling;
import nl.rivm.screenit.model.OrganisatieType;
import nl.rivm.screenit.model.RegioBvoContactGegevens;
import nl.rivm.screenit.model.ScreeningOrganisatie;
import nl.topicuszorg.organisatie.model.Adres;

import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.panel.GenericPanel;
import org.apache.wicket.model.IModel;
import nl.rivm.screenit.main.web.component.validator.EmailAddressValidator;

public class SpecifiekeContactinformatiePanel extends GenericPanel<Instelling>
{

	private static final long serialVersionUID = 1L;

	private List<String> screeningOrganisatieBvoObject = Arrays.asList(new String[] { "Dk", "Bmhk" });

	private boolean isSO;

	public SpecifiekeContactinformatiePanel(String id, IModel<Instelling> model, boolean inzien)
	{
		super(id, model);
		isSO = model.getObject().getOrganisatieType().equals(OrganisatieType.SCREENINGSORGANISATIE);
		ScreeningOrganisatie screeningOrganisatie = (ScreeningOrganisatie) model.getObject();

		if (screeningOrganisatie.getRegioBvoContactGegevensDk() == null)
		{
			screeningOrganisatie.setRegioBvoContactGegevensDk(new RegioBvoContactGegevens());
		}
		if (screeningOrganisatie.getRegioBvoContactGegevensBmhk() == null)
		{
			screeningOrganisatie.setRegioBvoContactGegevensBmhk(new RegioBvoContactGegevens());
		}

		if (screeningOrganisatie.getRegioBvoContactGegevensDk().getAntwoordnummerAdres() == null)
		{
			screeningOrganisatie.getRegioBvoContactGegevensDk().setAntwoordnummerAdres(new Adres());
		}
		if (screeningOrganisatie.getRegioBvoContactGegevensBmhk().getAntwoordnummerAdres() == null)
		{
			screeningOrganisatie.getRegioBvoContactGegevensBmhk().setAntwoordnummerAdres(new Adres());
		}

		if (screeningOrganisatie.getRegioBvoContactGegevensDk().getPostbusnummerAdres() == null)
		{
			screeningOrganisatie.getRegioBvoContactGegevensDk().setPostbusnummerAdres(new Adres());
		}
		if (screeningOrganisatie.getRegioBvoContactGegevensBmhk().getPostbusnummerAdres() == null)
		{
			screeningOrganisatie.getRegioBvoContactGegevensBmhk().setPostbusnummerAdres(new Adres());
		}

		for (String bvo : screeningOrganisatieBvoObject)
		{
			ComponentHelper.addTextField(this, "regioBvoContactGegevens" + bvo + ".telefoon", isSO, 20, inzien);

			ComponentHelper.addTextField(this, "regioBvoContactGegevens" + bvo + ".email", isSO, 100, inzien).add(EmailAddressValidator.getInstance());

			ComponentHelper.addTextField(this, "regioBvoContactGegevens" + bvo + ".postbusnummerAdres.huisnummer", isSO, 10, Integer.class, inzien);

			ComponentHelper.newPostcodeTextField(this, "regioBvoContactGegevens" + bvo + ".postbusnummerAdres.postcode", isSO, inzien);

			ComponentHelper.addTextField(this, "regioBvoContactGegevens" + bvo + ".postbusnummerAdres.plaats", isSO, 200, inzien);

			ComponentHelper.addTextField(this, "regioBvoContactGegevens" + bvo + ".antwoordnummerAdres.huisnummer", isSO, 10, Integer.class, inzien);

			ComponentHelper.newPostcodeTextField(this, "regioBvoContactGegevens" + bvo + ".antwoordnummerAdres.postcode", isSO, inzien);

			ComponentHelper.addTextField(this, "regioBvoContactGegevens" + bvo + ".antwoordnummerAdres.plaats", isSO, 200, inzien);

			WebMarkupContainer clientPortaalVrijeTekstContainer = new WebMarkupContainer("clientPortaalVrijeTekstContainer" + bvo);
			clientPortaalVrijeTekstContainer.setOutputMarkupId(true);
			ComponentHelper.addTextArea(clientPortaalVrijeTekstContainer, "regioBvoContactGegevens" + bvo + ".clientPortaalVrijeTekst", isSO, 256, inzien);
			add(clientPortaalVrijeTekstContainer);
		}
	}
}
