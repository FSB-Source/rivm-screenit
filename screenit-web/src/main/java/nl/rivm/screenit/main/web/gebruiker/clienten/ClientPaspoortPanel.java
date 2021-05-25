
package nl.rivm.screenit.main.web.gebruiker.clienten;

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

import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.gebruiker.gedeeld.MammaDoelgroepIndicatorPanel;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.GbaPersoon;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.model.mamma.MammaDossier;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.util.AdresUtil;
import nl.rivm.screenit.util.DateUtil;
import nl.rivm.screenit.util.NaamUtil;

import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.panel.GenericPanel;
import org.apache.wicket.model.CompoundPropertyModel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.spring.injection.annot.SpringBean;

public class ClientPaspoortPanel extends GenericPanel<Client>
{
	private static final long serialVersionUID = 1L;

	@SpringBean
	private ICurrentDateSupplier dateSupplier;

	public ClientPaspoortPanel(String id, IModel<Client> model)
	{
		super(id, new CompoundPropertyModel<>(model));

		Client client = getModelObject();
		GbaPersoon persoon = client.getPersoon();

		add(new Label("persoon.voornaam"));
		add(new Label("persoon.achternaam", NaamUtil.titelVoorlettersTussenvoegselEnAanspreekAchternaam(client)));
		add(new Label("persoon.bsn"));
		add(new Label("persoon.geslacht"));
		add(new Label("geboortenaam", NaamUtil.getGeboorteTussenvoegselEnAchternaam(persoon)));

		add(new Label("persoon.geboortedatum", DateUtil.getGeboortedatum(persoon)));
		add(new Label("persoon.overlijdensdatum").setVisible(persoon.getOverlijdensdatum() != null));
		add(new Label("gbaLocatiebeschrijving", AdresUtil.getAdres(persoon.getGbaAdres())));
		add(new Label("persoon.gbaAdres.postcode"));
		add(new Label("persoon.gbaAdres.plaats"));
		if (AdresUtil.isTijdelijkAdres(persoon, dateSupplier.getDateTime()))
		{
			add(new Label("tijdelijkadres", getString("message.letop.tijdelijkadres")));
		}
		else
		{
			add(new Label("tijdelijkadres", Model.of("Nee")));
		}
		add(new Label("persoon.telefoonnummer1"));
		add(new Label("persoon.telefoonnummer2"));
		add(new Label("mammaDossier.dubbeleTijdReden")
		{
			private static final long serialVersionUID = 1L;

			@Override
			protected void onConfigure()
			{
				super.onConfigure();
				MammaDossier mammaDossier = ClientPaspoortPanel.this.getModelObject().getMammaDossier();
				setVisible(mammaDossier != null && mammaDossier.getDubbeleTijdReden() != null &&
					(ScreenitSession.get().checkPermission(Recht.GEBRUIKER_SCREENING_MAMMA_DOSSIERGEGEVENS, Actie.INZIEN)
						|| ScreenitSession.get().checkPermission(Recht.GEBRUIKER_SCREENING_MAMMA_AFSPRAKEN_BEHEER, Actie.INZIEN)
						|| ScreenitSession.get().checkPermission(Recht.GEBRUIKER_SCREENING_MAMMA_TEHUIS, Actie.INZIEN)));
			}
		});
		add(new MammaDoelgroepIndicatorPanel("doelgroep", model.getObject().getMammaDossier(), true));
	}
}
