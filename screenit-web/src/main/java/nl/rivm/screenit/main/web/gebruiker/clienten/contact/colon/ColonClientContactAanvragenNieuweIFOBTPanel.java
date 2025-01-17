
package nl.rivm.screenit.main.web.gebruiker.clienten.contact.colon;

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

import java.util.Date;
import java.util.List;

import nl.rivm.screenit.main.model.TypeGebeurtenis;
import nl.rivm.screenit.main.web.gebruiker.clienten.contact.AbstractClientContactActiePanel;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.ClientContactActie;
import nl.rivm.screenit.model.colon.ColonDossier;
import nl.rivm.screenit.model.colon.ColonScreeningRonde;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.GbaStatus;

import org.wicketstuff.datetime.markup.html.basic.DateLabel;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.basic.EnumLabel;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;

public class ColonClientContactAanvragenNieuweIFOBTPanel extends AbstractClientContactActiePanel<ClientContactActie>
{

	private static final long serialVersionUID = 1L;

	public ColonClientContactAanvragenNieuweIFOBTPanel(String id, IModel<ClientContactActie> model, IModel<Client> client, List<Object> extraPanelParams)
	{
		super(id, model);

		Bevolkingsonderzoek type = null;
		Date verstuurd = null;
		int rondeNr = 0;
		ColonDossier colonDossier = client.getObject().getColonDossier();
		ColonScreeningRonde laatsteScreeningRonde = colonDossier.getLaatsteScreeningRonde();
		if (laatsteScreeningRonde != null && laatsteScreeningRonde.getLaatsteUitnodiging() != null)
		{
			verstuurd = laatsteScreeningRonde.getLaatsteUitnodiging().getVerstuurdDatum();
		}
		type = laatsteScreeningRonde.getBevolkingsonderzoek();
		rondeNr = colonDossier.getScreeningRondes().size();

		add(new EnumLabel<Bevolkingsonderzoek>("type", type));
		add(DateLabel.forDatePattern("datum", Model.of(verstuurd), "dd-MM-yyyy HH:mm:ss"));
		add(new EnumLabel<TypeGebeurtenis>("gebeurtenis", TypeGebeurtenis.UITNODIGING));
		add(new WebMarkupContainer("gbaMessageContainer").setVisible(!GbaStatus.INDICATIE_AANWEZIG.equals(client.getObject().getGbaStatus())));
		add(new Label("rondeNr", rondeNr));
	}

}
