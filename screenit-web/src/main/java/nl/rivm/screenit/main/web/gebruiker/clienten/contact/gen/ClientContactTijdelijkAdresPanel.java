
package nl.rivm.screenit.main.web.gebruiker.clienten.contact.gen;

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

import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import nl.rivm.screenit.model.enums.ExtraOpslaanKey;
import nl.rivm.screenit.main.web.component.ComponentHelper;
import nl.rivm.screenit.main.web.gebruiker.clienten.contact.AbstractClientContactActiePanel;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.ClientContactActie;
import nl.rivm.screenit.model.GbaPersoon;
import nl.rivm.screenit.model.TijdelijkAdres;
import nl.rivm.screenit.util.DateUtil;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.model.IModel;
import org.apache.wicket.validation.validator.PatternValidator;

public class ClientContactTijdelijkAdresPanel extends AbstractClientContactActiePanel<ClientContactActie>
{

	private static final long serialVersionUID = 1L;

	private IModel<TijdelijkAdres> tijdelijkAdresModel;

	public ClientContactTijdelijkAdresPanel(String id, IModel<ClientContactActie> model, IModel<Client> client, List<Object> extraPanelParams)
	{
		super(id, model);
		GbaPersoon persoon = client.getObject().getPersoon();
		tijdelijkAdresModel = ModelUtil.cModel(new TijdelijkAdres());
		if (persoon.getTijdelijkAdres() != null)
		{
			tijdelijkAdresModel = ModelUtil.cModel(persoon.getTijdelijkAdres());
		}

		boolean inzien = false;
		WebMarkupContainer container = new WebMarkupContainer("container", tijdelijkAdresModel);
		add(container);
		ComponentHelper.addTextField(container, "straat", true, 43, inzien);
		ComponentHelper.addTextField(container, "huisnummer", true, 10, Integer.class, inzien);
		ComponentHelper.addTextField(container, "huisletter", false, 1, inzien).add(new PatternValidator(".*[a-zA-Z].*"));
		ComponentHelper.addTextField(container, "huisnummerToevoeging", false, 26, inzien);
		ComponentHelper.addTextField(container, "huisnummerAanduiding", false, 2, inzien);
		ComponentHelper.newPostcodeTextField(container, "postcode", true, inzien);
		ComponentHelper.addTextField(container, "plaats", true, 200, inzien);
		ComponentHelper.addTextField(container, "startDatum", true, 10, Date.class, inzien);
		ComponentHelper.addTextField(container, "eindDatum", false, 10, Date.class, inzien);
	}

	private TijdelijkAdres getTijdelijkAdres()
	{
		return ModelUtil.nullSafeGet(tijdelijkAdresModel);
	}

	@Override
	public Map<ExtraOpslaanKey, Object> getOpslaanObjecten()
	{
		Map<ExtraOpslaanKey, Object> opslaanObjecten = new HashMap<>();
		opslaanObjecten.put(ExtraOpslaanKey.TIJDELIJK_ADRES, getTijdelijkAdres());
		return opslaanObjecten;
	}

	@Override
	public void validate()
	{
		TijdelijkAdres tijdelijkAdres = getTijdelijkAdres();
		if (tijdelijkAdres != null && tijdelijkAdres.getStartDatum() != null && tijdelijkAdres.getEindDatum() != null
			&& DateUtil.compareBefore(tijdelijkAdres.getEindDatum(), tijdelijkAdres.getStartDatum()))
		{
			error(getString("error.eindatum.voor.begindatum"));
		}
	}

	@Override
	protected void onDetach()
	{
		super.onDetach();
		ModelUtil.nullSafeDetach(tijdelijkAdresModel);
	}
}
