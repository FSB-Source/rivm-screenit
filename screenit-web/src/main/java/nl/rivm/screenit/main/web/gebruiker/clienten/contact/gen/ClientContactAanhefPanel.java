package nl.rivm.screenit.main.web.gebruiker.clienten.contact.gen;

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

import java.util.EnumMap;
import java.util.List;
import java.util.Map;

import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.main.web.gebruiker.clienten.contact.AbstractClientContactActiePanel;
import nl.rivm.screenit.model.Aanhef;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.ClientContactActie;
import nl.rivm.screenit.model.enums.ExtraOpslaanKey;
import nl.rivm.screenit.util.NaamUtil;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.markup.html.form.EnumChoiceRenderer;
import org.apache.wicket.markup.html.form.RadioChoice;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;

@Slf4j
public class ClientContactAanhefPanel extends AbstractClientContactActiePanel<ClientContactActie>
{

	private IModel<Aanhef> aanhefModel;

	public ClientContactAanhefPanel(String id, IModel<ClientContactActie> model, IModel<Client> client, List<Object> extraPanelParams)
	{
		super(id, model);
		aanhefModel = Model.of(Aanhef.bepaalJuisteAanhef(client.getObject().getPersoon()));
		RadioChoice<Aanhef> aanhefRadioChoice = new RadioChoice<>("aanhef", aanhefModel, Aanhef.aanhefVormenClienten(), new EnumChoiceRenderer<>()
		{
			@Override
			public Object getDisplayValue(Aanhef aanhef)
			{
				Client voorbeeld = client.getObject();
				voorbeeld.getPersoon().setAanhef(aanhef);
				return NaamUtil.getGewensteAanspreekVorm(voorbeeld);
			}
		});
		aanhefRadioChoice.setPrefix("<label class=\"radio\">");
		aanhefRadioChoice.setSuffix("</label>");
		aanhefRadioChoice.setOutputMarkupId(true);
		add(aanhefRadioChoice);
	}

	private Aanhef getAanhef()
	{
		return ModelUtil.nullSafeGet(aanhefModel);
	}

	@Override
	public Map<ExtraOpslaanKey, Object> getOpslaanObjecten()
	{
		Map<ExtraOpslaanKey, Object> opslaanObjecten = new EnumMap<>(ExtraOpslaanKey.class);
		opslaanObjecten.put(ExtraOpslaanKey.AANHEF, getAanhef());
		return opslaanObjecten;
	}

	@Override
	protected void onDetach()
	{
		super.onDetach();
		ModelUtil.nullSafeDetach(aanhefModel);
	}
}
