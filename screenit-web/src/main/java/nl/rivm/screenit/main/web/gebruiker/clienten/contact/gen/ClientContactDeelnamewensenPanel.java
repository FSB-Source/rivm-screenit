package nl.rivm.screenit.main.web.gebruiker.clienten.contact.gen;

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

import java.util.EnumMap;
import java.util.List;
import java.util.Map;

import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.dto.alg.client.contact.DeelnamewensDto;
import nl.rivm.screenit.main.service.algemeen.DeelnamemodusService;
import nl.rivm.screenit.main.web.component.ComponentHelper;
import nl.rivm.screenit.main.web.gebruiker.clienten.contact.AbstractClientContactActiePanel;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.ClientContactActie;
import nl.rivm.screenit.model.enums.ExtraOpslaanKey;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.model.CompoundPropertyModel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.spring.injection.annot.SpringBean;

@Slf4j
public class ClientContactDeelnamewensenPanel extends AbstractClientContactActiePanel<ClientContactActie>
{
	@SpringBean
	private DeelnamemodusService deelnamemodusService;

	private final IModel<Client> clientModel;

	private final CompoundPropertyModel<DeelnamewensDto> deelnamewensDtoModel;

	public ClientContactDeelnamewensenPanel(String id, IModel<ClientContactActie> model, IModel<Client> client, List<Object> extraPanelParams)
	{
		super(id, model);
		this.clientModel = client;
		deelnamewensDtoModel = new CompoundPropertyModel<>(deelnamemodusService.getDeelnamewensDto(getClient()));

		var container = new WebMarkupContainer("container", deelnamewensDtoModel);
		add(container);
		container.add(ComponentHelper.newCheckBox("deelnamewensBmhk", !getDeelnamewensDto().isDeelnamewensBmhk()));
		container.add(ComponentHelper.newCheckBox("deelnamewensBk", !getDeelnamewensDto().isDeelnamewensBk()));
	}

	private DeelnamewensDto getDeelnamewensDto()
	{
		return ModelUtil.nullSafeGet(deelnamewensDtoModel);
	}

	@Override
	public Map<ExtraOpslaanKey, Object> getOpslaanObjecten()
	{
		Map<ExtraOpslaanKey, Object> opslaanObjecten = new EnumMap<>(ExtraOpslaanKey.class);
		opslaanObjecten.put(ExtraOpslaanKey.DEELNAMEWENSEN, getDeelnamewensDto());
		return opslaanObjecten;
	}

	@Override
	public void validate()
	{
		if (!deelnamemodusService.heeftNieuweDeelnamewensGeselecteerd(getClient(), getDeelnamewensDto()))
		{
			error(getString("error.geen.deelnamewens.geselecteerd"));
		}
	}

	private Client getClient()
	{
		return clientModel.getObject();
	}

	@Override
	protected void onDetach()
	{
		super.onDetach();
		ModelUtil.nullSafeDetach(clientModel);
	}
}
