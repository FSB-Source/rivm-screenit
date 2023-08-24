
package nl.rivm.screenit.main.web.gebruiker.clienten.contact.cervix;

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

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Map;

import nl.rivm.screenit.model.enums.ExtraOpslaanKey;
import nl.rivm.screenit.main.web.gebruiker.clienten.contact.AbstractClientContactAfmeldenPanel;
import nl.rivm.screenit.model.AfmeldingType;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.ClientContactActie;
import nl.rivm.screenit.model.ClientContactManier;
import nl.rivm.screenit.model.cervix.CervixAfmelding;
import nl.rivm.screenit.model.cervix.enums.CervixAfmeldingReden;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.util.ListModel;

public class CervixClientContactAfmeldenPanel extends AbstractClientContactAfmeldenPanel<CervixAfmelding, CervixAfmeldingReden>
{

	private static final long serialVersionUID = 1;

	public CervixClientContactAfmeldenPanel(String id, IModel<ClientContactActie> model, IModel<Client> client, List<Object> extraPanelParams)
	{
		super(id, model, client, extraPanelParams);
	}

	@Override
	protected IModel<CervixAfmelding> getAfmeldingModel(List<Object> extraPanelParams)
	{
		IModel<CervixAfmelding> afmeldingModel = ModelUtil.cModel(new CervixAfmelding());
		CervixAfmelding afmelding = afmeldingModel.getObject();
		afmelding.setReden(CervixAfmeldingReden.ANDERS);
		return afmeldingModel;
	}

	@Override
	protected void setRedenenContainerVisible(AjaxRequestTarget target, CervixAfmelding afmelding)
	{
		redenenContainer.setVisible(AfmeldingType.DEFINITIEF.equals(afmelding.getType()) && ClientContactManier.DIRECT.equals(afmelding.getManier()));
		target.add(redenenContainer);
	}

	@Override
	protected List<AfmeldingType> getAvailableAfmeldopties(IModel<Client> clientModel)
	{
		Client client = clientModel.getObject();

		return clientContactService.getAvailableAfmeldoptiesCervix(client, false);
	}

	@Override
	public Map<ExtraOpslaanKey, Object> getOpslaanObjecten()
	{
		CervixAfmelding afmelding = this.afmeldingModel.getObject();
		if (afmelding.getType() == AfmeldingType.EENMALIG)
		{
			afmelding.setReden(null);
		}

		return super.getOpslaanObjecten();
	}

	@Override
	protected IModel<List<CervixAfmeldingReden>> getRedenenModel()
	{
		return new ListModel<>(new ArrayList<>(Arrays.asList(CervixAfmeldingReden.values())));
	}
}
