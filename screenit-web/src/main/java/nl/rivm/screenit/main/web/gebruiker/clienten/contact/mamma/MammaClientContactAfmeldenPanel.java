
package nl.rivm.screenit.main.web.gebruiker.clienten.contact.mamma;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
 * %%
 * Copyright (C) 2012 - 2020 Facilitaire Samenwerking Bevolkingsonderzoek
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
import java.util.List;

import nl.rivm.screenit.main.web.gebruiker.clienten.contact.AbstractClientContactAfmeldenPanel;
import nl.rivm.screenit.model.AanvraagBriefStatus;
import nl.rivm.screenit.model.AfmeldingType;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.ClientContactActie;
import nl.rivm.screenit.model.ClientContactManier;
import nl.rivm.screenit.model.mamma.MammaAfmelding;
import nl.rivm.screenit.model.mamma.enums.MammaAfmeldingReden;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.model.IModel;

public class MammaClientContactAfmeldenPanel extends AbstractClientContactAfmeldenPanel<MammaAfmelding, MammaAfmeldingReden>
{
	private static final long serialVersionUID = 1;

	public MammaClientContactAfmeldenPanel(String id, IModel<ClientContactActie> model, IModel<Client> client, List<Object> extraPanelParams)
	{
		super(id, model, client, extraPanelParams);
	}

	@Override
	protected IModel<MammaAfmelding> getAfmeldingModel(List<Object> extraPanelParams)
	{
		IModel<MammaAfmelding> afmeldingModel = ModelUtil.cModel(new MammaAfmelding());
		MammaAfmelding afmelding = afmeldingModel.getObject();
		afmelding.setAfmeldingStatus((AanvraagBriefStatus) extraPanelParams.stream().filter(p -> p instanceof AanvraagBriefStatus).findFirst().orElse(null));
		return afmeldingModel;
	}

	@Override
	protected void setRedenenContainerVisible(AjaxRequestTarget target, MammaAfmelding afmelding)
	{
		boolean voorwaardeBijEenmalig = AfmeldingType.EENMALIG.equals(afmelding.getType());
		boolean voorwaardeBijDefinitief = AfmeldingType.DEFINITIEF.equals(afmelding.getType()) && ClientContactManier.DIRECT.equals(afmelding.getManier());
		redenenContainer.setVisible(voorwaardeBijEenmalig || voorwaardeBijDefinitief);
		target.add(redenenContainer);

	}

	@Override
	protected IModel<List<MammaAfmeldingReden>> getRedenenModel()
	{
		return new IModel<List<MammaAfmeldingReden>>()
		{

			private static final long serialVersionUID = 1L;

			@Override
			public List<MammaAfmeldingReden> getObject()
			{
				if (AfmeldingType.EENMALIG.equals(afmeldingModel.getObject().getType()))
				{
					return MammaAfmeldingReden.eenmaligeRedenen();
				}
				else if (AfmeldingType.DEFINITIEF.equals(afmeldingModel.getObject().getType()))
				{
					return MammaAfmeldingReden.definitieveRedenen();
				}
				else
				{
					return new ArrayList<>();
				}
			}

		};
	}

	@Override
	public List<String> getOpslaanMeldingen()
	{
		List<String> meldingen = super.getOpslaanMeldingen();
		if (clientContactService.heeftOpenMammaAfspraak(clientModel.getObject()))
		{
			meldingen.add("client heeft een mammografie afspraak. Deze wordt direct geannuleerd.");
		}
		return meldingen;
	}

	@Override
	protected List<AfmeldingType> getAvailableAfmeldopties(IModel<Client> client)
	{
		return clientContactService.getAvailableAfmeldoptiesMamma(client.getObject(), false);
	}
}
