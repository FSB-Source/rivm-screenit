
package nl.rivm.screenit.main.web.gebruiker.clienten.contact.colon;

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

import lombok.Getter;
import lombok.Setter;

import nl.rivm.screenit.main.web.gebruiker.clienten.contact.AbstractClientContactAfmeldenPanel;
import nl.rivm.screenit.model.AanvraagBriefStatus;
import nl.rivm.screenit.model.AfmeldingType;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.ClientContactActie;
import nl.rivm.screenit.model.ClientContactManier;
import nl.rivm.screenit.model.colon.ColonAfmelding;
import nl.rivm.screenit.model.colon.enums.ColonAfmeldingReden;
import nl.rivm.screenit.model.enums.ExtraOpslaanKey;
import nl.rivm.screenit.service.colon.ColonTijdelijkAfmeldenJaartallenService;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.form.RadioChoice;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.PropertyModel;
import org.apache.wicket.model.util.ListModel;
import org.apache.wicket.spring.injection.annot.SpringBean;

@Getter
@Setter
public class ColonClientContactAfmeldenPanel extends AbstractClientContactAfmeldenPanel<ColonAfmelding, ColonAfmeldingReden>
{
	private static final long serialVersionUID = 1;

	@SpringBean
	private ColonTijdelijkAfmeldenJaartallenService tijdelijkAfmeldenJaartallenService;

	private WebMarkupContainer tijdelijkAfmeldenJaartallenContainer;

	private Integer gekozenJaartalTijdelijkAfmelden;

	public ColonClientContactAfmeldenPanel(String id, IModel<ClientContactActie> model, IModel<Client> client, List<Object> extraPanelParams)
	{
		super(id, model, client, extraPanelParams);
	}

	private void setTijdelijkAfmeldenJaartalContainer(Client client)
	{
		var beschikbareJaartallenTijdelijkAfmelden = tijdelijkAfmeldenJaartallenService.bepaalMogelijkeAfmeldJaren(client);
		if (!beschikbareJaartallenTijdelijkAfmelden.isEmpty())
		{
			gekozenJaartalTijdelijkAfmelden = beschikbareJaartallenTijdelijkAfmelden.get(0);
		}

		RadioChoice<Integer> tijdelijkAfmeldenJaartallen = new RadioChoice<>("tijdelijkAfmeldenJaartallen", new PropertyModel<>(this, "gekozenJaartalTijdelijkAfmelden"),
			beschikbareJaartallenTijdelijkAfmelden);
		tijdelijkAfmeldenJaartallen.setPrefix("<label class=\"radio\">");
		tijdelijkAfmeldenJaartallen.setSuffix("</label>");
		tijdelijkAfmeldenJaartallen.setRequired(true);

		tijdelijkAfmeldenJaartallenContainer.setOutputMarkupPlaceholderTag(true);
		tijdelijkAfmeldenJaartallenContainer.add(tijdelijkAfmeldenJaartallen);
		tijdelijkAfmeldenJaartallenContainer.setVisible(false);
	}

	private void setTijdelijkAfmeldenJaartallenContainerVisible(AjaxRequestTarget target, ColonAfmelding afmelding)
	{
		boolean voorwaardeBijTijdelijk = AfmeldingType.TIJDELIJK.equals(afmelding.getType()) && ClientContactManier.DIRECT.equals(afmelding.getManier());
		tijdelijkAfmeldenJaartallenContainer.setVisible(voorwaardeBijTijdelijk);
		target.add(tijdelijkAfmeldenJaartallenContainer);
	}

	@Override
	protected WebMarkupContainer vulEnGetExtraAfmeldOptiesContainer(WebMarkupContainer extraOptiesContainer)
	{
		tijdelijkAfmeldenJaartallenContainer = extraOptiesContainer;
		setTijdelijkAfmeldenJaartalContainer(clientModel.getObject());

		return tijdelijkAfmeldenJaartallenContainer;
	}

	@Override
	protected IModel<ColonAfmelding> getAfmeldingModel(List<Object> extraPanelParams)
	{
		IModel<ColonAfmelding> afmeldingModel = ModelUtil.ccModel(new ColonAfmelding());
		ColonAfmelding afmelding = afmeldingModel.getObject();
		afmelding.setAfmeldingStatus((AanvraagBriefStatus) extraPanelParams.stream().filter(AanvraagBriefStatus.class::isInstance).findFirst().orElse(null));
		return afmeldingModel;
	}

	@Override
	protected void setRedenenContainerVisible(AjaxRequestTarget target, ColonAfmelding afmelding)
	{
		redenenContainer.setVisible(false);
		setTijdelijkAfmeldenJaartallenContainerVisible(target, afmelding);
		target.add(redenenContainer);
	}

	@Override
	protected IModel<List<ColonAfmeldingReden>> getRedenenModel()
	{
		List<ColonAfmeldingReden> afmeldredenen = new ArrayList<>(Arrays.asList(ColonAfmeldingReden.values()));
		afmeldredenen.remove(ColonAfmeldingReden.PROEF_BEVOLKINGSONDERZOEK);
		afmeldredenen.remove(ColonAfmeldingReden.ONTERECHT);

		return new ListModel<>(afmeldredenen);
	}

	@Override
	public List<String> getOpslaanMeldingen()
	{
		List<String> meldingen = super.getOpslaanMeldingen();
		if (clientContactService.heeftOpenIntakeAfspraak(clientModel.getObject()))
		{
			meldingen.add("Cliënt heeft een coloscopie intake afspraak. Deze wordt geannuleerd.");
		}
		return meldingen;
	}

	@Override
	protected List<AfmeldingType> getAvailableAfmeldopties(IModel<Client> client)
	{
		return clientContactService.getAvailableAfmeldoptiesColon(client.getObject(), false);
	}

	@Override
	public Map<ExtraOpslaanKey, Object> getOpslaanObjecten()
	{
		var obslaanObjecten = super.getOpslaanObjecten();
		if (gekozenJaartalTijdelijkAfmelden != null)
		{
			var afmelding = (ColonAfmelding) obslaanObjecten.get(ExtraOpslaanKey.AFMELDING);
			afmelding.setTijdelijkAfmeldenTotJaartal(gekozenJaartalTijdelijkAfmelden);
		}
		return obslaanObjecten;
	}
}
