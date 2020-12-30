
package nl.rivm.screenit.main.web.client.dashboard.colon;

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
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import nl.rivm.screenit.main.service.ClientContactService;
import nl.rivm.screenit.main.service.ExtraOpslaanKey;
import nl.rivm.screenit.main.web.gebruiker.clienten.contact.AbstractClientContactActiePanel;
import nl.rivm.screenit.model.AanvraagBriefStatus;
import nl.rivm.screenit.model.AfmeldingType;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.ClientContactActie;
import nl.rivm.screenit.model.colon.ColonAfmelding;
import nl.rivm.screenit.model.colon.enums.ColonAfmeldingReden;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.form.EnumChoiceRenderer;
import org.apache.wicket.markup.html.form.Radio;
import org.apache.wicket.markup.html.form.RadioChoice;
import org.apache.wicket.markup.html.form.RadioGroup;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.spring.injection.annot.SpringBean;

public class ClientColonAfmeldenPanel extends AbstractClientContactActiePanel<ClientContactActie>
{

	private static final long serialVersionUID = 1L;

	private IModel<ColonAfmelding> afmeldingModel;

	@SpringBean
	private ClientContactService clientContactService;

	private final IModel<Client> client;

	public ClientColonAfmeldenPanel(String id, IModel<ClientContactActie> model, IModel<Client> client, List<Object> extraPanelParams)
	{
		super(id, model);
		this.client = client;

		ColonAfmelding afmelding = new ColonAfmelding();
		afmelding.setReden(ColonAfmeldingReden.GEEN_REDEN);
		for (Object param : extraPanelParams)
		{
			if (param instanceof AanvraagBriefStatus)
			{
				afmelding.setAfmeldingStatus((AanvraagBriefStatus) param);
				break;
			}
		}
		afmeldingModel = ModelUtil.cModel(afmelding);
		WebMarkupContainer container = new WebMarkupContainer("container", afmeldingModel);
		add(container);

		WebMarkupContainer typeContainer = new WebMarkupContainer("typeContainer");
		typeContainer.setOutputMarkupId(true);
		container.add(typeContainer);

		List<AfmeldingType> listType = clientContactService.getAvailableAfmeldoptiesColon(client.getObject(), true);
		RadioGroup<AfmeldingType> type = new RadioGroup<>("type");
		Radio<AfmeldingType> eenmalig = new Radio<AfmeldingType>("eenmalig", Model.of(AfmeldingType.EENMALIG));
		type.add(eenmalig.setVisible(listType.contains(AfmeldingType.EENMALIG)));
		Radio<AfmeldingType> definitief = new Radio<AfmeldingType>("definitief", Model.of(AfmeldingType.DEFINITIEF));
		type.add(definitief.setVisible(listType.contains(AfmeldingType.DEFINITIEF)));
		type.setOutputMarkupId(true);
		type.setRequired(true);
		typeContainer.add(type);

		List<ColonAfmeldingReden> afmeldredenen = new ArrayList<ColonAfmeldingReden>(Arrays.asList(ColonAfmeldingReden.values()));
		afmeldredenen.remove(ColonAfmeldingReden.PROEF_BEVOLKINGSONDERZOEK);
		afmeldredenen.remove(ColonAfmeldingReden.ONTERECHT);
		RadioChoice<ColonAfmeldingReden> reden = new RadioChoice<ColonAfmeldingReden>("reden", afmeldredenen, new EnumChoiceRenderer<ColonAfmeldingReden>(this));
		reden.setPrefix("<label class=\"radio\">");
		reden.setSuffix("</label>");
		reden.setOutputMarkupId(true);
		container.add(reden);
	}

	@Override
	public Map<ExtraOpslaanKey, Object> getOpslaanObjecten()
	{
		Map<ExtraOpslaanKey, Object> objecten = new HashMap<>();
		ColonAfmelding afmelding = afmeldingModel.getObject();
		objecten.put(ExtraOpslaanKey.AFMELDING, afmelding);
		return objecten;
	}

	@Override
	public List<String> getOpslaanMeldingen()
	{
		List<String> meldingen = super.getOpslaanMeldingen();
		if (clientContactService.heeftOpenIntakeAfspraak(client.getObject()))
		{
			meldingen.add("U heeft een coloscopie intake afspraak. Deze wordt geannuleerd.");
		}
		return meldingen;
	}

	@Override
	protected void onDetach()
	{
		super.onDetach();
		ModelUtil.nullSafeDetach(afmeldingModel);
		ModelUtil.nullSafeDetach(client);
	}
}
