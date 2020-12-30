package nl.rivm.screenit.main.web.client.dashboard;

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
import java.util.Map;

import nl.rivm.screenit.main.service.ExtraOpslaanKey;
import nl.rivm.screenit.main.web.component.bezwaar.edit.BezwaarCheckBox;
import nl.rivm.screenit.main.web.gebruiker.clienten.contact.AbstractClientContactActiePanel;
import nl.rivm.screenit.model.AanvraagBriefStatus;
import nl.rivm.screenit.model.BezwaarMoment;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.ClientContactActie;
import nl.rivm.screenit.model.ClientContactManier;
import nl.rivm.screenit.model.algemeen.BezwaarGroupViewWrapper;
import nl.rivm.screenit.model.algemeen.BezwaarViewWrapper;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.service.BezwaarService;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.markup.html.list.ListItem;
import org.apache.wicket.markup.html.list.ListView;
import org.apache.wicket.model.IModel;
import org.apache.wicket.spring.injection.annot.SpringBean;

public class ClientBaseBezwaarPanel extends AbstractClientContactActiePanel<ClientContactActie>
{
	private static final long serialVersionUID = 1L;

	@SpringBean
	private BezwaarService bezwaarService;

	@SpringBean
	private ICurrentDateSupplier currentDateSupplier;

	private IModel<BezwaarMoment> bezwaarModel;

	private IModel<Client> clientModel;

	private List<BezwaarGroupViewWrapper> wrappers;

	private List<BezwaarGroupViewWrapper> wrappersAndereBvos = new ArrayList<>();

	private Bevolkingsonderzoek bevolkingsonderzoek;

	public ClientBaseBezwaarPanel(String id, IModel<ClientContactActie> model, IModel<Client> clientModel, List<Object> extraPanelParams)
	{
		super(id, model);

		this.clientModel = clientModel;
		this.bezwaarModel = ModelUtil.cModel(new BezwaarMoment());
		BezwaarMoment modelObject = bezwaarModel.getObject();
		modelObject.setManier(ClientContactManier.DIRECT);
		modelObject.setClient(clientModel.getObject());
		modelObject.setStatus(AanvraagBriefStatus.BRIEF);
		modelObject.setStatusDatum(currentDateSupplier.getDate());

		BezwaarMoment laatsteVoltooideMoment = clientModel.getObject().getLaatstVoltooideBezwaarMoment();
		bevolkingsonderzoek = (Bevolkingsonderzoek) extraPanelParams.stream().filter(p -> p instanceof Bevolkingsonderzoek).findFirst().orElse(null);
		for (Bevolkingsonderzoek bvo : Bevolkingsonderzoek.values())
		{
			if (bevolkingsonderzoek == bvo)
			{
				wrappers = bezwaarService.getGroupWrapperForClientPortaal(laatsteVoltooideMoment, bevolkingsonderzoek);
			}
			else
			{
				wrappersAndereBvos.addAll(bezwaarService.getGroupWrapperForClientPortaal(laatsteVoltooideMoment, bvo));
			}
		}

		for (BezwaarGroupViewWrapper wrapper : wrappers)
		{
			if (wrapper.getBevolkingsonderzoek() == null)
			{
				ListView<BezwaarViewWrapper> bezwaren = new ListView<BezwaarViewWrapper>("algemeenBezwaren", wrapper.getBezwaren())
				{
					private static final long serialVersionUID = 1L;

					@Override
					protected void populateItem(ListItem<BezwaarViewWrapper> item)
					{
						item.add(new BezwaarCheckBox("bezwaarCheckBox", item.getModel()));
					}
				};
				add(bezwaren);
			}
			else
			{
				ListView<BezwaarViewWrapper> bezwaren = new ListView<BezwaarViewWrapper>("bvoBezwaren", wrapper.getBezwaren())
				{
					private static final long serialVersionUID = 1L;

					@Override
					protected void populateItem(ListItem<BezwaarViewWrapper> item)
					{
						item.add(new BezwaarCheckBox("bezwaarCheckBox", item.getModel()));
					}
				};
				add(bezwaren);
			}
		}
	}

	@Override
	public Map<ExtraOpslaanKey, Object> getOpslaanObjecten()
	{
		Map<ExtraOpslaanKey, Object> objecten = super.getOpslaanObjecten();
		for (BezwaarGroupViewWrapper wrapper : wrappersAndereBvos)
		{
			if (wrapper.getBevolkingsonderzoek() != null && wrapper.getBevolkingsonderzoek() != bevolkingsonderzoek)
			{
				wrappers.add(wrapper);
			}
		}
		objecten.put(ExtraOpslaanKey.BEZWAAR, bezwaarModel.getObject());
		objecten.put(ExtraOpslaanKey.BEZWAAR_WRAPPERS, wrappers);

		return objecten;
	}

	@Override
	public void validate()
	{
		if (!bezwarenGewijzigd())
		{
			error(getString("error.bezwaar.niet.gewijzigd"));
		}
	}

	private boolean bezwarenGewijzigd()
	{
		BezwaarMoment laatsteVoltooideBezwaarMoment = clientModel.getObject().getLaatstVoltooideBezwaarMoment();

		return bezwaarService.bezwarenGewijzigd(laatsteVoltooideBezwaarMoment, wrappers, bevolkingsonderzoek);
	}

	@Override
	protected void onDetach()
	{
		super.onDetach();
		ModelUtil.nullSafeDetach(bezwaarModel);
		ModelUtil.nullSafeDetach(clientModel);
	}
}
