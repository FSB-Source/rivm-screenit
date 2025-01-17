package nl.rivm.screenit.main.web.gebruiker.testen.cervix.timeline.popups;

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

import java.util.ArrayList;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;

import nl.rivm.screenit.main.web.component.dropdown.ScreenitDropdown;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.InpakbareUitnodiging;
import nl.rivm.screenit.model.cervix.CervixScreeningRonde;
import nl.rivm.screenit.model.cervix.CervixUitnodiging;
import nl.topicuszorg.wicket.hibernate.SimpleListHibernateModel;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.markup.html.form.IChoiceRenderer;
import org.apache.wicket.model.IModel;

public abstract class TestCervixUitnodigingenPopup extends TestCervixAbstractPopupPanel
{
	private IModel<CervixUitnodiging> uitnodigingModel;

	private IModel<List<CervixUitnodiging>> uitnodigingenModel;

	private HashMap<Long, SimpleListHibernateModel<CervixUitnodiging>> uitnodigingenMap = new HashMap<>();

	public TestCervixUitnodigingenPopup(String id, IModel<List<Client>> clientModel)
	{
		super(id, clientModel);

		Client eersteClient = clientModel.getObject().get(0);
		List<CervixUitnodiging> mogelijkeUitnodigingen = new ArrayList<>();

		for (CervixScreeningRonde ronde : eersteClient.getCervixDossier().getScreeningRondes())
		{
			List<CervixUitnodiging> uitnodigingenList = ronde.getUitnodigingen();
			uitnodigingenList.sort(Comparator.comparing(InpakbareUitnodiging::getUitnodigingsId));
			for (CervixUitnodiging uitnodiging : uitnodigingenList)
			{
				if (magUitnodiging(uitnodiging))
				{
					if (uitnodigingModel == null)
					{
						uitnodigingModel = ModelUtil.sModel(uitnodiging);
					}
					mogelijkeUitnodigingen.add(uitnodiging);
					List<CervixUitnodiging> uitnodigingen = new ArrayList<>();
					uitnodigingen.add(uitnodiging);
					SimpleListHibernateModel<CervixUitnodiging> uitnodigingenModel = new SimpleListHibernateModel<>(uitnodigingen);
					uitnodigingenMap.put(uitnodiging.getUitnodigingsId(), uitnodigingenModel);
				}
			}
		}

		uitnodigingenModel = ModelUtil.listModel(mogelijkeUitnodigingen);

		if (getModelObject().size() > 1)
		{
			for (Client client : getModelObject().subList(1, getModelObject().size()))
			{
				for (CervixScreeningRonde ronde : client.getCervixDossier().getScreeningRondes())
				{
					for (int i = 0; i < ronde.getUitnodigingen().size(); i++)
					{
						CervixUitnodiging uitnodiging = ronde.getUitnodigingen().get(i);
						if (magUitnodiging(uitnodiging))
						{
							SimpleListHibernateModel<CervixUitnodiging> uitnodigingen = uitnodigingenMap.get(mogelijkeUitnodigingen.get(i).getUitnodigingsId());
							uitnodigingen.add(uitnodiging);
						}
					}
				}
			}
		}

		ScreenitDropdown<CervixUitnodiging> uitnodigingDropDown = new ScreenitDropdown<>("uitnodigingen", uitnodigingModel, uitnodigingenModel,
			new IChoiceRenderer<CervixUitnodiging>()
			{
				@Override
				public Object getDisplayValue(CervixUitnodiging uitnodiging)
				{
					String monsterId = uitnodiging.getMonster().getMonsterId();
					if (monsterId != null)
					{
						return "Monster-id: " + monsterId;
					}
					return "Uitnodiging-id: " + uitnodiging.getUitnodigingsId();
				}

				@Override
				public String getIdValue(CervixUitnodiging uitnodiging, int index)
				{
					return uitnodiging.getUitnodigingsId().toString();
				}

				@Override
				public CervixUitnodiging getObject(String id, IModel<? extends List<? extends CervixUitnodiging>> choices)
				{
					if (id != null)
					{
						return choices.getObject().stream().filter(u -> u.getUitnodigingsId().toString().equals(id)).findFirst().orElse(null);
					}
					return null;
				}

			});
		uitnodigingDropDown.setRequired(true);
		add(uitnodigingDropDown);

	}

	protected abstract boolean magUitnodiging(CervixUitnodiging uitnodiging);

	protected List<CervixUitnodiging> getCurrentUitnodigingen()
	{
		List<CervixUitnodiging> uitnodigingen = new ArrayList<>();
		Long eersteClientUitnodigingId = uitnodigingModel.getObject().getUitnodigingsId();
		if (eersteClientUitnodigingId != null && uitnodigingenMap.containsKey(eersteClientUitnodigingId))
		{
			uitnodigingenMap.get(eersteClientUitnodigingId).getObject().forEach(uitnodiging -> getModelObject().forEach(client -> {
				client.getCervixDossier().getScreeningRondes().forEach(ronde -> ronde.getUitnodigingen().forEach(u -> {
					if (u.getId().equals(uitnodiging.getId()))
					{
						uitnodigingen.add(u);
					}
				}));
			}));
		}
		return uitnodigingen;
	}

	@Override
	protected void onDetach()
	{
		super.onDetach();
		for (IModel<List<CervixUitnodiging>> uitnodiging : uitnodigingenMap.values())
		{
			ModelUtil.nullSafeDetach(uitnodiging);
		}
		ModelUtil.nullSafeDetach(uitnodigingModel);
		ModelUtil.nullSafeDetach(uitnodigingenModel);
	}

	protected CervixUitnodiging getUitnodiging()
	{
		return uitnodigingModel.getObject();
	}
}
