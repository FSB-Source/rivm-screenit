package nl.rivm.screenit.main.web.gebruiker.testen.colon.timeline.popups;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
 * %%
 * Copyright (C) 2012 - 2021 Facilitaire Samenwerking Bevolkingsonderzoek
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
import java.util.HashMap;
import java.util.List;

import nl.rivm.screenit.main.service.TestTimelineService;
import nl.rivm.screenit.main.web.component.dropdown.ScreenitDropdown;
import nl.rivm.screenit.main.web.gebruiker.testen.gedeeld.timeline.popups.AbstractTestBasePopupPanel;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.RetourredenAfhandeling;
import nl.rivm.screenit.model.colon.ColonScreeningRonde;
import nl.rivm.screenit.model.colon.ColonUitnodiging;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.wicket.hibernate.SimpleListHibernateModel;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.markup.html.form.ChoiceRenderer;
import org.apache.wicket.markup.html.form.IChoiceRenderer;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class TestRetourzendingPopup extends AbstractTestBasePopupPanel
{

	private static final long serialVersionUID = 1L;

	private static final Logger LOG = LoggerFactory.getLogger(TestRetourzendingPopup.class);

	@SpringBean
	private TestTimelineService testTimelineService;

	private IModel<RetourredenAfhandeling> redenModel = ModelUtil.cModel(new RetourredenAfhandeling());

	private IModel<ColonUitnodiging> uitnodigingModel = ModelUtil.cModel(new ColonUitnodiging());

	private HashMap<Long, SimpleListHibernateModel<ColonUitnodiging>> uitnodigingenMap = new HashMap<>();

	@SpringBean
	private HibernateService hibernateService;

	public TestRetourzendingPopup(String id, IModel<List<Client>> clientModel)
	{
		super(id, clientModel);
		ColonScreeningRonde ronde = getModelObject().get(0).getColonDossier().getLaatsteScreeningRonde();

		List<ColonUitnodiging> uitnodigingVoorRetourzending = new ArrayList<ColonUitnodiging>();
		for (ColonUitnodiging uitnodiging : ronde.getUitnodigingen())
		{
			if (uitnodiging.isVerstuurdDoorInpakcentrum() && uitnodiging.getGekoppeldeTest().getUitslag() == null && uitnodiging.getAntwoordFormulier() == null)
			{
				uitnodigingVoorRetourzending.add(uitnodiging);
				List<ColonUitnodiging> uitnodigingen = new ArrayList<>();
				uitnodigingen.add(uitnodiging);
				SimpleListHibernateModel<ColonUitnodiging> uitnodigingenModel = new SimpleListHibernateModel<>(uitnodigingen);
				uitnodigingenMap.put(uitnodiging.getUitnodigingsId(), uitnodigingenModel);
			}
		}
		IModel<List<ColonUitnodiging>> uitnodigingenModel = ModelUtil.listModel(uitnodigingVoorRetourzending);

		int rondeSize = ronde.getUitnodigingen().size();

		if (getModelObject().size() > 1)
		{
			for (Client client : getModelObject().subList(1, getModelObject().size()))
			{
				ronde = client.getColonDossier().getLaatsteScreeningRonde();
				if (rondeSize == ronde.getUitnodigingen().size())
				{
					for (int i = 0; i < ronde.getUitnodigingen().size(); i++)
					{
						ColonUitnodiging uitnodiging = ronde.getUitnodigingen().get(i);
						if (uitnodiging.isVerstuurdDoorInpakcentrum() && uitnodiging.getGekoppeldeTest().getUitslag() == null && uitnodiging.getAntwoordFormulier() == null)
						{
							SimpleListHibernateModel<ColonUitnodiging> uitnodigingen = uitnodigingenMap.get(uitnodigingVoorRetourzending.get(i).getUitnodigingsId());
							uitnodigingen.add(uitnodiging);
						}
					}
				}
				else
				{
					LOG.error("Het aantal uitnodigingenVoorRetourzending voor testclient(id: " + client.getId() + ")" + " komt niet overeen met de overige clienten");
				}
			}
		}

		ScreenitDropdown<ColonUitnodiging> uitnodigingDropDown = new ScreenitDropdown<ColonUitnodiging>("uitnodigingen", uitnodigingModel, uitnodigingenModel,
			new IChoiceRenderer<ColonUitnodiging>()
			{

				private static final long serialVersionUID = 1L;

				@Override
				public Object getDisplayValue(ColonUitnodiging object)
				{
					return "Uitnodiging(" + object.getUitnodigingsId() + ")";
				}

				@Override
				public String getIdValue(ColonUitnodiging object, int index)
				{
					if (object.getUitnodigingsId() != null)
					{
						return object.getUitnodigingsId().toString();
					}
					return null;
				}

				@Override
				public ColonUitnodiging getObject(String id, IModel<? extends List<? extends ColonUitnodiging>> choices)
				{
					if (id != null)
					{
						return choices.getObject().stream().filter(u -> u.getUitnodigingsId().toString().equals(id)).findFirst().orElse(null);
					}
					return null;
				}

			});
		uitnodigingDropDown.setNullValid(true);
		uitnodigingDropDown.setRequired(true);
		uitnodigingDropDown.setLabel(Model.of("Uitnodiging"));
		add(uitnodigingDropDown);

		List<RetourredenAfhandeling> retourRedenAfhandelingen = hibernateService.loadAll(RetourredenAfhandeling.class);

		ScreenitDropdown<RetourredenAfhandeling> retourDropDown = new ScreenitDropdown<>("retourzendingReden", redenModel, ModelUtil.listModel(retourRedenAfhandelingen),
			new ChoiceRenderer<>("retourReden"));
		retourDropDown.setNullValid(true);
		retourDropDown.setRequired(true);
		retourDropDown.setLabel(Model.of("Retourzending reden"));
		add(retourDropDown);
	}

	@Override
	protected void opslaan()
	{
		String reden = redenModel.getObject().getRetourReden();

		List<ColonUitnodiging> uitnodigingList = new ArrayList<>();
		if (uitnodigingModel.getObject().getUitnodigingsId() != null)
		{
			Long eersteClientUitnodigingId = uitnodigingModel.getObject().getUitnodigingsId();
			if (uitnodigingenMap.containsKey(eersteClientUitnodigingId))
			{
				uitnodigingList = uitnodigingenMap.get(eersteClientUitnodigingId).getObject();
			}
			for (ColonUitnodiging uitnodiging : uitnodigingList)
			{
				testTimelineService.retourzendingOntvangen(uitnodiging, reden);
			}
		}
	}

	@Override
	protected void onDetach()
	{
		super.onDetach();
		for (IModel<List<ColonUitnodiging>> uitnodiging : uitnodigingenMap.values())
		{
			ModelUtil.nullSafeDetach(uitnodiging);
		}
		ModelUtil.nullSafeDetach(uitnodigingModel);
	}
}
