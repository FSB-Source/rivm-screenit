package nl.rivm.screenit.main.web.gebruiker.testen.mamma.timeline.popups;

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

import java.util.Comparator;
import java.util.List;
import java.util.stream.Collectors;

import nl.rivm.screenit.main.service.mamma.MammaTestTimelineService;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.component.ComponentHelper;
import nl.rivm.screenit.main.web.component.dropdown.ScreenitDropdown;
import nl.rivm.screenit.main.web.gebruiker.testen.gedeeld.timeline.popups.AbstractTestBasePopupPanel;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.mamma.MammaScreeningsEenheid;
import nl.rivm.screenit.service.mamma.MammaBaseTestTimelineService;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.markup.html.form.ChoiceRenderer;
import org.apache.wicket.markup.html.form.TextField;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.PropertyModel;
import org.apache.wicket.spring.injection.annot.SpringBean;

abstract public class TestMammaKiesSeEnUitnodigingsNrVoorAfspraakPopup extends AbstractTestBasePopupPanel
{
	@SpringBean
	protected MammaBaseTestTimelineService mammaBaseTestTimelineService;

	@SpringBean
	private MammaTestTimelineService screeningsEenheidDao;

	@SpringBean
	private HibernateService hibernateService;

	private String uitnodigingsNr = "";

	private IModel<MammaScreeningsEenheid> screeningsEenheid;

	public TestMammaKiesSeEnUitnodigingsNrVoorAfspraakPopup(String id, IModel<List<Client>> clientModel)
	{
		super(id, clientModel);

		TextField<String> accessionNumberTF = new TextField<>("uitnodigingsNr");
		accessionNumberTF.setVisible(clientModel.getObject().size() == 1);
		accessionNumberTF.setModel(new PropertyModel<>(this, "uitnodigingsNr"));
		add(accessionNumberTF);

		ScreenitDropdown<MammaScreeningsEenheid> screeningsEenheidDropDown;

		final List<MammaScreeningsEenheid> screeningsEenhedenMetRoute = getScreeningsEenhedenMetRoute(
			screeningsEenheidDao.getActieveScreeningsEenheden(ScreenitSession.get().getScreeningOrganisatie()));
		screeningsEenhedenMetRoute.sort(Comparator.comparing(MammaScreeningsEenheid::getCode));
		IModel<List<MammaScreeningsEenheid>> seList = ModelUtil
			.listRModel(screeningsEenhedenMetRoute);
		if (!seList.getObject().isEmpty())
		{
			setScreeningsEenheid(seList.getObject().get(0));
		}
		screeningsEenheidDropDown = ComponentHelper.newDropDownChoice("screeningsEenheid", seList, new ChoiceRenderer<>("naam"), true);

		screeningsEenheidDropDown.setModel(new PropertyModel<>(this, "screeningsEenheid"));
		add(screeningsEenheidDropDown);
	}

	public MammaScreeningsEenheid getScreeningsEenheid()
	{
		return ModelUtil.nullSafeGet(screeningsEenheid);
	}

	public String getUitnodigingsNr()
	{
		return uitnodigingsNr;
	}

	public void setScreeningsEenheid(MammaScreeningsEenheid screeningsEenheid)
	{
		this.screeningsEenheid = ModelUtil.sModel(screeningsEenheid);
	}

	@Override
	protected void onDetach()
	{
		super.onDetach();
		ModelUtil.nullSafeDetach(screeningsEenheid);
	}

	private List<MammaScreeningsEenheid> getScreeningsEenhedenMetRoute(List<MammaScreeningsEenheid> actieveScreeningsEenheden)
	{
		return actieveScreeningsEenheden.stream().filter(se -> !se.getStandplaatsPerioden().isEmpty()).collect(Collectors.toList());
	}
}
