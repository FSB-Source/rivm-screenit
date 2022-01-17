package nl.rivm.screenit.main.web.gebruiker.testen.cervix.timeline.popups;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
 * %%
 * Copyright (C) 2012 - 2022 Facilitaire Samenwerking Bevolkingsonderzoek
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

import java.util.Arrays;
import java.util.List;

import nl.rivm.screenit.main.web.component.NaamChoiceRenderer;
import nl.rivm.screenit.main.web.gebruiker.testen.gedeeld.timeline.components.TestEnumRadioChoice;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.cervix.CervixUitnodiging;
import nl.rivm.screenit.model.cervix.enums.CervixHpvBeoordelingWaarde;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.markup.html.form.RadioChoice;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;

public class TestCervixGeanalyseerdOpHpvPopup extends TestCervixUitnodigingenLaboratoriaPopup
{

	private static final long serialVersionUID = 1L;

	private IModel<CervixHpvBeoordelingWaarde> hpvUitslagModel = Model.of();

	public TestCervixGeanalyseerdOpHpvPopup(String id, IModel<List<Client>> clientModel)
	{
		super(id, clientModel);

		RadioChoice<CervixHpvBeoordelingWaarde> hpvUitslag = new TestEnumRadioChoice<>("hpvUitslag", hpvUitslagModel, Arrays.asList(CervixHpvBeoordelingWaarde.values()),
			new NaamChoiceRenderer<>());
		hpvUitslag.setPrefix("<label class=\"radio\">");
		hpvUitslag.setSuffix("</label>");
		hpvUitslag.setOutputMarkupId(true);
		hpvUitslag.setRequired(true);
		add(hpvUitslag);
	}

	@Override
	protected boolean magUitnodiging(CervixUitnodiging uitnodiging)
	{
		return testTimelineService.magGeanalyseerdOpHpv(uitnodiging);
	}

	@Override
	protected void opslaan()
	{
		for (CervixUitnodiging uitnodiging : getCurrentUitnodigingen())
		{
			baseTestTimelineService.geanalyseerdOpHpv(uitnodiging, hpvUitslagModel.getObject(), laboratoriumModel.getObject());
		}
	}

	@Override
	protected void onDetach()
	{
		super.onDetach();
		ModelUtil.nullSafeDetach(hpvUitslagModel);
	}
}
