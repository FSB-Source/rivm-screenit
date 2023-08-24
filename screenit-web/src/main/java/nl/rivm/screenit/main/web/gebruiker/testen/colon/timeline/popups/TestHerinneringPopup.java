package nl.rivm.screenit.main.web.gebruiker.testen.colon.timeline.popups;

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
import java.util.List;

import nl.rivm.screenit.main.model.testen.TestTimeLineDossierTijdstip;
import nl.rivm.screenit.main.service.colon.ColonTestTimelineService;
import nl.rivm.screenit.main.web.gebruiker.testen.gedeeld.timeline.components.TestEnumRadioChoice;
import nl.rivm.screenit.main.web.gebruiker.testen.gedeeld.timeline.popups.AbstractTestBasePopupPanel;
import nl.rivm.screenit.model.Client;

import org.apache.wicket.markup.html.form.EnumChoiceRenderer;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.form.RadioChoice;
import org.apache.wicket.model.CompoundPropertyModel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.spring.injection.annot.SpringBean;
public class TestHerinneringPopup extends AbstractTestBasePopupPanel
{
	private static final long serialVersionUID = 1L;

	@SpringBean
	private ColonTestTimelineService colonTestTimeLineService;

	private Form<Void> form;

	private IModel<TestTimeLineDossierTijdstip> dossierTijdStipModel;

	public TestHerinneringPopup(String id, IModel<List<Client>> clientModel)
	{
		super(id, clientModel);
		dossierTijdStipModel = new CompoundPropertyModel<>(TestTimeLineDossierTijdstip.DAG_HERINNERING_VERSTUREN);

		List<TestTimeLineDossierTijdstip> redenen = new ArrayList<>();
		redenen.add(TestTimeLineDossierTijdstip.DAG_HERINNERING_VERSTUREN);
		redenen.add(TestTimeLineDossierTijdstip.DAG_NA_HERINNERING_VERSTUREN);

		RadioChoice<TestTimeLineDossierTijdstip> reden = new TestEnumRadioChoice<>("reden", dossierTijdStipModel, redenen,
			new EnumChoiceRenderer<>(this));
		reden.setPrefix("<label class=\"radio\">");
		reden.setSuffix("</label>");
		reden.setOutputMarkupId(true);
		add(reden);
	}

	@Override
	protected void opslaan()
	{
		TestTimeLineDossierTijdstip tijdStip = dossierTijdStipModel.getObject();
		for (Client client : getModelObject())
		{
			colonTestTimeLineService.ifobtHerinneringVersturen(client, tijdStip);
		}
	}

}
