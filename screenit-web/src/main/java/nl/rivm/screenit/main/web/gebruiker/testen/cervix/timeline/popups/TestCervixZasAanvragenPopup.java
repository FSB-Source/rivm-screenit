package nl.rivm.screenit.main.web.gebruiker.testen.cervix.timeline.popups;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
 * %%
 * Copyright (C) 2012 - 2024 Facilitaire Samenwerking Bevolkingsonderzoek
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

import java.util.List;

import nl.rivm.screenit.main.service.cervix.CervixUitnodigingService;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.gebruiker.testen.gedeeld.timeline.components.TestEnumRadioChoice;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.service.cervix.enums.CervixTestTimeLineDossierTijdstip;
import nl.topicuszorg.wicket.input.radiochoice.BooleanRadioChoice;

import org.apache.wicket.markup.html.form.EnumChoiceRenderer;
import org.apache.wicket.markup.html.form.RadioChoice;
import org.apache.wicket.model.CompoundPropertyModel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.PropertyModel;
import org.apache.wicket.spring.injection.annot.SpringBean;

public class TestCervixZasAanvragenPopup extends TestCervixAbstractPopupPanel
{
	@SpringBean
	private CervixUitnodigingService uitnodigingService;

	private IModel<CervixTestTimeLineDossierTijdstip> dossierTijdStipModel;

	private final Boolean nieuweZas;

	public TestCervixZasAanvragenPopup(String id, IModel<List<Client>> clientModel)
	{
		super(id, clientModel);

		nieuweZas = uitnodigingService.getVerwachteEersteZASMonsterIdLetter(ScreenitSession.get().getInstelling()) == 'C';
		var acties = testTimelineService.getZasSnelKeuzeOpties(clientModel.getObject().get(0));
		dossierTijdStipModel = new CompoundPropertyModel<>(acties.get(acties.size() - 1));

		var reden = new TestEnumRadioChoice<>("acties", dossierTijdStipModel, acties, new EnumChoiceRenderer<>(this));
		reden.setPrefix("<label class=\"radio\">");
		reden.setSuffix("</label>");
		reden.setOutputMarkupId(true);
		add(reden);

		RadioChoice<Boolean> nieuweZasRadio = new BooleanRadioChoice("nieuweZas", new PropertyModel<>(this, "nieuweZas"));
		nieuweZasRadio.setPrefix("<label class=\"radio\">");
		nieuweZasRadio.setSuffix("</label>");
		add(nieuweZasRadio);
	}

	@Override
	protected void opslaan()
	{
		CervixTestTimeLineDossierTijdstip tijdStip = dossierTijdStipModel.getObject();
		for (Client client : getModelObject())
		{
			baseTestTimelineService.maakZasMonster(client, ScreenitSession.get().getLoggedInInstellingGebruiker(), tijdStip, nieuweZas);
		}
	}

}
