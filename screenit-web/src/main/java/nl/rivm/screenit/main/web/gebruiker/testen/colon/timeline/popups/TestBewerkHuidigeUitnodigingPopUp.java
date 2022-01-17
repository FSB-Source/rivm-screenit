package nl.rivm.screenit.main.web.gebruiker.testen.colon.timeline.popups;

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

import java.util.ArrayList;
import java.util.List;

import nl.rivm.screenit.main.model.testen.TestTimeLineDossierTijdstip;
import nl.rivm.screenit.main.service.TestTimelineService;
import nl.rivm.screenit.main.web.gebruiker.testen.gedeeld.timeline.components.TestEnumRadioChoice;
import nl.rivm.screenit.main.web.gebruiker.testen.gedeeld.timeline.popups.AbstractTestBasePopupPanel;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.colon.ColonDossier;
import nl.rivm.screenit.model.colon.ColonScreeningRonde;
import nl.rivm.screenit.model.colon.ColonUitnodiging;
import nl.rivm.screenit.service.ICurrentDateSupplier;

import org.apache.wicket.markup.html.form.EnumChoiceRenderer;
import org.apache.wicket.markup.html.form.RadioChoice;
import org.apache.wicket.model.CompoundPropertyModel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.spring.injection.annot.SpringBean;

public class TestBewerkHuidigeUitnodigingPopUp extends AbstractTestBasePopupPanel
{

	private static final long serialVersionUID = 1L;

	@SpringBean
	private TestTimelineService testTimeLineService;

	@SpringBean
	private ICurrentDateSupplier currentDateSupplier;

	private IModel<TestTimeLineDossierTijdstip> dossierTijdStipModel;

	public TestBewerkHuidigeUitnodigingPopUp(String id, IModel<List<Client>> clientModel)
	{
		super(id, clientModel);
		dossierTijdStipModel = new CompoundPropertyModel<>(TestTimeLineDossierTijdstip.DAG_NA_UITNODIGING_KOPPELEN);

		List<TestTimeLineDossierTijdstip> redenen = new ArrayList<TestTimeLineDossierTijdstip>();
		if (!isColonUitnodigingAlVerstuurdNaarInpakcentrum())
		{
			redenen.add(TestTimeLineDossierTijdstip.DAG_UITNODIGING_VERSTUREN);
		}
		redenen.add(TestTimeLineDossierTijdstip.DAG_NA_UITNODIGING_KOPPELEN);

		RadioChoice<TestTimeLineDossierTijdstip> reden = new TestEnumRadioChoice<TestTimeLineDossierTijdstip>("reden", dossierTijdStipModel, redenen,
			new EnumChoiceRenderer<>(this));
		reden.setPrefix("<label class=\"radio\">");
		reden.setSuffix("</label>");
		reden.setOutputMarkupId(true);
		add(reden);
	}

	private boolean isColonUitnodigingAlVerstuurdNaarInpakcentrum()
	{
		ColonDossier dossier = getModelObject().get(0).getColonDossier();
		if (dossier.getLaatsteScreeningRonde() != null && dossier.getLaatsteScreeningRonde().getLaatsteUitnodiging() != null)
		{
			ColonScreeningRonde ronde = dossier.getLaatsteScreeningRonde();
			ColonUitnodiging uitnodiging = ronde.getLaatsteUitnodiging();
			return uitnodiging.isVerstuurd();
		}
		return false;
	}

	@Override
	protected void opslaan()
	{
		TestTimeLineDossierTijdstip tijdStip = dossierTijdStipModel.getObject();
		for (Client client : getModelObject())
		{
			testTimeLineService.bewerkUitnodiging(client, tijdStip);
		}
	}

}
