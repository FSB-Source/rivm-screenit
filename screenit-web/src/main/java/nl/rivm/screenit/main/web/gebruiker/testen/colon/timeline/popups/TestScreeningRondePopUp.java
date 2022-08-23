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

import nl.rivm.screenit.PreferenceKey;
import nl.rivm.screenit.main.model.testen.TestTimeLineDossierTijdstip;
import nl.rivm.screenit.main.service.colon.ColonTestTimelineService;
import nl.rivm.screenit.main.web.component.dropdown.ScreenitDropdown;
import nl.rivm.screenit.main.web.gebruiker.testen.gedeeld.timeline.components.TestEnumRadioChoice;
import nl.rivm.screenit.main.web.gebruiker.testen.gedeeld.timeline.popups.AbstractTestBasePopupPanel;
import nl.rivm.screenit.main.web.security.SecurityConstraint;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.colon.ColonDossier;
import nl.rivm.screenit.model.colon.ColonOnderzoeksVariant;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.util.ColonScreeningRondeUtil;
import nl.topicuszorg.preferencemodule.service.SimplePreferenceService;

import org.apache.wicket.markup.html.form.DropDownChoice;
import org.apache.wicket.markup.html.form.EnumChoiceRenderer;
import org.apache.wicket.markup.html.form.RadioChoice;
import org.apache.wicket.model.CompoundPropertyModel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.wicketstuff.shiro.ShiroConstraint;

@SecurityConstraint(
	actie = Actie.INZIEN,
	checkScope = true,
	constraint = ShiroConstraint.HasPermission,
	recht = Recht.TESTEN,
	bevolkingsonderzoekScopes = {
		Bevolkingsonderzoek.COLON, Bevolkingsonderzoek.CERVIX })
public class TestScreeningRondePopUp extends AbstractTestBasePopupPanel
{

	private static final long serialVersionUID = 1L;

	@SpringBean
	private ColonTestTimelineService colonTestTimeLineService;

	@SpringBean
	private SimplePreferenceService simplePreferenceService;

	private IModel<TestTimeLineDossierTijdstip> dossierTijdStipModel;

	private IModel<ColonOnderzoeksVariant> onderzoeksVariantModel = Model.of(ColonOnderzoeksVariant.STANDAARD);

	public TestScreeningRondePopUp(String id, IModel<List<Client>> clientModel)
	{
		super(id, clientModel);
		int aantalRondesUitnodigingsbriefZonderFit = simplePreferenceService.getInteger(PreferenceKey.COLON_AANTAL_RONDES_UITNODIGINGSBRIEF_ZONDER_FIT.name());

		boolean magUitnodigingMetFitMaken = ColonScreeningRondeUtil.magUitnodigingMetFitMaken(clientModel.getObject().get(0).getColonDossier(),
			aantalRondesUitnodigingsbriefZonderFit);

		List<TestTimeLineDossierTijdstip> redenen = new ArrayList<>();
		if (magUitnodigingMetFitMaken)
		{
			dossierTijdStipModel = new CompoundPropertyModel<>(TestTimeLineDossierTijdstip.DAG_NA_UITNODIGING_KOPPELEN);
			if (!isErEenVooraankondiging())
			{
				redenen.add(TestTimeLineDossierTijdstip.DAG_VOORAANKONDIGING_VERSTUREN);
			}
			else
			{
				redenen.add(TestTimeLineDossierTijdstip.DAG_UITNODIGING_AANMAKEN);
			}
			redenen.add(TestTimeLineDossierTijdstip.DAG_UITNODIGING_VERSTUREN);
			redenen.add(TestTimeLineDossierTijdstip.DAG_NA_UITNODIGING_KOPPELEN);
		}
		else
		{
			dossierTijdStipModel = new CompoundPropertyModel<>(TestTimeLineDossierTijdstip.DAG_NA_UITNODIGING_ZONDER_FIT);
			redenen.add(TestTimeLineDossierTijdstip.DAG_NA_UITNODIGING_ZONDER_FIT);
		}

		RadioChoice<TestTimeLineDossierTijdstip> reden = new TestEnumRadioChoice<>("reden", dossierTijdStipModel, redenen,
			new EnumChoiceRenderer<>(this));
		reden.setPrefix("<label class=\"radio\">");
		reden.setSuffix("</label>");
		reden.setOutputMarkupId(true);
		add(reden);

		List<ColonOnderzoeksVariant> choices = new ArrayList<>();
		choices.add(ColonOnderzoeksVariant.STANDAARD);
		choices.add(ColonOnderzoeksVariant.VERGELIJKEND);
		DropDownChoice<ColonOnderzoeksVariant> onderzoeksVariant = new ScreenitDropdown<>("onderzoeksVariant", onderzoeksVariantModel, choices, new EnumChoiceRenderer<>(this));
		add(onderzoeksVariant);
	}

	private boolean isErEenVooraankondiging()
	{
		ColonDossier dossier = getModelObject().get(0).getColonDossier();
		return dossier.getColonVooraankondiging() != null;
	}

	@Override
	protected void opslaan()
	{
		TestTimeLineDossierTijdstip tijdStip = dossierTijdStipModel.getObject();
		for (Client client : getModelObject())
		{
			colonTestTimeLineService.maakNieuweScreeningRonde(client, tijdStip, onderzoeksVariantModel.getObject());
		}
	}
}
