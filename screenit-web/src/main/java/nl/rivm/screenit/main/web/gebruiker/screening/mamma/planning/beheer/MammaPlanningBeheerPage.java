package nl.rivm.screenit.main.web.gebruiker.screening.mamma.planning.beheer;

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

import java.util.List;

import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.component.dropdown.ScreenitDropdown;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.planning.MammaPlanningBasePage;
import nl.rivm.screenit.main.web.security.SecurityConstraint;
import nl.rivm.screenit.model.ScreeningOrganisatie;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.service.InstellingService;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.form.AjaxFormComponentUpdatingBehavior;
import org.apache.wicket.markup.html.form.ChoiceRenderer;
import org.apache.wicket.markup.html.panel.EmptyPanel;
import org.apache.wicket.markup.html.panel.Panel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.wicketstuff.shiro.ShiroConstraint;

@SecurityConstraint(
	actie = Actie.INZIEN,
	checkScope = true,
	constraint = ShiroConstraint.HasPermission,
	recht = { Recht.GEBRUIKER_SCREENING_MAMMA_PLANNING },
	bevolkingsonderzoekScopes = { Bevolkingsonderzoek.MAMMA })
public class MammaPlanningBeheerPage extends MammaPlanningBasePage
{
	@SpringBean
	private InstellingService instellingService;

	private ScreenitDropdown<ScreeningOrganisatie> screeningOrganisatieDropdown;

	private IModel<ScreeningOrganisatie> screeningOrganisatieModel;

	private Panel gegevensPanel;

	public MammaPlanningBeheerPage()
	{
		addSoDropdown();
		addSoGegevensPanel();
	}

	private void addSoDropdown()
	{
		ScreeningOrganisatie sessionSo = ScreenitSession.get().getScreeningOrganisatie();
		List<ScreeningOrganisatie> actieveScreeningOrganisaties = instellingService.getActieveInstellingen(ScreeningOrganisatie.class);

		screeningOrganisatieModel = ModelUtil.csModel(sessionSo != null ? sessionSo : actieveScreeningOrganisaties.get(0));
		IModel<List<ScreeningOrganisatie>> screeningOrganisatiesModel = ModelUtil.listRModel(actieveScreeningOrganisaties, false);

		screeningOrganisatieDropdown = new ScreenitDropdown<>("regio", screeningOrganisatieModel, screeningOrganisatiesModel, new ChoiceRenderer<>("naam"));
		screeningOrganisatieDropdown.setEnabled(sessionSo == null);
		screeningOrganisatieDropdown.add(new AjaxFormComponentUpdatingBehavior("change")
		{
			@Override
			protected void onUpdate(AjaxRequestTarget target)
			{
				target.add(gegevensPanel);
			}
		});
		screeningOrganisatieDropdown.setVisible(sessionSo == null);
		add(screeningOrganisatieDropdown);
	}

	private void addSoGegevensPanel()
	{
		gegevensPanel = screeningOrganisatieModel != null ? new MammaPlanningBeheerGegevensPanel("borstkankerGegevens", screeningOrganisatieModel)
			: new EmptyPanel("borstkankerGegevens");
		add(gegevensPanel);
	}
}
