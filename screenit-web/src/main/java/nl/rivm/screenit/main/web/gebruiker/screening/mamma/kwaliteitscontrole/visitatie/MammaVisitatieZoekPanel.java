package nl.rivm.screenit.main.web.gebruiker.screening.mamma.kwaliteitscontrole.visitatie;

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

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import nl.rivm.screenit.main.model.mamma.beoordeling.MammaVisitatieWerklijstZoekObject;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.component.dropdown.ScreenitListMultipleChoice;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.kwaliteitscontrole.panels.MammaKwaliteitscontroleZoekPanel;
import nl.rivm.screenit.model.OrganisatieType;
import nl.rivm.screenit.model.mamma.enums.MammaVisitatieStatus;

import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.form.EnumChoiceRenderer;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.model.IModel;

public abstract class MammaVisitatieZoekPanel extends MammaKwaliteitscontroleZoekPanel<MammaVisitatieWerklijstZoekObject>
{

	public MammaVisitatieZoekPanel(String id, IModel<MammaVisitatieWerklijstZoekObject> model)
	{
		super(id, model);
	}

	@Override
	protected void addAdditionalZoekFields(Form<MammaVisitatieWerklijstZoekObject> zoekForm)
	{
		WebMarkupContainer statusContainer = new WebMarkupContainer("statussenContainer");
		List<MammaVisitatieStatus> choices = new ArrayList<>(Arrays.asList(MammaVisitatieStatus.values()));
		if (ScreenitSession.get().getInstelling().getOrganisatieType() == OrganisatieType.KWALITEITSPLATFORM)
		{
			choices.remove(MammaVisitatieStatus.INGEPLAND);
		}
		ScreenitListMultipleChoice<MammaVisitatieStatus> typeSelector = new ScreenitListMultipleChoice<>("statussen",
			choices, new EnumChoiceRenderer<>(this));
		statusContainer.add(typeSelector);
		statusContainer.setOutputMarkupId(true);

		zoekForm.add(statusContainer);
	}

}
