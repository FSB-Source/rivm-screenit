package nl.rivm.screenit.main.web.gebruiker.screening.mamma.kwaliteitscontrole.fotobespreking;

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

import java.util.Arrays;

import nl.rivm.screenit.main.model.mamma.beoordeling.MammaFotobesprekingWerklijstZoekObject;
import nl.rivm.screenit.main.web.component.dropdown.ScreenitListMultipleChoice;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.kwaliteitscontrole.panels.MammaKwaliteitscontroleZoekPanel;
import nl.rivm.screenit.model.mamma.enums.MammaFotobesprekingType;

import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.form.EnumChoiceRenderer;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.model.IModel;

public abstract class MammaFotobesprekingZoekPanel extends MammaKwaliteitscontroleZoekPanel<MammaFotobesprekingWerklijstZoekObject>
{

	public MammaFotobesprekingZoekPanel(String id, IModel<MammaFotobesprekingWerklijstZoekObject> model)
	{
		super(id, model);
	}

	@Override
	protected void addAdditionalZoekFields(Form<MammaFotobesprekingWerklijstZoekObject> zoekForm)
	{
		WebMarkupContainer typeContainer = new WebMarkupContainer("typeContainer");
		ScreenitListMultipleChoice<MammaFotobesprekingType> typeSelector = new ScreenitListMultipleChoice<>("typen",
			Arrays.asList(MammaFotobesprekingType.values()), new EnumChoiceRenderer<>(this));
		typeContainer.add(typeSelector);
		typeContainer.setOutputMarkupId(true);

		zoekForm.add(typeContainer);
	}

}
