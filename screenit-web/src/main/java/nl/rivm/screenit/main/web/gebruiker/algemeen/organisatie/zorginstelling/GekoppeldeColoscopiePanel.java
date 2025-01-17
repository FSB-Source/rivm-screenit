package nl.rivm.screenit.main.web.gebruiker.algemeen.organisatie.zorginstelling;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
 * %%
 * Copyright (C) 2012 - 2025 Facilitaire Samenwerking Bevolkingsonderzoek
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
import java.util.stream.Collectors;

import nl.rivm.screenit.model.Instelling;
import nl.rivm.screenit.model.colon.ColonIntakelocatie;
import nl.rivm.screenit.model.colon.ColoscopieLocatie;
import nl.rivm.screenit.service.InstellingService;

import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.panel.GenericPanel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.spring.injection.annot.SpringBean;

public class GekoppeldeColoscopiePanel<T extends Instelling> extends GenericPanel<T>
{
	@SpringBean
	private InstellingService instellingService;

	public GekoppeldeColoscopiePanel(String id, IModel<T> model)
	{
		super(id, model);
	}

	@Override
	protected void onInitialize()
	{
		super.onInitialize();

		List<ColonIntakelocatie> coloscopiesCentrums = instellingService.getChildrenOrganisaties(getModelObject(), ColonIntakelocatie.class);
		List<ColoscopieLocatie> coloscopiesLocaties = instellingService.getChildrenOrganisaties(getModelObject(), ColoscopieLocatie.class);
		add(new Label("coloscopieLocaties", stringLocaties(coloscopiesLocaties)));
		add(new Label("intakeLocaties", stringLocaties(coloscopiesCentrums)));
	}

	protected String stringLocaties(List<? extends Instelling> instellingLijst)
	{
		List<String> instellingsNamen = instellingLijst.stream().map(Instelling::getNaam).collect(Collectors.toList());
		return !instellingsNamen.isEmpty() ? String.join(", ", instellingsNamen) : "Er zijn geen locaties gekoppeld";
	}
}
