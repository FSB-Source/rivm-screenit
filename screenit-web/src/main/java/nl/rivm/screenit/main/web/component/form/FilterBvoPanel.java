
package nl.rivm.screenit.main.web.component.form;

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

import java.util.List;

import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.component.dropdown.ScreenitListMultipleChoice;
import nl.rivm.screenit.model.IBevolkingsonderzoek;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.service.AutorisatieService;

import org.apache.wicket.markup.html.form.EnumChoiceRenderer;
import org.apache.wicket.markup.html.panel.GenericPanel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.PropertyModel;
import org.apache.wicket.spring.injection.annot.SpringBean;

public class FilterBvoPanel<T extends IBevolkingsonderzoek> extends GenericPanel<T>
{

	private static final long serialVersionUID = 1L;

	@SpringBean
	private AutorisatieService autorisatieService;

	public FilterBvoPanel(String id, IModel<T> model, boolean altijdZichtbaar)
	{
		super(id, model);

		ScreenitSession screenitSession = ScreenitSession.get();
		List<Bevolkingsonderzoek> bvosGeselecteerdInGeneriekBvoFilter = screenitSession.getOnderzoeken();
		boolean heeftRechtenVoorMeerDan1Bvo = autorisatieService.getBevolkingsonderzoeken(screenitSession.getLoggedInInstellingGebruiker()).size() > 1;

		add(new ScreenitListMultipleChoice<Bevolkingsonderzoek>("bevolkingsonderzoeken", new PropertyModel<List<Bevolkingsonderzoek>>(model, "bevolkingsonderzoeken"),
			bvosGeselecteerdInGeneriekBvoFilter, new EnumChoiceRenderer<Bevolkingsonderzoek>()));

		setVisible(heeftRechtenVoorMeerDan1Bvo && bvosGeselecteerdInGeneriekBvoFilter != null && (bvosGeselecteerdInGeneriekBvoFilter.size() >= 2 || altijdZichtbaar));
	}
}
