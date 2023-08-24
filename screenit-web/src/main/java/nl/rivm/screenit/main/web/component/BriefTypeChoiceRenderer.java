package nl.rivm.screenit.main.web.component;

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

import nl.rivm.screenit.util.EnumStringUtil;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.BriefType;

import org.apache.wicket.Application;
import org.apache.wicket.markup.html.form.ChoiceRenderer;

public class BriefTypeChoiceRenderer extends ChoiceRenderer<BriefType>
{

	private static final long serialVersionUID = 1L;

	@Override
	public Object getDisplayValue(BriefType object)
	{
		String key = EnumStringUtil.getPropertyString(object);
		String briefnaam = Application.get().getResourceSettings().getLocalizer().getString(key, null);
		if (!object.isActief())
		{
			briefnaam += " " + Application.get().getResourceSettings().getLocalizer().getString("BriefType.niet.meer.in.gebruik", null);
		}
		return Bevolkingsonderzoek.getAfkortingen(object.getOnderzoeken()) + " - " + briefnaam;
	}

}
