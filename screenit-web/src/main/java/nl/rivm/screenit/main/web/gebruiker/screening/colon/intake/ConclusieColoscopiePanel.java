
package nl.rivm.screenit.main.web.gebruiker.screening.colon.intake;

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

import java.util.Date;

import nl.rivm.screenit.main.web.component.ComponentHelper;
import nl.rivm.screenit.model.colon.ColonIntakeAfspraak;

import org.apache.wicket.markup.html.form.CheckBox;
import org.apache.wicket.markup.html.panel.GenericPanel;

public class ConclusieColoscopiePanel extends GenericPanel<ColonIntakeAfspraak>
{

	public ConclusieColoscopiePanel(String id, boolean allReadOnly)
	{
		super(id);

		ComponentHelper.addTextField(this, "conclusie.datumColoscopie", true, 10, Date.class, allReadOnly);

		CheckBox opVerzoekClient = ComponentHelper.newCheckBox("conclusie.coloscopieDatumOpVerzoekClient");
		opVerzoekClient.setEnabled(!allReadOnly);
		add(opVerzoekClient);
	}
}
