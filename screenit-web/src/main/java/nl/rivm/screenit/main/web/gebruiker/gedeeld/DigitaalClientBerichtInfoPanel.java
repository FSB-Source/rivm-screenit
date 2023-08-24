package nl.rivm.screenit.main.web.gebruiker.gedeeld;

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

import nl.rivm.screenit.model.DigitaalClientBericht;
import nl.rivm.screenit.util.EnumStringUtil;

import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.panel.GenericPanel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;

public class DigitaalClientBerichtInfoPanel extends GenericPanel<DigitaalClientBericht<?>>
{
	public DigitaalClientBerichtInfoPanel(String id, IModel<DigitaalClientBericht<?>> model)
	{
		super(id, model);
	}

	@Override
	protected void onInitialize()
	{
		super.onInitialize();
		maakClientBerichtInfoContent();
	}

	private void maakClientBerichtInfoContent()
	{
		var digitaalBericht = getModel().getObject();
		var berichtTemplateNaam = EnumStringUtil.maakStringMetBvoEnEnumPropertyString(digitaalBericht.getDigitaalBerichtTemplateType(), this::getString);
		var enumString = EnumStringUtil.getPropertyString(digitaalBericht.getDigitaalBerichtTemplateType().getBerichtType());
		var getoondeOntvangerInfoString = getString("label." + enumString);
		var panelTitelString = getString("titel." + enumString);

		var templateNaam = new Label("digitaalBerichtTemplateType", Model.of(berichtTemplateNaam));
		var infoOntvangendAdres = new Label("ontvanger");
		var getoondeOntvangerInfo = new Label("ontvangerInfo", getoondeOntvangerInfoString);
		var panelTitel = new Label("panelTitel", panelTitelString);

		add(templateNaam);
		add(infoOntvangendAdres);
		add(getoondeOntvangerInfo);
		add(panelTitel);
	}
}
