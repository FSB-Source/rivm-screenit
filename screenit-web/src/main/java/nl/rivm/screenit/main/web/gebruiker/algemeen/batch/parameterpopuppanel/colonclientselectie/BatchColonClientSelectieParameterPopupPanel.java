package nl.rivm.screenit.main.web.gebruiker.algemeen.batch.parameterpopuppanel.colonclientselectie;

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

import java.util.Map;

import nl.rivm.screenit.main.web.component.ComponentHelper;
import nl.rivm.screenit.main.web.gebruiker.algemeen.batch.parameterpopuppanel.IBatchParameterPopupPanel;
import nl.rivm.screenit.model.enums.JobStartParameter;

import org.apache.wicket.markup.html.basic.EnumLabel;
import org.apache.wicket.markup.html.form.CheckBox;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.panel.GenericPanel;
import org.apache.wicket.model.Model;

public class BatchColonClientSelectieParameterPopupPanel extends GenericPanel<Boolean> implements IBatchParameterPopupPanel
{

	private static final long serialVersionUID = 1L;

	public BatchColonClientSelectieParameterPopupPanel(String id, Form<?> form)
	{
		super(id, Model.of(Boolean.FALSE));

		add(new EnumLabel<>("parameterOmschrijving", JobStartParameter.COLON_SELECTIE_HERSTART));
		CheckBox checkBox = ComponentHelper.newCheckBox("checkbox", getModel());
		add(checkBox);
	}

	@Override
	public void vulJobParameters(Map<String, Object> jobParameters)
	{
		jobParameters.put(JobStartParameter.COLON_SELECTIE_HERSTART.name(), getModelObject());
	}
}
