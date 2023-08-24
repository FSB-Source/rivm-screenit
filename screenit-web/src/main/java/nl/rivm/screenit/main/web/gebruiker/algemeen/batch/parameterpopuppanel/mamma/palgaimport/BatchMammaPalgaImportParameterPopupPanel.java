package nl.rivm.screenit.main.web.gebruiker.algemeen.batch.parameterpopuppanel.mamma.palgaimport;

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

import java.util.Map;

import nl.rivm.screenit.main.web.component.ComponentHelper;
import nl.rivm.screenit.main.web.gebruiker.algemeen.batch.parameterpopuppanel.AbstractParameterPopupPanel;
import nl.rivm.screenit.model.batch.popupconfig.MammaPalgaGrondslag;
import nl.rivm.screenit.model.enums.JobStartParameter;

import org.apache.wicket.model.Model;

public class BatchMammaPalgaImportParameterPopupPanel extends AbstractParameterPopupPanel<MammaPalgaGrondslag>
{

	public BatchMammaPalgaImportParameterPopupPanel(String id)
	{
		super(id, Model.of((MammaPalgaGrondslag) null));
		ComponentHelper.addRadioChoice(this, "grondslag", getModel(), MammaPalgaGrondslag.class);
	}

	@Override
	public void vulJobParameters(Map<String, Object> jobParameters)
	{
		jobParameters.put(JobStartParameter.MAMMA_PALGA_IMPORT.name(), getModelObject().name());
	}
}
