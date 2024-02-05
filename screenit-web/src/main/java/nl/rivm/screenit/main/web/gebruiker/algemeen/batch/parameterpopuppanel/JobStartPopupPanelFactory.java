package nl.rivm.screenit.main.web.gebruiker.algemeen.batch.parameterpopuppanel;

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

import java.lang.reflect.InvocationTargetException;
import java.util.Arrays;

import nl.rivm.screenit.main.web.gebruiker.algemeen.batch.parameterpopuppanel.colonclientselectie.BatchColonClientSelectieParameterPopupPanel;
import nl.rivm.screenit.main.web.gebruiker.algemeen.batch.parameterpopuppanel.mamma.palgaexport.BatchMammaPalgaExportParameterPopupPanel;
import nl.rivm.screenit.main.web.gebruiker.algemeen.batch.parameterpopuppanel.mamma.palgaimport.BatchMammaPalgaImportParameterPopupPanel;
import nl.rivm.screenit.model.enums.JobType;

import org.apache.commons.lang.reflect.ConstructorUtils;
import org.apache.wicket.markup.html.form.Form;

public enum JobStartPopupPanelFactory
{
	CLIENT_SELECTIE(JobType.CLIENT_SELECTIE, BatchColonClientSelectieParameterPopupPanel.class),
	MAMMA_PALGA_CSV_EXPORT(JobType.MAMMA_PALGA_CSV_EXPORT, BatchMammaPalgaExportParameterPopupPanel.class),
	MAMMA_PALGA_CSV_IMPORT(JobType.MAMMA_PALGA_CSV_IMPORT, BatchMammaPalgaImportParameterPopupPanel.class),
	;

	private final JobType jobType;

	private final Class<? extends AbstractParameterPopupPanel<?>> popupPanelClass;

	JobStartPopupPanelFactory(JobType jobType, Class<? extends AbstractParameterPopupPanel<?>> popupPanelClass)
	{
		this.jobType = jobType;
		this.popupPanelClass = popupPanelClass;
	}

	public static BatchParameterPopupPanel create(JobType jobType, String panelId, Form<?> containingForm)
	{
		return Arrays.stream(values())
			.filter(factory -> factory.jobType == jobType)
			.findFirst()
			.map(factory -> factory.instantiatePanel(panelId, containingForm))
			.orElse(new EmptyBatchParameterPopupPanel(panelId));
	}

	private BatchParameterPopupPanel instantiatePanel(String panelId, Form<?> containingForm)
	{
		try
		{
			var constructorWithFormParameter = ConstructorUtils.getMatchingAccessibleConstructor(popupPanelClass, new Class[] { String.class, Form.class });
			if (constructorWithFormParameter != null)
			{
				return (AbstractParameterPopupPanel<?>) constructorWithFormParameter.newInstance(panelId, containingForm);
			}
			return (AbstractParameterPopupPanel<?>) ConstructorUtils.invokeConstructor(popupPanelClass, new Object[] { panelId });
		}
		catch (NoSuchMethodException | IllegalAccessException | InvocationTargetException | InstantiationException e)
		{
			throw new IllegalArgumentException("Fout bij aanmaken panel voor jobtype: " + jobType, e);
		}
	}
}
