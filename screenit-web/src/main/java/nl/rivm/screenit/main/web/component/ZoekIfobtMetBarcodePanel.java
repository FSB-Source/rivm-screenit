package nl.rivm.screenit.main.web.component;

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

import nl.rivm.screenit.model.colon.IFOBTTest;
import nl.rivm.screenit.service.colon.ColonBaseFITService;

import org.apache.commons.lang3.StringUtils;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.spring.injection.annot.SpringBean;

public class ZoekIfobtMetBarcodePanel extends ZoekMetScannedInputPanel
{

	@SpringBean
	private ColonBaseFITService colonFitService;

	public ZoekIfobtMetBarcodePanel(String id)
	{
		super(id);
	}

	@Override
	protected void processScannedInput(AjaxRequestTarget target)
	{
		if (StringUtils.isNotBlank(getScanInput()))
		{
			var fit = colonFitService.getFit(getScanInput()).orElse(null);
			ifobtFound(fit, target);
		}
		else
		{
			error("Geen valide barcode.");
		}
	}

	protected void ifobtFound(IFOBTTest ifobtTest, AjaxRequestTarget target)
	{
	}

}
