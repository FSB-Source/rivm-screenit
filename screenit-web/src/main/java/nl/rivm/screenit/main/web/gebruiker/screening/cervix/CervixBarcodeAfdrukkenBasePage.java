package nl.rivm.screenit.main.web.gebruiker.screening.cervix;

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

import nl.rivm.screenit.main.web.filter.SecurityHeadersFilter;

import org.apache.wicket.ajax.AbstractDefaultAjaxBehavior;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.attributes.CallbackParameter;
import org.apache.wicket.markup.head.IHeaderResponse;
import org.apache.wicket.markup.head.JavaScriptHeaderItem;
import org.apache.wicket.markup.head.PriorityHeaderItem;
import org.apache.wicket.markup.html.panel.EmptyPanel;
import org.apache.wicket.request.IRequestParameters;
import org.apache.wicket.request.http.WebResponse;

public abstract class CervixBarcodeAfdrukkenBasePage extends CervixScreeningBasePage
{
	private static final String BROWSER_PRINT_API_SOURCE = "assets/js/barcodeprinter/BrowserPrint-3.0.216.min.js";

	private static final String BARCODE_PRINT_API_SOURCE = "assets/js/barcodeprinter/barcodePrinter.js";

	private static final String ERROR_CALLBACK_MESSAGE = "ERROR_CALLBACK_MESSAGE";

	private AbstractDefaultAjaxBehavior errorCallbackBehavior;

	@Override
	protected void setHeaders(WebResponse response)
	{
		SecurityHeadersFilter.allowExtraConnectSrcInContentSecurityPolicy(response, "https://localhost:9101");
		super.setHeaders(response);
	}

	public void initialiseerZebraPrinterLibrary(IHeaderResponse response)
	{
		response.render(new PriorityHeaderItem(JavaScriptHeaderItem.forUrl(BROWSER_PRINT_API_SOURCE)));
		response.render(new PriorityHeaderItem(JavaScriptHeaderItem.forUrl(BARCODE_PRINT_API_SOURCE)));
		response.render(new PriorityHeaderItem(JavaScriptHeaderItem.forScript("zoekPrinter();", null)));
		response.render(new PriorityHeaderItem(
			JavaScriptHeaderItem.forScript("errorCallback=" + errorCallbackBehavior.getCallbackFunction(CallbackParameter.explicit(ERROR_CALLBACK_MESSAGE)) + ";", null)));
	}

	public void maakErrorCallbackBehavior()
	{
		errorCallbackBehavior = new AbstractDefaultAjaxBehavior()
		{
			@Override
			protected void respond(AjaxRequestTarget target)
			{
				IRequestParameters requestParameters = getComponent().getRequest().getRequestParameters();
				String errorMessage = requestParameters.getParameterValue(ERROR_CALLBACK_MESSAGE).toString();
				error(errorMessage);
			}
		};
		add(errorCallbackBehavior);
	}

	public EmptyPanel maakEmptyPanel(String id)
	{
		EmptyPanel panel = new EmptyPanel(id);
		panel.setVisible(false);
		panel.setOutputMarkupPlaceholderTag(true);
		panel.setOutputMarkupId(true);
		return panel;
	}
}
