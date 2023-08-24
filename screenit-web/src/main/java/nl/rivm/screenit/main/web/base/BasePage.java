
package nl.rivm.screenit.main.web.base;

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

import javax.servlet.http.HttpSession;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.markup.head.CssHeaderItem;
import org.apache.wicket.markup.head.IHeaderResponse;
import org.apache.wicket.markup.head.JavaScriptHeaderItem;
import org.apache.wicket.markup.head.PriorityHeaderItem;
import org.apache.wicket.markup.head.StringHeaderItem;
import org.apache.wicket.markup.html.WebPage;
import org.apache.wicket.protocol.http.servlet.ServletWebRequest;
import org.apache.wicket.request.Request;
import org.apache.wicket.request.cycle.RequestCycle;
import org.apache.wicket.resource.JQueryResourceReference;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public abstract class BasePage extends WebPage
{

	private static final Logger LOG = LoggerFactory.getLogger(BasePage.class);

	private static final long serialVersionUID = 1L;

	@SpringBean(name = "testModus")
	private Boolean testModus;

	public BasePage()
	{
		LOG.info("Pagina " + this.getClass().getName() + " wordt geladen");
		Request request = RequestCycle.get().getRequest();
		HttpSession httpSession = ((ServletWebRequest) request).getContainerRequest().getSession();
		LOG.debug("st" + httpSession.getMaxInactiveInterval());
	}

	@Override
	public void renderHead(IHeaderResponse response)
	{
		super.renderHead(response);
		response.render(new PriorityHeaderItem(StringHeaderItem.forString("<meta charset=\"utf-8\" />")));

		response.render(JavaScriptHeaderItem.forReference(JQueryResourceReference.getV3()));
		response.render(JavaScriptHeaderItem.forReference(getApplication().getJavaScriptLibrarySettings().getWicketAjaxReference()));
		response.render(JavaScriptHeaderItem.forUrl("assets/js/libs/qtip/jquery.qtip.min.js"));
		response.render(JavaScriptHeaderItem.forUrl("assets/js/scripts.js"));
		response.render(JavaScriptHeaderItem.forUrl("assets/js/scripts-medewerkerportaal.js"));
		response.render(JavaScriptHeaderItem.forUrl("assets/js/libs/select2/select2.min.js"));
		response.render(JavaScriptHeaderItem.forUrl("assets/js/bootstrap.min.js"));
		response.render(JavaScriptHeaderItem.forUrl("assets/js/radio/biradsKeuze.js"));
		response.render(JavaScriptHeaderItem.forUrl("assets/js/borst.js"));
		response.render(JavaScriptHeaderItem.forUrl("assets/js/checkbox/checkboxOnderzoeken.js"));
		response.render(JavaScriptHeaderItem.forUrl("assets/js/checkbox/checkboxSelectAll.js"));
		response.render(JavaScriptHeaderItem.forUrl("assets/js/checkbox/check-all.js"));
		response.render(JavaScriptHeaderItem.forUrl("assets/js/datepicker/datepickerShow.js"));
		response.render(JavaScriptHeaderItem.forUrl("assets/js/laesieiconen/mouseevents.js"));
		response.render(JavaScriptHeaderItem.forUrl("assets/js/laadEvents.js"));
		response.render(JavaScriptHeaderItem.forUrl("assets/js/brieventemplates/brieventemplatesUitklappen.js"));

		response.render(CssHeaderItem.forUrl("assets/js/libs/select2/select2.css"));
		response.render(CssHeaderItem.forUrl("assets/css/colonis-ui/jquery-ui-1.8.16.custom.css"));
		response.render(CssHeaderItem.forUrl("assets/js/libs/qtip/jquery.qtip.min.css"));
		response.render(CssHeaderItem.forUrl("assets/css/bootstrap/bootstrap.min.css"));
		response.render(CssHeaderItem.forUrl("assets/css/base_styles.css"));

		response.render(JavaScriptHeaderItem.forUrl("assets/js/datepicker/datepickerShow.js"));
		response.render(JavaScriptHeaderItem.forUrl("assets/js/datepicker/datepickerFilterDatum.js"));
		response.render(JavaScriptHeaderItem.forUrl("assets/js/datepicker/datepickerBooleanJaarEnMaand.js"));
		response.render(JavaScriptHeaderItem.forUrl("assets/js/datepicker/datepickerVandaag.js"));

		response.render(JavaScriptHeaderItem.forUrl("assets/js/button/buttonFormValues.js"));

		if (Boolean.TRUE.equals(testModus))
		{
			response.render(CssHeaderItem.forUrl("assets/css/test.css"));
		}

		String bevatFormulieren;
		if (bevatFormulieren())
		{
			bevatFormulieren = "true";
		}
		else
		{
			bevatFormulieren = "false";
		}

		response.render(JavaScriptHeaderItem.forScript("var bevatFormulieren=" + bevatFormulieren + ";", "bevatFormulieren"));
	}

	protected boolean bevatFormulieren()
	{
		return false;
	}

	public static void markeerFormulierenOpgeslagen(AjaxRequestTarget target)
	{
		target.appendJavaScript("resetFormValues();");
	}

	public abstract ScreenitContext getContext();

	public abstract void refreshFeedback(AjaxRequestTarget target);
}
