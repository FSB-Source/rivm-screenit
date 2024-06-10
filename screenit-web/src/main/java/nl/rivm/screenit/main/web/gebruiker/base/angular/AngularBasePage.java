package nl.rivm.screenit.main.web.gebruiker.base.angular;

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

import java.io.IOException;
import java.util.List;

import nl.rivm.screenit.main.web.ScreenitApplication;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.filter.SecurityHeadersFilter;
import nl.rivm.screenit.main.web.gebruiker.base.GebruikerBasePage;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.core.util.string.JavaScriptUtils;
import org.apache.wicket.markup.ComponentTag;
import org.apache.wicket.markup.head.CssUrlReferenceHeaderItem;
import org.apache.wicket.markup.head.IHeaderResponse;
import org.apache.wicket.markup.head.OnDomReadyHeaderItem;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.repeater.RepeatingView;
import org.apache.wicket.protocol.http.WebApplication;
import org.apache.wicket.request.cycle.RequestCycle;
import org.apache.wicket.request.http.WebResponse;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.wicketstuff.wiquery.core.javascript.JsStatement;

import com.fasterxml.jackson.databind.ObjectMapper;

public abstract class AngularBasePage extends GebruikerBasePage
{
	private static final ObjectMapper objectMapper = new ObjectMapper();

	@SpringBean(name = "medewerkerPortaalResourceUrl")
	private String medewerkerPortaalResourceUrl;

	private final List<String> scripts = List.of(
		"runtime.js",
		"polyfills.js",
		"main.js"
	);

	private final List<String> styles = List.of(
		"styles.css"
	);

	protected abstract String getComponent();

	@SpringBean(name = "applicationEnvironment")
	private String applicationEnvironment;

	@Override
	public void onInitialize()
	{
		super.onInitialize();

		var environmentData = new EnvironmentData();
		environmentData.setSession(ScreenitSession.get().getId());

		var appRoot = new WebMarkupContainer("appRoot")
		{
			@Override
			protected void onComponentTag(ComponentTag tag)
			{
				super.onComponentTag(tag);

				tag.getAttributes().put("ngCspNonce", WebApplication.get().getCspSettings().getNonce(RequestCycle.get()));
				tag.getAttributes().put("data-component", getComponent());
			}
		};
		add(appRoot);

		var scriptElements = getAngularScriptElements();
		add(scriptElements);

		try
		{
			var json = objectMapper.writeValueAsString(environmentData);

			var environmentDataContainer = new Label("environmentData", json);
			environmentDataContainer.setEscapeModelStrings(false);
			add(environmentDataContainer);
		}
		catch (IOException e)
		{
			throw new RuntimeException(e);
		}
	}

	private RepeatingView getAngularScriptElements()
	{
		var scriptElements = new RepeatingView("angularScripts");
		for (var script : scripts)
		{
			var scriptEl = new WebMarkupContainer(script)
			{
				@Override
				protected void onComponentTag(ComponentTag tag)
				{
					super.onComponentTag(tag);

					tag.getAttributes().put(JavaScriptUtils.ATTR_CSP_NONCE, WebApplication.get().getCspSettings().getNonce(RequestCycle.get()));
					tag.getAttributes().put(JavaScriptUtils.ATTR_SCRIPT_SRC, medewerkerPortaalResourceUrl + script + "?version=" + ScreenitApplication.get().getVersionString());
					tag.getAttributes().put(JavaScriptUtils.ATTR_TYPE, "module");
				}
			};
			scriptEl.setOutputMarkupId(true);
			scriptElements.add(scriptEl);
		}
		return scriptElements;
	}

	@Override
	public void refreshFeedback(AjaxRequestTarget target)
	{

	}

	@Override
	protected void setHeaders(WebResponse response)
	{
		if (applicationEnvironment.equalsIgnoreCase("development"))
		{
			SecurityHeadersFilter.allowExtraConnectSrcInContentSecurityPolicy(response, "http://localhost:4200 ws://localhost:4200");
		}
		super.setHeaders(response);
	}

	@Override
	public void renderHead(IHeaderResponse response)
	{
		super.renderHead(response);

		for (var style : styles)
		{
			response.render(
				new CssUrlReferenceHeaderItem(medewerkerPortaalResourceUrl + style + "?version=" + ScreenitApplication.get().getVersionString(), "screen", "stylesheet"));
		}
	}

	@Override
	protected void setupTimeout(IHeaderResponse response)
	{
		var jsStatement = new JsStatement();
		jsStatement.append("window.screenit = window.screenit || {};");
		jsStatement.append("window.screenit.keepAliveCallback=" + keepAliveBehavior.getCallbackFunction() + ";");
		jsStatement.append("window.screenit.logoutCallback=" + logoutBehavior.getCallbackFunction() + ";");
		response.render(OnDomReadyHeaderItem.forScript(jsStatement.render()));
	}
}
