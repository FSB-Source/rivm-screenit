package nl.rivm.screenit.main.web.component;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
 * %%
 * Copyright (C) 2012 - 2020 Facilitaire Samenwerking Bevolkingsonderzoek
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

import org.apache.commons.lang3.StringUtils;
import org.apache.wicket.Application;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.behavior.AbstractAjaxBehavior;
import org.apache.wicket.request.Response;
import org.apache.wicket.request.handler.resource.ResourceStreamRequestHandler;
import org.apache.wicket.request.http.WebResponse;
import org.apache.wicket.request.resource.ContentDisposition;
import org.apache.wicket.util.resource.IResourceStream;
import org.apache.wicket.util.time.Duration;

public abstract class AjaxDownload extends AbstractAjaxBehavior
{

	private static final long serialVersionUID = 1L;

	private boolean addAntiCache;

	public AjaxDownload()
	{
		this(true);
	}

	public AjaxDownload(boolean addAntiCache)
	{
		super();
		this.addAntiCache = addAntiCache;
	}

	public void initiate(AjaxRequestTarget target)
	{
		String url = getCallbackUrl().toString();

		if (addAntiCache)
		{
			url = url + (url.contains("?") ? "&" : "?");
			url = url + "antiCache=" + System.currentTimeMillis();
		}

		target.appendJavaScript("setTimeout(\"window.location.href='" + url + "'\", 100);");
	}

	@Override
	public void onRequest()
	{
		String fileName = getFileName();
		ResourceStreamRequestHandler handler = new ResourceStreamRequestHandler(getResourceStream(), fileName);
		handler.setContentDisposition(ContentDisposition.ATTACHMENT);
		handler.setCacheDuration(Duration.minutes(30));
		String contentType = getContentType();
		if (contentType == null && fileName != null)
		{
			contentType = Application.get().getMimeType(fileName);
		}
		Response response = getComponent().getRequestCycle().getResponse();
		if (StringUtils.isNotBlank(contentType) && response instanceof WebResponse)
		{
			((WebResponse) response).setContentType(contentType);
		}
		getComponent().getRequestCycle().scheduleRequestHandlerAfterCurrent(handler);
	}

	protected String getContentType()
	{
		return null;
	}

	protected String getFileName()
	{
		return null;
	}

	protected abstract IResourceStream getResourceStream();
}
