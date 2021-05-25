package nl.rivm.screenit.main.web.component.form.upload;

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

import java.util.Formatter;
import java.util.List;

import org.apache.wicket.ajax.AjaxEventBehavior;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.markup.head.IHeaderResponse;
import org.apache.wicket.markup.head.JavaScriptHeaderItem;
import org.apache.wicket.markup.head.OnDomReadyHeaderItem;
import org.apache.wicket.markup.html.form.upload.FileUpload;
import org.apache.wicket.markup.html.form.upload.FileUploadField;
import org.apache.wicket.model.IModel;
import org.apache.wicket.request.resource.JavaScriptResourceReference;
import org.apache.wicket.request.resource.ResourceReference;
import org.apache.wicket.util.lang.Bytes;

public class ScreenitFileUploadField extends FileUploadField
{
	private static final String FILE_SIZE_ERROR_TRIGGER = "FILE_SIZE_ERROR_TRIGGER";

	private static final ResourceReference JS = new JavaScriptResourceReference(
		ScreenitUploadProgressBar.class, "ScreenitFileUploadField.js");

	private Bytes maxFileSize;

	public ScreenitFileUploadField(String id)
	{
		super(id);
	}

	public ScreenitFileUploadField(String id, IModel<? extends List<FileUpload>> model)
	{
		super(id, model);
	}

	public ScreenitFileUploadField(String id, IModel<? extends List<FileUpload>> model, Bytes maxFileSize)
	{
		super(id, model);
		this.maxFileSize = maxFileSize;
		this.setOutputMarkupId(true);
		add(new AjaxEventBehavior(FILE_SIZE_ERROR_TRIGGER)
		{
			protected void onEvent(final AjaxRequestTarget target)
			{
				error(String.format(getString("fileSizeExceeded"), maxFileSize.toString(getSession().getLocale())));
			}
		});
	}

	@Override
	public void renderHead(final IHeaderResponse response)
	{
		if (this.maxFileSize != null)
		{
			response.render(JavaScriptHeaderItem.forReference(JS));
			StringBuilder builder = new StringBuilder(128);
			Formatter formatter = new Formatter(builder);
			formatter.format("new ScreenIT.FileUploadField('%s', '%s');", getMarkupId(), this.maxFileSize.bytes());
			response.render(OnDomReadyHeaderItem.forScript(builder.toString()));
		}
	}

}
