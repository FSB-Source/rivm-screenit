
package nl.rivm.screenit.main.web.gebruiker.algemeen.documenttemplatetesten;

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

import java.io.File;

import org.apache.commons.io.FileUtils;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.markup.html.AjaxLink;
import org.apache.wicket.markup.html.panel.Panel;

public class PdfViewerPanel extends Panel
{
	private static final long serialVersionUID = 1L;

	public PdfViewerPanel(String id, final File tempFile)
	{
		super(id);

		add(new PdfViewer("pdfObject", tempFile));
		add(new AjaxLink<Void>("closeXButton")
		{
			@Override
			public void onClick(AjaxRequestTarget target)
			{
				deleteTempFile(tempFile);
				onClose(target);
			}
		});

		add(new AjaxLink<Void>("sluitenBtn")
		{
			@Override
			public void onClick(AjaxRequestTarget target)
			{
				deleteTempFile(tempFile);
				onClose(target);
			}
		});
	}

	protected void onClose(AjaxRequestTarget target)
	{
	}

	public void deleteTempFile(File tempFile)
	{
		FileUtils.deleteQuietly(tempFile);
	}

}
