package nl.rivm.screenit.main.web.component.table;

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

import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.main.web.component.AjaxDownload;
import nl.rivm.screenit.model.UploadDocument;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.extensions.ajax.markup.html.IndicatingAjaxLink;
import org.apache.wicket.markup.html.panel.GenericPanel;
import org.apache.wicket.model.IModel;
import org.jetbrains.annotations.NotNull;

@Slf4j
public class UploadDocumentDownloadLinkPanel extends GenericPanel<UploadDocument>
{
	private final IModel<String> fileNameToLog;

	public UploadDocumentDownloadLinkPanel(String id, IModel<UploadDocument> model)
	{
		this(id, model, null);
	}

	public UploadDocumentDownloadLinkPanel(String id, IModel<UploadDocument> model, IModel<String> fileNameToLog)
	{
		super(id, model);
		this.fileNameToLog = fileNameToLog;
	}

	@Override
	protected void onInitialize()
	{
		super.onInitialize();
		add(new IndicatingAjaxLink<>("download", getModel())
		{
			@Override
			public void onClick(AjaxRequestTarget target)
			{
				UploadDocumentDownloadLinkPanel.this.onBeforeDownloadClick(target);
				getAjaxDownload().initiate(target);
			}
		});
	}

	@NotNull
	private AjaxDownload getAjaxDownload()
	{
		verwijderDubbeleDownloadBehavioursVanDePage();
		var download = new UploadDocumentDownloadBehavior(fileNameToLog, getModel());
		getPage().add(download);
		return download;
	}

	private void verwijderDubbeleDownloadBehavioursVanDePage()
	{

		var id = getModelObject().getId();
		getPage().getBehaviors(UploadDocumentDownloadBehavior.class)
			.stream().filter(b -> b.getUploadDocumentId().equals(id))
			.forEach(b -> getPage().remove(b));
	}

	protected void onBeforeDownloadClick(AjaxRequestTarget target)
	{

	}

	@Override
	protected void onConfigure()
	{
		super.onConfigure();
		UploadDocument document = getModelObject();
		setVisible(document != null && !Boolean.FALSE.equals(document.getActief()));
	}

}
