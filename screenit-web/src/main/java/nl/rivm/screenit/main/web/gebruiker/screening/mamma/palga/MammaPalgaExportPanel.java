package nl.rivm.screenit.main.web.gebruiker.screening.mamma.palga;

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

import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.component.ConfirmingIndicatingAjaxLink;
import nl.rivm.screenit.main.web.component.table.UploadDocumentDownloadLinkPanel;
import nl.rivm.screenit.main.web.gebruiker.base.GebruikerBasePage;
import nl.rivm.screenit.model.UploadDocument;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.service.LogService;
import nl.rivm.screenit.service.mamma.MammaPalgaService;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.panel.GenericPanel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.spring.injection.annot.SpringBean;

public class MammaPalgaExportPanel extends GenericPanel<UploadDocument>
{
	@SpringBean
	private LogService logService;

	@SpringBean
	private MammaPalgaService palgaService;

	public MammaPalgaExportPanel(String id, IModel<UploadDocument> model)
	{
		super(id, model);
	}

	@Override
	protected void onInitialize()
	{
		super.onInitialize();
		add(new Label("naam"));
		add(new UploadDocumentDownloadLinkPanel("downloadPanel", getModel())
		{
			@Override
			protected void onBeforeDownloadClick(AjaxRequestTarget target)
			{
				super.onBeforeDownloadClick(target);
				String logRegel = String.format("Gedownload: %s", getModelObject().getNaam());
				logService.logGebeurtenis(LogGebeurtenis.MAMMA_PALGA_CSV_EXPORT, ScreenitSession.get().getLoggedInAccount(), logRegel);
			}
		});
		add(new ConfirmingIndicatingAjaxLink<UploadDocument>("verwijderen", getModel(), ((GebruikerBasePage) getPage()).getDialog(),
			"palga.export.verwijderen")
		{
			@Override
			public void onClick(AjaxRequestTarget target)
			{
				palgaService.deleteExports(getModelObject().getNaam(), ScreenitSession.get().getLoggedInAccount());
				setResponsePage(new MammaPalgaUitwisselingPage());
			}
		}.setOutputMarkupId(true));
	}

}
