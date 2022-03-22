package nl.rivm.screenit.main.web.gebruiker.screening.mamma.be.popup;

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

import nl.rivm.screenit.main.web.gebruiker.algemeen.documenttemplatetesten.PdfViewer;
import nl.rivm.screenit.model.mamma.MammaBeoordeling;
import nl.rivm.screenit.model.mamma.MammaLezing;
import nl.rivm.screenit.service.UploadDocumentService;
import nl.rivm.screenit.service.mamma.MammaBaseBeoordelingService;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.markup.html.panel.GenericPanel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.spring.injection.annot.SpringBean;

public abstract class LezingPdfDialogPanel extends GenericPanel<MammaLezing>
{
	@SpringBean
	private MammaBaseBeoordelingService baseBeoordelingService;

	@SpringBean
	private UploadDocumentService uploadDocumentService;

	public LezingPdfDialogPanel(String id, IModel<MammaLezing> model)
	{
		super(id, model);

		MammaBeoordeling beoordeling = baseBeoordelingService.getBeoordelingVanLezing(getModelObject());
		if (beoordeling != null && beoordeling.getVerslagPdf() != null)
		{
			File verslag = uploadDocumentService.load(beoordeling.getVerslagPdf());
			add(new PdfViewer("verslagPdf", verslag, false));
		}
	}

	public abstract void close(AjaxRequestTarget target);
}
