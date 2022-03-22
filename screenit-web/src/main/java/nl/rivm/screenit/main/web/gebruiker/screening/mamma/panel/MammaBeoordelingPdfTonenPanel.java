package nl.rivm.screenit.main.web.gebruiker.screening.mamma.panel;

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

import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.main.web.gebruiker.algemeen.documenttemplatetesten.PdfViewer;
import nl.rivm.screenit.model.mamma.MammaBeoordeling;
import nl.rivm.screenit.service.UploadDocumentService;
import nl.rivm.screenit.service.mamma.MammaBaseVerslagService;

import org.apache.wicket.markup.html.panel.GenericPanel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.spring.injection.annot.SpringBean;

@Slf4j
public class MammaBeoordelingPdfTonenPanel extends GenericPanel<MammaBeoordeling>
{
	@SpringBean
	private UploadDocumentService uploadDocumentService;

	@SpringBean
	private MammaBaseVerslagService verslagService;

	public MammaBeoordelingPdfTonenPanel(String id, IModel<MammaBeoordeling> model)
	{
		super(id, model);
		if (getModelObject().getVerslagPdf() != null)
		{
			File verslag = uploadDocumentService.load(getModelObject().getVerslagPdf());
			add(new PdfViewer("verslagPdf", verslag, false));
		}
		else
		{
			File tempFile = maakVerwijsVerslagPdfFile();
			add(new PdfViewer("verslagPdf", tempFile, true));
		}
	}

	private File maakVerwijsVerslagPdfFile()
	{
		File tempFile = null;
		try
		{
			tempFile = verslagService.maakFileVoorPdfViewer(getModelObject());
		}
		catch (Exception e)
		{
			LOG.error("Error while generating pdf", e);
			error("Er is iets misgegaan met het genereren van het verwijsverslag.");
		}
		return tempFile;
	}
}
