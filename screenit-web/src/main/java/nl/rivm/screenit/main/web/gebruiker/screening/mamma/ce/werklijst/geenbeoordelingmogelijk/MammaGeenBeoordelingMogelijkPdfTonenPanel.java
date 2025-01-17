package nl.rivm.screenit.main.web.gebruiker.screening.mamma.ce.werklijst.geenbeoordelingmogelijk;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
 * %%
 * Copyright (C) 2012 - 2025 Facilitaire Samenwerking Bevolkingsonderzoek
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
import nl.rivm.screenit.service.mamma.MammaGeenBeoordelingMogelijkBriefService;

import org.apache.wicket.markup.html.panel.GenericPanel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class MammaGeenBeoordelingMogelijkPdfTonenPanel extends GenericPanel<MammaBeoordeling>
{

	private static final Logger LOG = LoggerFactory.getLogger(MammaGeenBeoordelingMogelijkPdfTonenPanel.class);

	@SpringBean
	private MammaGeenBeoordelingMogelijkBriefService briefService;

	public MammaGeenBeoordelingMogelijkPdfTonenPanel(String id, IModel<MammaBeoordeling> model)
	{
		super(id, model);
		File tempFile = maakPdfFile();
		add(new PdfViewer("briefPdf", tempFile, true));
	}

	private File maakPdfFile()
	{
		File tempFile = null;
		try
		{
			tempFile = briefService.maakFileVoorPdfViewer(getModelObject());
		}
		catch (Exception e)
		{
			LOG.error("Error while generating pdf", e);
			error("Er is iets misgegaan met het genereren van de brief.");
		}
		return tempFile;
	}
}
