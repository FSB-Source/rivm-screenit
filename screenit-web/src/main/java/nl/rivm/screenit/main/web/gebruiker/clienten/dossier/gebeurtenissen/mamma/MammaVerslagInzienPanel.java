package nl.rivm.screenit.main.web.gebruiker.clienten.dossier.gebeurtenissen.mamma;

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

import java.io.File;

import nl.rivm.screenit.main.model.ScreeningRondeGebeurtenis;
import nl.rivm.screenit.main.web.gebruiker.algemeen.documenttemplatetesten.PdfViewer;
import nl.rivm.screenit.main.web.gebruiker.clienten.dossier.gebeurtenissen.AbstractGebeurtenisDetailPanel;
import nl.rivm.screenit.model.UploadDocument;
import nl.rivm.screenit.model.mamma.MammaScreeningRonde;
import nl.rivm.screenit.service.UploadDocumentService;

import org.apache.wicket.model.IModel;
import org.apache.wicket.spring.injection.annot.SpringBean;

public class MammaVerslagInzienPanel extends AbstractGebeurtenisDetailPanel
{

	@SpringBean
	private UploadDocumentService uploadDocumentService;

	public MammaVerslagInzienPanel(String id, IModel<ScreeningRondeGebeurtenis> model)
	{
		super(id, model);
		UploadDocument file = ((MammaScreeningRonde) getModelObject().getScreeningRondeGebeurtenissen().getScreeningRonde()).getLaatsteUitnodiging().getLaatsteAfspraak()
			.getOnderzoek().getLaatsteBeoordeling().getVerslagPdf();
		File verslag = uploadDocumentService.load(file);
		add(new PdfViewer("verslag", verslag, false));
	}
}
