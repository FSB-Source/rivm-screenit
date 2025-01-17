package nl.rivm.screenit.service.mamma.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
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
import java.util.Comparator;

import nl.rivm.screenit.model.BriefDefinitie;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.MailMergeContext;
import nl.rivm.screenit.model.enums.BriefType;
import nl.rivm.screenit.model.mamma.MammaBeoordeling;
import nl.rivm.screenit.model.mamma.MammaBrief;
import nl.rivm.screenit.model.mamma.MammaScreeningRonde;
import nl.rivm.screenit.service.AsposeService;
import nl.rivm.screenit.service.BaseBriefService;
import nl.rivm.screenit.service.ClientService;
import nl.rivm.screenit.service.UploadDocumentService;
import nl.rivm.screenit.service.mamma.MammaBaseBeoordelingService;
import nl.rivm.screenit.service.mamma.MammaGeenBeoordelingMogelijkBriefCreator;
import nl.rivm.screenit.service.mamma.MammaGeenBeoordelingMogelijkBriefService;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import com.aspose.words.Document;

@Service
@Transactional(propagation = Propagation.SUPPORTS, readOnly = true)
public class MammaGeenBeoordelingMogelijkBriefServiceImpl implements MammaGeenBeoordelingMogelijkBriefService
{
	@Autowired
	private BaseBriefService briefService;

	@Autowired
	private UploadDocumentService uploadDocumentService;

	@Autowired
	private AsposeService asposeService;

	@Autowired
	private ClientService clientService;

	@Autowired
	private MammaBaseBeoordelingService beoordelingService;

	private final BriefType briefType = BriefType.MAMMA_GEEN_BEOORDELING_MOGELIJK;

	@Override
	public File maakFileVoorPdfViewer(MammaBeoordeling beoordeling) throws Exception
	{
		Document document = genereerBrief(beoordeling);
		return briefService.genereerPdf(document, "geenBeoordelingMogelijkBrief", false);
	}

	private Document genereerBrief(MammaBeoordeling beoordeling) throws Exception
	{
		MailMergeContext context = maakMailMergeContext(beoordeling);

		File briefTemplate = haalBriefTemplateOp();

		return asposeService.processDocumentWithCreator(context, briefTemplate, new MammaGeenBeoordelingMogelijkBriefCreator(), true);
	}

	private MailMergeContext maakMailMergeContext(MammaBeoordeling beoordeling)
	{
		MailMergeContext context = new MailMergeContext();

		Client client = beoordelingService.getClientVanBeoordeling(beoordeling);
		context.setClient(client);
		MammaScreeningRonde screeningRonde = beoordelingService.getScreeningRonde(beoordeling);
		MammaBrief brief = screeningRonde.getBrieven().stream()
			.filter(mammaBrief -> briefType.equals(mammaBrief.getBriefType()))
			.max(Comparator.comparing(MammaBrief::getCreatieDatum))
			.orElseThrow(() -> new IllegalStateException("Geen brief gevonden van type MAMMA_GEEN_BEOORDELING_MOGELIJK"));
		context.setBrief(brief);
		context.putValue(MailMergeContext.CONTEXT_MAMMA_CE, clientService.bepaalCe(client));

		return context;
	}

	private File haalBriefTemplateOp()
	{
		BriefDefinitie definitie = briefService.getNieuwsteBriefDefinitie(briefType);

		return uploadDocumentService.load(definitie.getDocument());
	}
}
