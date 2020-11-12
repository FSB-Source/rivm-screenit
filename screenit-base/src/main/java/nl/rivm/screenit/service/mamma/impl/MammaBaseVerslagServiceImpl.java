package nl.rivm.screenit.service.mamma.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
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

import java.io.File;

import nl.rivm.screenit.model.BriefDefinitie;
import nl.rivm.screenit.model.MailMergeContext;
import nl.rivm.screenit.model.UploadDocument;
import nl.rivm.screenit.model.enums.BriefType;
import nl.rivm.screenit.model.enums.FileStoreLocation;
import nl.rivm.screenit.model.enums.MergeField;
import nl.rivm.screenit.model.mamma.MammaBeoordeling;
import nl.rivm.screenit.model.mamma.enums.MammaBeoordelingStatus;
import nl.rivm.screenit.service.AsposeService;
import nl.rivm.screenit.service.BaseBriefService;
import nl.rivm.screenit.service.ClientService;
import nl.rivm.screenit.service.FileService;
import nl.rivm.screenit.service.mamma.MammaBaseBeoordelingService;
import nl.rivm.screenit.service.mamma.MammaBaseVerslagService;
import nl.rivm.screenit.service.mamma.be.verslag.MammaVerslagDocumentCreator;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import com.aspose.words.Document;

@Service
@Transactional(propagation = Propagation.SUPPORTS, readOnly = true)
public class MammaBaseVerslagServiceImpl implements MammaBaseVerslagService
{
	private static final Logger LOG = LoggerFactory.getLogger(MammaBaseVerslagServiceImpl.class);

	@Autowired
	private BaseBriefService briefService;

	@Autowired
	private FileService fileService;

	@Autowired
	private AsposeService asposeService;

	@Autowired
	private MammaBaseBeoordelingService beoordelingService;

	@Autowired
	private HibernateService hibernateService;

	@Autowired
	private ClientService clientService;

	@Override
	public File maakFileVoorPdfViewer(MammaBeoordeling beoordeling) throws Exception
	{
		Document document = genereerVerslag(beoordeling, BriefType.MAMMA_VERWIJSVERSLAG);
		return briefService.genereerPdf(document, "verwijsverslag", false);
	}

	@Override
	public File maakFileVoorPdfViewer(MammaBeoordeling beoordeling, BriefType briefType) throws Exception
	{
		Document document = genereerVerslag(beoordeling, briefType);
		return briefService.genereerPdf(document, "verwijsverslag", false);
	}

	@Override
	public void verslagNaarFileStoreSchrijven(MammaBeoordeling beoordeling) throws Exception
	{
		File tmpVerslag = maakFileVoorPdfViewer(beoordeling);
		UploadDocument document = new UploadDocument();
		document.setActief(true);
		document.setContentType("application/pdf");
		document.setFile(tmpVerslag);
		fileService.saveOrUpdateUploadDocument(document, FileStoreLocation.MAMMA_VERSLAG, beoordelingService.getClientVanBeoordeling(beoordeling).getId());
		beoordeling.setVerslagPdf(document);
		hibernateService.saveOrUpdateAll(document, beoordeling);
		if (tmpVerslag.exists() && !tmpVerslag.delete())
		{
			fileService.delete(document, true);
			LOG.error("Kon geen verslag wegschrijven naar de filestore voor beoordeling {}", beoordeling.getId());
			throw new RuntimeException("Kon tijdelijk bestand niet verwijderen");
		}
	}

	private Document genereerVerslag(MammaBeoordeling beoordeling, BriefType briefType) throws Exception
	{
		MailMergeContext context = maakMailMergeContext(beoordeling);
		File briefTemplate = haalBriefTemplateOp(briefType);
		if (briefType.equals(BriefType.MAMMA_VERWIJSVERSLAG))
		{
			beoordeling.getVerslagLezing().setBeoordeling(beoordeling);
			return asposeService.processDocumentWithCreator(context, briefTemplate, new MammaVerslagDocumentCreator(beoordeling.getVerslagLezing()), true);
		}
		else
		{
			return asposeService.processDocumentWithCreator(context, briefTemplate, new MammaVerslagDocumentCreator(null), true);
		}
	}

	private MailMergeContext maakMailMergeContext(MammaBeoordeling beoordeling)
	{
		MailMergeContext context = new MailMergeContext();
		context.putValue(MergeField.MAMMA_ONDERZOEK_DATUM.getFieldName(), beoordeling.getOnderzoek().getCreatieDatum());
		context.setClient(beoordelingService.getClientVanBeoordeling(beoordeling));
		context.setBrief(beoordeling.getOnderzoek().getAfspraak().getUitnodiging().getScreeningRonde().getLaatsteBrief());
		context.putValue(MailMergeContext.CONTEXT_MAMMA_CE, clientService.bepaalCe(context.getClient()));
		return context;
	}

	private File haalBriefTemplateOp(BriefType briefType)
	{
		BriefDefinitie definitie = briefService.getNieuwsteBriefDefinitie(briefType);
		return fileService.load(definitie.getDocument());
	}

	@Override
	public File getVerslagFile(MammaBeoordeling beoordeling) throws Exception
	{
		File verslag;
		if (beoordeling.getStatus() == MammaBeoordelingStatus.UITSLAG_GUNSTIG)
		{
			verslag = maakFileVoorPdfViewer(beoordeling, BriefType.MAMMA_VERSLAG_UITSLAG_NEGATIEF);
		}
		else
		{
			verslag = fileService.load(beoordeling.getVerslagPdf());
		}
		return verslag;
	}
}
