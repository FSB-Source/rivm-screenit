package nl.rivm.screenit.batch.jobs.mamma.brieven.client.genererenstep;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-bk
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

import java.text.SimpleDateFormat;
import java.util.Date;

import lombok.AllArgsConstructor;

import nl.rivm.screenit.batch.jobs.brieven.genereren.AbstractBrievenGenererenWriter;
import nl.rivm.screenit.batch.jobs.mamma.brieven.client.MammaBriefConstants;
import nl.rivm.screenit.document.BaseDocumentCreator;
import nl.rivm.screenit.model.MailMergeContext;
import nl.rivm.screenit.model.ScreeningOrganisatie;
import nl.rivm.screenit.model.UploadDocument;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.BriefType;
import nl.rivm.screenit.model.enums.FileStoreLocation;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.model.mamma.MammaBrief;
import nl.rivm.screenit.model.mamma.MammaMergedBrieven;
import nl.rivm.screenit.model.mamma.MammaStandplaats;
import nl.rivm.screenit.service.AsposeService;
import nl.rivm.screenit.service.ClientService;
import nl.rivm.screenit.service.UploadDocumentService;
import nl.rivm.screenit.service.mamma.MammaBaseBeoordelingService;
import nl.rivm.screenit.service.mamma.be.verslag.MammaVerslagDocumentCreator;
import nl.rivm.screenit.util.mamma.MammaScreeningRondeUtil;

import org.apache.commons.io.FileUtils;
import org.springframework.stereotype.Component;

import com.aspose.words.Document;
import com.aspose.words.ImportFormatMode;

@Component
@AllArgsConstructor
public class MammaBrievenGenererenWriter extends AbstractBrievenGenererenWriter<MammaBrief, MammaMergedBrieven>
{
	private final ClientService clientService;

	private final MammaBaseBeoordelingService beoordelingService;

	private final AsposeService asposeService;

	private final UploadDocumentService uploadDocumentService;

	@Override
	protected MammaMergedBrieven createConcreteMergedBrieven(Date aangemaaktOp)
	{
		var executionContext = getStepExecutionContext();
		var briefType = BriefType.valueOf(executionContext.getString(MammaBrievenGenererenPartitioner.KEY_BRIEFTYPE));
		var screeningOrganisatie = getHibernateService().load(ScreeningOrganisatie.class, executionContext.getLong(MammaBrievenGenererenPartitioner.KEY_SCREENINGORGANISATIEID));

		var mergedBrieven = new MammaMergedBrieven();
		mergedBrieven.setScreeningOrganisatie(screeningOrganisatie);
		mergedBrieven.setCreatieDatum(aangemaaktOp);
		mergedBrieven.setBriefType(briefType);

		return mergedBrieven;
	}

	@Override
	public String getMergedBrievenNaam(MammaMergedBrieven brieven)
	{
		var standplaatsId = (Long) getStepExecutionContext().get(MammaBrievenGenererenPartitioner.KEY_MAMMASTANDPLAATSID);
		var tijdelijk = (Boolean) getStepExecutionContext().get(MammaBrievenGenererenPartitioner.KEY_TIJDELIJK);
		var eersteRonde = (Boolean) getStepExecutionContext().get(MammaBrievenGenererenPartitioner.KEY_EERSTE_RONDE);

		String naam = "";
		var dateFormat = new SimpleDateFormat("yyyy-MM-dd_HH.mm");
		if (brieven.getCreatieDatum() != null)
		{
			naam += dateFormat.format(brieven.getCreatieDatum()) + "-";
		}
		if (brieven.getScreeningOrganisatie() != null)
		{
			var soNaam = brieven.getScreeningOrganisatie().getNaam();
			soNaam = soNaam.replaceAll(" ", "_");
			naam += soNaam + "-";
		}
		if (standplaatsId != null)
		{
			var standplaats = getHibernateService().load(MammaStandplaats.class, standplaatsId);
			naam += standplaats.getNaam().replaceAll(" ", "_") + "-";
		}
		if (Boolean.TRUE.equals(tijdelijk))
		{
			naam += "tijdelijk-";
		}
		if (brieven.getBriefType() != null)
		{
			naam += brieven.getBriefType().name().toLowerCase();
		}

		if (Boolean.TRUE.equals(eersteRonde))
		{
			naam += MammaBrievenGenererenPartitioner.KEY_EERSTE_RONDE;
		}

		naam = addPdfCounter(naam);

		return naam + ".pdf";
	}

	@Override
	public void additionalActiesWithDocument(MailMergeContext context, MammaBrief brief, Document chunkDocument) throws Exception
	{
		mergeBijlagen(context, chunkDocument);
	}

	private void mergeBijlagen(MailMergeContext context, Document chunkDocument) throws Exception
	{
		var briefTypeApart = (Boolean) getStepExecutionContext().get(MammaBrievenGenererenPartitioner.KEY_BRIEFTYPEAPART);
		var standplaatsId = (Long) getStepExecutionContext().get(MammaBrievenGenererenPartitioner.KEY_MAMMASTANDPLAATSID);

		if (Boolean.TRUE.equals(briefTypeApart) && standplaatsId != null)
		{
			var tijdelijk = (Boolean) getStepExecutionContext().get(MammaBrievenGenererenPartitioner.KEY_TIJDELIJK);
			var standplaats = getHibernateService().load(MammaStandplaats.class, standplaatsId);
			UploadDocument standplaatsLocatieBijlage;
			if (Boolean.TRUE.equals(tijdelijk))
			{
				standplaatsLocatieBijlage = standplaats.getTijdelijkeLocatie().getStandplaatsLocatieBijlage();
			}
			else
			{
				standplaatsLocatieBijlage = standplaats.getLocatie().getStandplaatsLocatieBijlage();
			}

			if (standplaatsLocatieBijlage != null && standplaatsLocatieBijlage.getActief())
			{
				var bijlage = uploadDocumentService.load(standplaatsLocatieBijlage);
				var bijlageBytes = FileUtils.readFileToByteArray(bijlage);
				var bijlageDocument = asposeService.processDocument(bijlageBytes, context);
				chunkDocument.getLastSection().getHeadersFooters().linkToPrevious(false);
				chunkDocument.appendDocument(bijlageDocument, ImportFormatMode.KEEP_SOURCE_FORMATTING);
			}
		}
	}

	@Override
	public void additionalMergedContext(MailMergeContext context)
	{
		context.putValue(MailMergeContext.CONTEXT_MAMMA_CE, clientService.bepaalCe(context.getClient()));
	}

	@Override
	protected String getRapportageAantalBrievenKey()
	{
		return MammaBriefConstants.RAPPORTAGEKEYAANTALBRIEVEN;
	}

	@Override
	public FileStoreLocation getFileStoreLocation()
	{
		return FileStoreLocation.MAMMA_MERGED_BRIEVEN;
	}

	@Override
	public LogGebeurtenis getMergeProbleemLogGebeurtenis()
	{
		return LogGebeurtenis.MAMMA_BRIEF_MERGE_FOUT;
	}

	@Override
	public LogGebeurtenis getOnvolledigAdresLogGebeurtenis()
	{
		return LogGebeurtenis.MAMMA_ONVOLLEDIG_ADRES;
	}

	@Override
	public BaseDocumentCreator getDocumentCreator(MailMergeContext context)
	{
		if (BriefType.MAMMA_ONGUNSTIGE_UITSLAG_BIRADS_4_5_ZONDER_HUISARTS.equals(context.getBrief().getBriefType()) ||
			BriefType.MAMMA_ONGUNSTIGE_UITSLAG_BIRADS_0_ZONDER_HUISARTS.equals(context.getBrief().getBriefType()) && context.getClient() != null)
		{
			var beoordeling = beoordelingService.getBeoordelingMetVerslagLezing(MammaScreeningRondeUtil.getAfspraakVanLaatsteOnderzoek(context.getClient().getMammaDossier()));
			if (beoordeling != null)
			{
				beoordeling.getVerslagLezing().setBeoordeling(beoordeling);
				return new MammaVerslagDocumentCreator(beoordeling.getVerslagLezing());
			}
		}
		return null;
	}

	@Override
	public Bevolkingsonderzoek[] getBevolkingsonderzoeken()
	{
		return new Bevolkingsonderzoek[] { Bevolkingsonderzoek.MAMMA };
	}
}
