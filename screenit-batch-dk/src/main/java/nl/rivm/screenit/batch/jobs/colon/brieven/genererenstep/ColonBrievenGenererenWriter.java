package nl.rivm.screenit.batch.jobs.colon.brieven.genererenstep;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-dk
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

import java.util.Date;

import nl.rivm.screenit.batch.jobs.brieven.genereren.AbstractBrievenGenererenWriter;
import nl.rivm.screenit.batch.jobs.colon.brieven.ColonBrievenConstants;
import nl.rivm.screenit.model.MailMergeContext;
import nl.rivm.screenit.model.ScreeningOrganisatie;
import nl.rivm.screenit.model.colon.ColonBrief;
import nl.rivm.screenit.model.colon.ColonMergedBrieven;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.BriefType;
import nl.rivm.screenit.model.enums.FileStoreLocation;
import nl.rivm.screenit.model.enums.LogGebeurtenis;

import org.springframework.stereotype.Component;

@Component
public class ColonBrievenGenererenWriter extends AbstractBrievenGenererenWriter<ColonBrief, ColonMergedBrieven>
{
	@Override
	protected ColonMergedBrieven createConcreteMergedBrieven(Date aangemaaktOp)
	{
		var context = getStepExecutionContext();
		var briefType = BriefType.valueOf(context.getString(ColonBrievenGenererenPartitioner.KEY_BRIEFTYPE));
		var screeningOrganisatie = getHibernateService().load(ScreeningOrganisatie.class,
			context.getLong(ColonBrievenGenererenPartitioner.KEY_SCREENINGORGANISATIEID));

		var mergedBrieven = new ColonMergedBrieven();
		mergedBrieven.setScreeningOrganisatie(screeningOrganisatie);
		mergedBrieven.setCreatieDatum(aangemaaktOp);
		mergedBrieven.setBriefType(briefType);
		return mergedBrieven;
	}

	@Override
	protected String getRapportageAantalBrievenKey()
	{
		return ColonBrievenConstants.RAPPORTAGEKEYAANTALBRIEVEN;
	}

	@Override
	public void additionalMergedContext(MailMergeContext context)
	{
		var brief = (ColonBrief) context.getBrief();
		context.setIntakeAfspraak(brief.getIntakeAfspraak());
		context.setVorigeIntakeAfspraak(brief.getVorigeIntakeAfspraak());
	}

	@Override
	public FileStoreLocation getFileStoreLocation()
	{
		return FileStoreLocation.COLON_MERGED_BRIEVEN;
	}

	@Override
	public LogGebeurtenis getMergeProbleemLogGebeurtenis()
	{
		return LogGebeurtenis.COLON_BRIEF_MERGE_FOUT;
	}

	@Override
	public LogGebeurtenis getOnvolledigAdresLogGebeurtenis()
	{
		return LogGebeurtenis.COLON_ONVOLLEDIG_ADRES;
	}

	@Override
	public Bevolkingsonderzoek[] getBevolkingsonderzoeken()
	{
		return new Bevolkingsonderzoek[] { Bevolkingsonderzoek.COLON };
	}
}
