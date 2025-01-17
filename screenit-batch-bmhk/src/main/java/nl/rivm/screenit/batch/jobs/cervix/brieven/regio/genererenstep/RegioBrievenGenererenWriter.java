package nl.rivm.screenit.batch.jobs.cervix.brieven.regio.genererenstep;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-bmhk
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

import java.util.Date;

import nl.rivm.screenit.batch.jobs.brieven.genereren.AbstractBrievenGenererenWriter;
import nl.rivm.screenit.batch.jobs.cervix.brieven.regio.RegioBrievenConstants;
import nl.rivm.screenit.model.MailMergeContext;
import nl.rivm.screenit.model.ScreeningOrganisatie;
import nl.rivm.screenit.model.cervix.CervixRegioBrief;
import nl.rivm.screenit.model.cervix.CervixRegioMergedBrieven;
import nl.rivm.screenit.model.enums.BriefType;
import nl.rivm.screenit.model.enums.FileStoreLocation;
import nl.rivm.screenit.model.enums.LogGebeurtenis;

import org.springframework.stereotype.Component;

@Component
public class RegioBrievenGenererenWriter extends AbstractBrievenGenererenWriter<CervixRegioBrief, CervixRegioMergedBrieven>
{
	@Override
	protected CervixRegioMergedBrieven createConcreteMergedBrieven(Date aangemaaktOp)
	{
		var context = getStepExecutionContext();
		var briefType = BriefType.valueOf(context.getString(RegioBrievenGenererenPartitioner.KEY_BRIEFTYPE));
		var screeningOrganisatie = getHibernateService().load(ScreeningOrganisatie.class, context.getLong(RegioBrievenGenererenPartitioner.KEY_SCREENINGORGANISATIEID));

		var mergedBrieven = new CervixRegioMergedBrieven();
		mergedBrieven.setScreeningOrganisatie(screeningOrganisatie);
		mergedBrieven.setCreatieDatum(aangemaaktOp);
		mergedBrieven.setBriefType(briefType);
		return mergedBrieven;
	}

	@Override
	public void additionalMergedContext(MailMergeContext context)
	{
		var brief = (CervixRegioBrief) context.getBrief();
		context.putValue(MailMergeContext.CONTEXT_CERVIX_HUISARTS, brief.getHuisarts());
	}

	@Override
	protected String getRapportageAantalBrievenKey()
	{
		return RegioBrievenConstants.RAPPORTAGEKEYAANTALBRIEVEN;
	}

	@Override
	public FileStoreLocation getFileStoreLocation()
	{
		return FileStoreLocation.ALGEMEEN_MERGED_BRIEVEN;
	}

	@Override
	public LogGebeurtenis getMergeProbleemLogGebeurtenis()
	{
		return LogGebeurtenis.REGIO_BRIEF_MERGE_FOUT;
	}

	@Override
	public LogGebeurtenis getOnvolledigAdresLogGebeurtenis()
	{
		return null;
	}
}
