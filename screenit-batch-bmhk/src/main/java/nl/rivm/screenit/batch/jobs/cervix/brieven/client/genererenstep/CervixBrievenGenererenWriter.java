package nl.rivm.screenit.batch.jobs.cervix.brieven.client.genererenstep;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-bmhk
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
import nl.rivm.screenit.batch.jobs.cervix.brieven.client.CervixBriefConstants;
import nl.rivm.screenit.model.MailMergeContext;
import nl.rivm.screenit.model.ScreeningOrganisatie;
import nl.rivm.screenit.model.cervix.CervixBrief;
import nl.rivm.screenit.model.cervix.CervixMergedBrieven;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.BriefType;
import nl.rivm.screenit.model.enums.FileStoreLocation;
import nl.rivm.screenit.model.enums.LogGebeurtenis;

import org.springframework.batch.item.ExecutionContext;

public class CervixBrievenGenererenWriter extends AbstractBrievenGenererenWriter<CervixBrief, CervixMergedBrieven>
{

	@Override
	protected CervixMergedBrieven createConcreteMergedBrieven(Date aangemaaktOp)
	{
		ExecutionContext context = getStepExecutionContext();
		BriefType briefType = BriefType.valueOf(context.getString(CervixBrievenGenererenPartitioner.KEY_BRIEFTYPE));
		ScreeningOrganisatie screeningOrganisatie = getHibernateService().load(ScreeningOrganisatie.class,
			context.getLong(CervixBrievenGenererenPartitioner.KEY_SCREENINGORGANISATIEID));

		CervixMergedBrieven mergedBrieven = new CervixMergedBrieven();
		mergedBrieven.setScreeningOrganisatie(screeningOrganisatie);
		mergedBrieven.setCreatieDatum(aangemaaktOp);
		mergedBrieven.setBriefType(briefType);

		return mergedBrieven;
	}

	@Override
	public void additionalMergedContext(MailMergeContext context)
	{
		CervixBrief brief = (CervixBrief) context.getBrief();
		if (brief.getUitnodiging() != null)
		{
			context.setCervixUitnodiging(brief.getUitnodiging());
		}
		else if (brief.getMonster() != null)
		{
			context.setCervixUitnodiging(brief.getMonster().getUitnodiging());
		}
		else if (brief.getLabformulier() != null)
		{
			context.setCervixUitnodiging(brief.getLabformulier().getUitstrijkje().getUitnodiging());
		}
	}

	@Override
	protected String getRapportageAantalBrievenKey()
	{
		return CervixBriefConstants.RAPPORTAGEKEYAANTALBRIEVEN;
	}

	@Override
	public FileStoreLocation getFileStoreLocation()
	{
		return FileStoreLocation.CERVIX_MERGED_BRIEVEN;
	}

	@Override
	public LogGebeurtenis getMergeProbleemLogGebeurtenis()
	{
		return LogGebeurtenis.CERVIX_BRIEF_MERGE_FOUT;
	}

	@Override
	public LogGebeurtenis getOnvolledigAdresLogGebeurtenis()
	{
		return LogGebeurtenis.CERVIX_ONVOLLEDIG_ADRES;
	}

	@Override
	public Bevolkingsonderzoek[] getBevolkingsonderzoeken()
	{
		return new Bevolkingsonderzoek[] { Bevolkingsonderzoek.CERVIX };
	}
}
