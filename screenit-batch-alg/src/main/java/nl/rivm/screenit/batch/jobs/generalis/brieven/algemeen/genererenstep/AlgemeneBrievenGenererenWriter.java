package nl.rivm.screenit.batch.jobs.generalis.brieven.algemeen.genererenstep;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-alg
 * %%
 * Copyright (C) 2012 - 2021 Facilitaire Samenwerking Bevolkingsonderzoek
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
import nl.rivm.screenit.batch.jobs.generalis.brieven.algemeen.AlgemeneBrievenConstants;
import nl.rivm.screenit.model.ScreeningOrganisatie;
import nl.rivm.screenit.model.algemeen.AlgemeneBrief;
import nl.rivm.screenit.model.algemeen.AlgemeneMergedBrieven;
import nl.rivm.screenit.model.enums.BriefType;
import nl.rivm.screenit.model.enums.FileStoreLocation;
import nl.rivm.screenit.model.enums.LogGebeurtenis;

import org.springframework.batch.item.ExecutionContext;

public class AlgemeneBrievenGenererenWriter extends AbstractBrievenGenererenWriter<AlgemeneBrief, AlgemeneMergedBrieven>
{
	@Override
	protected AlgemeneMergedBrieven createConcreteMergedBrieven(Date aangemaaktOp)
	{
		ExecutionContext context = getStepExecutionContext();
		BriefType briefType = BriefType.valueOf(context.getString(AlgemeneBrievenGenererenPartitioner.KEY_BRIEFTYPE));
		ScreeningOrganisatie screeningOrganisatie = getHibernateService().load(ScreeningOrganisatie.class,
			context.getLong(AlgemeneBrievenGenererenPartitioner.KEY_SCREENINGORGANISATIEID));

		AlgemeneMergedBrieven mergedBrieven = new AlgemeneMergedBrieven();
		mergedBrieven.setScreeningOrganisatie(screeningOrganisatie);
		mergedBrieven.setCreatieDatum(aangemaaktOp);
		mergedBrieven.setBriefType(briefType);

		return mergedBrieven;
	}

	@Override
	protected String getRapportageAantalBrievenKey()
	{
		return AlgemeneBrievenConstants.RAPPORTAGEKEYAANTALBRIEVEN;
	}

	@Override
	public FileStoreLocation getFileStoreLocation()
	{
		return FileStoreLocation.ALGEMEEN_MERGED_BRIEVEN;
	}

	@Override
	public LogGebeurtenis getMergeProbleemLogGebeurtenis()
	{
		return LogGebeurtenis.ALGEMENE_BRIEF_MERGE_FOUT;
	}

	@Override
	public LogGebeurtenis getOnvolledigAdresLogGebeurtenis()
	{
		return LogGebeurtenis.ALGEMENE_BRIEF_ONVOLLEDIG_ADRES;
	}
}
