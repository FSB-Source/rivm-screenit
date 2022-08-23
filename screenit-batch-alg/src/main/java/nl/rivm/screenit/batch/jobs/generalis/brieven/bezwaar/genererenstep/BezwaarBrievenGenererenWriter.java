package nl.rivm.screenit.batch.jobs.generalis.brieven.bezwaar.genererenstep;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-alg
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
import java.util.List;

import lombok.AllArgsConstructor;

import nl.rivm.screenit.batch.jobs.brieven.genereren.AbstractBrievenGenererenWriter;
import nl.rivm.screenit.batch.jobs.generalis.brieven.bezwaar.BezwaarBrievenConstants;
import nl.rivm.screenit.document.BaseDocumentCreator;
import nl.rivm.screenit.document.bezwaar.BezwaarDocumentCreatorOneDatasetCoupleTables;
import nl.rivm.screenit.model.BezwaarMoment;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.MailMergeContext;
import nl.rivm.screenit.model.ScreeningOrganisatie;
import nl.rivm.screenit.model.algemeen.BezwaarBrief;
import nl.rivm.screenit.model.algemeen.BezwaarGroupViewWrapper;
import nl.rivm.screenit.model.algemeen.BezwaarMergedBrieven;
import nl.rivm.screenit.model.enums.BriefType;
import nl.rivm.screenit.model.enums.FileStoreLocation;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.service.BezwaarService;
import nl.rivm.screenit.util.BriefUtil;

import org.springframework.batch.item.ExecutionContext;
import org.springframework.stereotype.Component;

@Component
@AllArgsConstructor
public class BezwaarBrievenGenererenWriter extends AbstractBrievenGenererenWriter<BezwaarBrief, BezwaarMergedBrieven>
{

	private final BezwaarService bezwaarService;

	@Override
	protected BezwaarMergedBrieven createConcreteMergedBrieven(Date aangemaaktOp)
	{
		ExecutionContext context = getStepExecutionContext();
		BriefType briefType = BriefType.valueOf(context.getString(BezwaarBrievenGenererenPartitioner.KEY_BRIEFTYPE));
		ScreeningOrganisatie screeningOrganisatie = getHibernateService().load(ScreeningOrganisatie.class,
			context.getLong(BezwaarBrievenGenererenPartitioner.KEY_SCREENINGORGANISATIEID));

		BezwaarMergedBrieven mergedBrieven = new BezwaarMergedBrieven();
		mergedBrieven.setScreeningOrganisatie(screeningOrganisatie);
		mergedBrieven.setCreatieDatum(aangemaaktOp);
		mergedBrieven.setBriefType(briefType);
		return mergedBrieven;
	}

	@Override
	public BaseDocumentCreator getDocumentCreator(MailMergeContext context)
	{
		Client client = context.getClient();
		BezwaarBrief brief = (BezwaarBrief) BriefUtil.getOrigineleBrief(context.getBrief());
		BezwaarMoment moment = getBezwaarMomentVoorBrief(client, brief);
		List<BezwaarGroupViewWrapper> wrappers = bezwaarService.getEditBezwaarGroupViewWrappers(client, moment, brief.getBriefType() != BriefType.CLIENT_BEZWAAR_BEVESTIGING);
		return new BezwaarDocumentCreatorOneDatasetCoupleTables(wrappers);
	}

	private BezwaarMoment getBezwaarMomentVoorBrief(Client client, BezwaarBrief brief)
	{
		switch (brief.getBriefType())
		{
		case CLIENT_BEZWAAR_AANVRAAG:
		case CLIENT_BEZWAAR_HANDTEKENING:

			return client.getLaatstVoltooideBezwaarMoment();
		case CLIENT_BEZWAAR_BEVESTIGING:

			return brief.getBezwaarMoment();
		default:
			throw new IllegalStateException("Ongeldig bezwaarbrieftype " + brief.getBriefType());
		}
	}

	@Override
	protected String getRapportageAantalBrievenKey()
	{
		return BezwaarBrievenConstants.RAPPORTAGEKEYAANTALBRIEVEN;
	}

	@Override
	public FileStoreLocation getFileStoreLocation()
	{
		return FileStoreLocation.ALGEMEEN_MERGED_BRIEVEN;
	}

	@Override
	public LogGebeurtenis getMergeProbleemLogGebeurtenis()
	{
		return LogGebeurtenis.BEZWAAR_BRIEF_MERGE_FOUT;
	}

	@Override
	public LogGebeurtenis getOnvolledigAdresLogGebeurtenis()
	{
		return LogGebeurtenis.BEZWAAR_ONVOLLEDIG_ADRES;
	}

}
