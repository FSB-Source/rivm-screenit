package nl.rivm.screenit.batch.jobs.cervix.herinneren.allsteps;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-bmhk
 * %%
 * Copyright (C) 2012 - 2024 Facilitaire Samenwerking Bevolkingsonderzoek
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

import lombok.AllArgsConstructor;

import nl.rivm.screenit.batch.jobs.cervix.herinneren.CervixHerinnerenConstants;
import nl.rivm.screenit.batch.jobs.helpers.BaseWriter;
import nl.rivm.screenit.model.cervix.CervixBrief;
import nl.rivm.screenit.model.cervix.CervixScreeningRonde;
import nl.rivm.screenit.model.cervix.CervixUitnodiging;
import nl.rivm.screenit.model.enums.BriefType;
import nl.rivm.screenit.service.BaseBriefService;
import nl.rivm.screenit.service.cervix.Cervix2023StartBepalingService;
import nl.rivm.screenit.service.cervix.CervixFactory;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;

import org.jetbrains.annotations.NotNull;
import org.springframework.stereotype.Component;

@Component
@AllArgsConstructor
public class CervixHerinnerenWriter extends BaseWriter<CervixUitnodiging>
{
	private final BaseBriefService briefService;

	private final CervixFactory factory;

	private final HibernateService hibernateService;

	private final Cervix2023StartBepalingService bmhk2023StartBepalingService;

	@Override
	public void write(CervixUitnodiging uitnodiging) throws Exception
	{
		uitnodiging.setHerinneren(false);
		hibernateService.saveOrUpdate(uitnodiging);

		CervixBrief nieuweBrief;
		if (!bmhk2023StartBepalingService.rondeValtBinnenBmhk2023(uitnodiging.getScreeningRonde()))
		{
			nieuweBrief = getNieuweBriefPreBmhk2023(uitnodiging);
		}
		else
		{
			nieuweBrief = getNieuweBriefNaBmhk2023(uitnodiging);
		}

		aantallenContextOphogen(CervixHerinnerenConstants.TOTAAL_AANTAL_BRIEVEN_KEY, CervixHerinnerenConstants.BRIEF_TYPE_KEY, nieuweBrief.getBriefType());
	}

	@NotNull
	private CervixBrief getNieuweBriefPreBmhk2023(CervixUitnodiging uitnodiging)
	{
		CervixBrief nieuweBrief;
		var ronde = uitnodiging.getScreeningRonde();
		var brief = uitnodiging.getBrief();

		switch (uitnodiging.getMonsterType())
		{
		case UITSTRIJKJE:
			if (BriefType.getCervixUitnodigingen().contains(brief.getBriefType()))
			{
				nieuweBrief = briefService.maakBvoBrief(ronde, BriefType.CERVIX_HERINNERING_UITNODIGING);
			}
			else
			{
				nieuweBrief = maakControleUitstrijkjeBrief(ronde, brief);
			}
			break;
		case ZAS:
			nieuweBrief = briefService.maakBvoBrief(ronde, BriefType.CERVIX_HERINNERING_ZAS_UITNODIGING);
			break;
		default:
			throw new IllegalStateException("Monstertype niet herkent");
		}

		return nieuweBrief;
	}

	@NotNull
	private CervixBrief getNieuweBriefNaBmhk2023(CervixUitnodiging uitnodiging)
	{
		CervixBrief nieuweBrief;
		var ronde = uitnodiging.getScreeningRonde();
		var brief = uitnodiging.getBrief();

		switch (uitnodiging.getMonsterType())
		{
		case UITSTRIJKJE:
			if (BriefType.getCervixUitnodigingen().contains(brief.getBriefType()))
			{
				var nieuweUitnodiging = factory.maakHerinneringNonResponderMetZas(uitnodiging.getBrief().getClient());
				nieuweBrief = nieuweUitnodiging.getBrief();
			}
			else
			{
				nieuweBrief = maakControleUitstrijkjeBrief(ronde, brief);
			}
			break;
		case ZAS:
			nieuweBrief = bepaalNieuweBriefBijZasNaBmhk2023(uitnodiging);
			break;
		default:
			throw new IllegalStateException("Monstertype niet herkent");
		}

		return nieuweBrief;
	}

	private CervixBrief bepaalNieuweBriefBijZasNaBmhk2023(CervixUitnodiging uitnodiging)
	{
		CervixBrief nieuweBrief;
		var ronde = uitnodiging.getScreeningRonde();

		var briefType = uitnodiging.getBrief().getBriefType();
		switch (briefType)
		{
		case CERVIX_ZAS_UITNODIGING:
		case CERVIX_ZAS_NIET_ANALYSEERBAAR_OF_ONBEOORDEELBAAR:
			nieuweBrief = briefService.maakBvoBrief(ronde, BriefType.CERVIX_HERINNERING_ZAS_UITNODIGING);
			break;
		case CERVIX_ZAS_NON_RESPONDER:
			nieuweBrief = briefService.maakBvoBrief(ronde, BriefType.CERVIX_LAATSTE_HERINNERING);
			break;
		case CERVIX_ZAS_COMBI_UITNODIGING_30:
			nieuweBrief = briefService.maakBvoBrief(ronde, BriefType.CERVIX_HERINNERING_30_JARIGEN);
			break;
		default:
			throw new IllegalStateException("Onbruikbare brieftype van uitnodiging met ZAS: " + briefType);
		}

		return nieuweBrief;
	}

	@NotNull
	private CervixBrief maakControleUitstrijkjeBrief(CervixScreeningRonde ronde, CervixBrief brief)
	{
		var nieuweBrief = briefService.maakBvoBrief(ronde, brief.getBriefType());
		nieuweBrief.setHerdruk(brief);

		var nieuweUitnodiging = factory.maakUitnodiging(ronde, nieuweBrief);
		nieuweUitnodiging.setHerinnering(true);

		hibernateService.saveOrUpdateAll(nieuweBrief, nieuweUitnodiging);
		return nieuweBrief;
	}
}
