package nl.rivm.screenit.batch.jobs.mamma.brieven.client.genererenstep;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-bk
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

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.atomic.AtomicBoolean;

import lombok.AllArgsConstructor;
import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.PreferenceKey;
import nl.rivm.screenit.batch.jobs.brieven.genereren.AbstractBrievenGenererenPartitioner;
import nl.rivm.screenit.batch.repository.MammaBriefRepository;
import nl.rivm.screenit.model.OrganisatieType;
import nl.rivm.screenit.model.ScreeningOrganisatie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.BriefType;
import nl.topicuszorg.hibernate.spring.services.impl.OpenHibernate5Session;
import nl.topicuszorg.preferencemodule.service.SimplePreferenceService;

import org.springframework.batch.item.ExecutionContext;
import org.springframework.stereotype.Component;

@Component
@Slf4j
@AllArgsConstructor
public class MammaBrievenGenererenPartitioner extends AbstractBrievenGenererenPartitioner
{
	public static final String KEY_SCREENINGORGANISATIEID = "mamma.screeningorganisatie.id";

	public static final String KEY_BRIEFTYPE = "mamma.brieftype";

	public static final String KEY_BRIEFTYPEAPART = "apart";

	public static final String KEY_MAMMASTANDPLAATSID = "mamma.mammastandplaats.id";

	public static final String KEY_TIJDELIJK = "tijdelijk";

	public static final String KEY_EERSTE_RONDE = "_ronde1";

	private final SimplePreferenceService preferenceService;

	private final MammaBriefRepository briefRepository;

	@Override
	protected void fillingData(Map<String, ExecutionContext> partities, ScreeningOrganisatie organisatie)
	{
		for (BriefType briefType : getBriefTypes())
		{
			if (briefType.getVerzendendeOrganisatieType() != OrganisatieType.SCREENINGSORGANISATIE)
			{
				continue;
			}

			if (BriefType.getMammaBriefApart().contains(briefType))
			{
				var isEersteRondeBrief = isEersteRondeBrief(briefType);

				partitionBuilders(partities, organisatie.getId(), briefType, true, null, false, isEersteRondeBrief);

				partitionBuilders(partities, organisatie.getId(), briefType, true, null, true, isEersteRondeBrief);

				for (Long standplaatsId : getStandplaatsenIdsMetBrief(organisatie, briefType))
				{

					partitionBuilders(partities, organisatie.getId(), briefType, true, standplaatsId, false, isEersteRondeBrief);

					partitionBuilders(partities, organisatie.getId(), briefType, true, standplaatsId, true, isEersteRondeBrief);
				}
			}
			else
			{

				partitionBuilders(partities, organisatie.getId(), briefType, false, null, null, null);
			}
		}
	}

	private List<BriefType> getBriefTypes()
	{
		return new ArrayList<>(BriefType.getBriefTypes(true, Bevolkingsonderzoek.MAMMA));
	}

	private Set<Long> getStandplaatsenIdsMetBrief(ScreeningOrganisatie screeningOrganisatie, BriefType brieftype)
	{
		Set<Long> standplaatsenIds = new HashSet<>();
		standplaatsenIds.addAll(briefRepository.getAfspraakStandplaatsenIdsMetBrief(screeningOrganisatie, brieftype));
		standplaatsenIds.addAll(briefRepository.getUitnodigingStandplaatsenIdsMetBrief(screeningOrganisatie, brieftype));
		if (!standplaatsenIds.isEmpty())
		{
			LOG.info("{} standplaats(en) gevonden met brieftype {} en organisatieId '{}'", standplaatsenIds.size(), brieftype.name(), screeningOrganisatie.getId());
		}
		return standplaatsenIds;
	}

	private Boolean isEersteRondeBrief(BriefType briefType)
	{
		var annoteerEersteRonde = new AtomicBoolean();
		OpenHibernate5Session.withoutTransaction().run(() ->
		{
			annoteerEersteRonde.set(preferenceService.getBoolean(PreferenceKey.MAMMA_ANNOTEER_EERSTE_RONDE.name(), Boolean.FALSE));
		});

		return Boolean.TRUE.equals(annoteerEersteRonde.get()) && BriefType.getMammaEersteRondeBrieftype().contains(briefType);
	}

	void partitionBuilders(Map<String, ExecutionContext> partities, long organisatieID, BriefType briefType, boolean briefApart, Long standPlaatsID, Boolean tijdelijk,
		Boolean eersteBrief)
	{
		if (Boolean.TRUE.equals(eersteBrief))
		{
			partitionBuilder(partities, organisatieID, briefType, briefApart, standPlaatsID, tijdelijk, true);
			partitionBuilder(partities, organisatieID, briefType, briefApart, standPlaatsID, tijdelijk, false);
		}
		else
		{
			partitionBuilder(partities, organisatieID, briefType, briefApart, standPlaatsID, tijdelijk, null);
		}
	}

	private void partitionBuilder(Map<String, ExecutionContext> partities, long organisatieID, BriefType briefType, boolean briefApart, Long standPlaatsID,
		Boolean tijdelijk, Boolean eersteBrief)
	{
		var partitionIdentifier = organisatieID + briefType.name();
		var executionContext = new ExecutionContext();

		executionContext.put(KEY_SCREENINGORGANISATIEID, organisatieID);
		executionContext.put(KEY_BRIEFTYPE, briefType.name());
		executionContext.put(KEY_BRIEFTYPEAPART, briefApart);
		executionContext.put(KEY_MAMMASTANDPLAATSID, standPlaatsID);
		executionContext.put(KEY_TIJDELIJK, tijdelijk);
		executionContext.put(KEY_EERSTE_RONDE, eersteBrief);

		if (Boolean.TRUE.equals(tijdelijk))
		{
			partitionIdentifier += "_TL";
		}
		else if (Boolean.FALSE.equals(tijdelijk))
		{
			partitionIdentifier += "_SL";
		}

		if (standPlaatsID != null)
		{
			partitionIdentifier += "_" + standPlaatsID + "_BL";
		}

		if (Boolean.TRUE.equals(eersteBrief))
		{
			partitionIdentifier += "_ERT";
		}
		else if (Boolean.FALSE.equals(eersteBrief))
		{
			partitionIdentifier += "_ERF";
		}

		partities.put(partitionIdentifier, executionContext);
	}
}
