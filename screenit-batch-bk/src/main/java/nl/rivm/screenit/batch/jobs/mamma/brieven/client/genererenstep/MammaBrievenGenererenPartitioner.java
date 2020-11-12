package nl.rivm.screenit.batch.jobs.mamma.brieven.client.genererenstep;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-bk
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

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import nl.rivm.screenit.batch.jobs.brieven.genereren.AbstractBrievenGenererenPartitioner;
import nl.rivm.screenit.model.OrganisatieType;
import nl.rivm.screenit.model.ScreeningOrganisatie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.BriefType;
import nl.rivm.screenit.model.mamma.MammaBrief;
import nl.rivm.screenit.service.mamma.MammaBaseStandplaatsService;
import nl.rivm.screenit.util.query.ScreenitRestrictions;

import org.hibernate.Criteria;
import org.hibernate.criterion.Projections;
import org.hibernate.criterion.Restrictions;
import org.hibernate.sql.JoinType;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.batch.item.ExecutionContext;
import org.springframework.beans.factory.annotation.Autowired;

public class MammaBrievenGenererenPartitioner extends AbstractBrievenGenererenPartitioner
{
	public static final String KEY_SCREENINGORGANISATIEID = "mamma.screeningorganisatie.id";

	public static final String KEY_BRIEFTYPE = "mamma.brieftype";

	public static final String KEY_BRIEFTYPEAPART = "apart";

	public static final String KEY_MAMMASTANDPLAATSID = "mamma.mammastandplaats.id";

	public static final String KEY_TIJDELIJK = "tijdelijk";

	private static final Logger LOGGER = LoggerFactory.getLogger(MammaBrievenGenererenReader.class);

	@Autowired
	public MammaBaseStandplaatsService mammaStandplaatsService;

	@Override
	protected void fillingData(Map<String, ExecutionContext> partities, ScreeningOrganisatie organisatie)
	{
		for (BriefType briefType : getBriefTypes())
		{
			if (briefType.getVerzendendeOrganisatieType() == OrganisatieType.SCREENINGSORGANISATIE)
			{
				if (briefType == BriefType.MAMMA_AFSPRAAK_UITNODIGING ||
					briefType == BriefType.MAMMA_OPEN_UITNODIGING ||
					briefType == BriefType.MAMMA_UITNODIGING_MINDER_VALIDE ||
					briefType == BriefType.MAMMA_UITNODIGING_TEHUIS_ZONDER_DATUM ||
					briefType == BriefType.MAMMA_UITNODIGING_SUSPECT ||
					briefType == BriefType.MAMMA_AFSPRAAK_VERZET)
				{

					ExecutionContext executionContext = new ExecutionContext();
					executionContext.put(KEY_SCREENINGORGANISATIEID, organisatie.getId());
					executionContext.put(KEY_BRIEFTYPE, briefType.name());
					executionContext.put(KEY_BRIEFTYPEAPART, true);
					executionContext.put(KEY_MAMMASTANDPLAATSID, null);
					executionContext.put(KEY_TIJDELIJK, false);
					partities.put(organisatie.getId() + briefType.name() + "SL", executionContext);

					ExecutionContext executionContextTijdelijk = new ExecutionContext();
					executionContextTijdelijk.put(KEY_SCREENINGORGANISATIEID, organisatie.getId());
					executionContextTijdelijk.put(KEY_BRIEFTYPE, briefType.name());
					executionContextTijdelijk.put(KEY_BRIEFTYPEAPART, true);
					executionContextTijdelijk.put(KEY_MAMMASTANDPLAATSID, null);
					executionContextTijdelijk.put(KEY_TIJDELIJK, true);
					partities.put(organisatie.getId() + briefType.name() + "TL", executionContextTijdelijk);

					for (Long standplaatsId : getStandplaatsenIdsMetBrief(organisatie.getId(), briefType))
					{
						ExecutionContext executionContextBijlagen = new ExecutionContext();
						executionContextBijlagen.put(KEY_SCREENINGORGANISATIEID, organisatie.getId());
						executionContextBijlagen.put(KEY_BRIEFTYPE, briefType.name());
						executionContextBijlagen.put(KEY_BRIEFTYPEAPART, true);
						executionContextBijlagen.put(KEY_MAMMASTANDPLAATSID, standplaatsId);
						executionContextBijlagen.put(KEY_TIJDELIJK, false);
						partities.put(organisatie.getId() + briefType.name() + "SL" + standplaatsId + "BL", executionContextBijlagen);

						ExecutionContext executionContextTijdelijkBijlagen = new ExecutionContext();
						executionContextTijdelijkBijlagen.put(KEY_SCREENINGORGANISATIEID, organisatie.getId());
						executionContextTijdelijkBijlagen.put(KEY_BRIEFTYPE, briefType.name());
						executionContextTijdelijkBijlagen.put(KEY_BRIEFTYPEAPART, true);
						executionContextTijdelijkBijlagen.put(KEY_MAMMASTANDPLAATSID, standplaatsId);
						executionContextTijdelijkBijlagen.put(KEY_TIJDELIJK, true);
						partities.put(organisatie.getId() + briefType.name() + "TL" + standplaatsId + "BL", executionContextTijdelijkBijlagen);
					}
				}
				else
				{
					ExecutionContext executionContext = new ExecutionContext();
					executionContext.put(KEY_SCREENINGORGANISATIEID, organisatie.getId());
					executionContext.put(KEY_BRIEFTYPE, briefType.name());
					executionContext.put(KEY_BRIEFTYPEAPART, false);
					partities.put(organisatie.getId() + briefType.name(), executionContext);
				}
			}
		}
	}

	private List<BriefType> getBriefTypes()
	{
		List<BriefType> briefTypes = new ArrayList<BriefType>();
		for (BriefType briefType : BriefType.getBriefTypes(true, Bevolkingsonderzoek.MAMMA))
		{
			briefTypes.add(briefType);
		}
		return briefTypes;
	}

	private Set<Long> getStandplaatsenIdsMetBrief(Long screeningOrganisatieId, BriefType brieftype)
	{
		Set<Long> standplaatsenIds = new HashSet<>();
		standplaatsenIds.addAll(getAfspraakStandplaatsenIdsMetBrief(screeningOrganisatieId, brieftype));
		standplaatsenIds.addAll(getUitnodigingStandplaatsenIdsMetBrief(screeningOrganisatieId, brieftype));
		if (!standplaatsenIds.isEmpty())
		{
			LOGGER.info(String.format("%d standplaats(en) gevonden met brieftype %s en organisatieId %d", standplaatsenIds.size(), brieftype.name(), screeningOrganisatieId));
		}
		return standplaatsenIds;
	}

	private List<Long> getAfspraakStandplaatsenIdsMetBrief(Long screeningOrganisatieId, BriefType briefType)
	{
		Criteria crit = getBaseCriteria(screeningOrganisatieId, briefType);

		crit.add(Restrictions.isNotNull("laatsteAfspraak.id"));

		crit.createAlias("laatsteAfspraak.standplaatsPeriode", "afspraakstandplaatsPeriode");
		crit.createAlias("afspraakstandplaatsPeriode.standplaatsRonde", "afspraakStandplaatsRonde");
		crit.createAlias("afspraakStandplaatsRonde.standplaats", "afspraakStandplaats");

		crit.setProjection(Projections.distinct(Projections.property("afspraakStandplaats.id")));

		return crit.list();
	}

	private List<Long> getUitnodigingStandplaatsenIdsMetBrief(Long screeningOrganisatieId, BriefType briefType)
	{
		Criteria crit = getBaseCriteria(screeningOrganisatieId, briefType);

		crit.add(Restrictions.isNull("laatsteAfspraak.id"));

		crit.createAlias("laatsteUitnodiging.standplaatsRonde", "uitnodigingStandplaatsRonde");
		crit.createAlias("uitnodigingStandplaatsRonde.standplaats", "uitnodigingStandplaats");

		crit.setProjection(Projections.distinct(Projections.property("uitnodigingStandplaats.id")));

		return crit.list();
	}

	private Criteria getBaseCriteria(Long screeningOrganisatieId, BriefType briefType)
	{
		Criteria crit = getHibernateSession().createCriteria(MammaBrief.class);
		crit.createAlias("client", "client");
		crit.createAlias("client.persoon", "persoon");
		crit.createAlias("persoon.gbaAdres", "gbaAdres");
		crit.createAlias("gbaAdres.gbaGemeente", "gemeente");
		crit.createAlias("gemeente.screeningOrganisatie", "screeningOrganisatie");
		crit.add(Restrictions.eq("screeningOrganisatie.id", screeningOrganisatieId));

		ScreenitRestrictions.addClientBaseRestrictions(crit, "client", "persoon");

		crit.add(Restrictions.eq("gegenereerd", false));
		crit.add(Restrictions.eq("vervangen", false));
		crit.add(Restrictions.eq("tegenhouden", false));
		crit.add(Restrictions.eq("vervangendeProjectBrief", false));

		crit.add(Restrictions.eq("briefType", briefType));

		crit.createAlias("client.mammaDossier", "mammaDossier");
		crit.createAlias("mammaDossier.laatsteScreeningRonde", "laatsteScreeningRonde");

		crit.createAlias("laatsteScreeningRonde.laatsteUitnodiging", "laatsteUitnodiging");
		crit.createAlias("laatsteUitnodiging.laatsteAfspraak", "laatsteAfspraak", JoinType.LEFT_OUTER_JOIN);

		return crit;
	}
}
