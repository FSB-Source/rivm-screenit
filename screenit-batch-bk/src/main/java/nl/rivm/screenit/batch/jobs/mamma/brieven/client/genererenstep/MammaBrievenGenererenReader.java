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

import nl.rivm.screenit.batch.jobs.brieven.genereren.AbstractBrievenGenererenReader;
import nl.rivm.screenit.model.enums.BriefType;
import nl.rivm.screenit.model.enums.GbaStatus;
import nl.rivm.screenit.model.mamma.MammaBrief;
import nl.rivm.screenit.util.query.DateRestrictions;
import nl.topicuszorg.hibernate.restrictions.NvlRestrictions;

import org.hibernate.Criteria;
import org.hibernate.criterion.DetachedCriteria;
import org.hibernate.criterion.Projections;
import org.hibernate.criterion.Restrictions;
import org.hibernate.criterion.Subqueries;
import org.hibernate.sql.JoinType;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.batch.item.ExecutionContext;

public class MammaBrievenGenererenReader extends AbstractBrievenGenererenReader<MammaBrief>
{
	private static final Logger LOGGER = LoggerFactory.getLogger(MammaBrievenGenererenReader.class);

	@Override
	protected Long getScreeningOrganisatieId(ExecutionContext context)
	{
		return context.getLong(MammaBrievenGenererenPartitioner.KEY_SCREENINGORGANISATIEID);
	}

	@Override
	protected Criteria additionalRestrictions(Criteria crit, ExecutionContext context)
	{
		crit.add(Restrictions.eq("client.gbaStatus", GbaStatus.INDICATIE_AANWEZIG));
		BriefType briefType = BriefType.valueOf(context.getString(MammaBrievenGenererenPartitioner.KEY_BRIEFTYPE));
		crit.add(Restrictions.eq("briefType", briefType));

		if ((boolean) context.get(MammaBrievenGenererenPartitioner.KEY_BRIEFTYPEAPART))
		{
			crit.createAlias("client.mammaDossier", "mammaDossier");
			crit.createAlias("mammaDossier.laatsteScreeningRonde", "laatsteScreeningRonde");
			crit.createAlias("laatsteScreeningRonde.laatsteUitnodiging", "laatsteUitnodiging");

			crit.createAlias("laatsteUitnodiging.laatsteAfspraak", "laatsteAfspraak", JoinType.LEFT_OUTER_JOIN);
			crit.createAlias("laatsteAfspraak.standplaatsPeriode", "afspraakstandplaatsPeriode", JoinType.LEFT_OUTER_JOIN);
			crit.createAlias("afspraakstandplaatsPeriode.standplaatsRonde", "afspraakStandplaatsRonde", JoinType.LEFT_OUTER_JOIN);
			crit.createAlias("afspraakStandplaatsRonde.standplaats", "afspraakStandplaats", JoinType.LEFT_OUTER_JOIN);

			crit.createAlias("afspraakStandplaats.tijdelijkeLocatie", "afspraakTijdelijkeLocatie", JoinType.LEFT_OUTER_JOIN);
			crit.createAlias("afspraakTijdelijkeLocatie.standplaatsLocatieBijlage", "afspraakTijdelijkeLocatieBijlage", JoinType.LEFT_OUTER_JOIN);

			crit.createAlias("afspraakStandplaats.locatie", "afspraakLocatie", JoinType.LEFT_OUTER_JOIN);
			crit.createAlias("afspraakLocatie.standplaatsLocatieBijlage", "afspraakLocatieBijlage", JoinType.LEFT_OUTER_JOIN);

			crit.createAlias("laatsteUitnodiging.standplaatsRonde", "uitnodigingStandplaatsRonde");
			crit.createAlias("uitnodigingStandplaatsRonde.standplaats", "uitnodigingStandplaats");
			crit.createAlias("uitnodigingStandplaats.locatie", "uitnodigingLocatie");
			crit.createAlias("uitnodigingLocatie.standplaatsLocatieBijlage", "uitnodigingLocatieBijlage", JoinType.LEFT_OUTER_JOIN);

			boolean tijdelijk = (boolean) context.get(MammaBrievenGenererenPartitioner.KEY_TIJDELIJK);
			Long standplaatsId = (Long) context.get(MammaBrievenGenererenPartitioner.KEY_MAMMASTANDPLAATSID);
			if (tijdelijk && standplaatsId != null)
			{

				crit.add(Restrictions.isNotNull("laatsteAfspraak.id"));

				crit.add(DateRestrictions.geProperty("laatsteAfspraak.vanaf",
					"afspraakTijdelijkeLocatie.startDatum"));

				crit.add(DateRestrictions.leProperty("laatsteAfspraak.vanaf",
					"afspraakTijdelijkeLocatie.eindDatum"));
				crit.add(Restrictions.eq("afspraakStandplaats.id", standplaatsId));

				crit.add(Restrictions.or(
					Restrictions.eq("afspraakTijdelijkeLocatie.brievenApartPrinten", true),
					Restrictions.and(
						Restrictions.isNotNull("afspraakTijdelijkeLocatie.standplaatsLocatieBijlage"),
						Restrictions.eq("afspraakTijdelijkeLocatieBijlage.actief", true))));
			}
			else if (!tijdelijk && standplaatsId != null)
			{
				crit.add(Restrictions.or(

					Restrictions.and(
						Restrictions.isNotNull("laatsteAfspraak.id"),
						Restrictions.or(
							Restrictions.isNull("afspraakTijdelijkeLocatie.startDatum"),
							DateRestrictions.ltProperty("laatsteAfspraak.vanaf",
								"afspraakTijdelijkeLocatie.startDatum"),
							DateRestrictions.gtProperty("laatsteAfspraak.vanaf",
								"afspraakTijdelijkeLocatie.eindDatum")),
						Restrictions.eq("afspraakStandplaats.id", standplaatsId),

						Restrictions.or(
							Restrictions.eq("afspraakLocatie.brievenApartPrinten", true),
							Restrictions.and(
								Restrictions.isNotNull("afspraakLocatie.standplaatsLocatieBijlage"),
								Restrictions.eq("afspraakLocatieBijlage.actief", true)))),

					Restrictions.and(
						Restrictions.isNull("laatsteAfspraak.id"),
						Restrictions.eq("uitnodigingStandplaats.id", standplaatsId),

						Restrictions.or(
							Restrictions.eq("uitnodigingLocatie.brievenApartPrinten", true),
							Restrictions.and(
								Restrictions.isNotNull("uitnodigingLocatie.standplaatsLocatieBijlage"),
								Restrictions.eq("uitnodigingLocatieBijlage.actief", true))))));
			}
			else if (tijdelijk) 
			{

				crit.add(Restrictions.isNotNull("laatsteAfspraak.id"));

				crit.add(DateRestrictions.geProperty("laatsteAfspraak.vanaf",
					"afspraakTijdelijkeLocatie.startDatum"));
				crit.add(DateRestrictions.leProperty("laatsteAfspraak.vanaf",
					"afspraakTijdelijkeLocatie.eindDatum"));

				crit.add(Restrictions.and(
					NvlRestrictions.eq("afspraakTijdelijkeLocatie.brievenApartPrinten", false, "false"),
					Restrictions.or(
						Restrictions.isNull("afspraakTijdelijkeLocatie.standplaatsLocatieBijlage"),
						Restrictions.eq("afspraakTijdelijkeLocatieBijlage.actief", false))));
			}
			else 
			{
				crit.add(Restrictions.or(

					Restrictions.and(
						Restrictions.isNotNull("laatsteAfspraak.id"),
						Restrictions.or(
							Restrictions.isNull("afspraakTijdelijkeLocatie.startDatum"),
							DateRestrictions.ltProperty("laatsteAfspraak.vanaf",
								"afspraakTijdelijkeLocatie.startDatum"),
							DateRestrictions.gtProperty("laatsteAfspraak.vanaf",
								"afspraakTijdelijkeLocatie.eindDatum")),

						Restrictions.and(
							NvlRestrictions.eq("afspraakLocatie.brievenApartPrinten", false, "false"),
							Restrictions.or(
								Restrictions.isNull("afspraakLocatie.standplaatsLocatieBijlage"),
								Restrictions.eq("afspraakLocatieBijlage.actief", false)))),

					Restrictions.and(
						Restrictions.isNull("laatsteAfspraak.id"),
						NvlRestrictions.eq("uitnodigingLocatie.brievenApartPrinten", false, "false"),
						Restrictions.or(
							Restrictions.isNull("uitnodigingLocatie.standplaatsLocatieBijlage"),
							Restrictions.eq("uitnodigingLocatieBijlage.actief", false)))));
			}
			verifieerOfHetDeEersteBriefIs(crit, (Boolean) context.get(MammaBrievenGenererenPartitioner.KEY_EERSTE_RONDE));

		}
		return crit;
	}

	private void verifieerOfHetDeEersteBriefIs(Criteria criteria, Boolean eersteRonde)
	{
		if (eersteRonde != null)
		{
			DetachedCriteria subQuery = DetachedCriteria.forClass(MammaBrief.class);
			subQuery.createAlias("client", "briefClient");
			subQuery.add(Restrictions.eqProperty("briefClient.mammaDossier", "mammaDossier.id"));
			subQuery.add(Restrictions.in("briefType", BriefType.getMammaEersteRondeBrieftype()));
			subQuery.setProjection(Projections.count("id"));
			if (eersteRonde)
			{
				criteria.add(Subqueries.eq(1L, subQuery));
			}
			else
			{
				criteria.add(Subqueries.lt(1L, subQuery));
			}
		}
	}
}
