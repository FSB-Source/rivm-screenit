package nl.rivm.screenit.dao.colon.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
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

import java.time.LocalDate;
import java.util.Arrays;
import java.util.Date;
import java.util.List;

import nl.rivm.screenit.dao.colon.AfspraakDao;
import nl.rivm.screenit.model.berichten.enums.VerslagStatus;
import nl.rivm.screenit.model.colon.ColonIntakeAfspraak;
import nl.rivm.screenit.model.colon.ColonIntakelocatie;
import nl.rivm.screenit.model.colon.ConclusieTypeFilter;
import nl.rivm.screenit.model.colon.MdlVerslag;
import nl.rivm.screenit.model.colon.WerklijstIntakeFilter;
import nl.rivm.screenit.model.colon.enums.ColonAfspraakStatus;
import nl.rivm.screenit.model.colon.enums.ColonConclusieType;
import nl.rivm.screenit.model.colon.planning.ColonTijdslot;
import nl.rivm.screenit.util.DateUtil;
import nl.rivm.screenit.util.query.ScreenitRestrictions;
import nl.topicuszorg.hibernate.criteria.BaseCriteria;
import nl.topicuszorg.hibernate.criteria.ListCriteria;
import nl.topicuszorg.hibernate.spring.dao.impl.AbstractAutowiredDao;

import org.apache.commons.lang.StringUtils;
import org.hibernate.criterion.Conjunction;
import org.hibernate.criterion.DetachedCriteria;
import org.hibernate.criterion.Order;
import org.hibernate.criterion.Projections;
import org.hibernate.criterion.Restrictions;
import org.hibernate.criterion.Subqueries;
import org.hibernate.sql.JoinType;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import com.google.common.primitives.Ints;

@Repository
@Transactional(propagation = Propagation.SUPPORTS)
public class AfspraakDaoImpl extends AbstractAutowiredDao implements AfspraakDao
{

	@Override
	public void saveOrUpdate(ColonTijdslot tijdslot)
	{
		getSession().saveOrUpdate(tijdslot);
	}

	@Override
	public void delete(ColonTijdslot tijdslot)
	{
		getSession().delete(tijdslot);
	}

	@Override
	public List<ColonIntakeAfspraak> getAfsprakenVoorIntakelocatie(WerklijstIntakeFilter zoekFilter, ColonIntakelocatie intakelocatie, LocalDate vandaag, long first,
		long count, String property, boolean ascending)
	{
		BaseCriteria<? extends ColonIntakeAfspraak> criteria = createCriteria(zoekFilter, intakelocatie, vandaag);
		if (property.startsWith("conclusie"))
		{
			criteria.alias("conclusie", JoinType.LEFT_OUTER_JOIN);
		}
		else if (property.startsWith("volgendeUitnodiging"))
		{
			criteria.alias("client");
			criteria.alias("client.colonDossier", "dossier");
			criteria.alias("dossier.volgendeUitnodiging", "volgendeUitnodiging");
		}

		if (!ascending)
		{
			criteria.addOrder(Order.desc(property));
			if (property.equals("conclusie.type"))
			{
				criteria.addOrder(Order.desc("conclusie.onHoldReden"));
			}
			criteria.addOrder(Order.desc("id"));
		}
		else
		{
			criteria.addOrder(Order.asc(property));
			if (property.equals("conclusie.type"))
			{
				criteria.addOrder(Order.asc("conclusie.onHoldReden"));
			}
			criteria.addOrder(Order.asc("id"));
		}

		List<ColonIntakeAfspraak> afspraken = (List<ColonIntakeAfspraak>) criteria.list(getSession(), new ListCriteria(Ints.checkedCast(first), Ints.checkedCast(count)));
		if (moetNogOpGeboortedatumFilteren(zoekFilter))
		{
			filterGeboortedatum(zoekFilter.getGeboortedatum(), afspraken);
		}
		return afspraken;
	}

	private boolean moetNogOpGeboortedatumFilteren(WerklijstIntakeFilter zoekFilter)
	{
		return ColonAfspraakStatus.UITGEVOERD.equals(zoekFilter.getStatus()) && zoekFilter.getGeboortedatum() != null;
	}

	private BaseCriteria<ColonIntakeAfspraak> createCriteria(WerklijstIntakeFilter zoekFilter, ColonIntakelocatie intakelocatie, LocalDate vandaag)
	{
		BaseCriteria<ColonIntakeAfspraak> criteria = new BaseCriteria<ColonIntakeAfspraak>(ColonIntakeAfspraak.class);
		criteria.alias("kamer");
		criteria.add(Restrictions.eq("kamer.intakelocatie", intakelocatie));
		criteria.alias("client");
		criteria.alias("client.persoon", "persoon");
		criteria.alias("colonScreeningRonde");
		criteria.alias("client.colonDossier", "dossier");
		criteria.alias("conclusie", "conclusie", JoinType.LEFT_OUTER_JOIN);
		criteria.add(Restrictions.in("status", ColonAfspraakStatus.VOOR_AGENDA));
		criteria.add(Restrictions.eqProperty("dossier.laatsteScreeningRonde", "colonScreeningRonde"));

		if (StringUtils.isNotBlank(zoekFilter.getBsn()) && (!ColonAfspraakStatus.UITGEVOERD.equals(zoekFilter.getStatus()) || zoekFilter.getGeboortedatum() != null))
		{
			criteria.add(Restrictions.eq("persoon.bsn", zoekFilter.getBsn()));
		}
		else if (ColonAfspraakStatus.UITGEVOERD.equals(zoekFilter.getStatus()))
		{

			criteria.add(Restrictions.eq("persoon.bsn", "nobsn"));
		}
		DetachedCriteria verslagenCrit = DetachedCriteria.forClass(MdlVerslag.class);
		verslagenCrit.add(Restrictions.eq("status", VerslagStatus.AFGEROND));
		verslagenCrit.setProjection(Projections.distinct(Projections.property("screeningRonde.id")));

		LocalDate vanaf = DateUtil.toLocalDate(zoekFilter.getVanaf());
		LocalDate totEnMet = null;
		if (zoekFilter.getTotEnMet() != null)
		{
			totEnMet = DateUtil.toLocalDate(zoekFilter.getTotEnMet()).plusDays(1);
		}
		if (ColonAfspraakStatus.GEPLAND.equals(zoekFilter.getStatus()))
		{
			criteria.add(Restrictions.eq("status", ColonAfspraakStatus.GEPLAND));
			if (vanaf == null || !vanaf.isAfter(vandaag))
			{
				vanaf = vandaag;
			}
			if (zoekFilter.getEersteKeerZoeken())
			{
				totEnMet = vandaag.plusDays(1);
			}
		}
		else if (ColonAfspraakStatus.UITGEVOERD.equals(zoekFilter.getStatus()))
		{
			criteria.alias("colonScreeningRonde.ifobtTesten", "testen", JoinType.LEFT_OUTER_JOIN);
			criteria.alias("colonScreeningRonde.openUitnodiging", "openUitnodiging", JoinType.LEFT_OUTER_JOIN);
			criteria.add(
				Restrictions.or(
					Subqueries.propertyIn("colonScreeningRonde.id", verslagenCrit),
					Restrictions.and(
						Restrictions.eq("status", ColonAfspraakStatus.UITGEVOERD),
						Restrictions.isNotNull("conclusie.type"),
						Restrictions.ne("conclusie.type", ColonConclusieType.DOORVERWIJZEN_NAAR_ANDER_CENTRUM),
						Restrictions.ne("conclusie.type", ColonConclusieType.ON_HOLD))));
			criteria.add(Restrictions.isNull("persoon.overlijdensdatum"));
			criteria.add(Restrictions.or(
				ScreenitRestrictions.getLeeftijdsgrensRestrictions(null, zoekFilter.getMaxLeeftijd(), vandaag),
				Restrictions.not(
					ColonRestrictions.critRondeZonderVerslagNaVerlopenOngunstigeUitslag(DateUtil.toUtilDate(vandaag.minusDays(zoekFilter.getInterval())), "testen",
						"colonScreeningRonde")
				),
				Restrictions.isNotNull("openUitnodiging.id")
			));
			criteria.add(Restrictions.isNull("nieuweAfspraak"));
		}
		else
		{

			Conjunction critZonderConclusie = Restrictions.conjunction();
			critZonderConclusie.add(Restrictions.isNull("conclusie.type"));
			if (totEnMet != null && totEnMet.isBefore(vandaag))
			{
				critZonderConclusie.add(Restrictions.lt("vanaf", totEnMet.atStartOfDay()));
			}
			else
			{
				critZonderConclusie.add(Restrictions.lt("vanaf", vandaag.atStartOfDay()));
			}

			Conjunction critOnHold = Restrictions.conjunction();
			critOnHold.add(Restrictions.eq("conclusie.type", ColonConclusieType.ON_HOLD));

			Conjunction critDoorverwijzenMedischeRedenen = Restrictions.conjunction();
			critDoorverwijzenMedischeRedenen.add(Restrictions.eq("conclusie.type", ColonConclusieType.DOORVERWIJZEN_NAAR_ANDER_CENTRUM));
			critDoorverwijzenMedischeRedenen.add(
				Restrictions.or(
					Restrictions.and(
						Restrictions.eq("conclusie.doorverwijzingBevestigd", false),
						Restrictions.isNotNull("nieuweAfspraak")),
					Restrictions.and(
						Restrictions.eq("conclusie.doorverwijzingBevestigd", true),
						Restrictions.isNull("nieuweAfspraak"))
				));

			if (totEnMet != null)
			{
				critOnHold.add(Restrictions.lt("vanaf", totEnMet.atStartOfDay()));
				critDoorverwijzenMedischeRedenen.add(Restrictions.lt("vanaf", totEnMet.atStartOfDay()));
			}
			criteria.alias("dossier.volgendeUitnodiging", "volgendeUitnodiging");
			criteria.createAlias("volgendeUitnodiging.interval", "interval");
			criteria.add(Restrictions.gtProperty("volgendeUitnodiging.peildatum", "interval.berekendeReferentieDatum"));
			criteria.add(Restrictions.or(critZonderConclusie, critOnHold, critDoorverwijzenMedischeRedenen));
		}

		if (!ColonAfspraakStatus.UITGEVOERD.equals(zoekFilter.getStatus()))
		{
			criteria.add(Subqueries.propertyNotIn("colonScreeningRonde.id", verslagenCrit));
		}

		if (vanaf != null)
		{
			criteria.add(Restrictions.gt("vanaf", vanaf.atStartOfDay()));
		}
		if (zoekFilter.getStatus() != null && totEnMet != null)
		{
			criteria.add(Restrictions.lt("vanaf", totEnMet.atStartOfDay()));
		}
		if (zoekFilter.getConclusieTypeFilter() != null)
		{
			if (ConclusieTypeFilter.GEEN_CONCLUSIE.equals(zoekFilter.getConclusieTypeFilter()))
			{
				criteria.add(Restrictions.isNull("conclusie.type"));
			}
			else if (zoekFilter.getConclusieTypeFilter().getConclusieTypes().length == 1)
			{
				criteria.add(Restrictions.eq("conclusie.type", zoekFilter.getConclusieTypeFilter().getConclusieTypes()[0]));
			}
			else if (zoekFilter.getConclusieTypeFilter().getConclusieTypes().length > 1)
			{
				criteria.add(Restrictions.in("conclusie.type", Arrays.asList(zoekFilter.getConclusieTypeFilter().getConclusieTypes())));
			}
		}
		return criteria;
	}

	@Override
	public long countAfsprakenVoorIntakelocatie(WerklijstIntakeFilter zoekFilter, ColonIntakelocatie intakelocatie, LocalDate vandaag)
	{
		final var criteria = createCriteria(zoekFilter, intakelocatie, vandaag);
		if (moetNogOpGeboortedatumFilteren(zoekFilter))
		{
			List<ColonIntakeAfspraak> afspraken = criteria.list(getSession());
			filterGeboortedatum(zoekFilter.getGeboortedatum(), afspraken);
			return afspraken.size();
		}
		else
		{
			return criteria.countLong(getSession());
		}
	}

	private boolean filterGeboortedatum(Date geboortedatum, List<ColonIntakeAfspraak> afspraken)
	{
		return afspraken.removeIf(a -> !DateUtil.isGeboortedatumGelijk(DateUtil.toLocalDate(geboortedatum), a.getClient()));
	}
}
