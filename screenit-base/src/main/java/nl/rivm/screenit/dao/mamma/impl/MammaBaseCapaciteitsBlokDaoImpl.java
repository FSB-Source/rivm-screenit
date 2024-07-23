package nl.rivm.screenit.dao.mamma.impl;

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

import java.math.BigDecimal;
import java.util.Collection;
import java.util.Comparator;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import lombok.RequiredArgsConstructor;

import nl.rivm.screenit.PreferenceKey;
import nl.rivm.screenit.dao.mamma.MammaBaseCapaciteitsBlokDao;
import nl.rivm.screenit.dto.mamma.afspraken.MammaAfspraakDto;
import nl.rivm.screenit.dto.mamma.afspraken.MammaAfspraakReserveringView;
import nl.rivm.screenit.dto.mamma.afspraken.MammaCapaciteitBlokDto;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.ScreeningOrganisatie;
import nl.rivm.screenit.model.mamma.MammaBlokkade;
import nl.rivm.screenit.model.mamma.MammaCapaciteitBlok;
import nl.rivm.screenit.model.mamma.MammaScreeningsEenheid;
import nl.rivm.screenit.model.mamma.MammaStandplaatsPeriode;
import nl.rivm.screenit.model.mamma.enums.MammaCapaciteitBlokType;
import nl.rivm.screenit.model.mamma.enums.MammaDoelgroep;
import nl.rivm.screenit.model.mamma.enums.MammaFactorType;
import nl.rivm.screenit.repository.mamma.MammaAfspraakReserveringRepository;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.util.DateUtil;
import nl.topicuszorg.hibernate.spring.dao.impl.AbstractAutowiredDao;
import nl.topicuszorg.preferencemodule.service.SimplePreferenceService;

import org.hibernate.Criteria;
import org.hibernate.Hibernate;
import org.hibernate.HibernateException;
import org.hibernate.criterion.CriteriaQuery;
import org.hibernate.criterion.Criterion;
import org.hibernate.criterion.DetachedCriteria;
import org.hibernate.criterion.Order;
import org.hibernate.criterion.Projections;
import org.hibernate.criterion.Restrictions;
import org.hibernate.criterion.Subqueries;
import org.hibernate.engine.spi.TypedValue;
import org.hibernate.sql.JoinType;
import org.springframework.stereotype.Repository;

@Repository
@RequiredArgsConstructor
public class MammaBaseCapaciteitsBlokDaoImpl extends AbstractAutowiredDao implements MammaBaseCapaciteitsBlokDao
{
	private final MammaAfspraakReserveringRepository afspraakReserveringRepository;

	private final SimplePreferenceService preferenceService;

	private final ICurrentDateSupplier currentDateSupplier;

	@Override
	public List<MammaCapaciteitBlok> getCapaciteitsBlokken(MammaScreeningsEenheid screeningEenheid, Date start, Date end, Collection<MammaCapaciteitBlokType> blokTypes)
	{
		var crit = getCapaciteitsBlokkenCriteria(screeningEenheid, start, end, blokTypes);
		return crit.list();
	}

	private Criteria getCapaciteitsBlokkenCriteria(MammaScreeningsEenheid screeningEenheid, Date start, Date end, Collection<MammaCapaciteitBlokType> blokTypes)
	{
		var crit = getSession().createCriteria(MammaCapaciteitBlok.class);
		crit.add(Restrictions.eq("screeningsEenheid", screeningEenheid));
		crit.add(Restrictions.le("vanaf", end));
		crit.add(Restrictions.ge("tot", start));
		if (blokTypes != null)
		{
			crit.add(Restrictions.in("blokType", blokTypes));
		}
		crit.addOrder(Order.asc("vanaf"));
		return crit;
	}

	@Override
	public Collection<MammaCapaciteitBlokDto> getNietGeblokkeerdeCapaciteitsBlokDtos(MammaStandplaatsPeriode standplaatsPeriode, Date start, Date end,
		Collection<MammaCapaciteitBlokType> blokTypes, Client client)
	{
		var screeningsEenheid = standplaatsPeriode.getScreeningsEenheid();
		var screeningOrganisatie = (ScreeningOrganisatie) Hibernate.unproxy(screeningsEenheid.getBeoordelingsEenheid().getParent().getRegio());

		var crit = getSession().createCriteria(MammaCapaciteitBlok.class, "blok");

		crit.createAlias("blok.afspraken", "afspraak", JoinType.LEFT_OUTER_JOIN);
		crit.createAlias("afspraak.uitnodiging", "uitnodiging", JoinType.LEFT_OUTER_JOIN);
		crit.createAlias("afspraak.opkomstkans", "opkomstkans", JoinType.LEFT_OUTER_JOIN);
		crit.createAlias("afspraak.standplaatsPeriode", "standplaatsPeriode", JoinType.LEFT_OUTER_JOIN);
		crit.createAlias("uitnodiging.screeningRonde", "screeningRonde", JoinType.LEFT_OUTER_JOIN);
		crit.createAlias("screeningRonde.dossier", "dossier", JoinType.LEFT_OUTER_JOIN);

		crit.add(Restrictions.eq("blok.screeningsEenheid", screeningsEenheid));
		crit.add(Restrictions.le("blok.vanaf", end));
		crit.add(Restrictions.ge("blok.tot", start));
		if (blokTypes != null)
		{
			crit.add(Restrictions.in("blok.blokType", blokTypes));
		}

		crit.add(Restrictions.or(Restrictions.isNull("afspraak.id"), Restrictions.eqProperty("uitnodiging.laatsteAfspraak.id", "afspraak.id")));

		var blokkadesOpEenBlokSubquery = DetachedCriteria.forClass(MammaBlokkade.class, "blokkade");
		blokkadesOpEenBlokSubquery.setProjection(Projections.id());
		blokkadesOpEenBlokSubquery.add(Restrictions.eq("blokkade.actief", true));
		blokkadesOpEenBlokSubquery.add(Restrictions.le("blokkade.vanaf", end));
		blokkadesOpEenBlokSubquery.add(Restrictions.ge("blokkade.totEnMet", start));
		blokkadesOpEenBlokSubquery.add(new ToDatePropertyExpression("blokkade.vanaf", "blok.tot", "<="));
		blokkadesOpEenBlokSubquery.add(new ToDatePropertyExpression("blokkade.totEnMet", "blok.tot", ">="));

		var blokkadeTypeDisjunction = Restrictions.disjunction();
		blokkadesOpEenBlokSubquery.add(blokkadeTypeDisjunction);

		blokkadeTypeDisjunction.add(Restrictions.eq("blokkade.screeningsEenheid.id", screeningsEenheid.getId()));

		blokkadeTypeDisjunction.add(Restrictions.eq("blokkade.regio.id", screeningOrganisatie.getId()));

		if ((standplaatsPeriode.getVanaf().before(end) || standplaatsPeriode.getVanaf().equals(end))
			&& (standplaatsPeriode.getTotEnMet().after(start) || standplaatsPeriode.getTotEnMet().equals(start)))
		{
			blokkadeTypeDisjunction.add(Restrictions.eq("blokkade.standplaats.id", standplaatsPeriode.getStandplaatsRonde().getStandplaats().getId()));
		}

		crit.add(Subqueries.notExists(blokkadesOpEenBlokSubquery));

		crit.addOrder(Order.asc("blok.id"));
		crit.addOrder(Order.asc("afspraak.vanaf"));

		crit.setProjection(Projections.projectionList() 
			.add(Projections.property("afspraak.vanaf")) 
			.add(Projections.property("blok.id")) 
			.add(Projections.property("dossier.doelgroep")) 
			.add(Projections.property("dossier.tehuis.id")) 
			.add(Projections.property("dossier.eersteOnderzoek")) 
			.add(Projections.property("opkomstkans.opkomstkans")) 
			.add(Projections.property("blok.vanaf")) 
			.add(Projections.property("blok.tot")) 
			.add(Projections.property("blok.blokType")) 
			.add(Projections.property("blok.aantalOnderzoeken")) 
			.add(Projections.property("blok.minderValideAfspraakMogelijk")) 
		);

		Map<Long, MammaCapaciteitBlokDto> capaciteitBlokDtoMap = new HashMap<>();
		List<Object[]> list = crit.list();
		for (var rowItems : list)
		{
			var capaciteitBlokId = (Long) rowItems[1];
			var afspraakVanaf = DateUtil.toLocalDateTime((Date) rowItems[0]);
			var capaciteitBlokDto = capaciteitBlokDtoMap.get(capaciteitBlokId);
			if (capaciteitBlokDto == null)
			{
				capaciteitBlokDto = new MammaCapaciteitBlokDto();
				capaciteitBlokDto.id = capaciteitBlokId;
				capaciteitBlokDto.vanaf = DateUtil.toLocalDateTime((Date) rowItems[6]);
				capaciteitBlokDto.tot = DateUtil.toLocalTime((Date) rowItems[7]);
				capaciteitBlokDto.blokType = (MammaCapaciteitBlokType) rowItems[8];
				capaciteitBlokDto.aantalOnderzoeken = (Integer) rowItems[9];
				capaciteitBlokDto.minderValideAfspraakMogelijk = (Boolean) rowItems[10];
				var aantalOnderzoeken = new BigDecimal(capaciteitBlokDto.aantalOnderzoeken);
				capaciteitBlokDto.beschikbareCapaciteit = aantalOnderzoeken.multiply(capaciteitBlokDto.blokType.getFactorType().getFactor(screeningOrganisatie));
				capaciteitBlokDto.standplaatsPeriode = standplaatsPeriode;
				capaciteitBlokDtoMap.put(capaciteitBlokId, capaciteitBlokDto);
			}
			if (afspraakVanaf != null)
			{
				var doelgroep = (MammaDoelgroep) rowItems[2];
				var tehuisId = (Long) rowItems[3];
				var eersteOnderzoek = (Boolean) rowItems[4];
				var opkomstkans = (BigDecimal) rowItems[5];
				var factor = MammaFactorType.getFactorType(tehuisId != null, doelgroep, eersteOnderzoek).getFactor(screeningOrganisatie);
				var afspraakDto = new MammaAfspraakDto();
				afspraakDto.setCapaciteitBlokDto(capaciteitBlokDto);
				afspraakDto.setVanaf(afspraakVanaf);
				afspraakDto.setBenodigdeCapaciteit(factor.multiply(opkomstkans));
				afspraakDto.setMinderValide(doelgroep.equals(MammaDoelgroep.MINDER_VALIDE));
				afspraakDto.setDubbeleTijd(doelgroep.equals(MammaDoelgroep.DUBBELE_TIJD));
				capaciteitBlokDto.afspraakDtos.add(afspraakDto);
			}
			var benodigdeCapaciteit = capaciteitBlokDto.afspraakDtos.stream().map(MammaAfspraakDto::getBenodigdeCapaciteit).reduce(BigDecimal.ZERO,
				BigDecimal::add);
			capaciteitBlokDto.vrijeCapaciteit = capaciteitBlokDto.beschikbareCapaciteit.subtract(benodigdeCapaciteit);
		}
		haalAfspraakReserveringenOpEnVoegToeAanCapaciteitBlokken(capaciteitBlokDtoMap, screeningOrganisatie, client);
		sorteerCapaciteitBlokkenOpAfspraakTijdEnZetAfspraakTot(capaciteitBlokDtoMap);
		return capaciteitBlokDtoMap.values();
	}

	private void haalAfspraakReserveringenOpEnVoegToeAanCapaciteitBlokken(Map<Long, MammaCapaciteitBlokDto> capaciteitBlokDtoMap, ScreeningOrganisatie screeningOrganisatie,
		Client client)
	{
		var maximaleReserveringsTijd = preferenceService.getInteger(PreferenceKey.MAMMA_AFSPRAAK_RESERVERING_GELDIG_VOOR.name(), 0);
		var vroegstOpTeHalenReservering = currentDateSupplier.getLocalDateTime().minusMinutes(maximaleReserveringsTijd);

		var capaciteitblokIds = capaciteitBlokDtoMap.keySet();
		var clientId = client != null ? client.getId() : null;
		var reserveringDtos = afspraakReserveringRepository.haalReserveringenOpVoorCapaciteitsblokken(vroegstOpTeHalenReservering, capaciteitblokIds, clientId);
		reserveringDtos.forEach(reservering -> converteerReserveringNaarAfspraakInCapaciteitBlok(reservering, capaciteitBlokDtoMap, screeningOrganisatie));
	}

	private void converteerReserveringNaarAfspraakInCapaciteitBlok(MammaAfspraakReserveringView reservering, Map<Long, MammaCapaciteitBlokDto> capaciteitBlokDtoMap,
		ScreeningOrganisatie screeningOrganisatie)
	{
		var doelgroep = reservering.getDoelgroep();
		var factor = MammaFactorType.getFactorType(reservering.getTehuisId() != null, doelgroep, reservering.getEersteOnderzoek()).getFactor(screeningOrganisatie);

		var capaciteitBlokDto = capaciteitBlokDtoMap.get(reservering.getCapaciteitBlokId());
		var afspraakDto = new MammaAfspraakDto();
		afspraakDto.setCapaciteitBlokDto(capaciteitBlokDto);
		afspraakDto.setVanaf(reservering.getVanaf());
		afspraakDto.setBenodigdeCapaciteit(factor.multiply(reservering.getOpkomstkans()));
		afspraakDto.setMinderValide(doelgroep.equals(MammaDoelgroep.MINDER_VALIDE));
		afspraakDto.setDubbeleTijd(doelgroep.equals(MammaDoelgroep.DUBBELE_TIJD));
		capaciteitBlokDto.afspraakDtos.add(afspraakDto);
	}

	private void sorteerCapaciteitBlokkenOpAfspraakTijdEnZetAfspraakTot(Map<Long, MammaCapaciteitBlokDto> capaciteitBlokDtoMap)
	{
		capaciteitBlokDtoMap.values().forEach(capaciteitBlokDto ->
		{
			capaciteitBlokDto.afspraakDtos.sort(Comparator.comparing(MammaAfspraakDto::getVanaf));
			MammaAfspraakDto vorigeAfspraakDto = null;
			for (var afspraakDto : capaciteitBlokDto.afspraakDtos)
			{
				if (vorigeAfspraakDto != null)
				{
					vorigeAfspraakDto.setTot(afspraakDto.getVanaf().toLocalTime());
				}
				vorigeAfspraakDto = afspraakDto;
			}
			if (vorigeAfspraakDto != null)
			{
				vorigeAfspraakDto.setTot(capaciteitBlokDto.tot);
			}
		});
	}

	@Override
	public Long countCapaciteitsBlokken(MammaScreeningsEenheid screeningEenheid, Date start, Date end, Collection<MammaCapaciteitBlokType> blokTypes)
	{
		var crit = getCapaciteitsBlokkenCriteria(screeningEenheid, start, end, blokTypes);
		crit.setProjection(Projections.rowCount());
		return (Long) crit.uniqueResult();
	}

	private class ToDatePropertyExpression implements Criterion
	{
		private final TypedValue[] noTypedValues = new TypedValue[0];

		private final String propertyName;

		private final String otherPropertyName;

		private final String op;

		private ToDatePropertyExpression(String propertyName, String otherPropertyName, String op)
		{
			this.propertyName = propertyName;
			this.otherPropertyName = otherPropertyName;
			this.op = op;
		}

		@Override
		public String toSqlString(Criteria criteria, CriteriaQuery criteriaQuery) throws HibernateException
		{
			final var lhsColumns = criteriaQuery.findColumns(propertyName, criteria);
			final var rhsColumns = criteriaQuery.findColumns(otherPropertyName, criteria);

			return lhsColumns[0] + "::date " + op + rhsColumns[0] + "::date";
		}

		@Override
		public TypedValue[] getTypedValues(Criteria criteria, CriteriaQuery criteriaQuery)
		{
			return noTypedValues;
		}

		@Override
		public String toString()
		{
			return propertyName + op + otherPropertyName;
		}

	}

}
