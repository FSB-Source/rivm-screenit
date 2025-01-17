package nl.rivm.screenit.dao.colon.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
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

import java.time.LocalDateTime;
import java.util.HashMap;
import java.util.List;

import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.dao.colon.RoosterDao;
import nl.rivm.screenit.model.colon.ColonAfspraakslotListViewWrapper;
import nl.rivm.screenit.model.colon.ColonIntakelocatie;
import nl.rivm.screenit.model.colon.RoosterListViewFilter;
import nl.rivm.screenit.model.colon.dto.VrijSlotZonderKamer;
import nl.rivm.screenit.model.colon.dto.VrijSlotZonderKamerFilter;
import nl.rivm.screenit.model.colon.enums.ColonAfspraakStatus;
import nl.rivm.screenit.model.colon.planning.ColonIntakekamer;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.util.DateUtil;
import nl.topicuszorg.hibernate.spring.dao.impl.AbstractAutowiredDao;

import org.apache.commons.lang.StringUtils;
import org.hibernate.SQLQuery;
import org.hibernate.transform.Transformers;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Repository;

import com.google.common.primitives.Ints;

@Repository
@Slf4j
public class RoosterDaoImpl extends AbstractAutowiredDao implements RoosterDao
{
	public static final String[] HERVERWERKING_MARKERS = new String[] { "zonder conclusie", "zonder passende screeningsronde", "geen ongunstige uitslag" };

	@Autowired
	private ICurrentDateSupplier currentDateSupplier;

	@Override
	public List<ColonAfspraakslotListViewWrapper> getAlleAfspraakslotsInPeriode(String sortProperty, boolean asc, RoosterListViewFilter filter,
		ColonIntakelocatie intakeLocatie)
	{
		var criteria = createAfspraakslotsCriteria(filter, intakeLocatie, sortProperty, asc);
		return criteria.list();
	}

	@Override
	public List<ColonAfspraakslotListViewWrapper> getAfspraakslots(String sortProperty, boolean asc, long first, long count, RoosterListViewFilter filter,
		ColonIntakelocatie intakeLocatie)
	{
		var criteria = createAfspraakslotsCriteria(filter, intakeLocatie, sortProperty, asc);
		if (first >= 0)
		{
			criteria.setFirstResult(Ints.checkedCast(first));
		}
		if (count >= 0)
		{
			criteria.setMaxResults(Ints.checkedCast(count));
		}

		return criteria.list();
	}

	private SQLQuery createAfspraakslotsCriteria(RoosterListViewFilter filter, ColonIntakelocatie intakeLocatie, String sortProperty, boolean asc)
	{
		getSession().flush();
		var selectFromQueryString = "select ";
		if (sortProperty == null)
		{
			selectFromQueryString += "count(afs.id) ";
		}
		else
		{
			selectFromQueryString += "ts.vanaf as \"startDatum\", ts.tot as \"eindDatum\", k.naam as \"kamer\", k.id as \"kamerId\", afs.id as \"afspraakslotId\", afs.capaciteit_mee_bepaald as \"capaciteitMeeBepaald\"";
		}
		selectFromQueryString += " from colon.afspraakslot afs "
			+ "join colon.tijdslot ts on afs.id=ts.id " 
			+ "join colon.intakekamer k on ts.kamer=k.id " 
			+ "join algemeen.org_organisatie il on il.id=k.intakelocatie "; 
		var whereQueryString = "where il.id=:intakelocatie and il.actief = true and k.actief = true ";

		var params = new HashMap<String, Object>();

		if (filter.getStatus() != null)
		{
			switch (filter.getStatus())
			{
			case INTAKE_GEPLAND:
				selectFromQueryString += "inner join colon.intakeafspraak ia on afs.id=ia.afspraakslot  ";
				whereQueryString += "and (ia.status=:status1 or ia.status=:status2) ";
				params.put("status1", ColonAfspraakStatus.GEPLAND.name());
				params.put("status2", ColonAfspraakStatus.UITGEVOERD.name());
				break;
			case GEBRUIKT_VOOR_CAPACITEIT:
				whereQueryString += "and afs.capaciteit_mee_bepaald = true ";
				selectFromQueryString += "left outer join colon.intakeafspraak ia on afs.id=ia.afspraakslot  ";
				whereQueryString += "and (ia.status is null or (ia.status!=:status1 and ia.status!=:status2)) ";
				params.put("status1", ColonAfspraakStatus.GEPLAND.name());
				params.put("status2", ColonAfspraakStatus.UITGEVOERD.name());
				whereQueryString += "and not EXISTS(select b.id from colon.tijdslot b where b.type='BLOKKADE' and b.kamer=k.id and b.vanaf<ts.tot and b.tot>ts.vanaf) ";
				break;
			case VRIJ_TE_VERPLAATSEN:
				whereQueryString += "and not exists(select id from colon.intakeafspraak ia where ia.afspraakslot=afs.id and (ia.status=:status1 or ia.status=:status2)) " 
					+ "and not EXISTS(select b.id from colon.tijdslot b where b.type='BLOKKADE' and b.kamer=k.id and b.vanaf<ts.tot and b.tot>ts.vanaf) ";
				if (filter.isRekeningHoudenMetCapaciteitMeeBepaald())
				{
					whereQueryString += "and afs.capaciteit_mee_bepaald = false";
				}
				params.put("status1", ColonAfspraakStatus.GEPLAND.name());
				params.put("status2", ColonAfspraakStatus.UITGEVOERD.name());
				break;
			case BLOKKADE:
				whereQueryString += "and EXISTS(select b.id from colon.tijdslot b where b.type='BLOKKADE' and b.kamer=k.id and b.vanaf<ts.tot and b.tot>ts.vanaf) ";
				break;
			default:
				break;
			}
		}
		whereQueryString += " and ts.tot>:vanaf and ts.vanaf<:tot ";
		String orderByQueryString = "";
		if (sortProperty != null)
		{
			orderByQueryString += "order by ";
			boolean supportedSortProperty = true;
			switch (sortProperty)
			{
			case "vanaf":
				orderByQueryString += "ts.vanaf ";
				break;
			case "tot":
				orderByQueryString += "ts.tot ";
				break;
			case "kamer":
				orderByQueryString += "k.naam ";
				break;
			default:
				LOG.error("unknown sortProperty " + sortProperty + " filter " + filter.getStatus());
				supportedSortProperty = false;
				break;
			}
			if (supportedSortProperty)
			{
				if (asc)
				{
					orderByQueryString += "asc";
				}
				else
				{
					orderByQueryString += "desc";
				}
			}
		}

		var criteria = getSession().createNativeQuery(selectFromQueryString + whereQueryString + orderByQueryString);
		criteria
			.setParameter("tot", filter.getEindDatum())
			.setParameter("vanaf", filter.getStartDatum())
			.setParameter("intakelocatie", intakeLocatie.getId());

		for (var param : params.entrySet())
		{
			criteria.setParameter(param.getKey(), param.getValue());
		}
		if (sortProperty != null)
		{
			criteria.setResultTransformer(Transformers.aliasToBean(ColonAfspraakslotListViewWrapper.class));
		}

		return criteria;
	}

	@Override
	public List<VrijSlotZonderKamer> getVrijeSlotenZonderKamer(String sortProperty, boolean asc, long first, long count, VrijSlotZonderKamerFilter filter)
	{
		var query = createVrijSlotZonderKamerQuery(filter, sortProperty, asc);
		if (first >= 0)
		{
			query.setFirstResult(Ints.checkedCast(first));
		}
		if (count >= 0)
		{
			query.setMaxResults(Ints.checkedCast(count));
		}
		return query.list();
	}

	@Override
	public List<VrijSlotZonderKamer> getVrijeSlotenZonderKamer(String sortProperty, boolean asc, VrijSlotZonderKamerFilter filter)
	{
		var query = createVrijSlotZonderKamerQuery(filter, sortProperty, asc);
		return query.list();
	}

	@Override
	public List<VrijSlotZonderKamer> getVrijeSlotenZonderKamer(VrijSlotZonderKamerFilter filter)
	{
		var query = createVrijSlotZonderKamerQuery(filter, "niet", true);
		return query.list();
	}

	@Override
	public long getVrijeSlotenZonderKamerCount(VrijSlotZonderKamerFilter filter)
	{
		var query = createVrijSlotZonderKamerQuery(filter, null, true);
		return ((Number) query.uniqueResult()).longValue();
	}

	private SQLQuery createVrijSlotZonderKamerQuery(VrijSlotZonderKamerFilter filter, String sortProperty, boolean asc)
	{
		getSession().flush();

		var querySB = new StringBuilder();

		querySB.append("WITH bezoek_adres AS");
		querySB.append("(");
		querySB.append("	WITH organisatie_adressn_met_rank AS");
		querySB.append("	(");
		querySB.append("		SELECT");
		querySB.append("		row_number() OVER (PARTITION BY org_organisatie ORDER BY adressen) AS rank,");
		querySB.append("		org_organisatie, adressen");
		querySB.append("		FROM algemeen.org_organisatie_adressen");
		querySB.append("	)");
		querySB.append("	SELECT org_organisatie, adressen");
		querySB.append("	FROM organisatie_adressn_met_rank");
		querySB.append("	WHERE rank = 1");
		querySB.append(")");

		if (!Boolean.TRUE.equals(filter.getAlleenIntakelocaties()))
		{
			if (sortProperty == null)
			{
				querySB.append(" select count(distinct(ts.vanaf, il.id))");
			}
			else
			{
				querySB.append(
					" select distinct ts.vanaf as \"startTijd\", ts.tot as \"eindTijd\", il.id as \"intakelocatieId\", a.plaats as \"plaats\", il.naam as \"naam\"");
			}
		}
		else
		{
			if (sortProperty == null)
			{
				querySB.append(" select count(distinct(il.id))");
			}
			else
			{
				querySB.append(" select distinct il.id as \"intakelocatieId\", a.plaats as \"plaats\", il.naam as \"naam\"");
			}
		}

		if (!Boolean.TRUE.equals(filter.getAlleenIntakelocaties()))
		{
			querySB.append(" from colon.afspraakslot afs");
			querySB.append(" join colon.tijdslot ts on afs.id=ts.id");
			querySB.append(" join colon.intakekamer k on ts.kamer=k.id");
		}
		else
		{
			querySB.append(" from colon.intakekamer k");
		}
		querySB.append(" join algemeen.org_organisatie il on k.intakelocatie=il.id");
		querySB.append(" join bezoek_adres ba on il.id=ba.org_organisatie");
		querySB.append(" join gedeeld.org_adres a on ba.adressen=a.id");

		var params = new HashMap<String, Object>();
		querySB.append(" where il.actief = true");
		querySB.append(" and k.actief = true");
		if (!Boolean.TRUE.equals(filter.getAlleenIntakelocaties()))
		{
			querySB.append(" and not exists(select id from colon.intakeafspraak ia where ia.afspraakslot = afs.id and (ia.status=:status1 or ia.status=:status2))");
			params.put("status1", ColonAfspraakStatus.GEPLAND.name());
			params.put("status2", ColonAfspraakStatus.UITGEVOERD.name());
			querySB.append(" and ts.id not in (select iafs.id from colon.tijdslot b, colon.tijdslot iafs "
				+ "where b.type='BLOKKADE' and iafs.type='AFSPRAAKSLOT' "
				+ "and b.kamer=iafs.kamer and b.vanaf<iafs.tot and b.tot>iafs.vanaf and iafs.vanaf>:vanaf1 and iafs.vanaf<:totEnMet1 )");

			querySB.append(" and ts.vanaf>:vanaf and ts.vanaf<:totEnMet");
			params.put("vanaf", filter.getVanaf());
			params.put("totEnMet", DateUtil.plusDagen(filter.getTotEnMet(), 1));
			params.put("vanaf1", filter.getVanaf());
			params.put("totEnMet1", DateUtil.plusDagen(filter.getTotEnMet(), 1));
			if (filter.getIntakelocatieId() != null)
			{
				querySB.append(" and il.id = :intakelocatieId");
				params.put("intakelocatieId", filter.getIntakelocatieId());
			}
			if (filter.getNietIntakelocatieId() != null)
			{
				querySB.append(" and il.id != :nietIntakelocatieId");
				params.put("nietIntakelocatieId", filter.getNietIntakelocatieId());
			}
		}

		if (StringUtils.isNotBlank(filter.getNaam()))
		{
			querySB.append(" and il.naam ILIKE '%' || :naam || '%'");
			params.put("naam", filter.getNaam());
		}
		if (StringUtils.isNotBlank(filter.getPlaats()))
		{
			querySB.append(" and plaats ILIKE '%' || :plaats || '%'");
			params.put("plaats", filter.getPlaats());
		}

		if (sortProperty != null && !sortProperty.equals("niet"))
		{
			querySB.append(" order by");
			switch (sortProperty)
			{
			case "vanaf":
				querySB.append(" \"startTijd\"");
				break;
			case "naam":
				querySB.append(" \"naam\"");
				break;
			case "plaats":
				querySB.append(" \"plaats\"");
				break;
			default:
				LOG.error("unknown sortProperty " + sortProperty);
				break;
			}
			if (asc)
			{
				querySB.append(" asc");
			}
			else
			{
				querySB.append(" desc");
			}
		}

		var query = getSession().createNativeQuery(querySB.toString());
		for (var param : params.entrySet())
		{
			query.setParameter(param.getKey(), param.getValue());
		}
		if (sortProperty != null)
		{
			query.setResultTransformer(Transformers.aliasToBean(VrijSlotZonderKamer.class));
		}
		return query;
	}

	@Override
	public List<ColonIntakekamer> getKamers(LocalDateTime startTijd, Long intakelocatieId)
	{
		var querySB = new StringBuilder();

		querySB.append("select {k.*}");

		querySB.append(" from colon.afspraakslot afs");
		querySB.append(" join colon.tijdslot ts on afs.id=ts.id");
		querySB.append(" join colon.intakekamer k on ts.kamer=k.id");

		var params = new HashMap<String, Object>();
		querySB.append(" and k.actief = true");
		querySB.append(" and not exists(select id from colon.intakeafspraak ia where ia.afspraakslot = afs.id and (ia.status=:status1 or ia.status=:status2))");
		params.put("status1", ColonAfspraakStatus.GEPLAND.name());
		params.put("status2", ColonAfspraakStatus.UITGEVOERD.name());
		querySB.append(
			" and not EXISTS(select b.id from colon.tijdslot b where b.type='BLOKKADE' "
				+ "and b.kamer=ts.kamer and b.vanaf<ts.tot and b.tot>ts.vanaf)");
		querySB.append(" and ts.vanaf=:startTijd");
		params.put("startTijd", startTijd);
		querySB.append(" and k.intakelocatie = :intakelocatieId");
		params.put("intakelocatieId", intakelocatieId);

		var query = getSession().createNativeQuery(querySB.toString()).addEntity("k", ColonIntakekamer.class);
		for (var param : params.entrySet())
		{
			query.setParameter(param.getKey(), param.getValue());
		}
		return query.list();
	}

	@Override
	public long getAfspraakslotsCount(RoosterListViewFilter filter, ColonIntakelocatie intakeLocatie)
	{
		var criteria = createAfspraakslotsCriteria(filter, intakeLocatie, null, true);

		return ((Number) criteria.uniqueResult()).longValue();
	}
}
