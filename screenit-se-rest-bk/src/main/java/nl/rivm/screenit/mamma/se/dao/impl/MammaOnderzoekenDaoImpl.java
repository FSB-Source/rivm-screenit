package nl.rivm.screenit.mamma.se.dao.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-se-rest-bk
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
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import nl.rivm.screenit.mamma.se.dao.MammaOnderzoekenDao;
import nl.rivm.screenit.model.mamma.MammaAfspraak;
import nl.rivm.screenit.model.mamma.enums.MammaMammografieIlmStatus;
import nl.rivm.screenit.model.mamma.enums.MammaOnderzoekStatus;
import nl.rivm.screenit.util.DateUtil;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.hibernate.spring.dao.impl.AbstractAutowiredDao;

import org.hibernate.Criteria;
import org.hibernate.criterion.Projections;
import org.hibernate.criterion.Restrictions;
import org.hibernate.sql.JoinType;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Repository
@Transactional(propagation = Propagation.SUPPORTS, readOnly = true)
public class MammaOnderzoekenDaoImpl extends AbstractAutowiredDao implements MammaOnderzoekenDao
{

	@Autowired
	private HibernateService hibernateService;

	@Override
	public Map<Long, Integer> readOnderzochtVanSeOpWerkdag(Date beginDatum, String seCode)
	{
		return formatHibernateTupleListNaarMap(onderzoekBaseCriteria(seCode, beginDatum)
			.add(Restrictions.isNotNull("onderzoek.mammografie"))
			.add(Restrictions.isNotNull("mammografie.afgerondDoor"))
			.setProjection(Projections.projectionList()
				.add(Projections.groupProperty("mammografie.afgerondDoor.id"))
				.add(Projections.rowCount()))
			.list());
	}

	@Override
	public Map<Long, Integer> readAfgerondVanSeOpWerkdag(Date beginDatum, String seCode)
	{
		return formatHibernateTupleListNaarMap(readStatusCountVanSeOpWerkdag(beginDatum, seCode, MammaOnderzoekStatus.AFGEROND));
	}

	@Override
	public Map<Long, Integer> readOnderbrokenVanSeOpWerkdag(Date beginDatum, String seCode)
	{
		return formatHibernateTupleListNaarMap(readStatusCountVanSeOpWerkdag(beginDatum, seCode, MammaOnderzoekStatus.ONDERBROKEN));
	}

	@Override
	public Map<Long, Integer> readOnvolledigVanSeOpWerkdag(Date beginDatum, String seCode)
	{
		return formatHibernateTupleListNaarMap(readStatusCountVanSeOpWerkdag(beginDatum, seCode, MammaOnderzoekStatus.ONVOLLEDIG));
	}

	@Override
	public Map<Long, Integer> readAfwijkingenVanSeOpWerkdag(Date beginDatum, String seCode)
	{
		return formatHibernateTupleListNaarMap(onderzoekBaseCriteria(seCode, beginDatum)
			.add(Restrictions.isNotNull("onderzoek.signaleren"))
			.add(Restrictions.isNotNull("signaleren.afgerondDoor"))
			.add(Restrictions.eq("signaleren.heeftAfwijkingen", true))
			.setProjection(Projections.projectionList()
				.add(Projections.groupProperty("signaleren.afgerondDoor.id"))
				.add(Projections.rowCount()))
			.list());
	}

	@Override
	public Long readAantalOnderzoekenMetBeelden(Date beginDatum, String seCode)
	{
		return (Long) onderzoekBaseCriteria(seCode, beginDatum)
			.add(Restrictions.isNotNull("onderzoek.mammografie"))
			.add(Restrictions.isNotNull("mammografie.afgerondDoor"))
			.setProjection(Projections.rowCount())
			.uniqueResult();
	}

	@Override
	public Long readAantalOnderzoekenMetBeeldenBeschikbaarInIms(Date beginDatum, String seCode)
	{
		return (Long) onderzoekBaseCriteria(seCode, beginDatum)
			.add(Restrictions.isNotNull("onderzoek.mammografie"))
			.add(Restrictions.isNotNull("mammografie.afgerondDoor"))
			.add(Restrictions.eq("mammografie.ilmStatus", MammaMammografieIlmStatus.BESCHIKBAAR))
			.setProjection(Projections.rowCount())
			.uniqueResult();
	}

	@Override
	public Long readDoorgevoerdeOnderzoekenVanSeOpWerkdag(Date beginDatum, String seCode)
	{
		return (Long) onderzoekBaseCriteria(seCode, beginDatum)
			.add(Restrictions.eq("onderzoek.isDoorgevoerd", true))
			.setProjection(Projections.rowCount())
			.uniqueResult();
	}

	private Criteria onderzoekBaseCriteria(String seCode, Date beginDatum)
	{
		Date eindDatum = DateUtil.eindDag(beginDatum);
		return hibernateService.getHibernateSession().createCriteria(MammaAfspraak.class, "afspraak")
			.createAlias("afspraak.onderzoek", "onderzoek")
			.createAlias("onderzoek.signaleren", "signaleren", JoinType.LEFT_OUTER_JOIN)
			.createAlias("onderzoek.mammografie", "mammografie", JoinType.LEFT_OUTER_JOIN)
			.createAlias("afspraak.standplaatsPeriode", "standplaatsPeriode")
			.createAlias("standplaatsPeriode.screeningsEenheid", "screeningsEenheid")
			.add(Restrictions.eq("screeningsEenheid.code", seCode))
			.add(Restrictions.ge("afspraak.vanaf", beginDatum))
			.add(Restrictions.le("afspraak.vanaf", eindDatum));
	}

	private List readStatusCountVanSeOpWerkdag(Date beginDatum, String seCode, MammaOnderzoekStatus onderzoekStatus)
	{
		return onderzoekBaseCriteria(seCode, beginDatum)
			.add(Restrictions.eq("onderzoek.status", onderzoekStatus))
			.add(Restrictions.isNotNull("onderzoek.mammografie"))
			.add(Restrictions.isNotNull("mammografie.afgerondDoor"))
			.setProjection(Projections.projectionList()
				.add(Projections.groupProperty("mammografie.afgerondDoor.id"))
				.add(Projections.rowCount()))
			.list();
	}

	private Map<Long, Integer> formatHibernateTupleListNaarMap(List list)
	{
		Map<Long, Integer> result = new HashMap<>();
		Iterator it = list.iterator();
		while (it.hasNext())
		{
			Object instellingGebruikerIdEnCount[] = (Object[]) it.next();
			result.put((Long) instellingGebruikerIdEnCount[0], ((Long) instellingGebruikerIdEnCount[1]).intValue());
		}
		return result;
	}
}
