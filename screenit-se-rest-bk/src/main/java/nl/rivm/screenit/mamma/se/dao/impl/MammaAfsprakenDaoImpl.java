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

import java.time.LocalDate;
import java.util.Date;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import nl.rivm.screenit.mamma.se.dao.ClientIdentificatie;
import nl.rivm.screenit.mamma.se.dao.MammaAfsprakenDao;
import nl.rivm.screenit.model.mamma.MammaAfspraak;
import nl.rivm.screenit.model.mamma.enums.MammaAfspraakStatus;
import nl.rivm.screenit.model.mamma.enums.MammaIdentificatiesoort;
import nl.rivm.screenit.util.DateUtil;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.hibernate.spring.dao.impl.AbstractAutowiredDao;

import org.hibernate.Query;
import org.hibernate.criterion.Order;
import org.hibernate.criterion.Projections;
import org.hibernate.criterion.Restrictions;
import org.hibernate.sql.JoinType;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Profile;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Repository
@Transactional(propagation = Propagation.SUPPORTS, readOnly = true)
@Profile("!test")
public class MammaAfsprakenDaoImpl extends AbstractAutowiredDao implements MammaAfsprakenDao
{

	@Autowired
	private HibernateService hibernateService;

	@Override
	public Map<Long, ClientIdentificatie> readLaatsteIdentificatieVanClienten(List<Long> clientIds)
	{
		Query query = getSession().createSQLQuery(
			"select patient_id\n" +
				"  , identificatiesoort\n" +
				"  , identificatienummer\n" +
				"from (\n" +
				"       select pat.id as patient_id\n" +
				"         , afs.*\n" +
				"         , row_number() over(partition by pat.id order by afs.vanaf desc) as volgnr\n" +
				"       from mamma.afspraak afs\n" +
				"         inner join mamma.uitnodiging uit on afs.uitnodiging = uit.id\n" +
				"         inner join mamma.screening_ronde sr on uit.screening_ronde = sr.id\n" +
				"         inner join mamma.dossier dos on sr.dossier = dos.id\n" +
				"         inner join gedeeld.pat_patient pat on dos.id = pat.mamma_dossier\n" +
				"       where afs.identificatienummer is not null\n" +
				"             and pat.id in (:clientIds)\n" +
				"     ) as d where volgnr = 1");
		query.setParameterList("clientIds", clientIds);
		final List<Object[]> queryResult = query.list();
		Map<Long, ClientIdentificatie> result = new HashMap<>();
		for (Object[] entry : queryResult)
		{
			result.put(((java.math.BigInteger) entry[0]).longValue(),
				new ClientIdentificatie(MammaIdentificatiesoort.valueOf((String) entry[1]), (String) entry[2]));
		}
		return result;
	}

	@Override
	public Map<Long, Integer> readInschrijvingenVanSeInRange(Date beginDatum, Date eindDatum, String seCode)
	{
		List list = hibernateService.getHibernateSession().createCriteria(MammaAfspraak.class, "afspraak")
			.createAlias("afspraak.standplaatsPeriode", "standplaatsPeriode")
			.createAlias("standplaatsPeriode.screeningsEenheid", "screeningsEenheid")
			.add(Restrictions.eq("screeningsEenheid.code", seCode))
			.add(Restrictions.ge("vanaf", beginDatum))
			.add(Restrictions.le("vanaf", eindDatum))
			.add(Restrictions.isNotNull("ingeschrevenDoor"))
			.setProjection(Projections.projectionList()
				.add(Projections.groupProperty("ingeschrevenDoor.id"))
				.add(Projections.rowCount()))
			.list();
		Map<Long, Integer> result = new HashMap<>();
		Iterator it = list.iterator();
		while (it.hasNext())
		{
			Object instellingGebruikerIdEnCount[] = (Object[]) it.next();
			result.put((Long) instellingGebruikerIdEnCount[0], ((Long) instellingGebruikerIdEnCount[1]).intValue());
		}
		return result;
	}

	@Override
	public Date readDatumVanOudsteNietAfgeslotenOnderzoek(LocalDate vandaag, String seCode)
	{
		return (Date) hibernateService.getHibernateSession().createCriteria(MammaAfspraak.class, "afspraak")
			.createAlias("afspraak.standplaatsPeriode", "standplaatsPeriode")
			.createAlias("standplaatsPeriode.screeningsEenheid", "screeningsEenheid")
			.createAlias("afspraak.onderzoek", "onderzoek", JoinType.LEFT_OUTER_JOIN)
			.add(Restrictions.or(
				Restrictions.eq("afspraak.status", MammaAfspraakStatus.INGESCHREVEN),
				Restrictions.eq("afspraak.status", MammaAfspraakStatus.ONDERZOEK),
				Restrictions.eq("afspraak.status", MammaAfspraakStatus.SIGNALEREN),
				Restrictions.and(
					Restrictions.isNotNull("onderzoek"),
					Restrictions.eq("onderzoek.isDoorgevoerd", false))))
			.add(Restrictions.eq("screeningsEenheid.code", seCode))
			.add(Restrictions.gt("vanaf", DateUtil.toUtilDate(vandaag.minusMonths(2))))
			.add(Restrictions.lt("vanaf", DateUtil.toUtilDate(vandaag)))
			.addOrder(Order.asc("vanaf"))
			.setProjection(Projections.property("vanaf"))
			.setMaxResults(1)
			.uniqueResult();
	}
}
