package nl.rivm.screenit.dao.impl;

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

import java.util.HashMap;
import java.util.List;
import java.util.stream.Collectors;

import nl.rivm.screenit.dao.LogDao;
import nl.rivm.screenit.model.SortState;
import nl.rivm.screenit.model.logging.LogRegel;
import nl.rivm.screenit.model.logging.LoggingZoekCriteria;
import nl.rivm.screenit.util.query.SQLQueryUtil;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.hibernate.spring.dao.impl.AbstractAutowiredDao;

import org.apache.commons.lang.StringUtils;
import org.hibernate.SQLQuery;
import org.hibernate.type.LongType;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Repository;

import com.google.common.primitives.Ints;

@Repository
public class LogDaoImpl extends AbstractAutowiredDao implements LogDao
{
	@Autowired
	private HibernateService hibernateService;

	@Override
	public List<LogRegel> getLogRegels(LoggingZoekCriteria loggingZoekCriteria, int first, int count, SortState<String> sortState)
	{
		var logSql = getLogSql(loggingZoekCriteria, sortState);
		if (first >= 0)
		{
			logSql.setFirstResult(Ints.checkedCast(first));
		}
		if (count >= 0)
		{
			logSql.setMaxResults(Ints.checkedCast(count));
		}

		List<Long> logRegelIds = logSql.addScalar("id", LongType.INSTANCE).list();
		return logRegelIds.stream().map(id -> hibernateService.load(LogRegel.class, id)).collect(Collectors.toList());
	}

	public SQLQuery getLogSql(LoggingZoekCriteria loggingZoekCriteria, SortState<String> order)
	{
		var parameters = new HashMap<String, Object>();
		var select = "SELECT ";
		if (order == null)
		{
			select += "count(distinct lr.id) ";
		}
		else
		{
			select += "distinct lr.id ";
		}
		var from = "from gedeeld.log_regel lr ";
		var where = "";
		var groupby = "";
		var orderby = "";

		var joinPpe = false;
		var joinM = false;
		var joinLe = false;

		if (loggingZoekCriteria.getGebeurtenis() != null && !loggingZoekCriteria.getGebeurtenis().isEmpty())
		{
			where = SQLQueryUtil.whereOrAnd(where);
			var params = SQLQueryUtil.inExpressionParametersEnum("gebeurtenisParam", loggingZoekCriteria.getGebeurtenis());
			where += "lr.log_gebeurtenis in (:" + StringUtils.join(params.keySet(), ", :") + ") ";
			parameters.putAll(params);
		}
		if (loggingZoekCriteria.getVanaf() != null)
		{
			where = SQLQueryUtil.whereOrAnd(where);
			where += "lr.gebeurtenis_datum > :vanaf ";
			parameters.put("vanaf", loggingZoekCriteria.getVanaf());
		}
		if (loggingZoekCriteria.getTot() != null)
		{
			where = SQLQueryUtil.whereOrAnd(where);
			where += "lr.gebeurtenis_datum <= :tot ";
			parameters.put("tot", loggingZoekCriteria.getTot());
		}
		if (loggingZoekCriteria.getScreeningsEenheidId() != null)
		{
			where = SQLQueryUtil.whereOrAnd(where);
			where += "lr.screenings_eenheid = :screenings_eenheid ";
			parameters.put("screenings_eenheid", loggingZoekCriteria.getScreeningsEenheidId());
		}
		if (loggingZoekCriteria.getGebruikersnaam() != null)
		{
			from += "left outer join algemeen.org_medewerker m on lr.gebruiker = m.id ";
			joinM = true;
			where = SQLQueryUtil.whereOrAnd(where);
			where += "m.gebruikersnaam = :gebruikersnaam ";
			parameters.put("gebruikersnaam", loggingZoekCriteria.getGebruikersnaam());
		}

		if (loggingZoekCriteria.getBsnClient() != null)
		{
			from += "left outer join gedeeld.pat_patient ppa on lr.client = ppa.id ";
			from += "left outer join gedeeld.pat_persoon ppe on ppe.patient = ppa.id ";
			joinPpe = true;

			where = SQLQueryUtil.whereOrAnd(where);
			where += "ppe.bsn = :bsn ";
			parameters.put("bsn", loggingZoekCriteria.getBsnClient());
		}

		if (loggingZoekCriteria.getMelding() != null || loggingZoekCriteria.getLevel() != null && !loggingZoekCriteria.getLevel().isEmpty())
		{
			from += "left outer join gedeeld.log_event le on lr.log_event = le.id ";
			joinLe = true;
			if (loggingZoekCriteria.getMelding() != null)
			{
				where = SQLQueryUtil.whereOrAnd(where);
				where += "lower(le.melding) like :melding ";
				parameters.put("melding", "%" + loggingZoekCriteria.getMelding().toLowerCase() + "%");
			}
			if (loggingZoekCriteria.getLevel() != null && !loggingZoekCriteria.getLevel().isEmpty())
			{

				where = SQLQueryUtil.whereOrAnd(where);
				var params = SQLQueryUtil.inExpressionParametersEnum("levelParam", loggingZoekCriteria.getLevel());
				where += "le.level in (:" + StringUtils.join(params.keySet(), ", :") + ") ";
				parameters.putAll(params);
			}
		}

		if (loggingZoekCriteria.getBevolkingsonderzoeken() != null && !loggingZoekCriteria.getBevolkingsonderzoeken().isEmpty())
		{
			from += "left outer join gedeeld.log_regel_bevolkingsonderzoeken lrb on lrb.log_regel = lr.id ";
			where = SQLQueryUtil.whereOrAnd(where);
			var params = SQLQueryUtil.inExpressionParametersEnum("bvoParam", loggingZoekCriteria.getBevolkingsonderzoeken());
			where += "lrb.bevolkingsonderzoeken in (:" + StringUtils.join(params.keySet(), ", :") + ") ";
			parameters.putAll(params);
		}

		if (loggingZoekCriteria.getRegio() != null)
		{
			if (!joinPpe)
			{
				from += "left outer join gedeeld.pat_patient ppa on lr.client = ppa.id ";
				from += "left outer join gedeeld.pat_persoon ppe on ppe.patient = ppa.id ";
				joinPpe = true;
			}
			from += "left outer join gedeeld.org_adres aga on ppe.gba_adres = aga.id ";
			from += "left outer join algemeen.gemeente agm on agm.id = aga.gba_gemeente ";
			from += "left outer join algemeen.org_organisatie_medewerker om on om.id = lr.ingelogde_gebruiker ";
			where = SQLQueryUtil.whereOrAnd(where);
			where += "(agm.screening_organisatie = :soid or om.organisatie = :soid2)";
			parameters.put("soid", loggingZoekCriteria.getRegio());
			parameters.put("soid2", loggingZoekCriteria.getRegio());
		}

		if (order != null)
		{
			var orderByQueryString = "";
			switch (order.getSortParam())
			{
			case "logGebeurtenis":
				orderByQueryString += "lr.log_gebeurtenis ";
				break;
			case "gebeurtenisDatum":
				orderByQueryString += "lr.gebeurtenis_datum ";
				break;
			case "gebruiker.gebruikersnaam":
				if (!joinM)
				{
					from += "left outer join algemeen.org_medewerker m on lr.gebruiker = m.id ";
				}
				orderByQueryString += "m.gebruikersnaam ";
				break;
			case "persoon.achternaam":
				if (!joinPpe)
				{
					from += "left outer join gedeeld.pat_patient ppa on lr.client = ppa.id ";
					from += "left outer join gedeeld.pat_persoon ppe on ppe.patient = ppa.id ";
				}
				orderByQueryString += "ppe.achternaam ";
				break;
			case "logEvent":
				if (!joinLe)
				{
					from += "left outer join gedeeld.log_event le on lr.log_event = le.id ";
				}
				orderByQueryString += "le.melding ";
				break;
			default:
				orderByQueryString = "";
			}

			if (StringUtils.isNotBlank(orderByQueryString))
			{
				select += ", " + orderByQueryString;
				orderByQueryString = "order by " + orderByQueryString;
			}
			if (!orderByQueryString.equals("") && order.isAsc())
			{
				orderByQueryString += "asc";
			}
			else if (!orderByQueryString.equals(""))
			{
				orderByQueryString += "desc";
			}
			orderby = orderByQueryString;
		}

		var loggingSql = select + from + where + groupby + orderby;
		var criteria = getSession().createNativeQuery(loggingSql);
		for (var param : parameters.entrySet())
		{
			criteria.setParameter(param.getKey(), param.getValue());
		}
		return criteria;
	}

	@Override
	public long countLogRegels(LoggingZoekCriteria loggingZoekCriteria)
	{
		var logSql = getLogSql(loggingZoekCriteria, null);
		return ((Number) logSql.uniqueResult()).longValue();
	}

}
