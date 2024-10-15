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

import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.List;

import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.dao.ProjectDao;
import nl.rivm.screenit.model.SortState;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.project.Project;
import nl.rivm.screenit.model.project.ProjectStatus;
import nl.rivm.screenit.model.project.ProjectType;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.util.query.SQLQueryUtil;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.hibernate.spring.dao.impl.AbstractAutowiredDao;

import org.apache.commons.lang.StringUtils;
import org.apache.shiro.util.CollectionUtils;
import org.hibernate.SQLQuery;
import org.hibernate.type.DateType;
import org.hibernate.type.LongType;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Slf4j
@Repository
public class ProjectDaoImpl extends AbstractAutowiredDao implements ProjectDao
{

	@Autowired
	private HibernateService hibernateService;

	@Autowired
	private ICurrentDateSupplier currentDateSupplier;

	private SQLQuery getDefaultSqlProjectQuery(Project zoekObject, List<Long> instellingIdsProject, List<Long> instellingIdsBriefproject, SortState<String> order)
	{
		var vandaag = currentDateSupplier.getDate();
		var parameters = new HashMap<String, Object>();
		var select = order == null ? "SELECT count(p.id) " : "SELECT p.id ";
		var from = "from algemeen.project p ";
		var where = "";
		var orderby = "";

		if (zoekObject.getProjectStatussen() != null && !zoekObject.getProjectStatussen().isEmpty())
		{
			var projectstatuswhere = getProjectStatusWhere(zoekObject.getProjectStatussen(), vandaag);
			if (projectstatuswhere != null)
			{
				where = SQLQueryUtil.whereOrAnd(where);
				where += projectstatuswhere;
				parameters.put("vandaag", vandaag);
			}
		}

		if (zoekObject.getProjectTypes() != null && !zoekObject.getProjectTypes().isEmpty())
		{
			where = SQLQueryUtil.whereOrAnd(where);
			var params = SQLQueryUtil.inExpressionParametersEnum("projecttype", zoekObject.getProjectTypes());
			where += " p.type in (:" + StringUtils.join(params.keySet(), ", :") + ") ";
			parameters.putAll(params);
		}

		if (zoekObject.getBevolkingsonderzoeken() != null && !zoekObject.getBevolkingsonderzoeken().isEmpty())
		{
			from += "left outer join algemeen.project_bevolkingsonderzoeken pb on p.id = pb.project ";
			where = SQLQueryUtil.whereOrAnd(where);
			var params = SQLQueryUtil.inExpressionParametersEnum("onderzoek", zoekObject.getBevolkingsonderzoeken());
			where += "pb.bevolkingsonderzoeken in (:" + StringUtils.join(params.keySet(), ", :") + ") ";
			parameters.putAll(params);
		}

		var whereProject = "";
		var whereBriefproject = "";

		if (!CollectionUtils.isEmpty(instellingIdsProject) && zoekObject.getProjectTypes().contains(ProjectType.PROJECT))
		{
			var paramsProject = SQLQueryUtil.inExpressionParametersLong("organisatie", instellingIdsProject);
			whereProject = "p.type = 'PROJECT' and p.organisatie in (:" + StringUtils.join(paramsProject.keySet(), ", :") + ")";
			parameters.putAll(paramsProject);
		}

		if (!CollectionUtils.isEmpty(instellingIdsBriefproject) && zoekObject.getProjectTypes().contains(ProjectType.BRIEFPROJECT))
		{
			var paramsBriefproject = SQLQueryUtil.inExpressionParametersLong("organisatie", instellingIdsBriefproject);
			whereBriefproject = "p.type = 'BRIEFPROJECT' and p.organisatie in (:" + StringUtils.join(paramsBriefproject.keySet(), ", :") + ")";
			parameters.putAll(paramsBriefproject);
		}

		if (StringUtils.isNotBlank(zoekObject.getNaam()))
		{
			where = SQLQueryUtil.whereOrAnd(where);
			where += "p.naam = :naam";
			parameters.put("naam", zoekObject.getNaam());
		}

		if (zoekObject.getGroepSelectieType() != null)
		{
			where = SQLQueryUtil.whereOrAnd(where);
			where += "p.groep_selectie_type = :groep_selectie_type";
			parameters.put("groep_selectie_type", zoekObject.getGroepSelectieType().name());
		}

		if (StringUtils.isNotEmpty(whereProject) || StringUtils.isNotEmpty(whereBriefproject))
		{
			from += "join algemeen.org_organisatie oop on p.organisatie = oop.id ";
			where = SQLQueryUtil.whereOrAnd(where);
			if (StringUtils.isNotEmpty(whereProject) && StringUtils.isNotEmpty(whereBriefproject))
			{
				where += "(" + whereProject + " or " + whereBriefproject + ")";
			}
			else if (StringUtils.isNotEmpty(whereProject))
			{
				where += whereProject;
			}
			else if (StringUtils.isNotEmpty(whereBriefproject))
			{
				where += whereBriefproject;
			}
		}
		where += " ";

		if (order != null)
		{
			LOG.debug(order.getSortParam());
			orderby = "order by ";
			switch (order.getSortParam())
			{
			case "naam":
				orderby += "p.naam ";
				break;
			case "status":
				orderby += "p.status ";
				break;
			case "startDatum":
				orderby += "p.start_datum ";
				break;
			case "eindDatum":
				orderby += "p.eind_datum ";
				break;
			case "contactpersoon.medewerker":
				from += "join algemeen.org_organisatie_medewerker oom on p.contactpersoon = oom.id ";
				from += "join algemeen.org_medewerker m on oom.medewerker = m.id ";
				orderby += "m.achternaam ";
				break;
			default:
				orderby = "";
			}
			if (!orderby.equals("") && order.isAsc())
			{
				orderby += "asc";
			}
			else if (!orderby.equals(""))
			{
				orderby += "desc";
			}
		}

		var projectenSql = select + from + where + orderby;
		LOG.debug(projectenSql);
		var criteria = getSession().createNativeQuery(projectenSql);
		for (var param : parameters.entrySet())
		{
			criteria.setParameter(param.getKey(), param.getValue());
		}
		return criteria;
	}

	private String getProjectStatusWhere(List<ProjectStatus> statussen, Date vandaag)
	{
		if (statussen != null && !statussen.isEmpty())
		{
			if (statussen.contains(ProjectStatus.NOG_TE_STARTEN) && statussen.contains(ProjectStatus.ACTIEF) && statussen.contains(ProjectStatus.BEEINDIGD))
			{
				return null;
			}
			else if (statussen.contains(ProjectStatus.NOG_TE_STARTEN) && statussen.contains(ProjectStatus.BEEINDIGD))
			{
				return "p.start_datum > :vandaag OR p.eind_datum <= :vandaag ";
			}
			else if (statussen.contains(ProjectStatus.NOG_TE_STARTEN) && statussen.contains(ProjectStatus.ACTIEF))
			{
				return "p.eind_datum > :vandaag ";
			}
			else if (statussen.contains(ProjectStatus.ACTIEF) && statussen.contains(ProjectStatus.BEEINDIGD))
			{
				return "p.start_datum <= :vandaag ";
			}
			else if (statussen.contains(ProjectStatus.ACTIEF))
			{
				return "p.start_datum <= :vandaag AND p.eind_datum > :vandaag ";
			}
			else if (statussen.contains(ProjectStatus.NOG_TE_STARTEN))
			{
				return "p.start_datum > :vandaag ";
			}
			else if (statussen.contains(ProjectStatus.BEEINDIGD))
			{
				return "p.eind_datum <= :vandaag ";
			}
		}
		return null;
	}

	@Override
	public List<Project> getProjecten(Project zoekObject, List<Long> instellingIdsProject, List<Long> instellingIdsBriefproject, long first, long count,
		SortState<String> sortState)
	{
		var query = getDefaultSqlProjectQuery(zoekObject, instellingIdsProject, instellingIdsBriefproject, sortState);
		query.addScalar("id", LongType.INSTANCE);
		if (count > 0)
		{
			query.setMaxResults((int) count);
		}
		if (first > 0)
		{
			query.setFirstResult((int) first);
		}
		List<Long> projectLongs = query.list();

		List<Project> projecten = new ArrayList<Project>();
		for (var l : projectLongs)
		{
			projecten.add(hibernateService.load(Project.class, l));
		}
		return projecten;
	}

	@Override
	public long getCountProjecten(Project zoekObject, List<Long> instellingIdsProject, List<Long> instellingIdsBriefproject)
	{
		var query = getDefaultSqlProjectQuery(zoekObject, instellingIdsProject, instellingIdsBriefproject, null);
		return ((Number) query.uniqueResult()).longValue();
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public void resetWachtOpStartProject(Bevolkingsonderzoek bvo)
	{
		var sql = String.format("update %s.%s set wacht_op_start_project = false where wacht_op_start_project = true or wacht_op_start_project is null",
			getSchema(bvo), getDossierTable(bvo));
		var query = getSession().createNativeQuery(sql);
		var aantal = query.executeUpdate();
		LOG.debug("Aantal gereset {}", aantal);
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public void setNieuwWachtOpStartProject(Bevolkingsonderzoek bvo, Date nu)
	{
		var sql = String.format("update %s.%s set wacht_op_start_project = true WHERE id in "
				+ "(select dossier_.id as y0_ " +
				"from gedeeld.project_client projectClient_ " +
				"inner join gedeeld.pat_patient patient_ on projectClient_.client=patient_.id " +
				"inner join %s.%s dossier_ on patient_.%s=dossier_.id " +
				"inner join gedeeld.project_groep groep1_ on projectClient_.groep=groep1_.id " +
				"inner join algemeen.project project2_ on groep1_.project=project2_.id " +
				"inner join algemeen.project_bevolkingsonderzoeken projectBevolkingsonderzoeken3_ on project2_.id=projectBevolkingsonderzoeken3_.project " +
				"where project2_.type='PROJECT' " +
				"and (project2_.start_datum>:startDatum or (project2_.eind_datum>:eindDatum and groep1_.actief=false)) " +
				"and projectClient_.actief=true " +
				"and projectBevolkingsonderzoeken3_.bevolkingsonderzoeken='%s')",
			getSchema(bvo), getDossierTable(bvo), getSchema(bvo), getDossierTable(bvo), getJoinColumn(bvo), bvo.name());
		var query = getSession().createNativeQuery(sql);
		query.setParameter("startDatum", nu, DateType.INSTANCE);
		query.setParameter("eindDatum", nu, DateType.INSTANCE);
		var aantal = query.executeUpdate();
		LOG.debug("Aantal geset {}", aantal);
	}

	private String getSchema(Bevolkingsonderzoek bvo)
	{
		switch (bvo)
		{
		case CERVIX:
			return "cervix";
		case COLON:
			return "colon";
		case MAMMA:
			return "mamma";
		default:
			throw new IllegalStateException();
		}
	}

	private String getDossierTable(Bevolkingsonderzoek bvo)
	{
		switch (bvo)
		{
		case CERVIX:
			return "dossier";
		case COLON:
			return "colon_dossier";
		case MAMMA:
			return "dossier";
		default:
			throw new IllegalStateException();
		}
	}

	private String getJoinColumn(Bevolkingsonderzoek bvo)
	{
		switch (bvo)
		{
		case CERVIX:
			return "cervix_dossier";
		case COLON:
			return "colon_dossier";
		case MAMMA:
			return "mamma_dossier";
		default:
			throw new IllegalStateException();
		}
	}

}
