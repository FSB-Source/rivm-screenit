
package nl.rivm.screenit.dao.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
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

import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import lombok.extern.slf4j.Slf4j;
import nl.rivm.screenit.dao.ProjectDao;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.ProjectParameterKey;
import nl.rivm.screenit.model.ScreeningOrganisatie;
import nl.rivm.screenit.model.SortState;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.BriefType;
import nl.rivm.screenit.model.project.Project;
import nl.rivm.screenit.model.project.ProjectAttribuut;
import nl.rivm.screenit.model.project.ProjectBestand;
import nl.rivm.screenit.model.project.ProjectBestandVerwerkingEntry;
import nl.rivm.screenit.model.project.ProjectBrief;
import nl.rivm.screenit.model.project.ProjectBriefActie;
import nl.rivm.screenit.model.project.ProjectBriefActieType;
import nl.rivm.screenit.model.project.ProjectClient;
import nl.rivm.screenit.model.project.ProjectClientAttribuut;
import nl.rivm.screenit.model.project.ProjectGroep;
import nl.rivm.screenit.model.project.ProjectStatus;
import nl.rivm.screenit.model.project.ProjectType;
import nl.rivm.screenit.model.project.ProjectVragenlijstStatus;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.util.query.SQLQueryUtil;
import nl.rivm.screenit.util.query.ScreenitRestrictions;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.hibernate.spring.dao.impl.AbstractAutowiredDao;

import org.apache.commons.lang.StringUtils;
import org.apache.shiro.util.CollectionUtils;
import org.hibernate.Criteria;
import org.hibernate.Query;
import org.hibernate.SQLQuery;
import org.hibernate.criterion.CriteriaQuery;
import org.hibernate.criterion.DetachedCriteria;
import org.hibernate.criterion.Order;
import org.hibernate.criterion.Projections;
import org.hibernate.criterion.Restrictions;
import org.hibernate.criterion.Subqueries;
import org.hibernate.engine.spi.SessionFactoryImplementor;
import org.hibernate.sql.JoinType;
import org.hibernate.type.LongType;
import org.hibernate.type.Type;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import com.google.common.primitives.Ints;

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
		Date vandaag = currentDateSupplier.getDate();
		Map<String, Object> parameters = new HashMap<String, Object>();
		String select = "SELECT p.id ";
		String from = "from algemeen.project p ";
		String where = "";
		String orderby = "";

		if (zoekObject.getProjectStatussen() != null && !zoekObject.getProjectStatussen().isEmpty())
		{
			String projectstatuswhere = getProjectStatusWhere(zoekObject.getProjectStatussen(), vandaag);
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
			Map<String, Object> params = SQLQueryUtil.inExpressionParametersEnum("projecttype", zoekObject.getProjectTypes());
			where += " p.type in (:" + StringUtils.join(params.keySet(), ", :") + ") ";
			parameters.putAll(params);
		}

		if (zoekObject.getBevolkingsonderzoeken() != null && !zoekObject.getBevolkingsonderzoeken().isEmpty())
		{
			from += "left outer join algemeen.project_bevolkingsonderzoeken pb on p.id = pb.project ";
			where = SQLQueryUtil.whereOrAnd(where);
			Map<String, Object> params = SQLQueryUtil.inExpressionParametersEnum("onderzoek", zoekObject.getBevolkingsonderzoeken());
			where += "pb.bevolkingsonderzoeken in (:" + StringUtils.join(params.keySet(), ", :") + ") ";
			parameters.putAll(params);
		}

		String whereProject = "";
		String whereBriefproject = "";

		if (!CollectionUtils.isEmpty(instellingIdsProject) && zoekObject.getProjectTypes().contains(ProjectType.PROJECT))
		{
			Map<String, Object> paramsProject = SQLQueryUtil.inExpressionParametersLong("organisatie", instellingIdsProject);
			whereProject = "p.type = 'PROJECT' and p.organisatie in (:" + StringUtils.join(paramsProject.keySet(), ", :") + ")";
			parameters.putAll(paramsProject);
		}

		if (!CollectionUtils.isEmpty(instellingIdsBriefproject) && zoekObject.getProjectTypes().contains(ProjectType.BRIEFPROJECT))
		{
			Map<String, Object> paramsBriefproject = SQLQueryUtil.inExpressionParametersLong("organisatie", instellingIdsBriefproject);
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
			from += "inner join algemeen.org_organisatie oop on p.organisatie = oop.id ";
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
				from += "inner join algemeen.org_organisatie_medewerker oom on p.contactpersoon = oom.id ";
				from += "inner join algemeen.org_medewerker m on oom.medewerker = m.id ";
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

		String loggingSql = select + from + where + orderby;
		LOG.debug(loggingSql);
		SQLQuery criteria = getSession().createSQLQuery(loggingSql);
		for (Entry<String, Object> param : parameters.entrySet())
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
		SQLQuery query = getDefaultSqlProjectQuery(zoekObject, instellingIdsProject, instellingIdsBriefproject, sortState);
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
		for (Long l : projectLongs)
		{
			projecten.add(hibernateService.load(Project.class, l));
		}
		return projecten;
	}

	@Override
	public long getCountProjecten(Project zoekObject, List<Long> instellingIdsProject, List<Long> instellingIdsBriefproject)
	{
		SQLQuery query = getDefaultSqlProjectQuery(zoekObject, instellingIdsProject, instellingIdsBriefproject, null);
		return query.list().size();
	}

	private Criteria getDefaultGroepenCriteria(ProjectGroep zoekObject)
	{
		Criteria crit = getSession().createCriteria(ProjectGroep.class);
		if (zoekObject.getProject() != null)
		{
			crit.add(Restrictions.eq("project", zoekObject.getProject()));
		}
		if (zoekObject.getActief() != null)
		{
			crit.add(Restrictions.eq("actief", zoekObject.getActief()));
		}

		return crit;
	}

	@SuppressWarnings("unchecked")
	@Override
	public Iterator<ProjectGroep> getGroepen(ProjectGroep zoekObject, long first, long count, SortState<String> sortState)
	{
		Criteria crit = getDefaultGroepenCriteria(zoekObject);

		crit.addOrder(Order.asc(sortState.getSortParam()));
		if (!sortState.isAsc())
		{
			crit.addOrder(Order.desc(sortState.getSortParam()));
		}

		crit.setMaxResults((int) count);
		crit.setFirstResult((int) first);

		return crit.list().iterator();
	}

	@Override
	public long getCountGroepen(ProjectGroep zoekObject)
	{
		Criteria crit = getDefaultGroepenCriteria(zoekObject);
		crit.setProjection(Projections.rowCount());
		return ((Number) crit.uniqueResult()).longValue();
	}

	private Criteria getDefaultClientProjectenCriteria(ProjectClient pClient)
	{
		Criteria crit = getSession().createCriteria(ProjectClient.class);

		crit.createAlias("project", "project");

		if (pClient != null && pClient.getClient() != null)
		{
			crit.add(Restrictions.eq("client", pClient.getClient()));
		}

		return crit;
	}

	@SuppressWarnings("unchecked")
	@Override
	public Iterator<ProjectClient> getClientProjecten(ProjectClient pClient, long first, long count, SortState<String> sortState)
	{
		Criteria crit = getDefaultClientProjectenCriteria(pClient);

		crit.addOrder(Order.asc(sortState.getSortParam()));
		if (!sortState.isAsc())
		{
			crit.addOrder(Order.desc(sortState.getSortParam()));
		}

		crit.setMaxResults((int) count);
		crit.setFirstResult((int) first);

		return crit.list().iterator();
	}

	@Override
	public long getCountClientProjecten(ProjectClient client)
	{
		Criteria crit = getDefaultClientProjectenCriteria(client);
		crit.setProjection(Projections.rowCount());
		return ((Number) crit.uniqueResult()).longValue();
	}

	private Criteria getCriteriaProjectenBriefActies(ProjectBriefActie actie)
	{
		Criteria crit = getSession().createCriteria(ProjectBriefActie.class);
		crit.createAlias("project", "project");
		crit.add(Restrictions.ne("type", ProjectBriefActieType.HERINNERING));
		if (actie.getProject() != null)
		{
			crit.add(Restrictions.eq("project", actie.getProject()));
		}
		if (actie.getActief() != null)
		{
			crit.add(Restrictions.eq("actief", actie.getActief()));
		}

		return crit;

	}

	@SuppressWarnings("unchecked")
	@Override
	public Iterator<ProjectBriefActie> getProjectBriefActies(ProjectBriefActie actie, long first, long count, SortState<String> sortState)
	{
		Criteria crit = getCriteriaProjectenBriefActies(actie);
		crit.createAlias("document", "document");
		crit.createAlias("vragenlijst", "vragenlijst", JoinType.LEFT_OUTER_JOIN);
		crit.setMaxResults((int) count);
		crit.setFirstResult((int) first);
		if (sortState.isAsc())
		{
			crit.addOrder(Order.asc(sortState.getSortParam()));
		}
		else
		{
			crit.addOrder(Order.desc(sortState.getSortParam()));
		}
		return crit.list().iterator();
	}

	@Override
	public long getCountProjectBriefActies(ProjectBriefActie actie)
	{
		Criteria crit = getCriteriaProjectenBriefActies(actie);
		crit.setProjection(Projections.rowCount());
		return ((Number) crit.uniqueResult()).longValue();
	}

	@SuppressWarnings("unchecked")
	@Override
	public List<ProjectClient> getValideClientenVanProject(Project project, ProjectBriefActie definitie)
	{
		Date vandaag = currentDateSupplier.getDateMidnight();
		Criteria crit = getSession().createCriteria(ProjectClient.class);

		crit.add(Restrictions.eq("actief", Boolean.TRUE));

		crit.createAlias("project", "project");
		crit.add(
			Restrictions.and(
				Restrictions.gt("project.eindDatum", vandaag), 
				Restrictions.le("project.startDatum", vandaag)
			) 
		); 
		crit.add(Restrictions.eq("project", project));

		crit.createAlias("client", "client");
		crit.createAlias("client.persoon", "persoon", JoinType.INNER_JOIN);
		ScreenitRestrictions.addClientBaseRestrictions(crit, "client", "persoon");

		List<Bevolkingsonderzoek> onderzoeken = project.getExcludeerAfmelding();
		if (onderzoeken.contains(Bevolkingsonderzoek.COLON))
		{
			crit.createAlias("client.colonDossier", "colonDossier", JoinType.LEFT_OUTER_JOIN);
			crit.add(Restrictions.or(Restrictions.isNull("colonDossier.id"), Restrictions.eq("colonDossier.aangemeld", Boolean.TRUE)));
		}
		if (onderzoeken.contains(Bevolkingsonderzoek.CERVIX))
		{
			crit.createAlias("client.cervixDossier", "cervixDossier", JoinType.LEFT_OUTER_JOIN);
			crit.add(Restrictions.or(Restrictions.isNull("cervixDossier.id"), Restrictions.eq("cervixDossier.aangemeld", Boolean.TRUE)));
		}
		if (onderzoeken.contains(Bevolkingsonderzoek.MAMMA))
		{
			crit.createAlias("client.mammaDossier", "mammaDossier", JoinType.LEFT_OUTER_JOIN);
			crit.add(Restrictions.or(Restrictions.isNull("mammaDossier.id"), Restrictions.eq("mammaDossier.aangemeld", Boolean.TRUE)));
		}

		DetachedCriteria subquery = DetachedCriteria.forClass(ProjectBrief.class);
		subquery.add(Restrictions.eq("definitie", definitie));
		subquery.setProjection(Projections.distinct(Projections.property("projectClient")));
		crit.add(Subqueries.propertyNotIn("id", subquery));

		crit.createAlias("groep", "groep");
		crit.add(Restrictions.eq("groep.actief", Boolean.TRUE));

		return crit.list();
	}

	@SuppressWarnings("unchecked")
	@Override
	public List<Long> getActieveActiesVoorBrieven(ScreeningOrganisatie so)
	{
		Date vandaag = currentDateSupplier.getDate();
		Criteria crit = getSession().createCriteria(ProjectBrief.class);
		crit.createAlias("projectClient", "projectClient");
		crit.createAlias("projectClient.groep", "groep");
		crit.createAlias("projectClient.project", "project");
		crit.createAlias("projectClient.client", "client");
		crit.createAlias("client.persoon", "persoon");
		crit.createAlias("persoon.gbaAdres", "adres");
		crit.createAlias("adres.gemeente", "gemeente");
		crit.createAlias("gemeente.screeningOrganisatie", "screeningorganisatie");
		crit.createAlias("definitie", "definitie");

		crit.add(Restrictions.eq("screeningorganisatie", so));
		crit.add(Restrictions.eq("projectClient.actief", Boolean.TRUE));
		crit.add(Restrictions.eq("groep.actief", Boolean.TRUE));
		crit.add(
			Restrictions.and(
				Restrictions.gt("project.eindDatum", vandaag), 
				Restrictions.lt("project.startDatum", vandaag)
			) 
		); 

		ScreenitRestrictions.addClientBaseRestrictions(crit, "client", "persoon");

		crit.setProjection(Projections.property("definitie.id"));

		return crit.list();
	}

	@SuppressWarnings("unchecked")
	@Override
	public List<Project> getAllProjectenWhereProjectBriefActieHasBriefType(BriefType type)
	{
		Criteria crit = getSession().createCriteria(Project.class);
		crit.createAlias("projectBriefActies", "acties");

		crit.add(Restrictions.ge("eindDatum", currentDateSupplier.getDateTimeMidnight().toDate()));
		crit.add(Restrictions.eq("acties.briefType", type));

		crit.setResultTransformer(Criteria.DISTINCT_ROOT_ENTITY);

		return crit.list();
	}

	@SuppressWarnings("unchecked")
	@Override
	public List<ProjectBrief> getAllProjectBriefForHerinnering(ProjectBriefActie actie, Date verstuurdOp)
	{
		Date vandaag = currentDateSupplier.getDate();
		Criteria crit = getSession().createCriteria(ProjectBrief.class);
		crit.createAlias("projectClient", "projectClient");
		crit.createAlias("projectClient.groep", "projectGroep");
		crit.createAlias("projectClient.project", "project");
		crit.createAlias("mergedBrieven", "mergedBrieven");
		crit.createAlias("definitie", "definitie");
		crit.createAlias("vragenlijstAntwoordenHolder", "vragenlijstAntwoorden", JoinType.LEFT_OUTER_JOIN);

		crit.add(Restrictions.eq("projectGroep.actief", Boolean.TRUE));
		crit.add(Restrictions.eq("projectClient.actief", Boolean.TRUE));

		crit.add(
			Restrictions.and(
				Restrictions.gt("project.eindDatum", vandaag), 
				Restrictions.lt("project.startDatum", vandaag)
			) 
		); 

		crit.add(Restrictions.isNotNull("definitie.vragenlijst"));

		crit.add(Restrictions.ge("mergedBrieven.printDatum", verstuurdOp));
		crit.add(Restrictions.eq("definitie", actie.getBaseActie()));

		crit.add(
			Restrictions.or(
				Restrictions.isNull("vragenlijstAntwoorden.status"), 
				Restrictions.ne("vragenlijstAntwoorden.status", ProjectVragenlijstStatus.AFGEROND) 
			));

		DetachedCriteria subqueryAlVerstuurd = DetachedCriteria.forClass(ProjectBrief.class);
		subqueryAlVerstuurd.createAlias("mergedBrieven", "mergedBrieven");
		subqueryAlVerstuurd.add(Restrictions.eq("definitie", actie));
		subqueryAlVerstuurd.add(Restrictions.isNotNull("mergedBrieven.printDatum"));
		subqueryAlVerstuurd.setProjection(Projections.property("client.id"));

		crit.add(Subqueries.propertyNotIn("client.id", subqueryAlVerstuurd));

		return crit.list();
	}

	@Override
	public boolean isVragenlijstGekoppeldAanNietBeeindigdProject(Long vragenlijstId)
	{
		Criteria crit = getSession().createCriteria(Project.class);
		crit.createAlias("projectBriefActies", "acties");

		crit.add(Restrictions.ge("eindDatum", currentDateSupplier.getDate()));
		crit.add(Restrictions.eq("acties.vragenlijst.id", vragenlijstId));

		crit.setProjection(Projections.rowCount());

		return ((Number) crit.uniqueResult()).longValue() > 0;
	}

	@Override
	public boolean isVragenlijstGekoppeldAanProject(Long vragenlijstId)
	{
		Criteria crit = getSession().createCriteria(ProjectBriefActie.class);

		crit.add(Restrictions.eq("vragenlijst.id", vragenlijstId));

		crit.setProjection(Projections.rowCount());

		return ((Number) crit.uniqueResult()).longValue() > 0;
	}

	@Override
	public Long getAantalInactieveProjectClientenVanProject(Project project)
	{
		Criteria crit = getSession().createCriteria(ProjectClient.class);
		crit.createAlias("project", "project");

		crit.add(Restrictions.eq("project", project));

		crit.add(Restrictions.eq("actief", false));

		crit.setProjection(Projections.rowCount());

		return ((Number) crit.uniqueResult()).longValue();
	}

	@Override
	public Long getAantalInactieveProjectClientenVanProjectGroep(ProjectGroep groep)
	{
		Criteria crit = getSession().createCriteria(ProjectClient.class);
		crit.createAlias("groep", "groep");

		crit.add(Restrictions.eq("groep", groep));

		crit.add(Restrictions.eq("actief", false));

		crit.setProjection(Projections.rowCount());

		return ((Number) crit.uniqueResult()).longValue();
	}

	@Override
	public Long getAantalProjectClientenVanProject(Project project)
	{
		Criteria crit = getSession().createCriteria(ProjectClient.class);
		crit.createAlias("project", "project");

		crit.add(Restrictions.eq("project", project));

		crit.setProjection(Projections.rowCount());

		return ((Number) crit.uniqueResult()).longValue();
	}

	@Override
	public Long getAantalProjectClientenVanProjectGroep(ProjectGroep groep)
	{
		Criteria crit = getSession().createCriteria(ProjectClient.class);
		crit.createAlias("groep", "groep");

		crit.add(Restrictions.eq("groep", groep));

		crit.setProjection(Projections.rowCount());

		return ((Number) crit.uniqueResult()).longValue();
	}

	@Override
	public ProjectBriefActie getProjectBriefActie(Client client, BriefType briefType)
	{

		Criteria criteria = getSession().createCriteria(ProjectBriefActie.class);
		criteria.createAlias("project", "project");
		criteria.createAlias("project.clienten", "client");
		criteria.createAlias("client.groep", "groep");

		Date nu = currentDateSupplier.getDate();
		criteria.add(Restrictions.le("project.startDatum", nu));
		criteria.add(Restrictions.gt("project.eindDatum", nu));
		criteria.add(Restrictions.eq("briefType", briefType));
		criteria.add(Restrictions.eq("type", ProjectBriefActieType.VERVANGENDEBRIEF));
		criteria.add(Restrictions.eq("client.client", client));
		criteria.add(Restrictions.eq("groep.actief", Boolean.TRUE));
		criteria.add(Restrictions.eq("client.actief", Boolean.TRUE));
		List list = criteria.list();
		if (!list.isEmpty())
		{
			return (ProjectBriefActie) list.get(0);
		}
		return null;
	}

	@Override
	public List<ProjectGroep> getActieveProjectGroepenVoorUitnodigingDK()
	{
		Criteria criteria = getSession().createCriteria(ProjectGroep.class);
		criteria.createAlias("project", "project");
		criteria.createAlias("project.parameters", "parameters");

		Date nu = currentDateSupplier.getDate();
		criteria.add(Restrictions.le("actiefDatum", nu));
		criteria.add(Restrictions.gt("uitnodigenVoorDKvoor", nu));
		criteria.add(Restrictions.eq("actief", Boolean.TRUE));
		criteria.add(Restrictions.gt("populatie", Integer.valueOf(0)));
		criteria.add(Restrictions.eq("parameters.key", ProjectParameterKey.COLON_UITNODIGEN_PRIORITEIT));
		criteria.add(Restrictions.isNotNull("parameters.value"));
		criteria.addOrder(new Order("parameters.value", true)
		{
			@Override
			public String toSqlString(Criteria criteria, CriteriaQuery criteriaQuery)
			{
				final String[] columns = criteriaQuery.getColumnsUsingProjection(criteria, getPropertyName());
				final Type type = criteriaQuery.getTypeUsingProjection(criteria, getPropertyName());
				final SessionFactoryImplementor factory = criteriaQuery.getFactory();
				final int[] sqlTypes = type.sqlTypes(factory);

				final StringBuilder fragment = new StringBuilder();
				for (int i = 0; i < columns.length; i++)
				{
					final StringBuilder expression = new StringBuilder();

					expression.append("CAST(" + columns[i] + " AS int)");

					fragment.append(
						factory.getDialect().renderOrderByElement(
							expression.toString(),
							null,
							isAscending() ? "asc" : "desc",
							factory.getSettings().getDefaultNullPrecedence()));
					if (i < columns.length - 1)
					{
						fragment.append(", ");
					}
				}

				return fragment.toString();
			}
		});
		criteria.addOrder(Order.asc("uitnodigenVoorDKvoor"));
		criteria.addOrder(Order.asc("actiefDatum"));
		List list = criteria.list();

		return list;
	}

	private Criteria getCriteriaProjectAttributen(ProjectAttribuut filterObject)
	{
		Criteria crit = getSession().createCriteria(ProjectAttribuut.class);
		crit.add(Restrictions.eq("project", filterObject.getProject()));
		if (filterObject.getActief() != null)
		{
			crit.add(Restrictions.eq("actief", filterObject.getActief()));
		}
		return crit;
	}

	@Override
	public Iterator<ProjectAttribuut> getProjectAttributen(ProjectAttribuut filterObject, long first, long count, SortState<String> sortState)
	{
		Criteria crit = getCriteriaProjectAttributen(filterObject);
		crit.setMaxResults(Ints.checkedCast(count));
		crit.setFirstResult(Ints.checkedCast(first));
		if (sortState.isAsc())
		{
			crit.addOrder(Order.asc(sortState.getSortParam()));
		}
		else
		{
			crit.addOrder(Order.desc(sortState.getSortParam()));
		}
		return crit.list().iterator();
	}

	@Override
	public Long getAantalProjectAttributen(ProjectAttribuut filterObject)
	{
		Criteria crit = getCriteriaProjectAttributen(filterObject);
		crit.setProjection(Projections.rowCount());

		return ((Number) crit.uniqueResult()).longValue();
	}

	private Criteria getCriteriaProjectBestanden(ProjectBestand zoekObject)
	{
		Criteria crit = getSession().createCriteria(ProjectBestand.class);
		crit.add(Restrictions.eq("project", zoekObject.getProject()));
		return crit;
	}

	@Override
	public Long getAantalProjectBestanden(ProjectBestand zoekObject)
	{
		Criteria crit = getCriteriaProjectBestanden(zoekObject);
		crit.setProjection(Projections.rowCount());

		return ((Number) crit.uniqueResult()).longValue();
	}

	@Override
	public Iterator<ProjectBestand> getProjectBestanden(ProjectBestand filterObject, long first, long count, SortState<String> sortState)
	{
		Criteria crit = getCriteriaProjectBestanden(filterObject);
		crit.createAlias("uploadDocument", "uploadDocument");
		crit.setMaxResults(Ints.checkedCast(count));
		crit.setFirstResult(Ints.checkedCast(first));
		if (sortState.isAsc())
		{
			crit.addOrder(Order.asc(sortState.getSortParam()));
		}
		else
		{
			crit.addOrder(Order.desc(sortState.getSortParam()));
		}
		return crit.list().iterator();
	}

	private Criteria getCriteriaProjectBestandVerwerkingEntries(ProjectBestandVerwerkingEntry entry)
	{
		Criteria crit = getSession().createCriteria(ProjectBestandVerwerkingEntry.class);
		crit.add(Restrictions.eq("verwerking", entry.getVerwerking()));
		return crit;
	}

	@Override
	public Long getAantalProjectbestandVerwerkingEntries(ProjectBestandVerwerkingEntry entry)
	{
		Criteria crit = getCriteriaProjectBestandVerwerkingEntries(entry);
		crit.setProjection(Projections.rowCount());

		return ((Number) crit.uniqueResult()).longValue();
	}

	@Override
	public Iterator<ProjectBestandVerwerkingEntry> getProjectBestandVerwerkingEntries(ProjectBestandVerwerkingEntry entry, long first, long count, SortState<String> sortState)
	{
		Criteria crit = getCriteriaProjectBestandVerwerkingEntries(entry);
		crit.setMaxResults(Ints.checkedCast(count));
		crit.setFirstResult(Ints.checkedCast(first));
		if (sortState.isAsc())
		{
			crit.addOrder(Order.asc(sortState.getSortParam()));
		}
		else
		{
			crit.addOrder(Order.desc(sortState.getSortParam()));
		}
		return crit.list().iterator();
	}

	@Override
	public ProjectClientAttribuut getProjectClientAttribuut(ProjectClient client, ProjectAttribuut attribuut)
	{
		Criteria crit = getSession().createCriteria(ProjectClientAttribuut.class);
		crit.add(Restrictions.eq("attribuut", attribuut));
		crit.add(Restrictions.eq("projectClient", client));
		return (ProjectClientAttribuut) crit.uniqueResult();
	}

	private Criteria getCriteriaAttributenVoorProjectClient(ProjectClientAttribuut filterModel)
	{
		Criteria crit = getSession().createCriteria(ProjectClientAttribuut.class);
		crit.createAlias("attribuut", "attribuut");
		crit.add(Restrictions.eq("projectClient", filterModel.getProjectClient()));
		return crit;
	}

	@Override
	public Long getAantalAttributenVoorProjectClient(ProjectClientAttribuut filterModel)
	{
		Criteria crit = getCriteriaAttributenVoorProjectClient(filterModel);
		crit.setProjection(Projections.rowCount());
		return ((Number) crit.uniqueResult()).longValue();
	}

	@Override
	public Iterator<ProjectClientAttribuut> getAttributenVoorProjectClient(ProjectClientAttribuut filterModel, long first, long count, SortState<String> sortState)
	{
		Criteria crit = getCriteriaAttributenVoorProjectClient(filterModel);
		crit.setMaxResults(Ints.checkedCast(count));
		crit.setFirstResult(Ints.checkedCast(first));
		if (sortState.isAsc())
		{
			crit.addOrder(Order.asc(sortState.getSortParam()));
		}
		else
		{
			crit.addOrder(Order.desc(sortState.getSortParam()));
		}
		return crit.list().iterator();
	}

	@Override
	public ProjectClient getProjectClient(Client client, ProjectBestand bestand)
	{
		Criteria crit = getSession().createCriteria(ProjectClient.class);
		crit.add(Restrictions.eq("project", bestand.getProject()));
		crit.add(Restrictions.eq("client", client));
		return (ProjectClient) crit.uniqueResult();
	}

	@Override
	public ProjectAttribuut getProjectAttribuut(ProjectAttribuut attribuut)
	{
		Criteria crit = getSession().createCriteria(ProjectAttribuut.class);
		crit.add(Restrictions.eq("project", attribuut.getProject()));

		crit.add(
			Restrictions.or(
				Restrictions.eq("mergeField", attribuut.getMergeField()), 
				Restrictions.eq("naam", attribuut.getNaam()) 
			));
		if (attribuut.getId() != null)
		{
			crit.add(Restrictions.ne("id", attribuut.getId()));
		}
		return (ProjectAttribuut) crit.uniqueResult();
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public void resetWachtOpStartProject(Bevolkingsonderzoek bvo)
	{
		String sql = String.format("update %s.%s set wacht_op_start_project = false where wacht_op_start_project = true or wacht_op_start_project is null",
				getSchema(bvo), getDossierTable(bvo));
		Query query = getSession().createSQLQuery(sql);
		int aantal = query.executeUpdate();
		LOG.debug("Aantal gereset " + aantal);
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public void setNieuwWachtOpStartProject(Bevolkingsonderzoek bvo, Date nu)
	{
		String sql = String.format("update %s.%s set wacht_op_start_project = true WHERE id in "
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
		Query query = getSession().createSQLQuery(sql);
		query.setDate("startDatum", nu);
		query.setDate("eindDatum", nu);
		int aantal = query.executeUpdate();
		LOG.debug("Aantal geset " + aantal);
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
