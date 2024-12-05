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

import java.util.List;

import nl.rivm.screenit.dao.mamma.MammaBaseFollowUpDao;
import nl.rivm.screenit.dto.mamma.MammaFollowUpInstellingDto;
import nl.rivm.screenit.model.Instelling;
import nl.rivm.screenit.model.ScreeningOrganisatie;
import nl.rivm.screenit.model.SortState;
import nl.rivm.screenit.model.berichten.enums.VerslagStatus;
import nl.rivm.screenit.model.berichten.enums.VerslagType;
import nl.rivm.screenit.model.mamma.MammaDossier;
import nl.rivm.screenit.model.mamma.MammaFollowUpRadiologieVerslag;
import nl.rivm.screenit.model.mamma.MammaFollowUpVerslag;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.topicuszorg.hibernate.spring.dao.impl.AbstractAutowiredDao;

import org.hibernate.Criteria;
import org.hibernate.criterion.DetachedCriteria;
import org.hibernate.criterion.Order;
import org.hibernate.criterion.Projections;
import org.hibernate.criterion.Restrictions;
import org.hibernate.criterion.Subqueries;
import org.hibernate.sql.JoinType;
import org.hibernate.transform.Transformers;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Repository
@Transactional(propagation = Propagation.SUPPORTS, readOnly = true)
public class MammaBaseFollowUpDaoImpl extends AbstractAutowiredDao implements MammaBaseFollowUpDao
{
	@Autowired
	private ICurrentDateSupplier dateSupplier;

	@Override
	public List<MammaFollowUpInstellingDto> zoekInstellingenMetOpenstaandePaVerslagen(ScreeningOrganisatie regio)
	{
		Criteria crit = createInstellingenMetOpenstaandePaVerslagenCriteria(regio);
		crit.setResultTransformer(Transformers.aliasToBean(MammaFollowUpInstellingDto.class));
		return crit.list();
	}

	@Override
	public List<MammaFollowUpRadiologieVerslag> zoekDossiersMetOpenstaandePaVerslagen(Instelling instelling, int first, int count, SortState<String> sortState)
	{
		Criteria criteria = createOpenstaandePaVerslagenCriteria(instelling);
		addSortAndPaging(criteria, sortState, first, count);
		return criteria.list();
	}

	private void addSortAndPaging(Criteria criteria, SortState<String> sortState, int first, int count)
	{
		String sortProperty = sortState.getSortParam();
		if (sortProperty != null)
		{
			if (sortState.isAsc())
			{
				criteria.addOrder(Order.asc(sortProperty));
			}
			else
			{
				criteria.addOrder(Order.desc(sortProperty));
			}
		}
		criteria.setFirstResult(Math.max(first, 0));
		if (count > 0)
		{
			criteria.setMaxResults(count);
		}
	}

	@Override
	public long countDossiersMetOpenstaandePaVerslagen(Instelling instelling)
	{
		Criteria crit = createOpenstaandePaVerslagenCriteria(instelling);
		crit.setProjection(Projections.rowCount());
		return (Long) crit.uniqueResult();
	}

	@Override
	public boolean heeftOpenstaandeFollowUpConclusie(MammaDossier dossier)
	{
		Criteria openstaandeFollowUpConclusieDossier = getSession().createCriteria(MammaDossier.class, "dossier");
		openstaandeFollowUpConclusieDossier.createAlias("dossier.laatsteBeoordelingMetUitslag", "beoordeling");
		openstaandeFollowUpConclusieDossier.createAlias("beoordeling.onderzoek", "onderzoek");
		openstaandeFollowUpConclusieDossier.createAlias("onderzoek.afspraak", "afspraak");
		openstaandeFollowUpConclusieDossier.createAlias("afspraak.uitnodiging", "uitnodiging");
		openstaandeFollowUpConclusieDossier.createAlias("uitnodiging.screeningRonde", "screeningRonde");

		openstaandeFollowUpConclusieDossier.createAlias("screeningRonde.followUpVerslagen", "followUpVerslag", JoinType.LEFT_OUTER_JOIN,
			Restrictions.eq("followUpVerslag.type", VerslagType.MAMMA_PA_FOLLOW_UP));
		openstaandeFollowUpConclusieDossier.createAlias("screeningRonde.followUpRadiologieVerslagen", "followUpRadiologieVerslag", JoinType.LEFT_OUTER_JOIN);

		openstaandeFollowUpConclusieDossier.add(Restrictions.eq("dossier.id", dossier.getId()));

		DetachedCriteria screeningRondesMetTeVerwachtenPaVerslag = createScreeningRondesMetTeVerwachtenPaVerslagCriteria(dossier.getId());
		openstaandeFollowUpConclusieDossier.add(Restrictions.or(
			Restrictions.and(
				Restrictions.isNotNull("followUpVerslag.id"),
				Restrictions.eq("followUpVerslag.status", VerslagStatus.AFGEROND),
				Restrictions.or(
					Restrictions.isNull("screeningRonde.followUpConclusieStatusGewijzigdOp"),
					Restrictions.gtProperty("followUpVerslag.datumVerwerkt", "screeningRonde.followUpConclusieStatusGewijzigdOp"))),
			Restrictions.and(
				Restrictions.isNotNull("followUpRadiologieVerslag.id"),
				Restrictions.isNotNull("followUpRadiologieVerslag.ingevoerdDoor"),
				Restrictions.isNull("screeningRonde.followUpConclusieStatus"),
				Subqueries.propertyNotIn("screeningRonde.id", screeningRondesMetTeVerwachtenPaVerslag))));

		openstaandeFollowUpConclusieDossier.setProjection(Projections.rowCount());

		return ((Long) openstaandeFollowUpConclusieDossier.uniqueResult()) > 0;
	}

	private Criteria createInstellingenMetOpenstaandePaVerslagenCriteria(ScreeningOrganisatie regio)
	{
		Criteria criteria = addOpenstaandePaVerslagenCriteria();
		if (regio != null)
		{
			criteria.createAlias("instelling.parent", "pInstelling", JoinType.LEFT_OUTER_JOIN);
			criteria.add(Restrictions.or(Restrictions.eq("instelling.parent", regio), Restrictions.eq("pInstelling.parent", regio)));
		}

		criteria.setProjection(Projections.projectionList()
			.add(Projections.groupProperty("instelling.id").as("instellingId"))
			.add(Projections.groupProperty("instelling.naam").as("instellingNaam"))
			.add(Projections.min("laatstGebeldOverPaVerslag").as("laatstGebeld"))
			.add(Projections.groupProperty("instelling.telefoon").as("telefoon"))
			.add(Projections.groupProperty("instelling.telefoon2").as("telefoon2")));

		return criteria;
	}

	private Criteria createOpenstaandePaVerslagenCriteria(Instelling instelling)
	{
		Criteria criteria = addOpenstaandePaVerslagenCriteria();
		criteria.add(Restrictions.eq("instelling.id", instelling.getId()));

		return criteria;
	}

	private Criteria addOpenstaandePaVerslagenCriteria()
	{
		Criteria criteria = getSession().createCriteria(MammaFollowUpRadiologieVerslag.class);

		DetachedCriteria screeningRondesMetAfgerondPaVerslag = getScreeningRondesMetAfgerondPaVerslagCriteria();

		criteria.createAlias("aangemaaktIn", "instelling");
		criteria.createAlias("screeningRonde", "radVerslagRonde");
		criteria.createAlias("radVerslagRonde.dossier", "dossier");
		criteria.createAlias("dossier.laatsteBeoordelingMetUitslag", "beoordeling");
		criteria.createAlias("beoordeling.onderzoek", "onderzoek");
		criteria.createAlias("onderzoek.afspraak", "afspraak");
		criteria.createAlias("afspraak.uitnodiging", "uitnodiging");
		criteria.add(Restrictions.eqProperty("uitnodiging.screeningRonde", "radVerslagRonde.id"));
		criteria.add(Restrictions.isNull("radVerslagRonde.followUpConclusieStatus"));
		criteria.add(Restrictions.eq("pathologieUitgevoerd", true));
		criteria.add(Subqueries.notExists(screeningRondesMetAfgerondPaVerslag));
		criteria.add(Restrictions.isNull("paVerslagNietTeVerwachten"));

		return criteria;
	}

	private DetachedCriteria getScreeningRondesMetAfgerondPaVerslagCriteria()
	{
		DetachedCriteria screeningRondesMetAfgerondPaVerslag = DetachedCriteria.forClass(MammaFollowUpVerslag.class, "paVerslag");
		screeningRondesMetAfgerondPaVerslag.add(Restrictions.eqProperty("paVerslag.screeningRonde", "radVerslagRonde.id"));
		screeningRondesMetAfgerondPaVerslag.add(Restrictions.eq("paVerslag.status", VerslagStatus.AFGEROND));
		screeningRondesMetAfgerondPaVerslag.add(Restrictions.eq("paVerslag.type", VerslagType.MAMMA_PA_FOLLOW_UP));
		screeningRondesMetAfgerondPaVerslag.setProjection(Projections.property("paVerslag.id"));

		return screeningRondesMetAfgerondPaVerslag;
	}

	private DetachedCriteria createScreeningRondesMetTeVerwachtenPaVerslagCriteria(Long dossierId)
	{
		DetachedCriteria screeningRondesMetTeVerwachtenPaVerslag = DetachedCriteria.forClass(MammaFollowUpRadiologieVerslag.class);
		screeningRondesMetTeVerwachtenPaVerslag.createAlias("screeningRonde", "screeningRonde");
		screeningRondesMetTeVerwachtenPaVerslag.createAlias("screeningRonde.dossier", "dossier");

		screeningRondesMetTeVerwachtenPaVerslag.add(Restrictions.eq("dossier.id", dossierId));
		screeningRondesMetTeVerwachtenPaVerslag.add(Restrictions.eq("pathologieUitgevoerd", true));
		screeningRondesMetTeVerwachtenPaVerslag.add(Restrictions.isNull("paVerslagNietTeVerwachten"));
		screeningRondesMetTeVerwachtenPaVerslag.setProjection(Projections.property("screeningRonde.id"));
		return screeningRondesMetTeVerwachtenPaVerslag;
	}
}
