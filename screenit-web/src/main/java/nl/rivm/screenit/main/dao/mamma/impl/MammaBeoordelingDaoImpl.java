package nl.rivm.screenit.main.dao.mamma.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
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
import java.util.List;

import nl.rivm.screenit.PreferenceKey;
import nl.rivm.screenit.main.dao.mamma.MammaBeoordelingDao;
import nl.rivm.screenit.main.model.mamma.beoordeling.MammaBaseWerklijstZoekObject;
import nl.rivm.screenit.main.model.mamma.beoordeling.MammaCeWerklijstZoekObject;
import nl.rivm.screenit.model.berichten.enums.VerslagType;
import nl.rivm.screenit.model.enums.BriefType;
import nl.rivm.screenit.model.mamma.MammaAfspraak;
import nl.rivm.screenit.model.mamma.MammaBeoordeling;
import nl.rivm.screenit.model.mamma.MammaFollowUpRadiologieVerslag;
import nl.rivm.screenit.model.mamma.MammaFollowUpVerslag;
import nl.rivm.screenit.model.mamma.MammaOnderzoek;
import nl.rivm.screenit.model.mamma.enums.MammaBeoordelingOpschortenReden;
import nl.rivm.screenit.model.mamma.enums.MammaBeoordelingStatus;
import nl.rivm.screenit.model.mamma.enums.MammaMammografieIlmStatus;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.util.DateUtil;
import nl.topicuszorg.hibernate.spring.dao.impl.AbstractAutowiredDao;
import nl.topicuszorg.preferencemodule.service.SimplePreferenceService;

import org.apache.commons.lang.StringUtils;
import org.apache.shiro.util.CollectionUtils;
import org.hibernate.Criteria;
import org.hibernate.criterion.DetachedCriteria;
import org.hibernate.criterion.Disjunction;
import org.hibernate.criterion.Order;
import org.hibernate.criterion.Projections;
import org.hibernate.criterion.Restrictions;
import org.hibernate.criterion.SimpleExpression;
import org.hibernate.criterion.Subqueries;
import org.hibernate.sql.JoinType;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Repository
@Transactional(propagation = Propagation.SUPPORTS, readOnly = true)
public class MammaBeoordelingDaoImpl extends AbstractAutowiredDao implements MammaBeoordelingDao
{
	@Autowired
	private ICurrentDateSupplier currentDateSupplier;

	@Autowired
	private SimplePreferenceService preferenceService;

	@Override
	public List<MammaBeoordeling> getAlleVorigeBeoordelingenMetBeelden(MammaBeoordeling beoordeling)
	{
		Criteria criteria = getSession().createCriteria(MammaBeoordeling.class, "beoordeling");
		criteria.createAlias("onderzoek", "onderzoek");
		criteria.createAlias("onderzoek.mammografie", "mammografie");
		criteria.createAlias("onderzoek.afspraak", "afspraak");
		criteria.createAlias("afspraak.uitnodiging", "uitnodiging");
		criteria.createAlias("uitnodiging.screeningRonde", "ronde");
		criteria.add(Restrictions.ne("id", beoordeling.getId()));
		criteria.add(Restrictions.eq("opschortReden", MammaBeoordelingOpschortenReden.NIET_OPSCHORTEN));
		criteria.add(Restrictions.eq("ronde.dossier.id", beoordeling.getOnderzoek().getAfspraak().getUitnodiging().getScreeningRonde().getDossier().getId()));
		criteria.add(Restrictions.eq("mammografie.ilmStatus", MammaMammografieIlmStatus.BESCHIKBAAR));
		criteria.addOrder(Order.desc("statusDatum"));
		return criteria.list();
	}

	@Override
	public List<MammaBeoordeling> getVorigeBeoordelingen(MammaBeoordeling beoordeling, int aantal, boolean inclusiefGunstige)
	{
		Criteria criteria = getSession().createCriteria(MammaBeoordeling.class);

		criteria.createAlias("onderzoek", "onderzoek");
		criteria.createAlias("onderzoek.afspraak", "afspraak");
		criteria.createAlias("afspraak.uitnodiging", "uitnodiging");
		criteria.createAlias("uitnodiging.screeningRonde", "ronde");
		criteria.add(Restrictions.ne("id", beoordeling.getId()));

		criteria.add(Restrictions.eq("ronde.dossier.id", beoordeling.getOnderzoek().getAfspraak().getUitnodiging().getScreeningRonde().getDossier().getId()));
		final SimpleExpression verslagGereedRestriction = Restrictions.eq("status", MammaBeoordelingStatus.UITSLAG_ONGUNSTIG);
		if (inclusiefGunstige)
		{
			criteria.add(Restrictions.or(verslagGereedRestriction, Restrictions.eq("status", MammaBeoordelingStatus.UITSLAG_GUNSTIG)));
		}
		else
		{
			criteria.add(verslagGereedRestriction);
		}
		criteria.addOrder(Order.desc("statusDatum"));
		criteria.setMaxResults(aantal);
		return criteria.list();
	}

	@Override
	public List<MammaBeoordeling> zoekFollowUpNietGedownloadBeoordelingen(MammaCeWerklijstZoekObject zoekObject, int first, int count, String sortProperty, boolean ascending)
	{
		Criteria criteria = createCriteria(zoekObject);
		addCeWerklijstCriteria(criteria, zoekObject);
		addFollowUpNietGedownloadCriteria(criteria);
		criteria.setProjection(Projections.property("laatsteBeoordeling"));
		return baseZoekBeoordelingen(criteria, first, count, sortProperty, ascending).list();
	}

	@Override
	public long countFollowUpNietGedownloadBeoordelingen(MammaCeWerklijstZoekObject zoekObject)
	{
		Criteria criteria = createCriteria(zoekObject);
		addCeWerklijstCriteria(criteria, zoekObject);
		addFollowUpNietGedownloadCriteria(criteria);
		criteria.setProjection(Projections.count("laatsteBeoordeling"));
		return (Long) criteria.uniqueResult();
	}

	private void addFollowUpNietGedownloadCriteria(Criteria criteria)
	{

		criteria.add(Restrictions.eqProperty("ronde.laatsteUitnodiging.id", "uitnodiging.id"));
		criteria.add(Restrictions.eqProperty("uitnodiging.laatsteAfspraak.id", "afspraak.id"));

		criteria.add(Restrictions.eq("beoordeling.status", MammaBeoordelingStatus.UITSLAG_ONGUNSTIG));
		criteria.add(Restrictions.le("beoordeling.statusDatum",
			DateUtil.toUtilDate(
				currentDateSupplier.getLocalDate().minusDays(preferenceService.getInteger(PreferenceKey.MAMMA_FOLLOW_UP_NIET_GEDOWNLOAD_WERKLIJST_NA_DAGEN.name(), 42)))));
		criteria.add(Restrictions.isNull("ronde.followUpConclusieStatus"));

		DetachedCriteria afspraakNaScreeningRondeVanBeoordeling = DetachedCriteria.forClass(MammaAfspraak.class, "a");
		afspraakNaScreeningRondeVanBeoordeling.createAlias("a.uitnodiging", "u");
		afspraakNaScreeningRondeVanBeoordeling.createAlias("u.screeningRonde", "r");
		afspraakNaScreeningRondeVanBeoordeling
			.add(Restrictions.and(Restrictions.eqProperty("r.dossier", "dossier.id"), Restrictions.gtProperty("r.creatieDatum", "ronde.creatieDatum")));
		afspraakNaScreeningRondeVanBeoordeling.setProjection(Projections.rowCount());
		criteria.add(Subqueries.eq(0L, afspraakNaScreeningRondeVanBeoordeling));

		DetachedCriteria folluwUpRadiologieVerslagen = DetachedCriteria.forClass(MammaFollowUpRadiologieVerslag.class);
		folluwUpRadiologieVerslagen.setProjection(Projections.property("screeningRonde"));
		criteria.add(Subqueries.propertyNotIn("ronde.id", folluwUpRadiologieVerslagen));

		DetachedCriteria followUpVerslagen = DetachedCriteria.forClass(MammaFollowUpVerslag.class);
		followUpVerslagen.add(Restrictions.eq("type", VerslagType.MAMMA_PA_FOLLOW_UP));
		followUpVerslagen.setProjection(Projections.property("screeningRonde"));
		criteria.add(Subqueries.propertyNotIn("ronde.id", followUpVerslagen));
	}

	private Criteria createOnderzoekCriteria()
	{
		return getSession().createCriteria(MammaOnderzoek.class, "onderzoek");
	}

	private void addCeWerklijstCriteria(Criteria criteria, MammaCeWerklijstZoekObject zoekObject)
	{
		criteria.createAlias("beoordeling.beoordelingsEenheid", "beoordelingsEenheid");
		criteria.createAlias("beoordelingsEenheid.parent", "centraleEenheid");
		criteria.createAlias("beoordeling.verslagLezing", "verslagLezing", JoinType.LEFT_OUTER_JOIN);
		if (!CollectionUtils.isEmpty(zoekObject.getCentraleEenheden()))
		{
			criteria.add(Restrictions.in("beoordelingsEenheid.parent", zoekObject.getCentraleEenheden()));
		}
		if (!CollectionUtils.isEmpty(zoekObject.getBeoordelingsEenheden()))
		{
			criteria.add(Restrictions.in("beoordeling.beoordelingsEenheid", zoekObject.getBeoordelingsEenheden()));
		}
	}

	private Criteria baseZoekBeoordelingen(Criteria crit, int first, int count, String sortProperty, boolean asc)
	{
		baseZoekBeoordelingenSortOrder(crit, sortProperty, asc);

		crit.setFirstResult(Math.max(first, 0));
		if (count > 0)
		{
			crit.setMaxResults(count);
		}
		return crit;
	}

	private void baseZoekBeoordelingenSortOrder(Criteria crit, String sortProperty, boolean asc)
	{
		if (sortProperty != null)
		{
			if (asc)
			{
				crit.addOrder(Order.asc(sortProperty));
			}
			else
			{
				crit.addOrder(Order.desc(sortProperty));
			}
		}

		crit.addOrder(Order.asc("onderzoek.creatieDatum")); 
	}

	private Criteria createCriteria(MammaBaseWerklijstZoekObject zoekObject)
	{
		Criteria criteria = createOnderzoekCriteria();

		criteria.createAlias("laatsteBeoordeling", "beoordeling");
		criteria.createAlias("screeningsEenheid", "se");
		criteria.createAlias("mammografie", "mammografie",
			JoinType.LEFT_OUTER_JOIN); 
		criteria.createAlias("afspraak", "afspraak");
		criteria.createAlias("afspraak.uitnodiging", "uitnodiging");
		criteria.createAlias("uitnodiging.screeningRonde", "ronde");
		criteria.createAlias("ronde.dossier", "dossier");
		criteria.createAlias("dossier.client", "client");
		criteria.createAlias("client.persoon", "persoon");
		criteria.createAlias("persoon.gbaAdres", "adres");
		createBasisInfoBeoordelingCriteria(zoekObject, criteria);
		return criteria;
	}

	private void createBasisInfoBeoordelingCriteria(MammaBaseWerklijstZoekObject zoekObject, Criteria criteria)
	{

		if (zoekObject.getGeboortedatum() != null)
		{
			criteria.add(Restrictions.eq("persoon.geboortedatum", zoekObject.getGeboortedatum()));
		}

		if (StringUtils.isNotBlank(zoekObject.getBsn()))
		{
			criteria.add(Restrictions.eq("persoon.bsn", zoekObject.getBsn()));
		}

		if (StringUtils.isNotBlank(zoekObject.getPostcode()))
		{
			criteria.add(Restrictions.eq("adres.postcode", zoekObject.getPostcode()));
		}

		if (zoekObject.getHuisnummer() != null)
		{
			criteria.add(Restrictions.eq("adres.huisnummer", zoekObject.getHuisnummer()));
		}

		if (!CollectionUtils.isEmpty(zoekObject.getScreeningsEenheden()))
		{
			criteria.add(Restrictions.in("screeningsEenheid", zoekObject.getScreeningsEenheden()));
		}

		List<MammaBeoordelingStatus> beoordelingStatussen = new ArrayList<>(zoekObject.getBeoordelingStatussen());
		if (!CollectionUtils.isEmpty(beoordelingStatussen))
		{
			if (beoordelingStatussen.contains(MammaBeoordelingStatus.UITSLAG_ONGUNSTIG))
			{
				addCritBijOnGunstigeUitslagAlleenMetNietAfgedrukteBrieven(criteria, beoordelingStatussen);
			}
			else
			{
				criteria.add(Restrictions.in("beoordeling.status", beoordelingStatussen));
			}
		}

		if (zoekObject.getOnderzoekType() != null)
		{
			criteria.add(Restrictions.eq("onderzoek.onderzoekType", zoekObject.getOnderzoekType()));
		}
	}

	private void addCritBijOnGunstigeUitslagAlleenMetNietAfgedrukteBrieven(Criteria criteria, List<MammaBeoordelingStatus> beoordelingStatussen)
	{
		beoordelingStatussen.remove(MammaBeoordelingStatus.UITSLAG_ONGUNSTIG);
		Disjunction or = Restrictions.disjunction();
		if (!CollectionUtils.isEmpty(beoordelingStatussen))
		{
			or.add(Restrictions.in("beoordeling.status", beoordelingStatussen));
		}
		criteria.createAlias("ronde.brieven", "brief");
		or.add(
			Restrictions.and(
				Restrictions.in("brief.briefType", BriefType.getMammaOngunstigeUitslagBriefTypen()),
				Restrictions.eq("brief.gegenereerd", false),
				Restrictions.eq("beoordeling.status", MammaBeoordelingStatus.UITSLAG_ONGUNSTIG)));
		criteria.add(or);
	}
}
