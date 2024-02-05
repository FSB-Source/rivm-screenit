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
import java.util.Arrays;
import java.util.EnumSet;
import java.util.List;
import java.util.stream.Collectors;

import nl.rivm.screenit.PreferenceKey;
import nl.rivm.screenit.main.dao.mamma.MammaBeoordelingDao;
import nl.rivm.screenit.main.model.mamma.beoordeling.MammaBaseWerklijstZoekObject;
import nl.rivm.screenit.main.model.mamma.beoordeling.MammaBeWerklijstZoekObject;
import nl.rivm.screenit.main.model.mamma.beoordeling.MammaCeWerklijstZoekObject;
import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.berichten.enums.VerslagType;
import nl.rivm.screenit.model.enums.BriefType;
import nl.rivm.screenit.model.mamma.MammaAfspraak;
import nl.rivm.screenit.model.mamma.MammaBeoordeling;
import nl.rivm.screenit.model.mamma.MammaFollowUpRadiologieVerslag;
import nl.rivm.screenit.model.mamma.MammaFollowUpVerslag;
import nl.rivm.screenit.model.mamma.MammaOnderzoek;
import nl.rivm.screenit.model.mamma.MammaScreeningsEenheid;
import nl.rivm.screenit.model.mamma.enums.MammaBIRADSWaarde;
import nl.rivm.screenit.model.mamma.enums.MammaBeoordelingOpschortenReden;
import nl.rivm.screenit.model.mamma.enums.MammaBeoordelingStatus;
import nl.rivm.screenit.model.mamma.enums.MammaMammografieIlmStatus;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.util.DateUtil;
import nl.rivm.screenit.util.query.DateRestrictions;
import nl.rivm.screenit.util.query.SmartSQLProjection;
import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject;
import nl.topicuszorg.hibernate.spring.dao.impl.AbstractAutowiredDao;
import nl.topicuszorg.preferencemodule.service.SimplePreferenceService;

import org.apache.commons.lang.StringUtils;
import org.apache.shiro.util.CollectionUtils;
import org.hibernate.Criteria;
import org.hibernate.criterion.Criterion;
import org.hibernate.criterion.DetachedCriteria;
import org.hibernate.criterion.Disjunction;
import org.hibernate.criterion.Order;
import org.hibernate.criterion.Projection;
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
	public long countCeWerklijstBeoordelingen(MammaCeWerklijstZoekObject zoekObject)
	{
		Criteria criteria = createCriteria(zoekObject);
		addCeWerklijstCriteria(criteria, zoekObject);
		criteria.setProjection(Projections.rowCount());
		return (Long) criteria.uniqueResult();
	}

	@Override
	public long countBeWerklijstBeoordelingen(MammaBeWerklijstZoekObject zoekObject)
	{
		Criteria crit = createCriteria(zoekObject);
		addBeWerklijstCriteria(crit, zoekObject);

		crit.setProjection(Projections.rowCount());
		return (Long) crit.uniqueResult();
	}

	@Override
	public List<MammaScreeningsEenheid> screeningsEenhedenMetBeWerklijstBeoordeling(MammaBeWerklijstZoekObject zoekObject)
	{
		Criteria crit = createCriteria(zoekObject);
		addBeWerklijstCriteria(crit, zoekObject);
		crit.setProjection(Projections.distinct(Projections.property("screeningsEenheid")));
		return crit.list();
	}

	@Override
	public List<MammaScreeningsEenheid> screeningsEenhedenMetCeWerklijstBeoordeling(MammaCeWerklijstZoekObject zoekObject)
	{
		Criteria crit = createCriteria(zoekObject);
		addCeWerklijstCriteria(crit, zoekObject);
		crit.setProjection(Projections.distinct(Projections.property("screeningsEenheid")));
		return crit.list();
	}

	@Override
	public List<MammaBeoordeling> zoekBeBeoordelingen(MammaBeWerklijstZoekObject zoekObject, int first, int count, String sortProperty, boolean asc)
	{
		Criteria criteria = createCriteria(zoekObject);
		addBeWerklijstCriteria(criteria, zoekObject);
		criteria.addOrder(Order.desc("verwijzend"));
		criteria.setProjection(Projections.projectionList()
			.add(Projections.property("laatsteBeoordeling"))
			.add(getVerwijzendProjection()));
		return baseZoekBeoordelingen(criteria, first, count, sortProperty, asc)
			.setResultTransformer(new MammaBeoordelingResultTransformer())
			.list();
	}

	private Projection getVerwijzendProjection()
	{
		String verwijzendeBiradsString = MammaBIRADSWaarde.getVerwijzendBIRADSWaarden().stream().map(birad -> "'" + birad + "'").collect(Collectors.joining(", "));
		return Projections.alias(
			new SmartSQLProjection(
				String.format("CASE " +
						"WHEN {beoordeling}.status = 'EERSTE_LEZING_OPGESLAGEN' " +
						"AND ({eersteLezing}.birads_rechts IN (%s) " +
						"OR {eersteLezing}.birads_links IN (%s)) THEN 1 " +
						"WHEN {beoordeling}.status = 'TWEEDE_LEZING_OPGESLAGEN' " +
						"AND ({tweedeLezing}.birads_rechts IN (%s) " +
						"OR {tweedeLezing}.birads_links IN (%s)) THEN 1 " +
						"ELSE 0 " +
						"END as verwijzend",
					verwijzendeBiradsString, verwijzendeBiradsString, verwijzendeBiradsString, verwijzendeBiradsString),
				new String[] {
					"verwijzend"
				},
				new org.hibernate.type.IntegerType[] {
					new org.hibernate.type.IntegerType()
				}),
			"verwijzend");
	}

	@Override
	public List<MammaBeoordeling> zoekCeBeoordelingen(MammaCeWerklijstZoekObject zoekObject, int first, int count, String sortProperty, boolean asc)
	{
		Criteria criteria = createCriteria(zoekObject);
		addCeWerklijstCriteria(criteria, zoekObject);
		criteria.setProjection(Projections.projectionList()
			.add(Projections.property("laatsteBeoordeling")));
		return baseZoekBeoordelingen(criteria, first, count, sortProperty, asc).list();
	}

	@Override
	@SuppressWarnings("unchecked")
	public List<Long> zoekBeoordelingenNummers(MammaBeWerklijstZoekObject zoekObject, String sortProperty, boolean asc)
	{
		Criteria crit = createCriteria(zoekObject);
		addBeWerklijstCriteria(crit, zoekObject);
		crit.addOrder(Order.desc("verwijzend"));
		crit.setProjection(Projections.projectionList()
			.add(Projections.property("laatsteBeoordeling"))
			.add(getVerwijzendProjection()));
		baseZoekBeoordelingenSortOrder(crit, sortProperty, asc);
		return ((List<AbstractHibernateObject>) crit.setResultTransformer(new MammaBeoordelingResultTransformer()).list())
			.stream()
			.map(AbstractHibernateObject::getId)
			.collect(Collectors.toList());
	}

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
	public int getAantalBeoordeeldInList(List<Long> beoordelingenIds)
	{
		Criteria criteria = createOnderzoekCriteria();
		criteria.createAlias("laatsteBeoordeling", "beoordeling");
		criteria.add(Restrictions.in("beoordeling.id", beoordelingenIds));
		criteria.add(
			Restrictions.or(
				Restrictions.eq("beoordeling.status", MammaBeoordelingStatus.EERSTE_LEZING_OPGESLAGEN),
				Restrictions.eq("beoordeling.status", MammaBeoordelingStatus.TWEEDE_LEZING_OPGESLAGEN)));

		criteria.setProjection(Projections.rowCount());
		return ((Long) criteria.uniqueResult()).intValue();
	}

	@Override
	public int getAantalBeoordeeld(MammaBeWerklijstZoekObject zoekObject)
	{
		Criteria criteria = createCriteria(zoekObject);
		addBeWerklijstCriteria(criteria, zoekObject);
		criteria.add(
			Restrictions.or(
				Restrictions.eq("beoordeling.status", MammaBeoordelingStatus.EERSTE_LEZING_OPGESLAGEN),
				Restrictions.eq("beoordeling.status", MammaBeoordelingStatus.TWEEDE_LEZING_OPGESLAGEN)));

		criteria.setProjection(Projections.rowCount());
		return ((Long) criteria.uniqueResult()).intValue();
	}

	@Override
	@SuppressWarnings("unchecked")
	public List<MammaBeoordeling> zoekGeenBeoordelingMogelijk(MammaCeWerklijstZoekObject zoekObject, int first, int count, String sortProperty, boolean ascending)
	{
		return baseZoekBeoordelingen(createGeenBeoordelingMogelijkCriteria(zoekObject), first, count, sortProperty, ascending).list();
	}

	@Override
	public long countGeenBeoordelingMogelijk(MammaCeWerklijstZoekObject zoekObject)
	{
		Criteria criteria = createGeenBeoordelingMogelijkCriteria(zoekObject);
		criteria.setProjection(Projections.rowCount());
		return (Long) criteria.uniqueResult();
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

	private Criteria createGeenBeoordelingMogelijkCriteria(MammaCeWerklijstZoekObject zoekObject)
	{
		Criteria criteria = createCriteria(zoekObject);
		addCeWerklijstCriteria(criteria, zoekObject);
		final List<MammaBeoordelingStatus> statussen = zoekObject.getBeoordelingStatussen();
		if (statussen != null && !statussen.isEmpty())
		{
			criteria.add(Restrictions.in("beoordeling.status", statussen));
		}
		criteria.setProjection(Projections.property("laatsteBeoordeling"));
		return criteria;
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

	private void addBeWerklijstCriteria(Criteria criteria, MammaBeWerklijstZoekObject zoekObject)
	{
		if (zoekObject.getBeoordelingsEenheid() != null)
		{
			criteria.add(Restrictions.eq("beoordeling.beoordelingsEenheid", zoekObject.getBeoordelingsEenheid()));
		}
		if (zoekObject.getBeoordelingStatussen().contains(MammaBeoordelingStatus.VERSLAG_MAKEN)
			|| zoekObject.getBeoordelingStatussen().contains(MammaBeoordelingStatus.VERSLAG_GEREED)
			|| zoekObject.getBeoordelingStatussen().contains(MammaBeoordelingStatus.VERSLAG_AFGEKEURD))
		{
			criteria.createAlias("beoordeling.verslagLezing", "verslagLezing", JoinType.LEFT_OUTER_JOIN);
			criteria.createAlias("beoordeling.eersteLezing", "eersteLezing", JoinType.LEFT_OUTER_JOIN);
			criteria.createAlias("beoordeling.tweedeLezing", "tweedeLezing", JoinType.LEFT_OUTER_JOIN);
			criteria.createAlias("beoordeling.discrepantieLezing", "discrepantieLezing", JoinType.LEFT_OUTER_JOIN);
			criteria.createAlias("beoordeling.arbitrageLezing", "arbitrageLezing", JoinType.LEFT_OUTER_JOIN);

			InstellingGebruiker gebruiker = zoekObject.getInstellingGebruiker();

			Criterion verslagGereedSituatie = Restrictions.and(
				Restrictions.eq("beoordeling.status", MammaBeoordelingStatus.VERSLAG_GEREED),
				Restrictions.eq("verslagLezing.beoordelaar", gebruiker),
				Restrictions.isNull("beoordeling.toegewezenGebruiker"));

			Criterion verslagToegewezenSituatie = Restrictions.and(
				Restrictions.in("beoordeling.status",
					EnumSet.of(MammaBeoordelingStatus.VERSLAG_GEREED, MammaBeoordelingStatus.VERSLAG_AFGEKEURD, MammaBeoordelingStatus.VERSLAG_MAKEN)),
				Restrictions.eq("beoordeling.toegewezenGebruiker", gebruiker));

			Criterion verslagMakenSituatie = Restrictions.and(
				Restrictions.eq("beoordeling.status", MammaBeoordelingStatus.VERSLAG_MAKEN),
				Restrictions.isNull("beoordeling.toegewezenGebruiker"),
				magVerslagMakenRestrictions(gebruiker));

			Criterion verslagAfgekeurdSituatie = Restrictions.and(
				Restrictions.eq("beoordeling.status", MammaBeoordelingStatus.VERSLAG_AFGEKEURD),
				Restrictions.eq("verslagLezing.beoordelaar", gebruiker),
				Restrictions.isNull("beoordeling.toegewezenGebruiker"));

			criteria.add(Restrictions.or(verslagGereedSituatie, verslagToegewezenSituatie, verslagAfgekeurdSituatie, verslagMakenSituatie));
		}
		else if (zoekObject.getBeoordelingStatussen().contains(MammaBeoordelingStatus.ARBITRAGE))
		{
			criteria.createAlias("beoordeling.eersteLezing", "eersteLezing", JoinType.LEFT_OUTER_JOIN);
			criteria.add(Restrictions.ne("eersteLezing.beoordelaar", zoekObject.getInstellingGebruiker()));

			criteria.createAlias("beoordeling.tweedeLezing", "tweedeLezing", JoinType.LEFT_OUTER_JOIN);
			criteria.add(Restrictions.ne("tweedeLezing.beoordelaar", zoekObject.getInstellingGebruiker()));
		}
		else if (zoekObject.getBeoordelingStatussen().contains(MammaBeoordelingStatus.DISCREPANTIE))
		{
			criteria.createAlias("beoordeling.eersteLezing", "eersteLezing");
			criteria.createAlias("beoordeling.tweedeLezing", "tweedeLezing");

			criteria.add(
				Restrictions.or(
					Restrictions.eq("eersteLezing.beoordelaar", zoekObject.getInstellingGebruiker()),
					Restrictions.eq("tweedeLezing.beoordelaar", zoekObject.getInstellingGebruiker())));
		}
		else
		{
			criteria.add(Restrictions.eq("mammografie.ilmStatus", MammaMammografieIlmStatus.BESCHIKBAAR));
			criteria.add(Restrictions.eq("isDoorgevoerd", true));
			criteria.createAlias("beoordeling.eersteLezing", "eersteLezing", JoinType.LEFT_OUTER_JOIN);
			criteria.createAlias("beoordeling.tweedeLezing", "tweedeLezing", JoinType.LEFT_OUTER_JOIN);

			criteria.add(
				Restrictions.or(

					Restrictions.and(
						Restrictions.in("beoordeling.status", Arrays.asList(MammaBeoordelingStatus.EERSTE_LEZING, MammaBeoordelingStatus.EERSTE_LEZING_OPGESLAGEN)),
						Restrictions.or(
							Restrictions.isNull("eersteLezing.id"),
							Restrictions.eq("eersteLezing.beoordelaar", zoekObject.getInstellingGebruiker()))),

					Restrictions.and(
						Restrictions.in("beoordeling.status", Arrays.asList(MammaBeoordelingStatus.TWEEDE_LEZING, MammaBeoordelingStatus.TWEEDE_LEZING_OPGESLAGEN)),
						Restrictions.ne("eersteLezing.beoordelaar", zoekObject.getInstellingGebruiker()),
						Restrictions.or(
							Restrictions.isNull("tweedeLezing.id"),
							Restrictions.eq("tweedeLezing.beoordelaar", zoekObject.getInstellingGebruiker())))));
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
		criteria.createAlias("mammografie", "mammografie", JoinType.LEFT_OUTER_JOIN);
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
				addCritBijGunstigeUitslagAlleenMetNietAfgedrukteBrieven(criteria, beoordelingStatussen);
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

	private void addCritBijGunstigeUitslagAlleenMetNietAfgedrukteBrieven(Criteria criteria, List<MammaBeoordelingStatus> beoordelingStatussen)
	{
		beoordelingStatussen.remove(MammaBeoordelingStatus.UITSLAG_ONGUNSTIG);
		Disjunction or = Restrictions.disjunction();
		if (!CollectionUtils.isEmpty(beoordelingStatussen))
		{
			or.add(Restrictions.in("beoordeling.status", beoordelingStatussen));
		}
		criteria.createAlias("ronde.brieven", "brief");
		or.add(
			Restrictions.and(Restrictions.in("brief.briefType", BriefType.getMammaOngunstigeUitslagBriefTypen()),
				Restrictions.eq("brief.gegenereerd", false),
				Restrictions.eq("beoordeling.status", MammaBeoordelingStatus.UITSLAG_ONGUNSTIG)));
		criteria.add(or);
	}

	private Criterion magVerslagMakenRestrictions(InstellingGebruiker gebruiker)
	{
		return Restrictions.or(
			Restrictions.and(
				Restrictions.isNotNull("arbitrageLezing.id"),
				Restrictions.or(
					Restrictions.and(
						Restrictions.or(
							Restrictions.in("eersteLezing.biradsLinks", MammaBIRADSWaarde.getVerwijzendBIRADSWaarden()),
							Restrictions.in("eersteLezing.biradsRechts", MammaBIRADSWaarde.getVerwijzendBIRADSWaarden())),
						Restrictions.eq("eersteLezing.beoordelaar", gebruiker)),
					Restrictions.and(
						Restrictions.or(
							Restrictions.in("tweedeLezing.biradsLinks", MammaBIRADSWaarde.getVerwijzendBIRADSWaarden()),
							Restrictions.in("tweedeLezing.biradsRechts", MammaBIRADSWaarde.getVerwijzendBIRADSWaarden())),
						Restrictions.eq("tweedeLezing.beoordelaar", gebruiker)),
					Restrictions.eq("arbitrageLezing.beoordelaar", gebruiker))),
			Restrictions.and(
				Restrictions.isNull("arbitrageLezing.id"),
				Restrictions.or(
					Restrictions.eq("eersteLezing.beoordelaar", gebruiker),
					Restrictions.eq("tweedeLezing.beoordelaar", gebruiker))));
	}

	@Override
	public List<MammaBeoordeling> zoekCeWerklijstProcesmonitoringBeoordelingen(MammaCeWerklijstZoekObject zoekObject, int first, int count, String sortProperty, boolean ascending)
	{
		Criteria criteria = createCriteria(zoekObject);
		addCeWerklijstCriteria(criteria, zoekObject);
		addMonitoringCriteria(criteria);
		baseZoekBeoordelingen(criteria, first, count, sortProperty, ascending);
		criteria.setProjection(Projections.property("laatsteBeoordeling"));
		return criteria.list();
	}

	private void addMonitoringCriteria(Criteria criteria)
	{
		criteria.add(DateRestrictions.lt("beoordeling.statusDatum", DateUtil.toUtilDate(DateUtil.minusWerkdagen(currentDateSupplier.getLocalDate(), 2))));
		criteria.add(Restrictions.isNotNull("onderzoek.mammografie"));
	}

	@Override
	public long countCeWerklijstProcesmonitoringBeoordelingen(MammaCeWerklijstZoekObject zoekObject)
	{
		Criteria criteria = createCriteria(zoekObject);
		addCeWerklijstCriteria(criteria, zoekObject);
		addMonitoringCriteria(criteria);
		criteria.setProjection(Projections.rowCount());
		return (Long) criteria.uniqueResult();
	}

}
