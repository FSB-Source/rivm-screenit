package nl.rivm.screenit.specification.algemeen;

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

import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.function.Function;

import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.From;
import javax.persistence.criteria.Join;
import javax.persistence.criteria.JoinType;
import javax.persistence.criteria.Path;
import javax.persistence.criteria.Predicate;
import javax.persistence.criteria.Root;

import lombok.AccessLevel;
import lombok.AllArgsConstructor;

import nl.rivm.screenit.model.BMHKLaboratorium;
import nl.rivm.screenit.model.BeoordelingsEenheid;
import nl.rivm.screenit.model.CentraleEenheid;
import nl.rivm.screenit.model.CentraleEenheid_;
import nl.rivm.screenit.model.Gebruiker;
import nl.rivm.screenit.model.Instelling;
import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.InstellingGebruiker_;
import nl.rivm.screenit.model.Instelling_;
import nl.rivm.screenit.model.Mammapoli;
import nl.rivm.screenit.model.OrganisatieType;
import nl.rivm.screenit.model.RadiologieAfdeling;
import nl.rivm.screenit.model.Rivm;
import nl.rivm.screenit.model.ScreeningOrganisatie;
import nl.rivm.screenit.model.SingleTableHibernateObject_;
import nl.rivm.screenit.model.Woonplaats;
import nl.rivm.screenit.model.ZorgInstelling;
import nl.rivm.screenit.model.ZorgInstelling_;
import nl.rivm.screenit.model.cervix.CervixHuisarts;
import nl.rivm.screenit.model.cervix.CervixHuisartsAdres;
import nl.rivm.screenit.model.cervix.CervixHuisartsAdres_;
import nl.rivm.screenit.model.cervix.CervixHuisarts_;
import nl.rivm.screenit.model.colon.ColonIntakelocatie;
import nl.rivm.screenit.model.colon.ColonIntakelocatie_;
import nl.rivm.screenit.model.colon.ColoscopieLocatie;
import nl.rivm.screenit.model.colon.ColoscopieLocatie_;
import nl.rivm.screenit.model.colon.IFobtLaboratorium;
import nl.rivm.screenit.model.colon.PaLaboratorium;
import nl.rivm.screenit.model.colon.PaLaboratorium_;
import nl.rivm.screenit.specification.ExtendedSpecification;
import nl.topicuszorg.organisatie.model.Adres;

import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.collections.MapUtils;
import org.apache.commons.lang.StringUtils;
import org.jetbrains.annotations.NotNull;
import org.springframework.data.jpa.domain.Specification;

import static nl.rivm.screenit.specification.SpecificationUtil.composePredicates;
import static nl.rivm.screenit.specification.SpecificationUtil.composePredicatesOr;
import static nl.rivm.screenit.specification.SpecificationUtil.containsCaseInsensitive;
import static nl.rivm.screenit.specification.SpecificationUtil.exactCaseInsensitive;
import static nl.rivm.screenit.specification.SpecificationUtil.join;
import static nl.rivm.screenit.specification.SpecificationUtil.skipWhenEmpty;
import static nl.rivm.screenit.specification.SpecificationUtil.skipWhenEmptyExtended;
import static nl.rivm.screenit.specification.SpecificationUtil.skipWhenNullExtended;
import static nl.rivm.screenit.specification.SpecificationUtil.treat;
import static nl.rivm.screenit.specification.algemeen.AdresSpecification.filterPostcodeContaining;
import static nl.rivm.screenit.specification.algemeen.MedewerkerSpecification.filterAchternaamContaining;
import static nl.rivm.screenit.specification.algemeen.MedewerkerSpecification.filterUzinummerContaining;

@AllArgsConstructor(access = AccessLevel.PRIVATE)
public class OrganisatieSpecification
{

	public static ExtendedSpecification<Instelling> filterActief(Boolean actief)
	{
		return skipWhenNullExtended(actief, (r, q, cb) -> cb.equal(r.get(Instelling_.actief), actief));
	}

	public static ExtendedSpecification<Instelling> filterNaamContaining(String naam)
	{
		return skipWhenEmptyExtended(naam, (r, q, cb) -> containsCaseInsensitive(cb, r.get(Instelling_.naam), naam));
	}

	public static ExtendedSpecification<Instelling> filterEmailsDeelsExactEnDeelsContaining(String email)
	{
		return skipWhenEmptyExtended(email, (r, q, cb) ->
		{
			var ceRoot = treat(r, CentraleEenheid.class, cb);
			return cb.or(
				exactCaseInsensitive(cb, r.get(Instelling_.email), email),
				exactCaseInsensitive(cb, ceRoot.get(CentraleEenheid_.email2), email),
				exactCaseInsensitive(cb, ceRoot.get(CentraleEenheid_.email3), email),
				exactCaseInsensitive(cb, ceRoot.get(CentraleEenheid_.email4), email),
				exactCaseInsensitive(cb, treat(r, ColonIntakelocatie.class, cb).get(ColonIntakelocatie_.emailSignaleringIntakelocatie), email),
				containsCaseInsensitive(cb, treat(r, CervixHuisarts.class, cb).get(CervixHuisarts_.extraEmails), email));
		});
	}

	public static ExtendedSpecification<Instelling> filterId(Long id)
	{
		return skipWhenNullExtended(id, (r, q, cb) -> cb.equal(r.get(SingleTableHibernateObject_.id), id));
	}

	public static Specification<Instelling> heeftNietOrganisatieTypes(List<OrganisatieType> excludeOrganisatieTypes)
	{
		return skipWhenEmpty(excludeOrganisatieTypes, (r, q, cb) -> cb.not(r.get(Instelling_.organisatieType).in(excludeOrganisatieTypes)));
	}

	public static Specification<Instelling> filterUniekeCodeContaining(String uniekeCode)
	{
		return skipWhenEmpty(uniekeCode, (r, q, cb) -> cb.or(
			containsCaseInsensitive(cb, r.get(Instelling_.uziAbonneenummer), uniekeCode),
			containsCaseInsensitive(cb, r.get(Instelling_.agbcode), uniekeCode),
			containsCaseInsensitive(cb, r.get(Instelling_.rootOid), uniekeCode)
		));
	}

	public static Specification<Instelling> heeftFqdn(String fqdn)
	{
		return (r, q, cb) -> cb.or(cb.equal(treat(r, ZorgInstelling.class, cb).get(ZorgInstelling_.fqdn), fqdn),
			cb.equal(treat(r, ZorgInstelling.class, cb).get(ZorgInstelling_.fqdn), fqdn), cb.equal(treat(r, PaLaboratorium.class, cb).get(PaLaboratorium_.fqdn), fqdn),
			cb.equal(treat(r, ColoscopieLocatie.class, cb).get(ColoscopieLocatie_.fqdn), fqdn));
	}

	public static Specification<Instelling> getZoekOrganisatiesSpecification(Instelling organisatie, Map<OrganisatieType, List<Instelling>> hierarchieCriteria,
		List<OrganisatieType> excludeOrganisatieTypes, LocalDateTime peilMoment)
	{
		return (r, q, cb) ->
		{
			var predicates = new ArrayList<Predicate>();
			predicates.add(forceerDtypesVoorAlleMogelijkeOrganisationTypes(r, cb));
			predicates.add(addHierachieCriteria(hierarchieCriteria, r, cb));

			var spec = filterActief(organisatie.getActief())
				.and(filterNaamContaining(organisatie.getNaam()))
				.and(filterEmailsDeelsExactEnDeelsContaining(organisatie.getEmail()))
				.and(filterId(organisatie.getId()))
				.and(heeftNietOrganisatieTypes(excludeOrganisatieTypes))
				.and(filterUniekeCodeContaining(organisatie.getUziAbonneenummer()))
				.and(getMedewerkerSpecifications(organisatie, peilMoment))
				.and(getAdresSpecifications(organisatie, cb));

			predicates.add(spec.toPredicate(r, q, cb));

			return composePredicates(cb, predicates);
		};
	}

	private static Predicate addHierachieCriteria(Map<OrganisatieType, List<Instelling>> hierarchieCriteria, Root<Instelling> r, CriteriaBuilder cb)
	{
		if (MapUtils.isNotEmpty(hierarchieCriteria))
		{
			var disjunctionPredicates = new ArrayList<Predicate>();
			hierarchieCriteria.entrySet().forEach(type -> disjunctionPredicates.add(addHierarchieCrit(type, r, cb)));
			return composePredicatesOr(cb, disjunctionPredicates);
		}
		return null;
	}

	private static Specification<Instelling> getMedewerkerSpecifications(Instelling organisatie, LocalDateTime peilMoment)
	{
		if (CollectionUtils.isNotEmpty(organisatie.getOrganisatieMedewerkers()))
		{
			var medewerker = organisatie.getOrganisatieMedewerkers().get(0).getMedewerker();
			if (StringUtils.isNotBlank(medewerker.getAchternaam()) || StringUtils.isNotBlank(medewerker.getUzinummer()))
			{
				return filterAchternaamContaining(medewerker.getAchternaam())
					.and(filterUzinummerContaining(medewerker.getUzinummer())
						.and(MedewerkerSpecification.isActiefEnActiefOpMoment(peilMoment))).with(medewerkerJoin())
					.and(OrganisatieMedewerkerSpecification.isActief().with(organisatieMedewerkerJoin()));
			}
		}
		return null;
	}

	private static Specification<Instelling> getAdresSpecifications(Instelling organisatie, CriteriaBuilder cb)
	{
		var adres = organisatie.getHuidigAdres();
		Specification<Instelling> spec = null;
		if (adres != null)
		{
			if (adres.getPlaats() != null)
			{
				spec = AdresSpecification.filterPlaatsContaining(adres.getPlaats()).with(adresJoin())
					.or(WoonplaatsSpecification.filterPlaatsContaining(adres.getPlaats()).with(woonplaatsJoin(cb)));
			}
			if (adres.getPostcode() != null)
			{
				var postcode = StringUtils.deleteWhitespace(adres.getPostcode());
				var postcodeFilter = filterPostcodeContaining(postcode);
				spec = postcodeFilter.with(adresJoin()).or(postcodeFilter.with(ri -> postadresJoin(cb, ri))).and(spec);
			}
		}
		return spec;
	}

	private static Predicate forceerDtypesVoorAlleMogelijkeOrganisationTypes(Root<Instelling> r, CriteriaBuilder cb)
	{
		var organisatiePredicates = new ArrayList<Predicate>();
		var mogelijkeOrganisatieClasses = List.of(Instelling.class, BMHKLaboratorium.class, BeoordelingsEenheid.class, CentraleEenheid.class, CervixHuisarts.class,
			ColonIntakelocatie.class, ColoscopieLocatie.class, IFobtLaboratorium.class, Mammapoli.class, PaLaboratorium.class, RadiologieAfdeling.class, Rivm.class,
			ScreeningOrganisatie.class, ZorgInstelling.class);
		mogelijkeOrganisatieClasses.forEach(c -> organisatiePredicates.add(cb.equal(treat(r, c, cb).type(), c)));
		return composePredicatesOr(cb, organisatiePredicates);
	}

	public static Predicate addHierarchieCrit(Map.Entry<OrganisatieType, List<Instelling>> type, Root<Instelling> root, CriteriaBuilder cb)
	{
		var predicates = new ArrayList<Predicate>();
		var organisatieType = type.getKey();
		var organisatiesVoorToegangslevel = type.getValue();
		predicates.add(cb.equal(root.get(Instelling_.organisatieType), organisatieType));
		if (CollectionUtils.isNotEmpty(organisatiesVoorToegangslevel))
		{
			var organisatieTypeToegangslevel = organisatiesVoorToegangslevel.get(0).getOrganisatieType();
			var idPath = root.get(SingleTableHibernateObject_.id);
			var isScreeningorganisatieToegangslevel = organisatieTypeToegangslevel == OrganisatieType.SCREENINGSORGANISATIE;
			switch (organisatieType)
			{
			case SCREENINGSORGANISATIE:
			case INPAKCENTRUM:
			case RIVM:
			case LABORATORIUM:
			case BMHK_LABORATORIUM:
			case HUISARTS:
				predicates.add(createCriteriaOrganisaties(idPath, organisatiesVoorToegangslevel, cb));
				break;
			case PA_LABORATORIUM:
				if (isScreeningorganisatieToegangslevel)
				{
					predicates.add(createCriteriaOrganisaties(
						parentJoin(parentJoin(join(treat(root, PaLaboratorium.class, cb), PaLaboratorium_.coloscopielocaties, JoinType.LEFT))), organisatiesVoorToegangslevel, cb));
				}
				else
				{
					predicates.add(createCriteriaOrganisaties(idPath, organisatiesVoorToegangslevel, cb));
				}
				break;
			case INTAKELOCATIE:
			case COLOSCOPIELOCATIE:
			case MAMMAPOLI:
			case RADIOLOGIEAFDELING:
				if (isScreeningorganisatieToegangslevel)
				{
					predicates.add(createCriteriaOrganisaties(parentJoin(parentJoin(root)), organisatiesVoorToegangslevel, cb));
				}
				else
				{
					predicates.add(createCriteriaOrganisaties(idPath, organisatiesVoorToegangslevel, cb));
				}
				break;
			case ZORGINSTELLING:
				if (isScreeningorganisatieToegangslevel)
				{
					predicates.add(createCriteriaOrganisaties(parentJoin(root), organisatiesVoorToegangslevel, cb));
				}
				else
				{
					predicates.add(createCriteriaOrganisaties(idPath, organisatiesVoorToegangslevel, cb));
				}
				break;
			case BEOORDELINGSEENHEID:
				if (isScreeningorganisatieToegangslevel)
				{
					predicates.add(createCriteriaOrganisaties(join(parentJoin(root), Instelling_.regio, JoinType.LEFT), organisatiesVoorToegangslevel, cb));
				}
				break;
			default:
				break;
			}
		}
		return composePredicates(cb, predicates);
	}

	@NotNull
	private static Function<From<?, ? extends Instelling>, From<?, ? extends InstellingGebruiker>> organisatieMedewerkerJoin()
	{
		return r -> join(r, Instelling_.organisatieMedewerkers);
	}

	@NotNull
	private static Function<From<?, ? extends Instelling>, From<?, ? extends Gebruiker>> medewerkerJoin()
	{
		return r ->
		{
			var organisatieMedewerkerJoin = organisatieMedewerkerJoin().apply(r);
			return join(organisatieMedewerkerJoin, InstellingGebruiker_.medewerker);
		};
	}

	@NotNull
	private static Function<From<?, ? extends Instelling>, From<?, ? extends Adres>> adresJoin()
	{
		return r -> join(r, Instelling_.adressen, JoinType.LEFT);
	}

	@NotNull
	private static Join<CervixHuisarts, CervixHuisartsAdres> postadresJoin(CriteriaBuilder cb, From<?, ? extends Instelling> r)
	{
		return join(treat(r, CervixHuisarts.class, cb), CervixHuisarts_.postadres, JoinType.LEFT);
	}

	@NotNull
	private static Function<From<?, ? extends Instelling>, From<?, ? extends Woonplaats>> woonplaatsJoin(CriteriaBuilder cb)
	{
		return r -> join(postadresJoin(cb, r), CervixHuisartsAdres_.woonplaats, JoinType.LEFT);
	}

	@NotNull
	private static Join<? extends Instelling, ? extends Instelling> parentJoin(From<?, ? extends Instelling> root)
	{
		return join(root, Instelling_.parent, JoinType.LEFT);
	}

	private static Predicate createCriteriaOrganisaties(Path<?> path, List<Instelling> organisaties, CriteriaBuilder cb)
	{
		if (organisaties.size() == 1)
		{
			if (path.getJavaType().equals(Long.class))
			{
				return cb.equal(path, organisaties.get(0).getId());
			}
			else
			{
				return cb.equal(path, organisaties.get(0));
			}
		}
		else
		{
			return path.in(organisaties);
		}
	}
}
