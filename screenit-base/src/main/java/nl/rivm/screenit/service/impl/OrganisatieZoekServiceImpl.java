package nl.rivm.screenit.service.impl;

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

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;

import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.Join;
import javax.persistence.criteria.JoinType;
import javax.persistence.criteria.Root;

import nl.rivm.screenit.dao.InstellingDao;
import nl.rivm.screenit.model.CentraleEenheid;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.Instelling;
import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.Instelling_;
import nl.rivm.screenit.model.OrganisatieType;
import nl.rivm.screenit.model.ScreeningOrganisatie;
import nl.rivm.screenit.model.SingleTableHibernateObject_;
import nl.rivm.screenit.model.Woonplaats_;
import nl.rivm.screenit.model.ZorgInstelling;
import nl.rivm.screenit.model.cervix.CervixHuisarts;
import nl.rivm.screenit.model.cervix.CervixHuisartsAdres_;
import nl.rivm.screenit.model.cervix.CervixHuisarts_;
import nl.rivm.screenit.model.colon.ColonIntakelocatie;
import nl.rivm.screenit.model.colon.ColoscopieCentrumWrapper;
import nl.rivm.screenit.model.colon.ColoscopieCentrumZoekCriteria;
import nl.rivm.screenit.model.colon.PaLaboratorium;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.ToegangLevel;
import nl.rivm.screenit.repository.algemeen.InstellingRepository;
import nl.rivm.screenit.service.AutorisatieService;
import nl.rivm.screenit.service.CoordinatenService;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.OrganisatieZoekService;
import nl.rivm.screenit.util.BigDecimalUtil;
import nl.topicuszorg.organisatie.model.Adres;

import org.apache.commons.collections.CollectionUtils;
import org.jetbrains.annotations.NotNull;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Sort;
import org.springframework.stereotype.Service;

import static nl.rivm.screenit.model.Instelling_.ADRESSEN;
import static nl.rivm.screenit.model.cervix.CervixHuisartsAdres_.WOONPLAATS;
import static nl.rivm.screenit.model.cervix.CervixHuisarts_.POSTADRES;
import static nl.rivm.screenit.specification.SpecificationUtil.join;
import static nl.rivm.screenit.specification.SpecificationUtil.treat;
import static nl.rivm.screenit.specification.algemeen.OrganisatieSpecification.filterActief;
import static nl.rivm.screenit.specification.algemeen.OrganisatieSpecification.getZoekOrganisatiesSpecification;
import static nl.rivm.screenit.specification.algemeen.OrganisatieSpecification.heeftFqdn;
import static nl.rivm.screenit.util.StringUtil.propertyChain;
import static nl.topicuszorg.organisatie.model.Adres_.PLAATS;
import static nl.topicuszorg.organisatie.model.Adres_.STRAAT;

@Service
public class OrganisatieZoekServiceImpl implements OrganisatieZoekService
{
	@Autowired
	private InstellingDao instellingDao;

	@Autowired
	private AutorisatieService autorisatieService;

	@Autowired
	private CoordinatenService coordinatenService;

	@Autowired
	private InstellingRepository organisatieRepository;

	@Autowired
	private ICurrentDateSupplier currentDateSupplier;

	@Override
	public List<Instelling> zoekOrganisaties(Instelling searchObject, List<OrganisatieType> selectedOrganisatieTypes, List<OrganisatieType> excludeOrganisatieTypes,
		InstellingGebruiker organisatieMedewerker, long first, long count, String sortProperty, boolean asc)
	{
		var hierarchieCriteria = getHierarchieCriteria(searchObject, selectedOrganisatieTypes, organisatieMedewerker);
		return zoekOrganisaties(searchObject, hierarchieCriteria, excludeOrganisatieTypes, first, count, sortProperty, asc);
	}

	private List<Instelling> zoekOrganisaties(Instelling searchObject, Map<OrganisatieType, List<Instelling>> hierarchieCriteria, List<OrganisatieType> excludeOrganisatieTypes,
		long first, long count, String sortProperty, boolean asc)
	{
		var spec = getZoekOrganisatiesSpecification(searchObject, hierarchieCriteria, excludeOrganisatieTypes, currentDateSupplier.getLocalDateTime());

		var alleGevondenOrganisaties = organisatieRepository.findWith(spec, q -> q.sortBy(getSort(sortProperty, asc), this::addJoinsForSortingOrCreateDedicatedOrders)).all();
		var alleUniekeGevondenOrganisaties = new ArrayList<>(new LinkedHashSet<>(alleGevondenOrganisaties));
		List<Instelling> resultaten = alleUniekeGevondenOrganisaties;
		if (first != -1)
		{
			if (alleUniekeGevondenOrganisaties.size() > first + count)
			{
				resultaten = alleUniekeGevondenOrganisaties.subList((int) first, (int) (first + count));
			}
			else if (alleUniekeGevondenOrganisaties.size() > first)
			{
				resultaten = alleUniekeGevondenOrganisaties.subList((int) first, alleUniekeGevondenOrganisaties.size());
			}
		}
		return resultaten;
	}

	private @NotNull Sort getSort(String sortProperty, boolean asc)
	{
		var direction = asc ? Sort.Direction.ASC : Sort.Direction.DESC;
		var sort = Sort.by(direction, sortProperty);
		if (sortProperty.startsWith(propertyChain(ADRESSEN, PLAATS)))
		{
			sort = sort.and(Sort.by(direction, propertyChain(POSTADRES, WOONPLAATS, Woonplaats_.NAAM)));
		}
		else if (sortProperty.startsWith(propertyChain(ADRESSEN, STRAAT)))
		{
			sort = sort.and(Sort.by(direction, propertyChain(POSTADRES, STRAAT)));
		}
		sort = sort.and(Sort.by(direction, SingleTableHibernateObject_.ID));
		return sort;
	}

	private javax.persistence.criteria.Order addJoinsForSortingOrCreateDedicatedOrders(Sort.Order order, Root<Instelling> r, CriteriaBuilder cb)
	{
		var sortProperty = order.getProperty();
		if (sortProperty.startsWith(ADRESSEN))
		{
			join(r, Instelling_.adressen, JoinType.LEFT);
		}
		else if (sortProperty.startsWith(POSTADRES))
		{
			var postadresJoin = join(treat(r, CervixHuisarts.class, cb), CervixHuisarts_.postadres, JoinType.LEFT);
			Join<?, ?> propertyJoin = postadresJoin;
			String simpleProperty;
			if (sortProperty.startsWith(propertyChain(POSTADRES, WOONPLAATS)))
			{
				propertyJoin = join(postadresJoin, CervixHuisartsAdres_.woonplaats, JoinType.LEFT);
				simpleProperty = sortProperty.substring(propertyChain(POSTADRES, WOONPLAATS).length() + 1);
			}
			else
			{
				simpleProperty = sortProperty.substring(POSTADRES.length() + 1);
			}
			var exp = propertyJoin.get(simpleProperty);
			return order.isAscending() ? cb.asc(exp) : cb.desc(exp);
		}
		return null;
	}

	@Override
	public long countOrganisaties(Instelling searchObject, List<OrganisatieType> selectedOrganisatieTypes, List<OrganisatieType> excludeOrganisatieTypes,
		InstellingGebruiker organisatieMedewerker)
	{
		var hierarchieCriteria = getHierarchieCriteria(searchObject, selectedOrganisatieTypes, organisatieMedewerker);
		var spec = getZoekOrganisatiesSpecification(searchObject, hierarchieCriteria, excludeOrganisatieTypes, currentDateSupplier.getLocalDateTime());
		return organisatieRepository.findWith(spec, Long.class, q -> q.projection(CriteriaBuilder::countDistinct)).one().orElse(0L);
	}

	private Map<OrganisatieType, List<Instelling>> getHierarchieCriteria(Instelling zoekInstelling, List<OrganisatieType> selectedOrganisatieTypes,
		InstellingGebruiker organisatieMedewerker)
	{
		var hierarchieCriteria = new HashMap<OrganisatieType, List<Instelling>>();
		var organisatieTypeGekozen = zoekInstelling.getOrganisatieType();
		if (organisatieTypeGekozen != null)
		{
			hierarchieCriteria.put(organisatieTypeGekozen, getOrganisatiesForGekozenOrganisatieType(organisatieMedewerker, organisatieTypeGekozen));
		}
		else if (CollectionUtils.isNotEmpty(selectedOrganisatieTypes))
		{
			for (var type : selectedOrganisatieTypes)
			{
				hierarchieCriteria.put(type, getOrganisatiesForGekozenOrganisatieType(organisatieMedewerker, type));
			}
		}
		return hierarchieCriteria;
	}

	private List<Instelling> getOrganisatiesForGekozenOrganisatieType(InstellingGebruiker organisatieMedewerker, OrganisatieType organisatieTypeGekozen)
	{
		var toegangLevel = autorisatieService.getToegangLevel(organisatieMedewerker, Actie.INZIEN, true, organisatieTypeGekozen.getRecht());

		return getOrganisatiesForNiveau(organisatieMedewerker, organisatieTypeGekozen, toegangLevel);
	}

	@Override
	public List<Instelling> getOrganisatiesForNiveau(InstellingGebruiker ingelogdeOrganisatieMedewerker, OrganisatieType organisatieTypeGekozen, ToegangLevel toegangLevel)
	{
		var organisaties = new ArrayList<Instelling>();
		var ingelogdVoorOrganisatie = ingelogdeOrganisatieMedewerker.getOrganisatie();
		switch (organisatieTypeGekozen)
		{
		case BMHK_LABORATORIUM:
		case ZORGINSTELLING:
		case INTAKELOCATIE:
		case COLOSCOPIELOCATIE:
		case PA_LABORATORIUM:
			switch (toegangLevel)
			{
			case INSTELLING:
				organisaties.add(ingelogdVoorOrganisatie);
				break;
			case REGIO:
				organisaties.addAll(screeningsorganisatiesWaarOrganisatieOndervalt(ingelogdVoorOrganisatie));
				break;
			default:
				break;
			}
			break;

		case SCREENINGSORGANISATIE:
			if (toegangLevel.equals(ToegangLevel.REGIO))
			{
				organisaties.addAll(screeningsorganisatiesWaarOrganisatieOndervalt(ingelogdVoorOrganisatie));
			}
			break;

		default:
			break;
		}
		return organisaties;
	}

	@Override
	public List<Instelling> screeningsorganisatiesWaarOrganisatieOndervalt(Instelling organisatie)
	{
		var screeningsorganisaties = new ArrayList<Instelling>();
		if (OrganisatieType.PA_LABORATORIUM == organisatie.getOrganisatieType())
		{
			for (var locatie : ((PaLaboratorium) organisatie).getColoscopielocaties())
			{
				screeningsorganisaties.addAll(screeningsorganisatiesWaarOrganisatieOndervalt(locatie));
			}
		}
		else if (OrganisatieType.SCREENINGSORGANISATIE == organisatie.getOrganisatieType())
		{
			screeningsorganisaties.add(organisatie);
		}
		else
		{
			var parent = organisatie.getParent();
			if (parent != null)
			{
				screeningsorganisaties.addAll(screeningsorganisatiesWaarOrganisatieOndervalt(parent));
			}
		}
		return screeningsorganisaties;
	}

	@Override
	public List<ColoscopieCentrumWrapper> zoekIntakeLocaties(ColoscopieCentrumZoekCriteria zoekObject, Client client, boolean alleenActiefKamers)
	{
		Map<OrganisatieType, List<Instelling>> types = Map.of(OrganisatieType.INTAKELOCATIE, List.of());

		var searchObject = new Instelling();
		searchObject.setNaam(zoekObject.getNaam());
		searchObject.add(new Adres());
		searchObject.getHuidigAdres().setPlaats(zoekObject.getPlaats());
		var organisaties = zoekOrganisaties(searchObject, types, null, -1, -1, "naam", true);
		var list = new ArrayList<ColoscopieCentrumWrapper>();

		var coordinaten = coordinatenService.getCoordinatenVanPersoon(client.getPersoon());

		for (var organisatie : organisaties)
		{
			var intakelocatie = (ColonIntakelocatie) organisatie;
			if (alleenActiefKamers)
			{
				var alleKamersInactief = true;
				for (var kamer : intakelocatie.getKamers())
				{
					if (kamer.getActief())
					{
						alleKamersInactief = false;
						break;
					}
				}
				if (alleKamersInactief)
				{
					continue;
				}
			}

			var wrapper = new ColoscopieCentrumWrapper();
			wrapper.setNaam(intakelocatie.getNaam());
			var adressen = intakelocatie.getAdressen();
			if (adressen != null && !adressen.isEmpty())
			{
				for (var adres : adressen)
				{
					if (adres.getPlaats() != null)
					{
						wrapper.setPlaats(adres.getPlaats());
						break;
					}

				}
			}
			wrapper.setId(intakelocatie.getId());

			var to = intakelocatie.getPostcodeCoordinaten();
			if (coordinaten.vanAdres != null && to != null)
			{
				var distance = BigDecimalUtil.berekenDistance(coordinaten.vanAdres, to);
				if (zoekObject.getAfstand() != null && zoekObject.getAfstand().doubleValue() < distance)
				{
					continue;
				}
				else
				{
					wrapper.setAfstand(BigDecimal.valueOf(distance));
				}
			}

			list.add(wrapper);
		}
		return list;
	}

	@Override
	public List<Instelling> getMogelijkeParents(Instelling organisatie, @NotNull InstellingGebruiker loggedInInstellingGebruiker)
	{
		if (organisatie.getOrganisatieType() == OrganisatieType.BEOORDELINGSEENHEID)
		{
			return getMogelijkeParentsVoorBeoordelingsEenheid(loggedInInstellingGebruiker);
		}

		Class<? extends Instelling> filter = null;
		switch (organisatie.getOrganisatieType())
		{
		case ZORGINSTELLING:
			filter = ScreeningOrganisatie.class;
			break;
		case MAMMAPOLI:
		case RADIOLOGIEAFDELING:
		case INTAKELOCATIE:
		case COLOSCOPIELOCATIE:
			filter = ZorgInstelling.class;
			break;
		default:
			break;
		}

		return (List<Instelling>) instellingDao.getActieveInstellingen(filter);
	}

	private List<Instelling> getMogelijkeParentsVoorBeoordelingsEenheid(InstellingGebruiker loggedInInstellingGebruiker)
	{
		var loggedInInstelling = loggedInInstellingGebruiker.getOrganisatie();
		if (OrganisatieType.SCREENINGSORGANISATIE == loggedInInstelling.getOrganisatieType())
		{
			Class<? extends Instelling> filter = CentraleEenheid.class;
			var screeningOrganisatie = (ScreeningOrganisatie) loggedInInstelling;
			return (List<Instelling>) instellingDao.getActieveInstellingenBinnenRegio(filter, screeningOrganisatie);
		}
		else
		{
			return new ArrayList<>();
		}
	}

	@Override
	public List<Instelling> getAllActieveOrganisatiesWithType(Class<? extends Instelling> organisatie)
	{
		return (List<Instelling>) instellingDao.getActieveInstellingen(organisatie);
	}

	@Override
	public List<Long> getZichtbareInstellingenOpToegangLevel(Instelling organisatie, ToegangLevel level, List<OrganisatieType> types)
	{
		var zichtbaar = new ArrayList<Long>();
		var regios = screeningsorganisatiesWaarOrganisatieOndervalt(organisatie);
		if (ToegangLevel.REGIO.equals(level) && CollectionUtils.isNotEmpty(regios) && !types.isEmpty())
		{
			var hierarchieCriteria = new HashMap<OrganisatieType, List<Instelling>>();
			for (var type : types)
			{
				hierarchieCriteria.put(type, regios);
			}
			var result = zoekOrganisaties(new Instelling(), hierarchieCriteria, null, -1, -1, "id", true);
			var regioIds = new ArrayList<Long>();
			result.forEach(i -> regioIds.add(i.getId()));
			zichtbaar.addAll(regioIds);
		}
		else if (ToegangLevel.LANDELIJK.equals(level) && !types.isEmpty())
		{
			for (var type : types)
			{
				zichtbaar.addAll(instellingDao.getLandelijkeInstellingIds(type));
			}
		}
		else
		{
			zichtbaar.add(organisatie.getId());
		}
		return zichtbaar;
	}

	@Override
	public ColoscopieCentrumWrapper getNearestIntakeLocatie(Client client)
	{
		var zoekObject = new ColoscopieCentrumZoekCriteria();
		var intakeLocaties = zoekIntakeLocaties(zoekObject, client, true);
		ColoscopieCentrumWrapper ilZonderAfstand = null;
		ColoscopieCentrumWrapper ilMetAfstand = null;
		for (var wrapper : intakeLocaties)
		{
			if (ilZonderAfstand == null && wrapper.getAfstand() == null)
			{
				ilZonderAfstand = wrapper;
			}
			else if (wrapper.getAfstand() != null && (ilMetAfstand == null || ilMetAfstand.getAfstand() != null && wrapper.getAfstand().compareTo(ilMetAfstand.getAfstand()) < 0))
			{
				ilMetAfstand = wrapper;
			}
		}
		if (ilMetAfstand != null)
		{
			return ilMetAfstand;
		}
		else
		{
			return ilZonderAfstand;
		}
	}

	@Override
	public List<Instelling> zoekOrganisatieMetFqdn(String fqdn)
	{
		return organisatieRepository.findAll(filterActief(true).and(heeftFqdn(fqdn)));
	}
}
