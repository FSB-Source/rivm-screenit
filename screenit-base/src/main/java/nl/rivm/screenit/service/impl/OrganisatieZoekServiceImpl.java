
package nl.rivm.screenit.service.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
 * %%
 * Copyright (C) 2012 - 2021 Facilitaire Samenwerking Bevolkingsonderzoek
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
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import nl.rivm.screenit.dao.InstellingDao;
import nl.rivm.screenit.dao.OrganisatieZoekDao;
import nl.rivm.screenit.model.CentraleEenheid;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.IGeografischeCoordinaten;
import nl.rivm.screenit.model.Instelling;
import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.OrganisatieType;
import nl.rivm.screenit.model.ScreeningOrganisatie;
import nl.rivm.screenit.model.ZorgInstelling;
import nl.rivm.screenit.model.colon.ColoscopieCentrum;
import nl.rivm.screenit.model.colon.ColoscopieCentrumWrapper;
import nl.rivm.screenit.model.colon.ColoscopieCentrumZoekCriteria;
import nl.rivm.screenit.model.colon.Kamer;
import nl.rivm.screenit.model.colon.PaLaboratorium;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.ToegangLevel;
import nl.rivm.screenit.service.AutorisatieService;
import nl.rivm.screenit.service.CoordinatenService;
import nl.rivm.screenit.service.OrganisatieZoekService;
import nl.rivm.screenit.util.BigDecimalUtil;
import nl.topicuszorg.organisatie.model.Adres;

import org.apache.commons.collections.CollectionUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Service
@Transactional(propagation = Propagation.SUPPORTS)
public class OrganisatieZoekServiceImpl implements OrganisatieZoekService
{

	@Autowired
	private OrganisatieZoekDao organisatieZoekDao;

	@Autowired
	private InstellingDao instellingDao;

	@Autowired
	private AutorisatieService autorisatieService;

	@Autowired
	private CoordinatenService coordinatenService;

	@Override
	public Iterator<Instelling> searchOrganisatie(Instelling searchObject, List<OrganisatieType> selectedOrganisatieTypes, List<OrganisatieType> excludeOrganisatieTypes,
		InstellingGebruiker instellingGebruiker, long first, long count, String sortProperty, boolean asc)
	{
		Map<OrganisatieType, List<Instelling>> hierarchieCriteria = getHierarchieCriteria(searchObject, selectedOrganisatieTypes, instellingGebruiker);
		return organisatieZoekDao.searchOrganisatie(searchObject, hierarchieCriteria, excludeOrganisatieTypes, first, count, sortProperty, asc);
	}

	@Override
	public long countOrganisatie(Instelling searchObject, List<OrganisatieType> selectedOrganisatieTypes, List<OrganisatieType> excludeOrganisatieTypes,
		InstellingGebruiker instellingGebruiker)
	{
		Map<OrganisatieType, List<Instelling>> hierarchieCriteria = getHierarchieCriteria(searchObject, selectedOrganisatieTypes, instellingGebruiker);
		return organisatieZoekDao.countOrganisatie(searchObject, hierarchieCriteria, excludeOrganisatieTypes);
	}

	private Map<OrganisatieType, List<Instelling>> getHierarchieCriteria(Instelling zoekInstelling, List<OrganisatieType> selectedOrganisatieTypes,
		InstellingGebruiker instellingGebruiker)
	{
		Map<OrganisatieType, List<Instelling>> hierarchieCriteria = new HashMap<>();
		OrganisatieType organisatieTypeGekozen = zoekInstelling.getOrganisatieType();
		if (organisatieTypeGekozen != null)
		{
			hierarchieCriteria.put(organisatieTypeGekozen, getOrganisatiesForGekozenOrganisatieType(instellingGebruiker, organisatieTypeGekozen));
		}
		else if (CollectionUtils.isNotEmpty(selectedOrganisatieTypes))
		{
			for (OrganisatieType type : selectedOrganisatieTypes)
			{
				hierarchieCriteria.put(type, getOrganisatiesForGekozenOrganisatieType(instellingGebruiker, type));
			}
		}
		return hierarchieCriteria;
	}

	private List<Instelling> getOrganisatiesForGekozenOrganisatieType(InstellingGebruiker instellingGebruiker, OrganisatieType organisatieTypeGekozen)
	{
		ToegangLevel toegangLevel = autorisatieService.getToegangLevel(instellingGebruiker, Actie.INZIEN, true, organisatieTypeGekozen.getRecht());

		return getOrganisatiesForNiveau(instellingGebruiker, organisatieTypeGekozen, toegangLevel);
	}

	@Override
	public List<Instelling> getOrganisatiesForNiveau(InstellingGebruiker instellingGebruiker, OrganisatieType organisatieTypeGekozen, ToegangLevel toegangLevel)
	{
		List<Instelling> instellingen = new ArrayList<>();
		switch (organisatieTypeGekozen)
		{
		case BMHK_LABORATORIUM:
		case ZORGINSTELLING:
		case COLOSCOPIECENTRUM:
		case COLOSCOPIELOCATIE:
		case PA_LABORATORIUM:
			switch (toegangLevel)
			{
			case INSTELLING:
				instellingen.add(instellingGebruiker.getOrganisatie());
				break;
			case REGIO:
				instellingen = findSOs(instellingGebruiker.getOrganisatie());
				break;
			default:
				break;
			}
			break;

		case SCREENINGSORGANISATIE:
			if (toegangLevel.equals(ToegangLevel.REGIO))
			{
				instellingen = findSOs(instellingGebruiker.getOrganisatie());
			}
			break;
		case INPAKCENTRUM:
		case LABORATORIUM:
		case HUISARTS:
			if (toegangLevel.equals(ToegangLevel.INSTELLING) || toegangLevel.equals(ToegangLevel.REGIO))
			{
				instellingen = findSOs(instellingGebruiker.getOrganisatie());
			}
			break;

		default:
			break;
		}
		return instellingen;
	}

	@Override
	public List<Instelling> findSOs(Instelling instelling)
	{
		List<Instelling> sos = new ArrayList<>();
		if (OrganisatieType.PA_LABORATORIUM.equals(instelling.getOrganisatieType()))
		{
			for (Instelling locatie : ((PaLaboratorium) instelling).getColoscopielocaties())
			{
				sos.addAll(findSOs(locatie));
			}
		}
		else if (OrganisatieType.SCREENINGSORGANISATIE.equals(instelling.getOrganisatieType()))
		{
			sos.add(instelling);
		}
		else
		{
			Instelling parent = instelling.getParent();
			if (parent != null)
			{
				sos.addAll(findSOs(parent));
			}
		}
		return sos;
	}

	@Override
	public List<ColoscopieCentrumWrapper> zoekIntakeLocaties(ColoscopieCentrumZoekCriteria zoekObject, Client client, boolean alleenActiefKamers)
	{
		Map<OrganisatieType, List<Instelling>> types = new HashMap<>();
		types.put(OrganisatieType.COLOSCOPIECENTRUM, new ArrayList<Instelling>());

		Instelling searchObject = new Instelling();
		searchObject.setNaam(zoekObject.getNaam());
		searchObject.add(new Adres());
		searchObject.getHuidigAdres().setPlaats(zoekObject.getPlaats());
		Iterator<Instelling> organisatie = organisatieZoekDao.searchOrganisatie(searchObject, types, null, -1, -1, "naam", true);
		List<ColoscopieCentrumWrapper> list = new ArrayList<>();

		PersoonCoordinaten coordinaten = coordinatenService.getCoordinatenVanPersoon(client.getPersoon());

		while (organisatie.hasNext())
		{
			ColoscopieCentrum instelling = (ColoscopieCentrum) organisatie.next();
			if (alleenActiefKamers)
			{
				boolean alleKamersInactief = true;
				for (Kamer kamer : instelling.getKamers())
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

			ColoscopieCentrumWrapper wrapper = new ColoscopieCentrumWrapper();
			wrapper.setNaam(instelling.getNaam());
			List<Adres> adressen = instelling.getAdressen();
			if (adressen != null && !adressen.isEmpty())
			{
				for (Adres adres : adressen)
				{
					if (adres.getPlaats() != null)
					{
						wrapper.setPlaats(adres.getPlaats());
						break;
					}

				}
			}
			wrapper.setId(instelling.getId());

			IGeografischeCoordinaten to = instelling.getPostcodeCoordinaten();
			if (coordinaten.vanAdres != null && to != null)
			{
				double distance = BigDecimalUtil.berekenDistance(coordinaten.vanAdres, to);
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
	@Transactional(propagation = Propagation.SUPPORTS)
	public List<Instelling> getMogelijkeParents(Instelling instelling, InstellingGebruiker loggedInInstellingGebruiker)
	{
		if (instelling.getOrganisatieType() == OrganisatieType.BEOORDELINGSEENHEID)
		{
			return getMogelijkeParentsVoorBeoordelingsEenheid(loggedInInstellingGebruiker);
		}

		Class<? extends Instelling> filter = null;
		switch (instelling.getOrganisatieType())
		{
		case ZORGINSTELLING:
			filter = ScreeningOrganisatie.class;
			break;
		case MAMMAPOLI:
		case RADIOLOGIEAFDELING:
		case COLOSCOPIECENTRUM:
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
		Instelling loggedInInstelling = loggedInInstellingGebruiker.getOrganisatie();
		if (OrganisatieType.SCREENINGSORGANISATIE.equals(loggedInInstelling.getOrganisatieType()))
		{
			Class<? extends Instelling> filter = CentraleEenheid.class;
			ScreeningOrganisatie screeningOrganisatie = (ScreeningOrganisatie) loggedInInstelling;
			return (List<Instelling>) instellingDao.getActieveInstellingenBinnenRegio(filter, screeningOrganisatie);
		}
		else
		{
			return new ArrayList<>();
		}
	}

	@Override
	public List<Instelling> getAllActieveOrganisatiesWithType(Class<? extends Instelling> instelling)
	{
		return (List<Instelling>) instellingDao.getActieveInstellingen(instelling);
	}

	@Override
	public List<Long> getZichtbareUltimviewInstellingIds(Instelling instelling, ToegangLevel level)
	{
		List<OrganisatieType> types = new ArrayList<OrganisatieType>();

		types.add(instelling.getOrganisatieType());

		return getZichtbareInstellingen(instelling, level, types);
	}

	@Override
	public List<Long> getZichtbateInstellingenOpToegangLevel(Instelling instelling, ToegangLevel level, List<OrganisatieType> types)
	{
		return getZichtbareInstellingen(instelling, level, types);
	}

	private List<Long> getZichtbareInstellingen(Instelling instelling, ToegangLevel level, List<OrganisatieType> types)
	{
		List<Long> zichtbaar = new ArrayList<Long>();
		List<Instelling> regios = findSOs(instelling);
		if (ToegangLevel.REGIO.equals(level) && level != null && CollectionUtils.isNotEmpty(regios) && types.size() != 0)
		{
			Map<OrganisatieType, List<Instelling>> hierarchieCriteria = new HashMap<>();
			for (OrganisatieType type : types)
			{
				hierarchieCriteria.put(type, regios);
			}
			Iterator<Instelling> result = organisatieZoekDao.searchOrganisatie(new Instelling(), hierarchieCriteria, null, -1, -1, "id", true);
			List<Long> regioIds = new ArrayList<Long>();
			while (result.hasNext())
			{
				regioIds.add(result.next().getId());
			}
			zichtbaar.addAll(regioIds);
		}
		else if (ToegangLevel.LANDELIJK.equals(level) && level != null && types.size() != 0)
		{
			for (OrganisatieType type : types)
			{
				zichtbaar.addAll(instellingDao.getLandelijkeInstellingIds(type));
			}
		}
		else
		{
			zichtbaar.add(instelling.getId());
		}
		return zichtbaar;
	}

	@Override
	public ColoscopieCentrumWrapper getNearestIntakeLocatie(Client client)
	{
		ColoscopieCentrumZoekCriteria zoekObject = new ColoscopieCentrumZoekCriteria();
		List<ColoscopieCentrumWrapper> intakeLocaties = zoekIntakeLocaties(zoekObject, client, true);
		ColoscopieCentrumWrapper ilZonderAfstand = null;
		ColoscopieCentrumWrapper ilMetAfstand = null;
		for (ColoscopieCentrumWrapper wrapper : intakeLocaties)
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
		else if (ilZonderAfstand != null)
		{
			return ilZonderAfstand;
		}
		return null;
	}
}
