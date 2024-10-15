package nl.rivm.screenit.main.service.mamma.impl;

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

import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import nl.rivm.screenit.main.model.mamma.beoordeling.MammaBeWerklijstZoekObject;
import nl.rivm.screenit.main.service.mamma.MammaBeWerklijstService;
import nl.rivm.screenit.main.service.mamma.MammaBeoordelingService;
import nl.rivm.screenit.main.specification.mamma.MammaBeoordelingWerklijstSpecification;
import nl.rivm.screenit.model.BeoordelingsEenheid;
import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.OrganisatieType;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.model.enums.MammaOnderzoekType;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.model.mamma.MammaBeoordeling;
import nl.rivm.screenit.model.mamma.MammaOnderzoek_;
import nl.rivm.screenit.model.mamma.MammaScreeningsEenheid;
import nl.rivm.screenit.model.mamma.enums.MammaBeoordelingStatus;
import nl.rivm.screenit.repository.mamma.MammaOnderzoekRepository;
import nl.rivm.screenit.service.AutorisatieService;
import nl.rivm.screenit.service.LogService;
import nl.rivm.screenit.service.mamma.MammaBaseBeoordelingService;
import nl.rivm.screenit.specification.mamma.MammaBeoordelingSpecification;
import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject_;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Sort;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import static nl.rivm.screenit.main.specification.mamma.MammaBeoordelingWerklijstSpecification.VERWIJZEND_SORT_PROPERTY;
import static nl.rivm.screenit.main.specification.mamma.MammaBeoordelingWerklijstSpecification.beWerklijstSpecification;
import static nl.rivm.screenit.main.specification.mamma.MammaBeoordelingWerklijstSpecification.heeftOnbevestigdeLezing;
import static nl.rivm.screenit.model.mamma.enums.MammaBeoordelingStatus.EERSTE_LEZING_OPGESLAGEN;
import static nl.rivm.screenit.model.mamma.enums.MammaBeoordelingStatus.TWEEDE_LEZING_OPGESLAGEN;

@Service
public class MammaBeWerklijstServiceImpl implements MammaBeWerklijstService
{
	@Autowired
	private MammaBaseBeoordelingService baseBeoordelingService;

	@Autowired
	private MammaBeoordelingService beoordelingService;

	@Autowired
	private AutorisatieService autorisatieService;

	@Autowired
	private MammaOnderzoekRepository onderzoekRepository;

	@Autowired
	private LogService logService;

	@Override
	public boolean heeftOnderzoekenInWerklijst(InstellingGebruiker gebruiker, BeoordelingsEenheid beoordelingsEenheid)
	{
		return ((beoordelingService.isBevoegdVoorArbitrage(gebruiker) && heeftArbitrageInWerklijst(gebruiker, beoordelingsEenheid))
			|| heeftDiscrepantieInWerklijst(gebruiker, beoordelingsEenheid) || heeftVerslagInWerklijst(gebruiker, beoordelingsEenheid));
	}

	private boolean heeftDiscrepantieInWerklijst(InstellingGebruiker gebruiker, BeoordelingsEenheid beoordelingsEenheid)
	{
		MammaBeWerklijstZoekObject zoekObject = maakZoekObject(gebruiker, beoordelingsEenheid, Collections.singletonList(MammaBeoordelingStatus.DISCREPANTIE));
		return countBeoordelingen(zoekObject) != 0L;
	}

	private boolean heeftArbitrageInWerklijst(InstellingGebruiker gebruiker, BeoordelingsEenheid beoordelingsEenheid)
	{
		MammaBeWerklijstZoekObject zoekObject = maakZoekObject(gebruiker, beoordelingsEenheid, Collections.singletonList(MammaBeoordelingStatus.ARBITRAGE));
		return countBeoordelingen(zoekObject) != 0L;
	}

	private boolean heeftVerslagInWerklijst(InstellingGebruiker gebruiker, BeoordelingsEenheid beoordelingsEenheid)
	{
		MammaBeWerklijstZoekObject zoekObject = maakZoekObject(gebruiker, beoordelingsEenheid,
			Arrays.asList(MammaBeoordelingStatus.VERSLAG_MAKEN, MammaBeoordelingStatus.VERSLAG_AFGEKEURD));
		return countBeoordelingen(zoekObject) != 0L;
	}

	private MammaBeWerklijstZoekObject maakZoekObject(InstellingGebruiker gebruiker, BeoordelingsEenheid beoordelingsEenheid, List<MammaBeoordelingStatus> stutussen)
	{
		MammaBeWerklijstZoekObject zoekObject = new MammaBeWerklijstZoekObject();

		if (autorisatieService.getToegangLevel(gebruiker, Actie.INZIEN, true, Recht.GEBRUIKER_SCREENING_MAMMA_BE_ONDERZOEKTYPE_FILTER) == null)
		{
			zoekObject.setOnderzoekType(MammaOnderzoekType.MAMMOGRAFIE);
		}

		zoekObject.setBeoordelingsEenheid(beoordelingsEenheid);
		zoekObject.setInstellingGebruiker(gebruiker);
		zoekObject.setBeoordelingStatussen(stutussen);
		return zoekObject;
	}

	@Override
	public List<MammaBeoordeling> zoekBeoordelingen(MammaBeWerklijstZoekObject zoekObject, long first, long count, Sort sort)
	{
		return onderzoekRepository.findWith(beWerklijstSpecification(zoekObject), MammaBeoordeling.class,
			q -> q.projection((cb, r) -> r.get(MammaOnderzoek_.laatsteBeoordeling))
				.sortBy(werklijstSortering(sort), MammaBeoordelingWerklijstSpecification::verwijzendSortering)
				.all(first, count));
	}

	@Override
	public long countBeoordelingen(MammaBeWerklijstZoekObject zoekObject)
	{
		return onderzoekRepository.count(beWerklijstSpecification(zoekObject));
	}

	@Override
	public List<Long> zoekBeoordelingenNummers(MammaBeWerklijstZoekObject zoekObject, Sort sort)
	{
		return onderzoekRepository.findWith(beWerklijstSpecification(zoekObject), Long.class,
			q -> q.projection((cb, r) -> r.get(MammaOnderzoek_.laatsteBeoordeling).get(AbstractHibernateObject_.id))
				.sortBy(werklijstSortering(sort), MammaBeoordelingWerklijstSpecification::verwijzendSortering)
				.all());
	}

	private Sort werklijstSortering(Sort sort)
	{
		return Sort.by(VERWIJZEND_SORT_PROPERTY)
			.and(sort)
			.and(Sort.by(MammaOnderzoek_.CREATIE_DATUM)); 
	}

	@Override
	public List<MammaScreeningsEenheid> zoekScreeningsEenhedenMetBeWerklijstBeoordeling(InstellingGebruiker loggedInInstellingGebruiker,
		List<MammaBeoordelingStatus> beschikbarePaginaStatussen)
	{
		var zoekObject = new MammaBeWerklijstZoekObject();
		zoekObject.setInstellingGebruiker(loggedInInstellingGebruiker);
		zoekObject.setBeoordelingStatussen(beschikbarePaginaStatussen);
		if (loggedInInstellingGebruiker.getOrganisatie().getOrganisatieType().equals(OrganisatieType.BEOORDELINGSEENHEID))
		{
			zoekObject.setBeoordelingsEenheid((BeoordelingsEenheid) loggedInInstellingGebruiker.getOrganisatie());
		}

		return onderzoekRepository.findWith(beWerklijstSpecification(zoekObject), MammaScreeningsEenheid.class,
			q -> q.projection((cb, r) -> r.get(MammaOnderzoek_.screeningsEenheid))
				.distinct().all());
	}

	@Override
	public int getAantalBeoordeeld(MammaBeWerklijstZoekObject zoekObject)
	{
		return (int) onderzoekRepository.count(beWerklijstSpecification(zoekObject).and(heeftOnbevestigdeLezing()));
	}

	@Override
	public int getAantalBeoordeeldInList(List<Long> beoordelingenIds)
	{
		if (beoordelingenIds.isEmpty())
		{
			return 0;
		}

		return (int) onderzoekRepository.count(MammaBeoordelingWerklijstSpecification.heeftOnbevestigdeLezing()
			.and(MammaBeoordelingSpecification.heeftIdIn(beoordelingenIds).with(MammaOnderzoek_.laatsteBeoordeling)));
	}

	@Override
	public boolean is1eOf2eLezingenTeBevestigen(InstellingGebruiker instellingGebruiker)
	{
		var zoekObject = new MammaBeWerklijstZoekObject();
		zoekObject.setInstellingGebruiker(instellingGebruiker);
		zoekObject.setBeoordelingStatussen(Arrays.asList(EERSTE_LEZING_OPGESLAGEN, TWEEDE_LEZING_OPGESLAGEN));
		return getAantalBeoordeeld(zoekObject) > 0;
	}

	@Override
	@Transactional
	public void bevestig1eEn2eLezingen(InstellingGebruiker instellingGebruiker)
	{
		var zoekObject = new MammaBeWerklijstZoekObject();
		zoekObject.setInstellingGebruiker(instellingGebruiker);
		zoekObject.setBeoordelingStatussen(Arrays.asList(EERSTE_LEZING_OPGESLAGEN, TWEEDE_LEZING_OPGESLAGEN));
		var beoordelingen = zoekBeoordelingen(zoekObject, -1, -1, Sort.unsorted());
		for (var beoordeling : beoordelingen)
		{
			baseBeoordelingService.bevestigLezing(beoordeling);
		}
		logService.logGebeurtenis(LogGebeurtenis.MAMMA_BEOORDELINGEN_GEACCORDEERD, instellingGebruiker, "Aantal beoordelingen: " + beoordelingen.size(), Bevolkingsonderzoek.MAMMA);
	}
}
