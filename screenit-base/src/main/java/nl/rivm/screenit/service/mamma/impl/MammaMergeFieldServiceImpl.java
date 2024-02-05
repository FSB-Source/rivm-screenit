package nl.rivm.screenit.service.mamma.impl;

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

import java.util.Optional;

import nl.rivm.screenit.model.Gebruiker;
import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.mamma.MammaBeoordeling;
import nl.rivm.screenit.model.mamma.MammaLezing;
import nl.rivm.screenit.model.mamma.enums.MammaBeoordelingStatus;
import nl.rivm.screenit.service.mamma.MammaBaseBeoordelingService;
import nl.rivm.screenit.service.mamma.MammaMergeFieldService;
import nl.topicuszorg.hibernate.spring.util.ApplicationContextProvider;

import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Service
@Transactional(propagation = Propagation.SUPPORTS, readOnly = true)
public class MammaMergeFieldServiceImpl implements MammaMergeFieldService
{
	private static boolean heeftBeoordelingStatusGunstig(MammaBeoordeling beoordeling)
	{
		return MammaBeoordelingStatus.UITSLAG_GUNSTIG.equals(beoordeling.getStatus());
	}

	private static boolean heeftVerslagLezingMetBeoordelaar(MammaBeoordeling beoordeling)
	{
		return beoordeling.getVerslagLezing() != null && beoordeling.getVerslagLezing().getBeoordelaar() != null;
	}

	public Gebruiker bepaalRadioloog1(MammaBeoordeling beoordeling)
	{
		if (beoordeling != null)
		{
			if (heeftBeoordelingStatusGunstig(beoordeling))
			{
				return bepaalEersteNietVerwijzendeRadioloog(beoordeling);
			}
			else if (heeftVerslagLezingMetBeoordelaar(beoordeling))
			{
				return beoordeling.getVerslagLezing().getBeoordelaar().getMedewerker();
			}
		}
		return null;
	}

	public Gebruiker bepaalRadioloog2(MammaBeoordeling beoordeling)
	{
		if (beoordeling != null)
		{
			if (heeftBeoordelingStatusGunstig(beoordeling))
			{
				return bepaalTweedeNietVerwijzendeRadioloog(beoordeling);
			}
			else if (heeftVerslagLezingMetBeoordelaar(beoordeling))
			{
				return bepaalTweedeVerwijzendeRadioloog(beoordeling);
			}
		}
		return null;
	}

	private Gebruiker bepaalTweedeVerwijzendeRadioloog(MammaBeoordeling beoordeling)
	{
		Gebruiker verslagMaker = beoordeling.getVerslagLezing().getBeoordelaar().getMedewerker();
		Gebruiker beoordelaarEersteLezing = beoordeling.getEersteLezing().getBeoordelaar().getMedewerker();
		Gebruiker beoordelaarTweedeLezing = beoordeling.getTweedeLezing().getBeoordelaar().getMedewerker();
		Optional<Gebruiker> beoordelaarDiscrepantie = Optional.of(beoordeling).map(MammaBeoordeling::getDiscrepantieLezing).map(MammaLezing::getBeoordelaar)
			.map(InstellingGebruiker::getMedewerker);
		Optional<Gebruiker> beoordelaarArbitrage = Optional.of(beoordeling).map(MammaBeoordeling::getArbitrageLezing).map(MammaLezing::getBeoordelaar)
			.map(InstellingGebruiker::getMedewerker);

		if (beoordelaarArbitrage.isPresent())
		{
			return !beoordelaarArbitrage.get().equals(verslagMaker) ? beoordelaarArbitrage.get()
				: bepaalVerwijzendeRadioloog(beoordeling, beoordelaarEersteLezing, beoordelaarTweedeLezing);
		}
		else if (beoordelaarDiscrepantie.isPresent() && !beoordelaarDiscrepantie.get().equals(verslagMaker))
		{
			return beoordelaarDiscrepantie.get();
		}
		else if (!beoordelaarEersteLezing.equals(verslagMaker))
		{
			return beoordelaarEersteLezing;
		}
		else if (!beoordelaarTweedeLezing.equals(verslagMaker))
		{
			return beoordelaarTweedeLezing;
		}
		throw new IllegalStateException("Er is geen geldige tweede beoordelaar");
	}

	private Gebruiker bepaalVerwijzendeRadioloog(MammaBeoordeling beoordeling, Gebruiker beoordelaarEersteLezing, Gebruiker beoordelaarTweedeLezing)
	{
		MammaBaseBeoordelingService beoordelingService = ApplicationContextProvider.getApplicationContext().getBean(MammaBaseBeoordelingService.class);

		if (beoordelingService.isLezingVerwijzen(beoordeling.getEersteLezing()))
		{
			return beoordelaarEersteLezing;
		}
		else if (beoordelingService.isLezingVerwijzen(beoordeling.getTweedeLezing()))
		{
			return beoordelaarTweedeLezing;
		}
		throw new IllegalStateException("Er is geen verwijzende lezing");
	}

	private Gebruiker bepaalEersteNietVerwijzendeRadioloog(MammaBeoordeling beoordeling)
	{
		MammaLezing eersteLezing = beoordeling.getEersteLezing();

		return beoordeling.getArbitrageLezing() != null ? beoordeling.getArbitrageLezing().getBeoordelaar().getMedewerker() : eersteLezing.getBeoordelaar().getMedewerker();
	}

	private Gebruiker bepaalTweedeNietVerwijzendeRadioloog(MammaBeoordeling beoordeling)
	{
		return beoordeling.getArbitrageLezing() != null ? bepaalAndereGunstigeLezing(beoordeling) : beoordeling.getTweedeLezing().getBeoordelaar().getMedewerker();
	}

	private Gebruiker bepaalAndereGunstigeLezing(MammaBeoordeling beoordeling)
	{
		MammaBaseBeoordelingService beoordelingService = ApplicationContextProvider.getApplicationContext().getBean(MammaBaseBeoordelingService.class);
		MammaLezing eersteLezing = beoordeling.getEersteLezing();

		return !beoordelingService.isLezingVerwijzen(eersteLezing) ? eersteLezing.getBeoordelaar().getMedewerker()
			: beoordeling.getTweedeLezing().getBeoordelaar().getMedewerker();
	}
}
