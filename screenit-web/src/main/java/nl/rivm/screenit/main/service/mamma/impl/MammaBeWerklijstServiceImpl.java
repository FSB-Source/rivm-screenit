package nl.rivm.screenit.main.service.mamma.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
 * %%
 * Copyright (C) 2012 - 2023 Facilitaire Samenwerking Bevolkingsonderzoek
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
import nl.rivm.screenit.model.BeoordelingsEenheid;
import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.MammaOnderzoekType;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.model.mamma.enums.MammaBeoordelingStatus;
import nl.rivm.screenit.service.AutorisatieService;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Service
@Transactional(propagation = Propagation.SUPPORTS, readOnly = true)
public class MammaBeWerklijstServiceImpl implements MammaBeWerklijstService
{
	@Autowired
	private MammaBeoordelingService beoordelingService;

	@Autowired
	private AutorisatieService autorisatieService;

	@Override
	public boolean heeftOnderzoekenInWerklijst(InstellingGebruiker gebruiker, BeoordelingsEenheid beoordelingsEenheid)
	{
		return ((beoordelingService.isBevoegdVoorArbitrage(gebruiker) && heeftArbitrageInWerklijst(gebruiker, beoordelingsEenheid))
			|| heeftDiscrepantieInWerklijst(gebruiker, beoordelingsEenheid) || heeftVerslagInWerklijst(gebruiker, beoordelingsEenheid));
	}

	private boolean heeftDiscrepantieInWerklijst(InstellingGebruiker gebruiker, BeoordelingsEenheid beoordelingsEenheid)
	{
		MammaBeWerklijstZoekObject zoekObject = maakZoekObject(gebruiker, beoordelingsEenheid, Collections.singletonList(MammaBeoordelingStatus.DISCREPANTIE));
		return beoordelingService.countOnderzoeken(zoekObject) != 0L;
	}

	private boolean heeftArbitrageInWerklijst(InstellingGebruiker gebruiker, BeoordelingsEenheid beoordelingsEenheid)
	{
		MammaBeWerklijstZoekObject zoekObject = maakZoekObject(gebruiker, beoordelingsEenheid, Collections.singletonList(MammaBeoordelingStatus.ARBITRAGE));
		return beoordelingService.countOnderzoeken(zoekObject) != 0L;
	}

	private boolean heeftVerslagInWerklijst(InstellingGebruiker gebruiker, BeoordelingsEenheid beoordelingsEenheid)
	{
		MammaBeWerklijstZoekObject zoekObject = maakZoekObject(gebruiker, beoordelingsEenheid,
			Arrays.asList(MammaBeoordelingStatus.VERSLAG_MAKEN, MammaBeoordelingStatus.VERSLAG_AFGEKEURD));
		return beoordelingService.countOnderzoeken(zoekObject) != 0L;
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
}
