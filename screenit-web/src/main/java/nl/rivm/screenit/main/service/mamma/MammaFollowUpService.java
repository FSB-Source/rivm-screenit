package nl.rivm.screenit.main.service.mamma;

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

import java.util.List;

import nl.rivm.screenit.main.model.mamma.MammaFollowUpConclusieChoice;
import nl.rivm.screenit.model.Account;
import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.ScreeningOrganisatie;
import nl.rivm.screenit.model.mamma.MammaBeoordeling;
import nl.rivm.screenit.model.mamma.MammaFollowUpRadiologieVerslag;
import nl.rivm.screenit.model.mamma.MammaFollowUpVerslag;
import nl.rivm.screenit.model.mamma.MammaScreeningRonde;
import nl.rivm.screenit.model.mamma.enums.MammaFollowUpConclusieStatus;

import org.springframework.data.domain.Sort;

public interface MammaFollowUpService
{

	void saveOrUpdateRadiologie(MammaFollowUpRadiologieVerslag verslag, InstellingGebruiker loggedInInstellingGebruiker);

	void saveFollowUpConclusieStatus(MammaScreeningRonde screeningRonde, MammaFollowUpConclusieStatus followUpConclusieStatus, Account loggedInInstellingGebruiker);

	void savePaVerslagNietTeVerwachten(MammaFollowUpRadiologieVerslag followUpRadiologieVerslag, Account loggedInInstellingGebruiker);

	MammaFollowUpConclusieStatus bepaalFollowUpConclusie(MammaScreeningRonde screeningRonde, MammaFollowUpConclusieChoice conclusieEnum);

	List<MammaFollowUpRadiologieVerslag> getIngevoerdeFollowUpRadiologieVerslagen(MammaScreeningRonde screeningRonde);

	List<MammaFollowUpVerslag> getAfgerondeFollowUpPathologieVerslagen(MammaScreeningRonde screeningRonde);

	List<MammaBeoordeling> zoekOpenstaandeFollowUpConclusies(ScreeningOrganisatie regio, long first, long count, Sort sort);

	long countOpenstaandeFollowUpConclusies(ScreeningOrganisatie regio);
}
