package nl.rivm.screenit.main.service.cervix;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
 * %%
 * Copyright (C) 2012 - 2022 Facilitaire Samenwerking Bevolkingsonderzoek
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

import java.io.File;
import java.io.FileNotFoundException;

import javax.xml.bind.JAXBException;

import nl.rivm.screenit.model.Account;
import nl.rivm.screenit.model.BMHKLaboratorium;
import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.ScreeningOrganisatie;
import nl.rivm.screenit.model.cervix.facturatie.CervixBetaalopdracht;
import nl.rivm.screenit.model.cervix.facturatie.CervixTarief;

import com.fasterxml.jackson.core.JsonProcessingException;

public interface CervixBetalingService
{

	void genereerCervixBetalingsSpecificatieEnSepaBestand(Long betaalopdrachtId);

	Long opslaanBetaalopdracht(CervixBetaalopdracht opdracht);

	void maakSepaBestand(File sepaBestand, CervixBetaalopdracht betaalOpdracht) throws JAXBException, FileNotFoundException;

	void maakSpecificatieBestand(File specificatieBestand, CervixBetaalopdracht opdracht) throws Exception;

	void verwijderSepaBestanden(CervixBetaalopdracht betaalopdracht, InstellingGebruiker loggedInInstellingGebruiker) throws JsonProcessingException;

	void archiveerBestaandeOpdrachten(ScreeningOrganisatie screeningOrganisatie);

	void toevoegenTarief(CervixTarief tarief, Account account);

	void berekenEinddatumCervixLaboratoriumTarief(BMHKLaboratorium laboratorium);

	String toevoegenIndexatieTarief(CervixTarief nieuweTarief, Account account) throws JsonProcessingException;

	void verwijderCervixTarief(CervixTarief tarief, Account account);
}
