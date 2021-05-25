package nl.rivm.screenit.service;

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

import java.io.File;
import java.io.IOException;
import java.io.OutputStream;
import java.text.ParseException;
import java.time.LocalDate;
import java.util.List;

import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.GbaPersoon;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.gba.Land;
import nl.rivm.screenit.service.impl.ImportBvoViaCsv;
import nl.topicuszorg.patientregistratie.persoonsgegevens.model.Persoon;

public interface TestService
{

	Client getClientByBsn(String bsn);

    GbaPersoon maakPersoon(LocalDate geboorteDatum);

	Client maakClient(GbaPersoon persoon);

	void createGbaFile(GbaPersoon persoon, File vo107template, OutputStream outputStream);

	void importClientenViaCsv(File file, ImportBvoViaCsv importBvoViaCsv) throws IOException, ParseException;

	boolean verwijderClientContacten(Client client, Bevolkingsonderzoek... onderzoeken);

	boolean verwijderClientContacten(Client client, boolean isDossierVerwijderdDK, boolean isDossierVerwijderdBMHK, boolean isDossierVerwijderdBK);

	boolean verwijderClientContacten(Client client, List<Bevolkingsonderzoek> onderzoeken);

	Land getGbaLand();

	void projectenVerwijderen(Client client);
}
