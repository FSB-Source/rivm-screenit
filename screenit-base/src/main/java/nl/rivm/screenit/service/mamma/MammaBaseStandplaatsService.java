package nl.rivm.screenit.service.mamma;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
 * %%
 * Copyright (C) 2012 - 2020 Facilitaire Samenwerking Bevolkingsonderzoek
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

import java.util.Date;
import java.util.List;

import nl.rivm.screenit.dto.mamma.afspraken.IMammaAfspraakWijzigenFilter;
import nl.rivm.screenit.dto.mamma.afspraken.MammaStandplaatsPeriodeMetAfstandDto;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.ScreeningOrganisatie;
import nl.rivm.screenit.model.UploadDocument;
import nl.rivm.screenit.model.mamma.MammaStandplaats;
import nl.rivm.screenit.model.mamma.MammaStandplaatsLocatie;
import nl.rivm.screenit.model.mamma.MammaStandplaatsOpmerking;
import nl.rivm.screenit.model.mamma.MammaStandplaatsPeriode;

public interface MammaBaseStandplaatsService
{
	boolean saveOrUpdateStandplaats(MammaStandplaats standplaats, InstellingGebruiker ingelogdeGebruiker);

	List<MammaStandplaats> zoekStandplaatsen(MammaStandplaats zoekObject, int first, int count, String sortProperty, boolean asc);

	long countStandplaatsen(MammaStandplaats zoekObject);

	long countActieveStandplaatsPeriodes(MammaStandplaats standplaats);

	boolean saveOrUpdateStandplaatsOpmerking(MammaStandplaatsOpmerking opmerking, MammaStandplaats standplaats, InstellingGebruiker loggedInInstellingGebruiker);

	boolean saveOrUpdateStandplaatsLocatie(MammaStandplaatsLocatie locatie, UploadDocument documentFromSelectedFile, MammaStandplaats standplaats,
		InstellingGebruiker ingelogdeGebruiker);

	List<MammaStandplaats> getActieveStandplaatsen(ScreeningOrganisatie ingelogdNamensRegio);

	String magStandplaatsInactiveren(MammaStandplaats standplaats);

	List<MammaStandplaatsPeriodeMetAfstandDto> getStandplaatsPeriodeMetAfstandDtos(Client client, IMammaAfspraakWijzigenFilter filter);

	MammaStandplaats getStandplaatsMetPostcode(Client client);

	MammaStandplaatsPeriode getEerstvolgendeStandplaatsPeriode(MammaStandplaats standplaats);

	String controleerUitnodigingenNaVeranderingLocatie(MammaStandplaats standplaats, String initieelAdres, Date initieelStartDatum, Date initieelEindDatum);

	String controleerUitnodigingenNaVeranderingTijdelijkeLocatie(MammaStandplaats standplaats, String initieelAdres, Date initieelStartDatum, Date initieelEindDatum);

	Double bepaalAfstand(MammaStandplaats standplaats, Client client);

	MammaStandplaatsLocatie getStandplaatsLocatie(MammaStandplaats standplaats, Date datum);

}
