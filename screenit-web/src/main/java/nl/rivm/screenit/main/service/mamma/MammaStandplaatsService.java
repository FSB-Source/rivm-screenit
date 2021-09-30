package nl.rivm.screenit.main.service.mamma;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
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

import java.util.Date;
import java.util.List;

import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.UploadDocument;
import nl.rivm.screenit.model.mamma.MammaStandplaats;
import nl.rivm.screenit.model.mamma.MammaStandplaatsLocatie;
import nl.rivm.screenit.model.mamma.MammaStandplaatsOpmerking;

public interface MammaStandplaatsService
{
	boolean saveOrUpdateStandplaats(MammaStandplaats standplaats, InstellingGebruiker ingelogdeGebruiker);

	List<MammaStandplaats> zoekStandplaatsen(MammaStandplaats zoekObject, int first, int count, String sortProperty, boolean asc);

	long countStandplaatsen(MammaStandplaats zoekObject);

	long countActieveStandplaatsPeriodes(MammaStandplaats standplaats);

	boolean saveOrUpdateStandplaatsOpmerking(MammaStandplaatsOpmerking opmerking, MammaStandplaats standplaats, InstellingGebruiker loggedInInstellingGebruiker);

	boolean saveOrUpdateStandplaatsLocatie(MammaStandplaatsLocatie locatie, UploadDocument documentFromSelectedFile, MammaStandplaats standplaats,
		InstellingGebruiker ingelogdeGebruiker, String initieelAdres, Date initieelStartDatum, Date initieelEindDatum);

	String magStandplaatsInactiveren(MammaStandplaats standplaats);

	MammaStandplaats getStandplaatsMetPostcode(Client client);

	String controleerUitnodigingenNaVeranderingLocatie(MammaStandplaats standplaats, String initieelAdres, Date initieelStartDatum, Date initieelEindDatum);

	String controleerUitnodigingenNaVeranderingTijdelijkeLocatie(MammaStandplaats standplaats, String initieelAdres, Date initieelStartDatum, Date initieelEindDatum);

	Double bepaalAfstand(MammaStandplaats standplaats, Client client);

	MammaStandplaatsLocatie getStandplaatsLocatie(MammaStandplaats standplaats, Date datum);

}
