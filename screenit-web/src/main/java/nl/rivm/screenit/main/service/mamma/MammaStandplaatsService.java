package nl.rivm.screenit.main.service.mamma;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
 * %%
 * Copyright (C) 2012 - 2025 Facilitaire Samenwerking Bevolkingsonderzoek
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

import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.UploadDocument;
import nl.rivm.screenit.model.mamma.MammaStandplaats;
import nl.rivm.screenit.model.mamma.MammaStandplaatsLocatie;
import nl.rivm.screenit.model.mamma.MammaStandplaatsOpmerking;

import com.google.common.collect.Range;

public interface MammaStandplaatsService
{
	boolean saveOrUpdateStandplaats(MammaStandplaats standplaats, InstellingGebruiker ingelogdeGebruiker);

	long countActieveStandplaatsPeriodes(MammaStandplaats standplaats);

	boolean saveOrUpdateStandplaatsOpmerking(MammaStandplaatsOpmerking opmerking, MammaStandplaats standplaats, InstellingGebruiker loggedInInstellingGebruiker);

	boolean saveOrUpdateStandplaatsLocatie(MammaStandplaatsLocatie locatie, UploadDocument documentFromSelectedFile, MammaStandplaats standplaats,
		InstellingGebruiker ingelogdeGebruiker, String oudeAdres, Range<Date> oudePeriode);

	String magStandplaatsInactiveren(MammaStandplaats standplaats);

	MammaStandplaats getStandplaatsMetPostcode(Client client);

	String controleerUitnodigingenNaVeranderingLocatie(MammaStandplaats standplaats);

	String controleerUitnodigingenNaVeranderingTijdelijkeLocatie(MammaStandplaats standplaats, String oudeAdres, Range<Date> oudePeriode);

	boolean heeftActieveOpmerking(MammaStandplaats standplaats);
}
