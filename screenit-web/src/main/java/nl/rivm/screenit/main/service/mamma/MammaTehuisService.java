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

import java.util.List;
import java.util.concurrent.Callable;
import java.util.concurrent.atomic.AtomicInteger;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.ScreeningOrganisatie;
import nl.rivm.screenit.model.mamma.MammaTehuis;
import nl.rivm.screenit.model.mamma.MammaTehuisOpmerking;

public interface MammaTehuisService
{
	List<IMammaTehuisDto> zoekTehuizen(MammaTehuis tehuis, ScreeningOrganisatie screeningOrganisatie, int first, int count, String sortProperty, boolean asc,
		Callable<IMammaTehuisDto> dtoFactory);

	long countTehuizen(MammaTehuis tehuis, ScreeningOrganisatie screeningOrganisatie);

	void deactiveerTehuis(MammaTehuis tehuis, InstellingGebruiker instellingGebruiker);

	boolean saveOrUpdateTehuisOpmerking(MammaTehuisOpmerking opmerking, MammaTehuis tehuis, InstellingGebruiker instellingGebruiker);

	void uitnodigen(MammaTehuis tehuis, AtomicInteger aantalClientenMetProjectBrief, AtomicInteger aantalClientenMetBrieven,
					AtomicInteger aantalClientenMetSuspectBrieven, InstellingGebruiker ingelogdeInstellingGebruiker);

    List<String> koppelClient(MammaTehuis tehuis, Client client);

    List<String> ontkoppelClient(MammaTehuis tehuis, Client client);
}
