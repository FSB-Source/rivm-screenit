package nl.rivm.screenit.clientportaal.mappers;

/*-
 * ========================LICENSE_START=================================
 * screenit-clientportaal
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

import nl.rivm.screenit.clientportaal.mappers.config.ScreenitMapperConfig;
import nl.rivm.screenit.clientportaal.model.ClientDto;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.util.AdresUtil;
import nl.rivm.screenit.util.DateUtil;
import nl.rivm.screenit.util.NaamUtil;
import nl.topicuszorg.organisatie.model.Adres;

import org.mapstruct.Mapper;
import org.mapstruct.Mapping;
import org.mapstruct.Mappings;
import org.mapstruct.Named;

@Mapper(config = ScreenitMapperConfig.class, uses = TijdelijkAdresMapper.class)
public interface ClientMapper
{
	@Mappings({
		@Mapping(source = "client", target = "voorletters", qualifiedByName = "conversionVoorletters"),
		@Mapping(source = "client", target = "aanspreekTussenvoegselEnAchternaam", qualifiedByName = "conversionAanspreekTussenvoegsel"),
		@Mapping(source = "persoon.bsn", target = "bsn"),
		@Mapping(source = "client", target = "geboortedatumDisplay", qualifiedByName = "conversionGeboortedatum"),
		@Mapping(source = "persoon.geslacht", target = "geslacht"),
		@Mapping(source = "persoon.gbaAdres", target = "adresTekst", qualifiedByName = "conversionAdres"),
		@Mapping(source = "persoon.emailadres", target = "emailadres"),
		@Mapping(source = "persoon.tijdelijkAdres", target = "tijdelijkAdresTekst", qualifiedByName = "conversionAdres"),
		@Mapping(source = "persoon.tijdelijkAdres", target = "tijdelijkAdres"),
		@Mapping(source = "persoon.telefoonnummer1", target = "telefoonnummer1"),
		@Mapping(source = "persoon.telefoonnummer2", target = "telefoonnummer2")
	})
	ClientDto clientToDto(Client client);

	@Named("conversionVoorletters")
	default String conversionVoorletters(Client client)
	{
		return NaamUtil.getVoorlettersClient(client);
	}

	@Named("conversionAanspreekTussenvoegsel")
	default String conversionAanspreekTussenvoegsel(Client client)
	{
		return NaamUtil.getAanspreekTussenvoegselEnAchternaam(client);
	}

	@Named("conversionGeboortedatum")
	default String conversionGeboortedatum(Client client)
	{
		return DateUtil.getGeboortedatum(client);
	}

	@Named("conversionAdres")
	default String conversionAdres(Adres adres)
	{
		return AdresUtil.getVolledigeAdresString(adres);
	}

}
