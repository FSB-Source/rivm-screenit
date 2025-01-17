package nl.rivm.screenit.clientportaal.model.mamma;

/*-
 * ========================LICENSE_START=================================
 * screenit-clientportaal
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

import java.time.LocalDate;
import java.util.ArrayList;
import java.util.List;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import nl.rivm.screenit.dto.mamma.afspraken.IMammaAfspraakWijzigenFilter;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.mamma.MammaScreeningsEenheid;
import nl.rivm.screenit.model.mamma.MammaStandplaats;
import nl.rivm.screenit.model.mamma.enums.MammaVerzettenReden;

import org.apache.commons.lang.StringUtils;

@Getter
@Setter
@NoArgsConstructor
public class MammaAfspraakWijzigenFilterDto implements IMammaAfspraakWijzigenFilter
{

	private LocalDate vanaf;

	private LocalDate totEnMet;

	private List<MammaStandplaats> standplaatsen = new ArrayList<>();

	private List<MammaScreeningsEenheid> screeningsEenheden = new ArrayList<>();

	private Client client;

	private String plaats;

	private Integer afstand;

	private Boolean extraOpties;

	private boolean buitenRegio;

	private MammaVerzettenReden verzettenReden;

	public static MammaAfspraakWijzigenFilterDto filterVoorOphalenAfsprakenBinnenPeriode(Client client, String plaats, String afstand, LocalDate vanaf, LocalDate totEnMet)
	{
		MammaAfspraakWijzigenFilterDto filterDto = new MammaAfspraakWijzigenFilterDto();
		filterDto.setClient(client);
		filterDto.setVanaf(vanaf);
		filterDto.setTotEnMet(totEnMet);
		if (StringUtils.isNotBlank(plaats))
		{
			filterDto.setPlaats(plaats);
		}
		if (StringUtils.isNotBlank(afstand))
		{
			filterDto.setAfstand(Integer.valueOf(afstand));
		}
		filterDto.setExtraOpties(true);
		filterDto.setBuitenRegio(false);
		return filterDto;
	}

	public static MammaAfspraakWijzigenFilterDto filterVoorOphalenStandplaatsenViaPlaatsOfAfstand(String plaats, String afstand, LocalDate vanaf, LocalDate totEnMet)
	{
		MammaAfspraakWijzigenFilterDto filterDto = new MammaAfspraakWijzigenFilterDto();
		filterDto.setVanaf(vanaf);
		filterDto.setTotEnMet(totEnMet);
		if (StringUtils.isNotBlank(plaats))
		{
			filterDto.setPlaats(plaats);
		}
		if (StringUtils.isNotBlank(afstand))
		{
			filterDto.setAfstand(Integer.valueOf(afstand));
		}
		filterDto.setExtraOpties(false);
		filterDto.setBuitenRegio(true);
		return filterDto;
	}
}
