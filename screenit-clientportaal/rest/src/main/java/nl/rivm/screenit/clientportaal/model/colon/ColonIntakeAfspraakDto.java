package nl.rivm.screenit.clientportaal.model.colon;

/*-
 * ========================LICENSE_START=================================
 * screenit-clientportaal
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

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import nl.rivm.screenit.clientportaal.model.ClientportaalBaseDto;
import nl.rivm.screenit.model.colon.ColonIntakeAfspraak;
import nl.rivm.screenit.model.colon.enums.ColonConclusieType;
import nl.rivm.screenit.model.colon.planning.AfspraakStatus;
import nl.rivm.screenit.util.AdresUtil;
import nl.rivm.screenit.util.DateUtil;

@Getter
@Setter
@AllArgsConstructor
@NoArgsConstructor
public class ColonIntakeAfspraakDto extends ClientportaalBaseDto
{

	private String weergaveAfspraakmoment;

	private String naamInstelling;

	private String adresString;

	private boolean afspraakAfgezegd;

	private boolean andereIntakelocatieOpVerzoekClient;

	public ColonIntakeAfspraakDto(ColonIntakeAfspraak intakeAfspraak)
	{
		this.weergaveAfspraakmoment = DateUtil.getWeergaveDatumClientportaal(DateUtil.toLocalDateTime(intakeAfspraak.getStartTime()));
		this.naamInstelling = intakeAfspraak.getLocation().getColoscopieCentrum().getNaam();
		this.afspraakAfgezegd = intakeAfspraak.getAfzegDatum() != null;
		this.adresString = AdresUtil.getVolledigeAdresString(intakeAfspraak.getLocation().getColoscopieCentrum().getEersteAdres());
		this.andereIntakelocatieOpVerzoekClient = bepaalAndereIntakeLocatieOpVerzoekClient(intakeAfspraak);
	}

	private boolean bepaalAndereIntakeLocatieOpVerzoekClient(ColonIntakeAfspraak afspraak)
	{
		var conclusie = afspraak.getConclusie();
		return afspraak.getStatus() == AfspraakStatus.UITGEVOERD && conclusie != null
			&& conclusie.getType() == ColonConclusieType.CLIENT_WIL_ANDERE_INTAKELOKATIE;
	}
}
