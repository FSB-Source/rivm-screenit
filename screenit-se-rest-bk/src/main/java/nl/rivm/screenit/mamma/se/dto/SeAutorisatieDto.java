package nl.rivm.screenit.mamma.se.dto;

/*-
 * ========================LICENSE_START=================================
 * screenit-se-rest-bk
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

import com.fasterxml.jackson.annotation.JsonFilter;
import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
@JsonFilter("autorisatieFilter")
public class SeAutorisatieDto
{
	private String displayName;

	private String username;

	private String medewerkercode;

	private String seCode;

	private String seNaam;

	private long instellingGebruikerId;

	private boolean inschrijven;

	private boolean onderzoeken;

	private boolean signaleren;

	private boolean kwaliteitsopname;

	private boolean connectiestatus;

	private SERechtDto inschrijvenRecht;

	private SERechtDto onderzoekenRecht;

	private SERechtDto signalerenRecht;

	private SERechtDto kwaliteitsopnameRecht;

	private SERechtDto connectiestatusRecht;

	private String navigatie;

	private int seMaxOfflineInlogPeriode;
}
