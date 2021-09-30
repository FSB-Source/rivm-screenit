package nl.rivm.screenit.huisartsenportaal.dto;

/*-
 * ========================LICENSE_START=================================
 * screenit-huisartsenportaal
 * %%
 * Copyright (C) 2016 - 2021 Facilitaire Samenwerking Bevolkingsonderzoek
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

public class BetalingFilterDto
{
	private LocatieDto locatie;

	private String clientNaam;

	private String betalingskenmerk;

	private Date betalingsdatumVanaf;

	private Date betalingsdatumTotenMet;

	private boolean isAlleenZonderBetalingskenmerk;

	public LocatieDto getLocatie() {
		return locatie;
	}

	public void setLocatie(LocatieDto locatie) {
		this.locatie = locatie;
	}

	public String getClientNaam() {
		return clientNaam;
	}

	public void setClientNaam(String clientNaam) {
		this.clientNaam = clientNaam;
	}

	public String getBetalingskenmerk() {
		return betalingskenmerk;
	}

	public void setBetalingskenmerk(String betalingskenmerk) {
		this.betalingskenmerk = betalingskenmerk;
	}

	public Date getBetalingsdatumVanaf() {
		return betalingsdatumVanaf;
	}

	public void setBetalingsdatumVanaf(Date betalingsdatumVanaf) {
		this.betalingsdatumVanaf = betalingsdatumVanaf;
	}

	public Date getBetalingsdatumTotenMet() {
		return betalingsdatumTotenMet;
	}

	public void setBetalingsdatumTotenMet(Date betalingsdatumTotenMet) {
		this.betalingsdatumTotenMet = betalingsdatumTotenMet;
	}

	public boolean isAlleenZonderBetalingskenmerk() {
		return isAlleenZonderBetalingskenmerk;
	}

	public void setAlleenZonderBetalingskenmerk(boolean alleenZonderBetalingskenmerk) {
		isAlleenZonderBetalingskenmerk = alleenZonderBetalingskenmerk;
	}
}
