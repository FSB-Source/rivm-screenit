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

import java.util.ArrayList;
import java.util.List;

import nl.rivm.screenit.huisartsenportaal.model.enums.Recht;

import com.fasterxml.jackson.databind.annotation.JsonSerialize;

@JsonSerialize
public class MederwerkerDto
{
	private Long id;

	private List<Recht> rollen = new ArrayList<Recht>();

	private boolean isOvereenkomstGetekend;

	public Long getId()
	{
		return id;
	}

	public void setId(Long id)
	{
		this.id = id;
	}

	public List<Recht> getRollen()
	{
		return rollen;
	}

	public void setRollen(List<Recht> rollen)
	{
		this.rollen = rollen;
	}

	public boolean isOvereenkomstGetekend() {
		return isOvereenkomstGetekend;
	}

	public void setOvereenkomstGetekend(boolean overeenkomstGetekend) {
		isOvereenkomstGetekend = overeenkomstGetekend;
	}
}
