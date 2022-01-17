package nl.rivm.screenit.mamma.se.dto;

/*-
 * ========================LICENSE_START=================================
 * screenit-se-rest-bk
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

public class DagProductieDto
{
	private Integer ingeschrevenCount;

	private Integer onderzochtCount;

	private Integer afgerondCount;

	private Integer onderbrokenCount;

	private Integer onvolledigCount;

	private Integer afwijkingenCount;

	public Integer getIngeschrevenCount()
	{
		return ingeschrevenCount;
	}

	public DagProductieDto setIngeschrevenCount(Integer ingeschrevenCount)
	{
		this.ingeschrevenCount = ingeschrevenCount;
		return this;
	}

	public Integer getOnderzochtCount()
	{
		return onderzochtCount;
	}

	public DagProductieDto setOnderzochtCount(Integer onderzochtCount)
	{
		this.onderzochtCount = onderzochtCount;
		return this;
	}

	public Integer getAfgerondCount()
	{
		return afgerondCount;
	}

	public DagProductieDto setAfgerondCount(Integer afgerondCount)
	{
		this.afgerondCount = afgerondCount;
		return this;
	}

	public Integer getOnderbrokenCount()
	{
		return onderbrokenCount;
	}

	public DagProductieDto setOnderbrokenCount(Integer onderbrokenCount)
	{
		this.onderbrokenCount = onderbrokenCount;
		return this;
	}

	public Integer getOnvolledigCount()
	{
		return onvolledigCount;
	}

	public DagProductieDto setOnvolledigCount(Integer onvolledigCount)
	{
		this.onvolledigCount = onvolledigCount;
		return this;
	}

	public Integer getAfwijkingenCount()
	{
		return afwijkingenCount;
	}

	public DagProductieDto setAfwijkingenCount(Integer afwijkingenCount)
	{
		this.afwijkingenCount = afwijkingenCount;
		return this;
	}
}
