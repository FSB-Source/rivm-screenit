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

public class VerrichtingZoekObjectDto
{
	private TableResultOptionsDto resultOptions;

	private VerrichtingFilterDto verrichtingenZoekObject;

	public TableResultOptionsDto getResultOptions()
	{
		return resultOptions;
	}

	public void setResultOptions(TableResultOptionsDto resultOptions)
	{
		this.resultOptions = resultOptions;
	}

	public VerrichtingFilterDto getVerrichtingenZoekObject()
	{
		return verrichtingenZoekObject;
	}

	public void setVerrichtingenZoekObject(VerrichtingFilterDto verrichtingenZoekObject)
	{
		this.verrichtingenZoekObject = verrichtingenZoekObject;
	}
}
