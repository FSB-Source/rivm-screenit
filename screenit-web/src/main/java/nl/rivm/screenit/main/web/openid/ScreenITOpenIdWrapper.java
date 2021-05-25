package nl.rivm.screenit.main.web.openid;

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

import java.io.Serializable;
import java.util.List;

import nl.topicuszorg.hibernate.object.annot.objects.Transient;

import org.apache.commons.collections.CollectionUtils;

@Transient
public class ScreenITOpenIdWrapper implements Serializable
{

	private static final long serialVersionUID = 1L;

	private String volledigenaam;

	private String emailadres;

	private Long medewerkerId;

	private Long organisatieId;

	private String toegangslevel;

	private String organisatieType;

	private List<Long> zichtbareOrganisaties;

	public String getVolledigenaam()
	{
		return volledigenaam;
	}

	public void setVolledigenaam(String volledigenaam)
	{
		this.volledigenaam = volledigenaam;
	}

	public String getEmailadres()
	{
		return emailadres;
	}

	public void setEmailadres(String emailadres)
	{
		this.emailadres = emailadres;
	}

	public Long getMedewerkerId()
	{
		return medewerkerId;
	}

	public void setMedewerkerId(Long medewerkerId)
	{
		this.medewerkerId = medewerkerId;
	}

	public Long getOrganisatieId()
	{
		return organisatieId;
	}

	public void setOrganisatieId(Long organisatieId)
	{
		this.organisatieId = organisatieId;
	}

	public String getOrganisatieType()
	{
		return organisatieType;
	}

	public void setOrganisatieType(String organisatieType)
	{
		this.organisatieType = organisatieType;
	}

	public String getToegangslevel()
	{
		return toegangslevel;
	}

	public void setToegangslevel(String toegangslevel)
	{
		this.toegangslevel = toegangslevel;
	}

	public List<Long> getZichtbareOrganisaties()
	{
		return zichtbareOrganisaties;
	}

	public void setZichtbareOrganisaties(List<Long> zichtbareOrganisaties)
	{
		this.zichtbareOrganisaties = zichtbareOrganisaties;
	}

	public String getAlleZichtbareOrganisaties()
	{
		StringBuilder sb = new StringBuilder();

		if (CollectionUtils.isNotEmpty(zichtbareOrganisaties))
		{
			int count = 1;
			int size = zichtbareOrganisaties.size();
			for (Long id : zichtbareOrganisaties)
			{
				sb.append(id.toString());
				if (size != count)
				{
					sb.append(",");
				}
				count++;
			}
		}

		return sb.toString();
	}

}
