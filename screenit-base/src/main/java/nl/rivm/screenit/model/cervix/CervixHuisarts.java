package nl.rivm.screenit.model.cervix;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
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

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.EnumType;
import javax.persistence.Enumerated;
import javax.persistence.FetchType;
import javax.persistence.OneToMany;
import javax.persistence.OneToOne;
import javax.persistence.Table;
import javax.persistence.Temporal;
import javax.persistence.TemporalType;
import javax.persistence.UniqueConstraint;

import nl.rivm.screenit.model.Instelling;
import nl.rivm.screenit.model.OrganisatieType;
import nl.rivm.screenit.model.cervix.enums.CervixHuisartsAanmeldStatus;

import org.hibernate.envers.Audited;

@Entity
@Table(
	schema = "cervix",
	name = "huisarts",
	uniqueConstraints = { @UniqueConstraint(columnNames = "postadres") })
@Audited
public class CervixHuisarts extends Instelling implements ICervixHuisartsportaalObject
{

	private static final long serialVersionUID = 1L;

	private Long huisartsportaalId;

	@Enumerated(EnumType.STRING)
	private CervixHuisartsAanmeldStatus aanmeldStatus;

	@OneToOne(fetch = FetchType.LAZY)
	private CervixHuisartsAdres postadres;

	@OneToMany(fetch = FetchType.LAZY, mappedBy = "huisarts")
	private List<CervixHuisartsLocatie> huisartsLocaties = new ArrayList<>();

	@Column(length = 255)
	private String extraEmails;

	@Temporal(TemporalType.TIMESTAMP)
	private Date mutatiedatum;

	@Column(length = 255)
	private String gebruikersnaamHuisartsenPortaal;

	public CervixHuisarts()
	{
		setOrganisatieType(OrganisatieType.HUISARTS);
	}

	public CervixHuisartsAanmeldStatus getAanmeldStatus()
	{
		return aanmeldStatus;
	}

	public void setAanmeldStatus(CervixHuisartsAanmeldStatus aanmeldStatus)
	{
		this.aanmeldStatus = aanmeldStatus;
	}

	public CervixHuisartsAdres getPostadres()
	{
		return postadres;
	}

	public void setPostadres(CervixHuisartsAdres postadres)
	{
		this.postadres = postadres;
	}

	public List<CervixHuisartsLocatie> getHuisartsLocaties()
	{
		return huisartsLocaties;
	}

	public void setHuisartsLocaties(List<CervixHuisartsLocatie> locaties)
	{
		this.huisartsLocaties = locaties;
	}

	@Override
	public Long getHuisartsportaalId()
	{
		return huisartsportaalId;
	}

	@Override
	public void setHuisartsportaalId(Long huisartsportaalId)
	{
		this.huisartsportaalId = huisartsportaalId;
	}

	@Override
	public void setScreenitId(Long id)
	{

		if (id != null)
		{
			setId(id);
		}
	}

	@Override
	public Long getScreenitId()
	{
		return getId();
	}

	public String getExtraEmails()
	{
		return extraEmails;
	}

	public void setExtraEmails(String extraEmails)
	{
		this.extraEmails = extraEmails;
	}

	@Override
	public Date getMutatiedatum()
	{
		return mutatiedatum;
	}

	@Override
	public void setMutatiedatum(Date mutatiedatum)
	{
		this.mutatiedatum = mutatiedatum;
	}

	public String getGebruikersnaamHuisartsenPortaal()
	{
		return gebruikersnaamHuisartsenPortaal;
	}

	public void setGebruikersnaamHuisartsenPortaal(String gebruikersnaamHuisartsenPortaal)
	{
		this.gebruikersnaamHuisartsenPortaal = gebruikersnaamHuisartsenPortaal;
	}

}
