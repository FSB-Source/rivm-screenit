package nl.rivm.screenit.model.logging;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
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

import java.util.Date;
import java.util.List;

import javax.persistence.CascadeType;
import javax.persistence.CollectionTable;
import javax.persistence.ElementCollection;
import javax.persistence.Entity;
import javax.persistence.EnumType;
import javax.persistence.Enumerated;
import javax.persistence.FetchType;
import javax.persistence.Index;
import javax.persistence.ManyToOne;
import javax.persistence.OneToOne;
import javax.persistence.Table;
import javax.persistence.Temporal;
import javax.persistence.TemporalType;

import lombok.Getter;
import lombok.Setter;

import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.Gebruiker;
import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.model.mamma.MammaScreeningsEenheid;
import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject;

import org.hibernate.annotations.Cache;
import org.hibernate.annotations.CacheConcurrencyStrategy;

import com.fasterxml.jackson.annotation.JsonIgnore;

@Setter
@Getter
@Entity
@Table(schema = "gedeeld", indexes = { @Index(name = "logGebeurtenis", columnList = "logGebeurtenis"), @Index(name = "gebeurtenisDatum", columnList = "gebeurtenisDatum") })
@Cache(usage = CacheConcurrencyStrategy.NONSTRICT_READ_WRITE, region = "screenit.cache")
public class LogRegel extends AbstractHibernateObject
{
	@Enumerated(EnumType.STRING)
	private LogGebeurtenis logGebeurtenis;

	@Temporal(TemporalType.TIMESTAMP)
	private Date gebeurtenisDatum;

	@ManyToOne(fetch = FetchType.LAZY)
	@JsonIgnore 
	private Gebruiker gebruiker;

	@ManyToOne(fetch = FetchType.LAZY)
	@JsonIgnore 
	private MammaScreeningsEenheid screeningsEenheid;

	@ManyToOne(fetch = FetchType.LAZY)
	@JsonIgnore 
	private InstellingGebruiker ingelogdeGebruiker;

	@ManyToOne(fetch = FetchType.LAZY)
	@JsonIgnore 
	private Client client;

	@OneToOne(cascade = CascadeType.ALL, fetch = FetchType.LAZY)
	private LogEvent logEvent;

	@ElementCollection(targetClass = Bevolkingsonderzoek.class)
	@Enumerated(EnumType.STRING)
	@CollectionTable(schema = "gedeeld", name = "log_regel_bevolkingsonderzoeken")
	private List<Bevolkingsonderzoek> bevolkingsonderzoeken;

	public String getAfkortingen()
	{
		List<Bevolkingsonderzoek> bvoList = bevolkingsonderzoeken;
		String bvoString = "";
		for (int i = 0; i < bvoList.size(); i++)
		{
			if (i != bvoList.size() - 1)
			{
				bvoString += bvoList.get(i).getAfkorting() + ", ";
			}
			else
			{
				bvoString += bvoList.get(i).getAfkorting();
			}
		}
		return bvoString;
	}

}
