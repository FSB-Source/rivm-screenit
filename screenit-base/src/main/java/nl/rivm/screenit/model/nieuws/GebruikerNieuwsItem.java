package nl.rivm.screenit.model.nieuws;

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

import java.util.Date;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.ManyToOne;
import javax.persistence.Table;
import javax.persistence.Temporal;
import javax.persistence.TemporalType;

import nl.rivm.screenit.model.Gebruiker;
import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject;

import org.hibernate.annotations.Cache;
import org.hibernate.annotations.CacheConcurrencyStrategy;
import org.hibernate.annotations.Cascade;
import org.hibernate.annotations.CascadeType;
import org.hibernate.envers.Audited;

@Entity
@Table(schema = "algemeen")
@Cache(usage = CacheConcurrencyStrategy.NONSTRICT_READ_WRITE, region = "screenit.cache")
@Audited
public class GebruikerNieuwsItem extends AbstractHibernateObject
{
	
	private static final long serialVersionUID = 1L;

	@ManyToOne
	@Cascade(CascadeType.SAVE_UPDATE)
	private Gebruiker gebruiker;

	@ManyToOne
	@Cascade(CascadeType.SAVE_UPDATE)
	private NieuwsItem nieuwsItem;

	@Column(nullable = false)
	@Temporal(TemporalType.TIMESTAMP)
	private Date nietZichtbaarVanaf;

	public Gebruiker getGebruiker()
	{
		return gebruiker;
	}

	public void setGebruiker(Gebruiker gebruiker)
	{
		this.gebruiker = gebruiker;
	}

	public NieuwsItem getNieuwsItem()
	{
		return nieuwsItem;
	}

	public void setNieuwsItem(NieuwsItem nieuwsItem)
	{
		this.nieuwsItem = nieuwsItem;
	}

	public Date getNietZichtbaarVanaf()
	{
		return nietZichtbaarVanaf;
	}

	public void setNietZichtbaarVanaf(Date nietZichtbaarVanaf)
	{
		this.nietZichtbaarVanaf = nietZichtbaarVanaf;
	}
}
