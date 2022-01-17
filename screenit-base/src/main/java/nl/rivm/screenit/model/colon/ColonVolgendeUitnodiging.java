package nl.rivm.screenit.model.colon;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
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

import java.util.Date;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Index;
import javax.persistence.ManyToOne;
import javax.persistence.OneToOne;
import javax.persistence.Table;
import javax.persistence.Temporal;
import javax.persistence.TemporalType;
import javax.persistence.UniqueConstraint;

import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject;

import org.hibernate.annotations.Cache;
import org.hibernate.annotations.CacheConcurrencyStrategy;
import org.hibernate.envers.Audited;
import org.hibernate.envers.NotAudited;

@Entity
@Table(schema = "colon", name = "volgende_uitnodiging", uniqueConstraints = { @UniqueConstraint(columnNames = "dossier") }, indexes = { @Index(columnList = "peildatum") })
@Cache(usage = CacheConcurrencyStrategy.NONSTRICT_READ_WRITE, region = "screenit.cache")
@Audited
public class ColonVolgendeUitnodiging extends AbstractHibernateObject
{
	@Column(nullable = false)
	@Temporal(TemporalType.DATE)
	private Date peildatum;

	@ManyToOne(optional = false)
	private ColonUitnodigingsinterval interval;

	@OneToOne(optional = false)
	@NotAudited
	private ColonDossier dossier;

	public Date getPeildatum()
	{
		return peildatum;
	}

	public void setPeildatum(Date peildatum)
	{
		this.peildatum = peildatum;
	}

	public ColonUitnodigingsinterval getInterval()
	{
		return interval;
	}

	public void setInterval(ColonUitnodigingsinterval interval)
	{
		this.interval = interval;
	}

	public ColonDossier getDossier()
	{
		return dossier;
	}

	public void setDossier(ColonDossier dossier)
	{
		this.dossier = dossier;
	}
}
