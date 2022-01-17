package nl.rivm.screenit.model.cervix;

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

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.Index;
import javax.persistence.OneToMany;
import javax.persistence.OneToOne;
import javax.persistence.Table;
import javax.persistence.Temporal;
import javax.persistence.TemporalType;
import javax.persistence.UniqueConstraint;

import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.Dossier;
import nl.rivm.screenit.model.cervix.cis.CervixCISHistorie;

import org.hibernate.annotations.Cache;
import org.hibernate.annotations.CacheConcurrencyStrategy;
import org.hibernate.envers.Audited;
import org.hibernate.envers.NotAudited;

@Entity
@Table(
	schema = "cervix",
	name = "dossier",
	indexes = { @Index(name = "idx_CERVIX_DOSSIER_VOLGENDE_RONDE_VANAF", columnList = "volgendeRondeVanaf"), @Index(name = "idx_CERVIX_DOSSIER_STATUS", columnList = "status") },
	uniqueConstraints = { @UniqueConstraint(columnNames = "laatste_screening_ronde"), @UniqueConstraint(columnNames = "laatste_afmelding") })
@Audited
public class CervixDossier extends Dossier<CervixScreeningRonde, CervixAfmelding>
{

	private static final long serialVersionUID = 1L;

	@OneToOne(mappedBy = "cervixDossier", optional = false)
	private Client client;

	@OneToOne(fetch = FetchType.LAZY)
	@NotAudited
	private CervixCISHistorie cisHistorie;

	@OneToMany(fetch = FetchType.LAZY, mappedBy = "dossier")
	@Cache(usage = CacheConcurrencyStrategy.NONSTRICT_READ_WRITE, region = "screenit.cache")
	private List<CervixScreeningRonde> screeningRondes = new ArrayList<>();

	@OneToOne(optional = true, fetch = FetchType.EAGER)
	private CervixScreeningRonde laatsteScreeningRonde;

	@OneToMany(mappedBy = "dossier", fetch = FetchType.LAZY)
	@Cache(usage = CacheConcurrencyStrategy.NONSTRICT_READ_WRITE, region = "screenit.cache")
	private List<CervixAfmelding> afmeldingen = new ArrayList<>();

	@OneToOne(optional = true, fetch = FetchType.EAGER)
	private CervixAfmelding laatsteAfmelding;

	@Temporal(TemporalType.DATE)
	private Date volgendeRondeVanaf;

	@Override
	public Client getClient()
	{
		return client;
	}

	@Override
	public void setClient(Client client)
	{
		this.client = client;
	}

	@Override
	public List<CervixScreeningRonde> getScreeningRondes()
	{
		return screeningRondes;
	}

	@Override
	public void setScreeningRondes(List<CervixScreeningRonde> screeningRondes)
	{
		this.screeningRondes = screeningRondes;
	}

	@Override
	public CervixScreeningRonde getLaatsteScreeningRonde()
	{
		return laatsteScreeningRonde;
	}

	@Override
	public void setLaatsteScreeningRonde(CervixScreeningRonde laatsteScreeningRonde)
	{
		this.laatsteScreeningRonde = laatsteScreeningRonde;
	}

	@Override
	public List<CervixAfmelding> getAfmeldingen()
	{
		return afmeldingen;
	}

	@Override
	public void setAfmeldingen(List<CervixAfmelding> afmeldingen)
	{
		this.afmeldingen = afmeldingen;
	}

	@Override
	public CervixAfmelding getLaatsteAfmelding()
	{
		return laatsteAfmelding;
	}

	@Override
	public void setLaatsteAfmelding(CervixAfmelding laatsteAfmelding)
	{
		this.laatsteAfmelding = laatsteAfmelding;
	}

	public Date getVolgendeRondeVanaf()
	{
		return volgendeRondeVanaf;
	}

	public void setVolgendeRondeVanaf(Date volgendeRondeVanaf)
	{
		this.volgendeRondeVanaf = volgendeRondeVanaf;
	}

	public CervixCISHistorie getCisHistorie()
	{
		return cisHistorie;
	}

	public void setCisHistorie(CervixCISHistorie cisHistorie)
	{
		this.cisHistorie = cisHistorie;
	}
}
