package nl.rivm.screenit.huisartsenportaal.model;

/*-
 * ========================LICENSE_START=================================
 * screenit-huisartsenportaal
 * %%
 * Copyright (C) 2016 - 2022 Facilitaire Samenwerking Bevolkingsonderzoek
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
import javax.persistence.FetchType;
import javax.persistence.Index;
import javax.persistence.ManyToOne;
import javax.persistence.OneToMany;
import javax.persistence.Table;
import javax.persistence.Temporal;
import javax.persistence.TemporalType;

import org.hibernate.envers.Audited;

@Entity
@Audited
@Table(name = "verrichting", indexes = { @Index(name = "idx_VERRICHTING_VERRICHTINGS_DATUM", columnList = "verrichtingsDatum") })
public class Verrichting extends AbstractReferenceObject
{
	@Column(nullable = false)
	private String regio;

	@Column(nullable = false)
	private String monsterId;

	@Column(nullable = false)
	@Temporal(TemporalType.TIMESTAMP)
	private Date verrichtingsDatum;

	@Column
	@Temporal(TemporalType.TIMESTAMP)
	private Date datumUitstrijkje;

	@Column
	@Temporal(TemporalType.TIMESTAMP)
	private Date formulierOntvangstDatum;

	@ManyToOne(fetch = FetchType.LAZY, optional = false)
	private Locatie huisartsLocatie;

	@ManyToOne(fetch = FetchType.LAZY)
	private Huisarts huisarts;

	@Column(nullable = false)
	private String clientNaam;

	@OneToMany(mappedBy = "verrichting", fetch = FetchType.LAZY)
	private List<Betaling> betalingen = new ArrayList<>();

	public List<Betaling> getBetalingen()
	{
		return betalingen;
	}

	public void setBetalingen(List<Betaling> betalingen)
	{
		this.betalingen = betalingen;
	}

	public String getRegio()
	{
		return regio;
	}

	public void setRegio(String regio)
	{
		this.regio = regio;
	}

	public String getMonsterId()
	{
		return monsterId;
	}

	public void setMonsterId(String monsterId)
	{
		this.monsterId = monsterId;
	}

	public Date getVerrichtingsDatum()
	{
		return verrichtingsDatum;
	}

	public void setVerrichtingsDatum(Date verrichtingsDatum)
	{
		this.verrichtingsDatum = verrichtingsDatum;
	}

	public Locatie getHuisartsLocatie()
	{
		return huisartsLocatie;
	}

	public void setHuisartsLocatie(Locatie huisartsLocatie)
	{
		this.huisartsLocatie = huisartsLocatie;
	}

	public String getClientNaam()
	{
		return clientNaam;
	}

	public void setClientNaam(String clientNaam)
	{
		this.clientNaam = clientNaam;
	}

	public Date getDatumUitstrijkje()
	{
		return datumUitstrijkje;
	}

	public void setDatumUitstrijkje(Date datumUitstrijkje)
	{
		this.datumUitstrijkje = datumUitstrijkje;
	}

	public Date getFormulierOntvangstDatum()
	{
		return formulierOntvangstDatum;
	}

	public void setFormulierOntvangstDatum(Date formulierOntvangstDatum)
	{
		this.formulierOntvangstDatum = formulierOntvangstDatum;
	}

	public Huisarts getHuisarts()
	{
		return huisarts;
	}

	public void setHuisarts(Huisarts huisarts)
	{
		this.huisarts = huisarts;
	}
}
